use crate::block::{Block, BlockInfo};
use crate::class::Class;
use crate::frame::Frame;
use crate::gc::api::{mmtk_alloc, mmtk_bind_mutator, mmtk_destroy_mutator, mmtk_handle_user_collection_request, mmtk_initialize_collection};
use crate::gc::object_model::{GCMagicId, OBJECT_REF_OFFSET};
use crate::gc::scanning::value_to_slot;
use crate::gc::{SOMSlot, MMTK_HAS_RAN_INIT_COLLECTION, MMTK_SINGLETON, SOMVM};
use crate::instance::Instance;
use crate::method::Method;
use crate::value::Value;
use crate::{INTERPRETER_RAW_PTR, UNIVERSE_RAW_PTR};
use core::mem::size_of;
use log::debug;
use mmtk::util::alloc::{Allocator, BumpAllocator};
use mmtk::util::constants::MIN_OBJECT_SIZE;
use mmtk::util::{Address, OpaquePointer, VMMutatorThread, VMThread};
use mmtk::vm::RootsWorkFactory;
use mmtk::{memory_manager, AllocationSemantics, Mutator};
use num_bigint::BigInt;
use std::collections::{HashSet, VecDeque};
use std::marker::PhantomData;
use std::sync::atomic::Ordering;
use structopt::lazy_static;

static GC_OFFSET: usize = 0;
static GC_ALIGN: usize = 8;
static GC_SEMANTICS: AllocationSemantics = AllocationSemantics::Default;

pub struct GCInterface {
    mutator: Box<Mutator<SOMVM>>,
    mutator_thread: VMMutatorThread,
    default_allocator: *mut BumpAllocator<SOMVM>,
    start_the_world_count: usize
}

impl Drop for GCInterface {
    fn drop(&mut self) {
        mmtk_destroy_mutator(self.mutator.as_mut())
    }
}

impl GCInterface {
    /// Initialize the GCInterface. Internally inits MMTk and fetches everything needed to actually communicate with the GC.
    pub fn init() -> Self {
        let (mutator_thread, mutator, default_allocator) = Self::init_mmtk();
        Self {
            mutator_thread,
            mutator,
            default_allocator,
            start_the_world_count: 0
        }
    }

    fn init_mmtk() -> (VMMutatorThread, Box<Mutator<SOMVM>>, *mut BumpAllocator<SOMVM>) {
        lazy_static::initialize(&MMTK_SINGLETON);

        if !MMTK_HAS_RAN_INIT_COLLECTION.load(Ordering::Acquire) {
            mmtk_initialize_collection(VMThread(OpaquePointer::UNINITIALIZED));
            MMTK_HAS_RAN_INIT_COLLECTION.store(true, Ordering::Release);
        }

        let tls = VMMutatorThread(VMThread(OpaquePointer::UNINITIALIZED)); // TODO: do I need a thread pointer here?
        let mutator = mmtk_bind_mutator(tls);

        let selector = memory_manager::get_allocator_mapping(
            &MMTK_SINGLETON,
            AllocationSemantics::Default,
        );
        let default_allocator_offset = Mutator::<SOMVM>::get_allocator_base_offset(selector);

        // At run time: allocate with the default semantics without resolving allocator
        let default_allocator: *mut BumpAllocator<SOMVM> = {
            let mutator_addr = Address::from_ref(&*mutator);
            unsafe {
                let ptr = mutator_addr + default_allocator_offset;
                ptr.as_mut_ref()
                // (mutator_addr + default_allocator_offset).as_mut_ref::<BumpAllocator<SOMVM>>()
            }
        };

        // (tls, mutator)
        (tls, mutator, default_allocator)
    }

    /// Dispatches a manual collection request to MMTk.
    pub fn full_gc_request(&self) {
        mmtk_handle_user_collection_request(self.mutator_thread)
    }

    pub fn allocate<T: HasTypeInfoForGC>(&mut self, obj: T) -> GCRef<T> {
        GCRef::<T>::alloc(obj, self)
    }
}

// copied off the openjdk implem? not sure what the point of this is really
struct SOMMutatorIterator<'a> {
    mutators: VecDeque<&'a mut Mutator<SOMVM>>,
    phantom_data: PhantomData<&'a ()>,
}

impl<'a> Iterator for SOMMutatorIterator<'a> {
    type Item = &'a mut Mutator<SOMVM>;

    fn next(&mut self) -> Option<Self::Item> {
        self.mutators.pop_front()
    }
}

impl GCInterface {
    pub fn block_for_gc(&mut self) {
        debug!("block_for_gc called...");

        let before_blocking_count = self.start_the_world_count;
        while self.start_the_world_count <= before_blocking_count {
            // ... wait
        }

        debug!("block_for_gc done.");
    }

    pub fn resume_mutators(&mut self) {
        debug!("resume_mutators called");
        self.start_the_world_count += 1;
    }

    pub fn stop_all_mutators<F>(&mut self, _mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<SOMVM>),
    {
        debug!("stop_all_mutators called");
        // mutator_visitor(self.mutator.as_mut());
    }

    pub(crate) fn get_mutator(&mut self, _tls: VMMutatorThread) -> &mut Mutator<SOMVM> {
        debug_assert!(self.mutator_thread == _tls); // not even sure that's correct
        self.mutator.as_mut()
    }
    pub fn get_all_mutators(&mut self) -> Box<dyn Iterator<Item = &mut Mutator<SOMVM>> + '_> {
        debug!("calling get_all_mutators");
        // frankly not sure how to implement that one
        // Box::new(vec![self.mutator.as_mut()].iter())

        let mut mutators = VecDeque::new();
        mutators.push_back(self.mutator.as_mut()); 
        
        let iterator = SOMMutatorIterator {
            mutators,
            phantom_data: PhantomData,
        };
        
        Box::new(iterator)
        
        // unsafe { Box::from_raw(std::ptr::null_mut())}
    }

    pub fn scan_vm_specific_roots(&self, mut factory: impl RootsWorkFactory<SOMSlot> + Sized) {
        debug!("calling scan_vm_specific_roots");
        
        unsafe {
            // let mut to_process: Vec<SOMSlot> = vec![];
            let mut to_process: HashSet<SOMSlot> = HashSet::new();

            let current_frame_addr = &(*INTERPRETER_RAW_PTR).current_frame;
            debug!("scanning root: current_frame (method: {})", current_frame_addr.to_obj().current_method.to_obj().signature);
            to_process.insert(SOMSlot::from_address(Address::from_ref(current_frame_addr)));

            for global in (*UNIVERSE_RAW_PTR).globals.values() {
                match value_to_slot(global) {
                    Some(slot) => {to_process.insert(slot);},
                    None => {}
                }
            }

            {
                let core = &(*UNIVERSE_RAW_PTR).core;

                let core_class_refs = [
                    &core.object_class,
                    &core.class_class,
                    &core.metaclass_class,
                    &core.nil_class,
                    &core.integer_class,
                    &core.double_class,
                    &core.array_class,
                    &core.method_class,
                    &core.primitive_class,
                    &core.symbol_class,
                    &core.string_class,
                    &core.system_class,
                    &core.block_class,
                    &core.block1_class,
                    &core.block2_class,
                    &core.block3_class,
                    &core.boolean_class,
                    &core.true_class,
                    &core.false_class,
                ];
                
                for core_cls in core_class_refs {
                    let addr = Address::from_ref(core_cls);
                    let slot = SOMSlot::from_address(addr);
                    to_process.insert(slot);
                }
            }
            debug!("scanning roots: all core classes");

            let to_process_vec: Vec<SOMSlot> = to_process.iter().map(|e| e.clone()).collect();
            factory.create_process_roots_work(to_process_vec)
        }
    }

    pub fn scan_roots_in_mutator_thread(&self, _mutator: &mut Mutator<SOMVM>, _factory: impl RootsWorkFactory<SOMSlot> + Sized) {
        debug!("calling scan_roots_in_mutator_thread (DOES NOTHING AT THE MOMENT");
    }
}

// ------

/// A pointer to the heap for GC.
#[derive(Debug)]
pub struct GCRef<T> {
    pub ptr: Address,
    pub _phantom: PhantomData<T>,
}

impl<T> Clone for GCRef<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for GCRef<T> {}

// Ugly, but sometimes we want a placeholder. Code may be refactorable to never need this though, I think.
impl<T> Default for GCRef<T> {
    fn default() -> Self {
        unsafe {
            GCRef {
                ptr: Address::from_usize(0),
                _phantom: PhantomData,
            }
        }
    }
}

impl<T> PartialEq for GCRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

// -------------------------------------

impl<T> GCRef<T> {
    /// Turn a GC pointer back into the type itself (as a reference)
    pub fn to_obj(&self) -> &mut T {
        debug_assert!(!self.ptr.is_zero());
        unsafe { &mut *(self.ptr.as_mut_ref()) }
        // unsafe { &mut *(self.ptr.to_mut_ptr::<T>()) }
    }

    #[inline(always)]
    pub fn as_ref(&self) -> &T {
        debug_assert!(!self.ptr.is_zero());
        unsafe { self.ptr.as_ref() }
    }
    
    /// Hacks for convenience, since I'm refactoring from Refcounts. TODO remove
    #[inline(always)]
    pub fn borrow(&self) -> &mut T {
        Self::to_obj(self)
    }

    /// same deal
    #[inline(always)]
    pub fn borrow_mut(&self) -> &mut T {
        Self::to_obj(self)
    }

    /// same dealll
    #[inline(always)]
    pub fn as_ptr(&self) -> &mut T {
        Self::to_obj(self)
    }

    /// Does the address not point to any data?
    /// We use this to avoid using an Option type in interpreter frames. Not sure if it's worth it though.
    #[inline(always)]
    pub fn is_empty(&self) -> bool { self.ptr.is_zero() }

    /// Convert a u64 into an address. Useful since we use NaN boxing, which returns values as 64 bits.
    #[inline(always)]
    pub fn from_u64(ptr: u64) -> GCRef<T> {
        unsafe {
            GCRef {
                ptr: Address::from_usize(ptr as usize),
                _phantom: PhantomData
            }
        }
    }
}

impl<T: HasTypeInfoForGC> GCRef<T> {
    // Allocates a type on the heap and returns a pointer to it.
    pub fn alloc(obj: T, gc_interface: &mut GCInterface) -> GCRef<T> {
        Self::alloc_with_size(obj, gc_interface, size_of::<T>())
    }

    // Allocates a type, but with a given size. Useful when an object needs more than what we tell Rust through defining a struct. 
    // (e.g. Value arrays stored directly in the heap - see BC Frame)
    pub fn alloc_with_size(obj: T, gc_interface: &mut GCInterface, size: usize) -> GCRef<T> {
        // Self::alloc_with_size_cached_allocator(obj, gc_interface, size)
        Self::alloc_with_size_allocator_uncached(obj, gc_interface, size)
    }

    #[inline(always)]
    // #[allow(dead_code, unused)]
    fn alloc_with_size_allocator_uncached(obj: T, gc_interface: &mut GCInterface, size: usize) -> GCRef<T> {
        debug_assert!(size >= MIN_OBJECT_SIZE);
        let mutator = gc_interface.mutator.as_mut();

        // TODO: not sure that's correct? adding VM header size (type info) to amount we allocate.
        let size = size + OBJECT_REF_OFFSET;
        
        let header_addr = mmtk_alloc(mutator, size, GC_ALIGN, GC_OFFSET, GC_SEMANTICS);
        debug_assert!(!header_addr.is_zero());
        let obj_addr = SOMVM::object_start_to_ref(header_addr);

        // AFAIK, this is not needed.
        // mmtk_post_alloc(mutator, SOMVM::object_start_to_ref(addr), size, GC_SEMANTICS);

        unsafe {
            // *addr.as_mut_ref() = obj;
            *header_addr.as_mut_ref() = T::get_magic_gc_id();
            *(obj_addr.to_raw_address().as_mut_ref()) = obj;
        }

        GCRef {
            ptr: obj_addr.to_raw_address(),
            _phantom: PhantomData,
        }
    }

    #[allow(dead_code, unused)]
    fn alloc_with_size_cached_allocator(obj: T, gc_interface: &mut GCInterface, size: usize) -> GCRef<T> {
        todo!("should not be ran before being adapted to match the cached version");
        
        debug_assert!(size >= MIN_OBJECT_SIZE);
        let allocator = unsafe {&mut (*gc_interface.default_allocator)};
        
        // TODO: not sure that's correct? adding VM header size (type info) to amount we allocate.
        let size = size + OBJECT_REF_OFFSET;
        
        let addr = allocator.alloc(size, GC_ALIGN, GC_OFFSET);
        debug_assert!(!addr.is_zero());
        let obj_addr = SOMVM::object_start_to_ref(addr);


        // let obj = SOMVM::object_start_to_ref(addr);
        // let space = allocator.get_space();
        // debug_assert!(!obj.to_raw_address().is_zero());
        // space.initialize_object_metadata(obj, true);
        
        let space = allocator.get_space();
        dbg!(space.name());
        space.initialize_object_metadata(obj_addr, true);

        dbg!("wa");
        unsafe {
            // *addr.as_mut_ref() = obj;

            // dbg!(addr);
            // dbg!(obj_addr);
            // dbg!();
            let header_ref: *mut usize = addr.as_mut_ref();
            *header_ref = 4774451407313061000; // 4242424242424242
            
            *(obj_addr.to_raw_address().as_mut_ref()) = obj;
            // obj_addr.to_header()
        }

        GCRef {
            ptr: obj_addr.to_raw_address(),
            _phantom: PhantomData,
        }
    }
}

/// Custom alloc function.
/// 
/// Exists for that traits to be able to choose how to allocate their data. 
/// Must call GCRef::<T>::alloc(_with_size) internally to get a GCRef, but I can't strictly enforce that with Rust's type system.
/// In practice, that's usually allowing for more memory than Rust might be able to infer from the struct size, and filling it with our own data. 
pub trait CustomAlloc<T> {
    fn alloc(obj: T, mutator: &mut GCInterface) -> GCRef<T>;
}

// for convenience, but easily removable
impl GCRef<String> {
    pub fn as_str(&self) -> &str {
        self.to_obj().as_str()
    }
}

pub trait HasTypeInfoForGC {
    fn get_magic_gc_id() -> GCMagicId;
}

impl HasTypeInfoForGC for String {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::String
    }
}
impl HasTypeInfoForGC for BigInt {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::BigInt
    }
}

impl HasTypeInfoForGC for Vec<u8> {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::ArrayU8
    }
}

impl HasTypeInfoForGC for Vec<Value> {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::ArrayVal
    }
}

impl HasTypeInfoForGC for BlockInfo {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::BlockInfo
    }
}

impl HasTypeInfoForGC for Instance {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::Instance
    }
}

impl HasTypeInfoForGC for Method {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::Method
    }
}

impl HasTypeInfoForGC for Block {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::Block
    }
}

impl HasTypeInfoForGC for Class {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::Class
    }
}

impl HasTypeInfoForGC for Frame {
    fn get_magic_gc_id() -> GCMagicId {
        GCMagicId::Frame
    }
}