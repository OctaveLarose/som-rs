use crate::block::{Block, BlockInfo};
use crate::class::Class;
use crate::frame::Frame;
use crate::gc::api::{mmtk_alloc, mmtk_bind_mutator, mmtk_destroy_mutator, mmtk_handle_user_collection_request, mmtk_initialize_collection, mmtk_is_in_mmtk_spaces, mmtk_set_fixed_heap_size};
use crate::gc::object_model::{GCMagicId, OBJECT_REF_OFFSET};
use crate::gc::scanning::value_to_slot;
use crate::gc::{SOMSlot, MMTK_SINGLETON, SOMVM};
use crate::instance::Instance;
use crate::method::Method;
use crate::value::Value;
use crate::{INTERPRETER_RAW_PTR, UNIVERSE_RAW_PTR};
use core::mem::size_of;
use log::debug;
use mmtk::util::alloc::{Allocator, BumpAllocator};
use mmtk::util::constants::MIN_OBJECT_SIZE;
use mmtk::util::{Address, OpaquePointer, VMMutatorThread, VMThread};
use mmtk::vm::slot::Slot;
use mmtk::vm::RootsWorkFactory;
use mmtk::{memory_manager, AllocationSemantics, MMTKBuilder, Mutator};
use num_bigint::BigInt;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::sync::atomic::{AtomicBool, Ordering};

static GC_OFFSET: usize = 0;
static GC_ALIGN: usize = 8;
static GC_SEMANTICS: AllocationSemantics = AllocationSemantics::Default;

pub static IS_WORLD_STOPPED: AtomicBool = AtomicBool::new(false);

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
        let builder: MMTKBuilder = {
            let mut builder = MMTKBuilder::new();
            
            let heap_success = mmtk_set_fixed_heap_size(&mut builder, 1024 * 1024);
            assert!(heap_success, "Couldn't set MMTk fixed heap size");

            // let gc_success = builder.set_option("plan", "NoGC");
            let gc_success = builder.set_option("plan", "MarkSweep");
            // let gc_success = builder.set_option("plan", "SemiSpace");
            assert!(gc_success, "Couldn't set GC plan");

            // let ok = builder.set_option("stress_factor", DEFAULT_STRESS_FACTOR.to_string().as_str());
            // assert!(ok);
            // let ok = builder.set_option("analysis_factor", DEFAULT_STRESS_FACTOR.to_string().as_str());
            // assert!(ok);
            
            builder
        };
        
        MMTK_SINGLETON.set({
            let mmtk = mmtk::memory_manager::mmtk_init::<SOMVM>(&builder);
            *mmtk
        }).unwrap_or_else(|_| panic!("couldn't set the MMTk singleton"));

        mmtk_initialize_collection(VMThread(OpaquePointer::UNINITIALIZED));
        

        let tls = VMMutatorThread(VMThread(OpaquePointer::UNINITIALIZED)); // TODO: do I need a thread pointer here?
        let mutator = mmtk_bind_mutator(tls);

        let selector = memory_manager::get_allocator_mapping(
            &MMTK_SINGLETON.get().unwrap(),
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
        mmtk_handle_user_collection_request(self.mutator_thread);
        Self::safepoint_maybe_pause_for_gc()
    }

    pub fn allocate<T: HasTypeInfoForGC>(&mut self, obj: T) -> GCRef<T> {
        GCRef::<T>::alloc(obj, self)
    }
}

impl GCInterface {
    pub fn block_for_gc(&mut self, _tls: VMMutatorThread) {
        AtomicBool::store(&IS_WORLD_STOPPED, true, Ordering::SeqCst);
        debug!("block_for_gc: stopped the world!");
        while AtomicBool::load(&IS_WORLD_STOPPED, Ordering::SeqCst) {}
        debug!("block_for_gc: world no longer stopped.");
    }

    pub unsafe fn resume_mutators(&mut self) {
        debug!("resuming mutators.");
        self.start_the_world_count += 1;
        AtomicBool::store(&IS_WORLD_STOPPED, false, Ordering::SeqCst);
    }

    pub fn stop_all_mutators<'a, F>(&'a mut self, mut mutator_visitor: F)
    where
        F: FnMut(&'a mut Mutator<SOMVM>),
    {
        debug!("stop_all_mutators called");

        while !AtomicBool::load(&IS_WORLD_STOPPED, Ordering::SeqCst) {
            // wait for world to be properly stopped
        }

        mutator_visitor(self.mutator.as_mut())
    }

    pub(crate) fn get_mutator(&mut self, _tls: VMMutatorThread) -> &mut Mutator<SOMVM> {
        debug_assert!(self.mutator_thread == _tls); // not even sure that's correct
        self.mutator.as_mut()
    }
    pub fn get_all_mutators(&mut self) -> Box<dyn Iterator<Item = &mut Mutator<SOMVM>> + '_> {
        debug!("calling get_all_mutators");
        Box::new(std::iter::once(self.mutator.as_mut()))
    }

    pub fn scan_vm_specific_roots(&self, _factory: impl RootsWorkFactory<SOMSlot> + Sized) {
        debug!("calling scan_vm_specific_roots (unused)");
    }

    pub fn scan_roots_in_mutator_thread(&self, _mutator: &mut Mutator<SOMVM>, mut factory: impl RootsWorkFactory<SOMSlot> + Sized) {
        debug!("calling scan_roots_in_mutator_thread");

        unsafe {
            // using a set to not have duplicate entries, but seems a tad slow and perhaps unnecessary?
            let mut to_process: HashSet<SOMSlot> = HashSet::new();

            let current_frame_addr = &(*INTERPRETER_RAW_PTR).current_frame;
            debug!("scanning root: current_frame (method: {})", current_frame_addr.to_obj().current_method.to_obj().signature);
            let slot = SOMSlot::from_address(Address::from_ref(current_frame_addr));
            to_process.insert(slot);

            for (_name, val) in (*UNIVERSE_RAW_PTR).globals.iter() {
                if let Some(gcref) = val.as_class() {
                    let addr = Address::from_ref(&gcref);
                    let slot = SOMSlot::from_address(addr);
                    debug_assert!(slot.load().is_some());
                    debug_assert!(mmtk_is_in_mmtk_spaces(slot.load().unwrap()));
                    to_process.insert(slot);
                } else {
                    // dbg!("skipping {:?}", val);
                }
            }

            let to_process_vec: Vec<SOMSlot> = to_process.drain().collect();
            factory.create_process_roots_work(to_process_vec);
            debug!("scanning roots: finished");
        }
    }
    
    // If GC is needed/ongoing, pause execution of main thread until GC is done.
    pub fn safepoint_maybe_pause_for_gc() {
        while AtomicBool::load(&IS_WORLD_STOPPED, Ordering::SeqCst) {}
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
        debug_assert_eq!(IS_WORLD_STOPPED.load(Ordering::SeqCst), false);
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

        // not sure that's correct? adding VM header size (type info) to amount we allocate.
        let size = size + OBJECT_REF_OFFSET;
        
        let header_addr = mmtk_alloc(mutator, size, GC_ALIGN, GC_OFFSET, GC_SEMANTICS);
        GCInterface::safepoint_maybe_pause_for_gc();
        
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
        
        // not sure that's correct? adding VM header size (type info) to amount we allocate.
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

/// Implements a per-type magic number.
/// GC needs to access type info from raw ObjectReference types, so data that gets put on the GC heap has an associated type ID that gets put in a per-allocation header.
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