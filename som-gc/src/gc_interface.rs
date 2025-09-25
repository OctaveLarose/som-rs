use crate::api::{
    mmtk_bind_mutator, mmtk_destroy_mutator, mmtk_handle_user_collection_request, mmtk_initialize_collection, mmtk_set_fixed_heap_size,
    mmtk_used_bytes,
};
use crate::gcref::Gc;
use crate::gcslice::GcSlice;
use crate::object_model::OBJECT_REF_OFFSET;
use crate::slot::SOMSlot;
use crate::{MMTK_SINGLETON, MMTK_TO_VM_INTERFACE, MUTATOR_WRAPPER, SOMVM};
use core::mem::size_of;
use log::debug;
use mmtk::util::alloc::Allocator;
#[cfg(feature = "marksweep")]
use mmtk::util::alloc::FreeListAllocator;
#[cfg(feature = "semispace")]
use mmtk::util::alloc::{BumpAllocator, BumpPointer};
use mmtk::util::constants::MIN_OBJECT_SIZE;
use mmtk::util::{Address, ObjectReference, OpaquePointer, VMMutatorThread, VMThread};
use mmtk::vm::SlotVisitor;
use mmtk::{memory_manager, AllocationSemantics, MMTKBuilder, Mutator};
use num_bigint::BigInt;
#[cfg(feature = "track-allocations")]
use std::collections::HashMap;
use std::sync::{Condvar, LazyLock, Mutex};
use std::time::{Duration, Instant};

#[cfg(not(any(feature = "marksweep", feature = "semispace")))]
compile_error!("Either marksweep or semispace must be enabled for this crate.");

#[cfg(all(feature = "semispace", feature = "marksweep"))]
compile_error!("Several GC strategies enabled: only one is allowed at a time.");

pub static WORLD_LOCK: LazyLock<(Mutex<bool>, Condvar)> = LazyLock::new(|| (Mutex::new(false), Condvar::new()));

static GC_OFFSET: usize = 0;
static GC_ALIGN: usize = 8;
// static GC_SEMANTICS: AllocationSemantics = AllocationSemantics::Default;

/// TODO rename, maybe MutatorWrapper
pub struct GCInterface {
    mutator: Box<Mutator<SOMVM>>,
    #[cfg(feature = "marksweep")]
    default_allocator: *mut FreeListAllocator<SOMVM>,
    #[cfg(feature = "semispace")]
    default_allocator: *mut mmtk::util::alloc::BumpAllocator<SOMVM>,
    #[cfg(feature = "semispace")]
    #[allow(unused)]
    alloc_bump_ptr: BumpPointer,
    mutator_thread: VMMutatorThread,
    is_collecting: bool,
    start_the_world_count: usize,
    total_gc_time: Duration,

    #[cfg(feature = "track-allocations")]
    pub total_program_repr_size: u128, // public as a hack
    #[cfg(feature = "track-allocations")]
    pub total_other_memory_size: u128, // this + program repr should account for all allocations
    #[cfg(feature = "track-allocations")]
    pub alloc_map: HashMap<AllocSiteMarker, usize>,
}

impl Drop for GCInterface {
    fn drop(&mut self) {
        // mmtk_handle_user_collection_request(self.mutator_thread);
        mmtk_destroy_mutator(self.mutator.as_mut())
    }
}

/// Callbacks used to provide MMTk->VM communication.
pub struct MMTKtoVMCallbacks {
    /// Scans an object. Needed for tracing.
    pub scan_object: fn(ObjectReference, &mut dyn SlotVisitor<SOMSlot>),
    /// Get the VM roots.
    pub get_roots_in_mutator_thread: fn(&mut Mutator<SOMVM>) -> Vec<SOMSlot>,
    /// Adapt an object after being copied elsewhere (not really at the moment needed except in one case)
    pub adapt_post_copy: fn(ObjectReference, ObjectReference),
    /// Get the size of the object. Needed when copying it
    pub get_object_size: fn(ObjectReference) -> usize,
}

impl GCInterface {
    /// Initialize the GCInterface. Internally inits MMTk and fetches everything needed to actually communicate with the GC.
    pub fn init<'a>(heap_size: usize, vm_callbacks: MMTKtoVMCallbacks) -> &'a mut Self {
        let (mutator_thread, mutator) = Self::init_mmtk(heap_size);
        #[cfg(feature = "marksweep")]
        let default_allocator = Self::get_default_allocator::<FreeListAllocator<SOMVM>>(mutator.as_ref());
        #[cfg(feature = "semispace")]
        let default_allocator = Self::get_default_allocator::<BumpAllocator<SOMVM>>(mutator.as_ref());

        let self_ = Box::new(Self {
            mutator_thread,
            mutator,
            is_collecting: false,
            default_allocator,
            #[cfg(feature = "semispace")]
            alloc_bump_ptr: BumpPointer::default(),
            start_the_world_count: 0,
            total_gc_time: Duration::new(0, 0),

            #[cfg(feature = "track-allocations")]
            total_program_repr_size: 0,
            #[cfg(feature = "track-allocations")]
            total_other_memory_size: 0,
            #[cfg(feature = "track-allocations")]
            alloc_map: HashMap::new(),
        });

        let gc_interface_ptr = Box::leak(self_);

        unsafe {
            // in the context of tests, this function gets invoked many times, so they can have already been initialized.
            // TODO: which makes me realize that this function's structure is subpar. Why do we return a NEW GCInterface at all, then?
            // The universe should likely use a reference to the OnceCell, or something... That'd be better.

            if MUTATOR_WRAPPER.get().is_none() {
                // very unsafe, very ugly: we duplicate a mutable reference to the GC interface ptr. need to avoid by implementing above idea
                let dup_ptr = &mut *(gc_interface_ptr as *mut GCInterface);
                MUTATOR_WRAPPER.set(dup_ptr).unwrap_or_else(|_| panic!("couldn't set mutator wrapper?"));
            }

            if MMTK_TO_VM_INTERFACE.get().is_none() {
                MMTK_TO_VM_INTERFACE.get_or_init(|| vm_callbacks);
            }
        }

        gc_interface_ptr
    }

    /// Initialize MMTk, and get from it all the info we need to initialize our interface
    fn init_mmtk(heap_size: usize) -> (VMMutatorThread, Box<Mutator<SOMVM>>) {
        let builder: MMTKBuilder = {
            let mut builder = MMTKBuilder::new();

            let heap_success = mmtk_set_fixed_heap_size(&mut builder, heap_size);
            assert!(heap_success, "Couldn't set MMTk fixed heap size");

            if cfg!(feature = "marksweep") {
                assert!(builder.set_option("plan", "MarkSweep"));
            } else if cfg!(feature = "semispace") {
                assert!(builder.set_option("plan", "SemiSpace"));
            } else {
                panic!("No GC plan set!")
            }

            #[cfg(feature = "stress_test")]
            assert!(builder.set_option("stress_factor", "4000000"));

            builder
        };

        if MMTK_SINGLETON.get().is_none() {
            MMTK_SINGLETON
                .set({
                    let mmtk = mmtk::memory_manager::mmtk_init::<SOMVM>(&builder);
                    *mmtk
                })
                .unwrap_or_else(|_| panic!("couldn't set the MMTk singleton"));

            mmtk_initialize_collection(VMThread(OpaquePointer::UNINITIALIZED));
        }

        let tls = VMMutatorThread(VMThread(OpaquePointer::UNINITIALIZED));
        let mutator = mmtk_bind_mutator(tls);

        (tls, mutator)
    }

    fn get_default_allocator<T>(mutator: &Mutator<SOMVM>) -> *mut T
    where
        T: Allocator<SOMVM>,
    {
        let selector = memory_manager::get_allocator_mapping(MMTK_SINGLETON.get().unwrap(), AllocationSemantics::Default);
        let default_allocator_offset = Mutator::<SOMVM>::get_allocator_base_offset(selector);

        // At run time: allocate with the default semantics without resolving allocator
        let default_allocator: *mut T = {
            let mutator_addr = Address::from_ref(mutator);
            unsafe {
                let allocator_ptr = mutator_addr + default_allocator_offset;
                allocator_ptr.as_mut_ref()
            }
        };

        default_allocator
    }

    /// Dispatches a manual collection request to MMTk.
    pub fn full_gc_request(&self) -> bool {
        mmtk_handle_user_collection_request(self.mutator_thread)
    }

    /// Returns the number of total GC collections.
    pub fn get_nbr_collections(&self) -> usize {
        self.start_the_world_count
    }

    /// Returns the number of used bytes
    pub fn get_used_bytes(&self) -> usize {
        mmtk_used_bytes()
    }

    /// Returns the total time spent performing GC.
    pub fn get_total_gc_time(&self) -> u128 {
        self.total_gc_time.as_millis()
    }

    /// Whether or not we're currently performing GC.
    /// Might be redundant with `is_world_stopped`, to be honest.
    pub fn is_currently_collecting(&self) -> bool {
        self.is_collecting
    }

    /// Block the main thread to perform GC.
    pub(crate) fn block_for_gc(&mut self, _tls: VMMutatorThread) {
        debug!("block_for_gc: stopping the world!");
        self.is_collecting = true;

        let (is_world_stopped, cvar) = &*WORLD_LOCK;
        {
            let mut lock = is_world_stopped.lock().unwrap();
            *lock = true;
        }

        let time_pre_gc = Instant::now();

        let result = cvar.wait_timeout_while(is_world_stopped.lock().unwrap(), Duration::from_secs(15), |pending| *pending).unwrap();
        if result.1.timed_out() {
            panic!("GC timed out: highly likely to be a crash in a GC thread.")
        }

        debug!("block_for_gc: world no longer stopped.");
        self.is_collecting = false;
        self.total_gc_time += Instant::now() - time_pre_gc;
    }

    pub(crate) fn stop_all_mutators<F>(&'static mut self, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<SOMVM>),
    {
        debug!("stop_all_mutators called");

        //while !AtomicBool::load(&IS_WORLD_STOPPED, Ordering::SeqCst) {
        //    // wait for world to be properly stopped (might not be needed)
        //}

        mutator_visitor(self.mutator.as_mut())
    }

    pub(crate) fn resume_mutators(&mut self) {
        debug!("resuming mutators.");
        self.start_the_world_count += 1;

        let (is_world_stopped, cvar) = &*WORLD_LOCK;
        let mut pending = is_world_stopped.lock().unwrap();
        *pending = false;
        cvar.notify_one();
    }

    pub(crate) fn get_mutator(&mut self, _tls: VMMutatorThread) -> &mut Mutator<SOMVM> {
        self.mutator.as_mut()
    }

    pub(crate) fn get_all_mutators(&mut self) -> Box<dyn Iterator<Item = &mut Mutator<SOMVM>> + '_> {
        debug!("calling get_all_mutators");
        Box::new(std::iter::once(self.mutator.as_mut()))
    }
}

/// Explicitly mentions what an allocation was requested for.
/// The intent is to help debugging: we can track where GC was triggered, and what triggered it. This is all to find GC bugs, and also track memory usage for experiments.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum AllocSiteMarker {
    AstFrame,
    MethodFrame,
    MethodFrameWithArgs,
    InitMethodFrame,
    Block,
    BlockFrame,
    Instance,
    Method,
    BlockMethod,
    Class,
    String,
    StringLiteral,
    BigInt,
    SliceAstExpression,
    VecValue,
    VecBCLiteral,
    SliceAstLiteral,
    RuntimeBlock,
}

/// To save on some space in the trait itself. Bit overkill, probably
pub trait SliceConstraint: SupportedSliceType + std::fmt::Debug {}
impl<T> SliceConstraint for T where T: SupportedSliceType + std::fmt::Debug {}

/// All functions necessary to allocate memory from within som-rs.
pub trait SOMAllocator {
    fn request_memory_for_type<T>(&mut self, type_size: usize, alloc_origin_marker: AllocSiteMarker) -> Gc<T>
    where
        T: HasTypeInfoForGC;
    fn request_bytes(&mut self, size: usize, _alloc_origin_marker: AllocSiteMarker) -> Address;
    fn request_bytes_for_slice(&mut self, slice_size: usize, alloc_origin_marker: AllocSiteMarker) -> Address;
    fn request_bytes_los(&mut self, size: usize, _alloc_origin_marker: AllocSiteMarker) -> Address;

    // #[deprecated(note="use alloc_with_marker instead")]
    fn alloc<T>(&mut self, obj: T, alloc_origin_marker: AllocSiteMarker) -> Gc<T>
    where
        T: HasTypeInfoForGC;
    fn alloc_with_size<T>(&mut self, obj: T, size: usize, alloc_origin_marker: AllocSiteMarker) -> Gc<T>
    where
        T: HasTypeInfoForGC;

    // Methods for allocating slices.
    fn alloc_safe_slice<T>(&mut self, obj: &[T], alloc_origin_marker: AllocSiteMarker) -> GcSlice<T>
    where
        T: SliceConstraint;
    fn write_slice_to_addr<T>(&mut self, slice_header_addr: Address, obj: &[T]) -> GcSlice<T>
    where
        T: SupportedSliceType + std::fmt::Debug;

    //#[deprecated]
    fn alloc_slice<T>(&mut self, obj: &[T], alloc_origin_marker: AllocSiteMarker) -> GcSlice<T>
    where
        T: SupportedSliceType + std::fmt::Debug;
}

impl SOMAllocator for GCInterface {
    /// Allocates a type on the heap and returns a pointer to it.
    /// Considers that the provided object's size can be trivially inferred with a `size_of` call (which isn't the case for all of our objects, e.g. frames)
    fn alloc<T: HasTypeInfoForGC>(&mut self, obj: T, alloc_origin_marker: AllocSiteMarker) -> Gc<T> {
        self.alloc_with_size(obj, size_of::<T>(), alloc_origin_marker)
    }

    /// Allocates a type, but with a given size.
    fn alloc_with_size<T: HasTypeInfoForGC>(&mut self, obj: T, size: usize, alloc_origin_marker: AllocSiteMarker) -> Gc<T> {
        debug_assert!(size >= MIN_OBJECT_SIZE);

        // adding VM header size (type info) to amount we allocate
        let header_addr = self.request_bytes(size + OBJECT_REF_OFFSET, alloc_origin_marker);

        debug_assert!(!header_addr.is_zero());
        let obj_addr = SOMVM::object_start_to_ref(header_addr);

        // AFAIK, this is not needed.
        // mmtk_post_alloc(mutator, SOMVM::object_start_to_ref(addr), size, GC_SEMANTICS);

        unsafe {
            *header_addr.as_mut_ref() = T::get_magic_gc_id();
            *(obj_addr.to_raw_address().as_mut_ref()) = obj;
        }

        Gc::from(obj_addr.to_raw_address())
    }

    /// Allocates a slice that only contains values that ARE NOT pointers.
    /// Allocating a Vec<i32> is fine, allocating a Vec<Value> is fine if they're all Integer values.
    /// Not unforced by the Rust type system atm, but we could make some nice traits for this. Just afraid that this would add unnecessary complexity.
    fn alloc_safe_slice<T: SupportedSliceType + std::fmt::Debug>(&mut self, obj: &[T], alloc_origin_marker: AllocSiteMarker) -> GcSlice<T> {
        let header_addr = self.request_bytes_for_slice(std::mem::size_of_val(obj), alloc_origin_marker);
        self.write_slice_to_addr(header_addr, obj)
    }

    /// Allocates a type on the heap and returns a pointer to it.
    /// Deprecated because too likely to be unsafe: GC triggered when allocating a slice makes the
    /// slice likely to be invalid.
    /// Now every uses should be replaced with alloc_safe_slice, or with `request_mem_for_slice` + `write_slice_to_addr`
    fn alloc_slice<T: SupportedSliceType + std::fmt::Debug>(&mut self, obj: &[T], alloc_origin_marker: AllocSiteMarker) -> GcSlice<T> {
        let header_addr = self.request_bytes_for_slice(std::mem::size_of_val(obj), alloc_origin_marker);
        self.write_slice_to_addr(header_addr, obj)
    }

    fn write_slice_to_addr<T: SupportedSliceType + std::fmt::Debug>(&mut self, slice_header_addr: Address, obj: &[T]) -> GcSlice<T> {
        let len_addr = SOMVM::object_start_to_ref(slice_header_addr);
        let obj_addr = len_addr.to_raw_address().add(size_of::<usize>());

        unsafe {
            *slice_header_addr.as_mut_ref() = T::get_magic_gc_slice_id();
            *len_addr.to_raw_address().as_mut_ref() = obj.len();
            std::ptr::copy_nonoverlapping(obj.as_ptr(), obj_addr.as_mut_ref(), obj.len());
        }

        GcSlice::new(len_addr.to_raw_address())
    }

    #[cfg(feature = "marksweep")]
    /// Request `size` bytes from MMTk.
    /// Importantly, this MAY TRIGGER A COLLECTION. Which means any function that relies on it must be mindful of this,
    /// such as by making sure no arguments are dangling on the Rust stack away from the GC's reach.
    fn request_bytes(&mut self, size: usize, _alloc_origin_marker: AllocSiteMarker) -> Address {
        unsafe { &mut (*self.default_allocator) }.alloc(size, GC_ALIGN, GC_OFFSET)
        // slow path, for debugging
        // crate::api::mmtk_alloc(&mut self.mutator, size, GC_ALIGN, GC_OFFSET, AllocationSemantics::Default)
    }

    #[cfg(feature = "semispace")]
    /// Request `size` bytes from MMTk.
    /// Importantly, this MAY TRIGGER A COLLECTION. Which means any function that relies on it must be mindful of this,
    /// such as by making sure no arguments are dangling on the Rust stack away from the GC's reach.
    fn request_bytes(&mut self, size: usize, _alloc_origin_marker: AllocSiteMarker) -> Address {
        //unsafe { &mut (*self.default_allocator) }.alloc(size, GC_ALIGN, GC_OFFSET)

        #[cfg(feature = "track-allocations")]
        {
            use AllocSiteMarker::*;
            match _alloc_origin_marker {
                AstFrame | Instance | MethodFrame | MethodFrameWithArgs | InitMethodFrame | BlockFrame | String | VecValue | BigInt
                | SliceAstLiteral | RuntimeBlock => self.total_other_memory_size += size as u128,
                Block | Method | BlockMethod | Class | SliceAstExpression | VecBCLiteral | StringLiteral => {
                    self.total_program_repr_size += size as u128
                }
            }
            *self.alloc_map.entry(_alloc_origin_marker).or_insert(0) += 1;
        }

        let _gc_watcher = self.start_the_world_count;

        // Release builds must not assume this value is unchanging: it can, that's the point of the check later on.
        std::hint::black_box(&self.start_the_world_count);

        let addr = unsafe { &mut (*self.default_allocator) }.alloc(size, GC_ALIGN, GC_OFFSET);
        std::sync::atomic::fence(std::sync::atomic::Ordering::Release);

        #[cfg(debug_assertions)]
        if self.start_the_world_count > _gc_watcher {
            println!("GC was triggered after allocating a {:?}", _alloc_origin_marker)
        }

        addr

        // TODO: this code should work, and -does-, but sometimes returns references to the old space, as far as i can tell.
        // code taken from MMTk docs. https://docs.mmtk.io/portingguide/perf_tuning/alloc.html#option-3-embed-the-fast-path-struct
        // let new_cursor = self.alloc_bump_ptr.cursor + size;
        // if new_cursor < self.alloc_bump_ptr.limit {
        //     let addr = self.alloc_bump_ptr.cursor;
        //     self.alloc_bump_ptr.cursor = new_cursor;
        //     addr
        // } else {
        //     let default_allocator = unsafe { &mut *self.default_allocator };
        //     default_allocator.bump_pointer = self.alloc_bump_ptr;
        //     let addr = default_allocator.alloc(size, GC_ALIGN, GC_OFFSET);
        //     // Copy bump pointer values to the fastpath BumpPointer so we will have an allocation buffer.
        //     self.alloc_bump_ptr = default_allocator.bump_pointer;
        //     addr
        // }
    }

    fn request_bytes_los(&mut self, size: usize, _alloc_origin_marker: AllocSiteMarker) -> Address {
        debug_assert!(
            size >= crate::mmtk().get_plan().constraints().max_non_los_default_alloc_bytes,
            "Requesting LOS for a non large object"
        );
        crate::api::mmtk_alloc(&mut self.mutator, size, GC_ALIGN, GC_OFFSET, AllocationSemantics::Los)
    }

    /// TODO doc + should likely deduce the size from the type
    fn request_memory_for_type<T: HasTypeInfoForGC>(&mut self, type_size: usize, alloc_origin_marker: AllocSiteMarker) -> Gc<T> {
        let mut bytes = self.request_bytes(type_size + OBJECT_REF_OFFSET, alloc_origin_marker);
        unsafe {
            *bytes.as_mut_ref::<u8>() = T::get_magic_gc_id();
            bytes += OBJECT_REF_OFFSET;
            bytes.into()
        }
    }

    fn request_bytes_for_slice(&mut self, slice_size: usize, alloc_origin_marker: AllocSiteMarker) -> Address {
        let mut size = {
            match slice_size {
                v if v < MIN_OBJECT_SIZE => MIN_OBJECT_SIZE,
                v => v,
            }
        };

        size += std::mem::size_of::<usize>(); // size stored at the start

        // slices can be big enough to warrant using large object storage.
        let header_addr = {
            match size <= crate::mmtk().get_plan().constraints().max_non_los_default_alloc_bytes {
                true => self.request_bytes(size + OBJECT_REF_OFFSET, alloc_origin_marker),
                false => self.request_bytes_los(size + OBJECT_REF_OFFSET, alloc_origin_marker),
            }
        };

        header_addr
    }
}

// ------------------

/// Implements a per-type magic number.
/// GC needs to access type info from raw ObjectReference types, so data that gets put on the GC heap has an associated type ID that gets put in a per-allocation header.
pub trait HasTypeInfoForGC {
    fn get_magic_gc_id() -> u8;
}

pub const STRING_MAGIC_ID: u8 = 10;
pub const BIGINT_MAGIC_ID: u8 = 11;

impl HasTypeInfoForGC for String {
    fn get_magic_gc_id() -> u8 {
        STRING_MAGIC_ID
    }
}
impl HasTypeInfoForGC for BigInt {
    fn get_magic_gc_id() -> u8 {
        BIGINT_MAGIC_ID
    }
}

//impl<T> HasTypeInfoForGC for GCSlice<T> {
//    fn get_magic_gc_id() -> u8 {
//        GCSLICE_MAGIC_ID
//    }
//}

pub trait SupportedSliceType {
    fn get_magic_gc_slice_id() -> u8;
}

impl<T: SupportedSliceType> HasTypeInfoForGC for GcSlice<T> {
    fn get_magic_gc_id() -> u8 {
        T::get_magic_gc_slice_id()
    }
}
