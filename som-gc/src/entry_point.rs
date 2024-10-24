use mmtk::{memory_manager, AllocationSemantics, Mutator};
use mmtk::util::Address;
use mmtk::util::alloc::BumpAllocator;
use crate::api::*;
use mmtk::util::opaque_pointer::*;
use crate::{SINGLETON, SOMVM};

pub fn init_gc() -> (VMMutatorThread, Box<Mutator<SOMVM>>, *mut BumpAllocator<SOMVM>) {
// pub fn init_gc() -> (VMMutatorThread, Box<Mutator<SOMVM>>) {
    if SINGLETON.get().is_none() {
        let mut builder = mmtk_create_builder();

        // let heap_success = mmtk_set_fixed_heap_size(&mut builder, 1048576);
        // assert!(heap_success, "Couldn't set MMTk fixed heap size");

        // let gc_success = builder.set_option("plan", "SemiSpace");
        let gc_success = builder.set_option("plan", "NoGC");
        assert!(gc_success, "Couldn't set GC plan");

        // let ok = builder.set_option("stress_factor", DEFAULT_STRESS_FACTOR.to_string().as_str());
        // assert!(ok);
        // let ok = builder.set_option("analysis_factor", DEFAULT_STRESS_FACTOR.to_string().as_str());
        // assert!(ok);

        mmtk_init(&mut builder);
        // let worked_thread = VMWorkerThread(VMThread(OpaquePointer::UNINITIALIZED));
        mmtk_initialize_collection(VMThread(OpaquePointer::UNINITIALIZED));
    }

    let tls = VMMutatorThread(VMThread(OpaquePointer::UNINITIALIZED)); // TODO: do I need a thread pointer here?
    let mutator = mmtk_bind_mutator(tls);

    let selector = memory_manager::get_allocator_mapping(
        SINGLETON.get().unwrap(),
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