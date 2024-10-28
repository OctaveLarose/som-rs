use crate::gc::{mmtk, SOMVM};
use crate::MMTK_TO_VM_INTERFACE;
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::vm::Collection;
use mmtk::vm::GCThreadContext;
use mmtk::Mutator;

pub struct VMCollection {}

// Documentation: https://docs.mmtk.io/api/mmtk/vm/collection/trait.Collection.html
impl Collection<SOMVM> for VMCollection {
    fn stop_all_mutators<F>(_tls: VMWorkerThread, mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<SOMVM>),
    {
        unsafe {(*MMTK_TO_VM_INTERFACE).stop_all_mutators(mutator_visitor);}
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        unsafe {(*MMTK_TO_VM_INTERFACE).resume_mutators();}
    }

    fn block_for_gc(tls: VMMutatorThread) {
        unsafe {(*MMTK_TO_VM_INTERFACE).block_for_gc(tls);}
    }

    fn spawn_gc_thread(_tls: VMThread, ctx: GCThreadContext<SOMVM>) {
        // copied from julia mmtk code
        // Just drop the join handle. The thread will run until the process quits.
        let _ = std::thread::spawn(move || {
            let worker_tls = VMWorkerThread(VMThread(OpaquePointer::from_address(unsafe {
                Address::from_usize(std::process::id() as usize)
                // Address::from_usize(unsafe { libc::gettid() as usize })
            })));

            // let worker_tls = VMWorkerThread(VMThread(OpaquePointer::UNINITIALIZED));
            
            match ctx {
                GCThreadContext::Worker(w) => {
                    mmtk::memory_manager::start_worker::<SOMVM>(mmtk(), worker_tls, w)
                }
            }
        });

    }
}
