use crate::{mmtk, SOMVM, VM_TO_MMTK_INTERFACE};
use mmtk::util::opaque_pointer::*;
use mmtk::util::Address;
use mmtk::util::VMWorkerThread;
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
        unsafe {
            (**(VM_TO_MMTK_INTERFACE.get_mut().unwrap())).stop_all_mutators(mutator_visitor);
        }
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        unsafe {
            (**(VM_TO_MMTK_INTERFACE.get_mut().unwrap())).resume_mutators();
        }
    }

    fn block_for_gc(tls: VMMutatorThread) {
        unsafe {
            (**(VM_TO_MMTK_INTERFACE.get_mut().unwrap())).block_for_gc(tls);
        }
    }

    fn spawn_gc_thread(_tls: VMThread, ctx: GCThreadContext<SOMVM>) {
        // copied from julia mmtk code
        // Just drop the join handle. The thread will run until the process quits.
        let _ = std::thread::spawn(move || {
            let worker_tls = VMWorkerThread(VMThread(OpaquePointer::from_address(unsafe {
                Address::from_usize(std::process::id() as usize)
                // Address::from_usize(unsafe { libc::gettid() as usize })
            })));

            match ctx {
                GCThreadContext::Worker(w) => mmtk::memory_manager::start_worker::<SOMVM>(mmtk(), worker_tls, w),
            }
        });
    }
}
