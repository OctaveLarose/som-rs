use crate::{SOMVM, VM_TO_MMTK_INTERFACE};
use mmtk::scheduler::GCWorker;
use mmtk::util::opaque_pointer::*;
use mmtk::util::ObjectReference;
use mmtk::vm::ActivePlan;
use mmtk::{Mutator, ObjectQueue};

pub struct VMActivePlan {}

// Documentation: https://docs.mmtk.io/api/mmtk/vm/active_plan/trait.ActivePlan.html
impl ActivePlan<SOMVM> for VMActivePlan {
    fn is_mutator(_tls: VMThread) -> bool {
        // I should properly check that the thread is a mutator, but we never use those thread identifier variables at the moment anyway, so
        true
    }

    /// Return a Mutator reference for the thread.
    fn mutator(tls: VMMutatorThread) -> &'static mut Mutator<SOMVM> {
        unsafe { (**(VM_TO_MMTK_INTERFACE.get_mut().unwrap())).get_mutator(tls) }
    }

    /// Return an iterator that includes all the mutators at the point of invocation.
    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut Mutator<SOMVM>> + 'a> {
        unsafe { (**(VM_TO_MMTK_INTERFACE.get_mut().unwrap())).get_all_mutators() }
    }

    fn number_of_mutators() -> usize {
        1
    }

    fn vm_trace_object<Q: ObjectQueue>(_queue: &mut Q, object: ObjectReference, _worker: &mut GCWorker<SOMVM>) -> ObjectReference {
        // I've had MMTk sometimes panic here. thus i reimplemented this one on our side, but only for debug purposes.
        // this should never be invoked.

        panic!("entering vm_trace_object for some reason: object {:?} not in mmtk space?", object)
    }
}
