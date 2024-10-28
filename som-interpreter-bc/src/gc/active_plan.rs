use crate::gc::SOMVM;
use crate::MMTK_TO_VM_INTERFACE;
use mmtk::scheduler::GCWorker;
use mmtk::util::opaque_pointer::*;
use mmtk::util::ObjectReference;
use mmtk::vm::ActivePlan;
use mmtk::{Mutator, ObjectQueue};

pub struct VMActivePlan {}

// Documentation: https://docs.mmtk.io/api/mmtk/vm/active_plan/trait.ActivePlan.html
impl ActivePlan<SOMVM> for VMActivePlan {
    fn is_mutator(_tls: VMThread) -> bool {
        // TODO: Properly check if the thread is a mutator
        true
    }

    /// Return a Mutator reference for the thread.
    fn mutator(tls: VMMutatorThread) -> &'static mut Mutator<SOMVM> {
        unsafe { (*MMTK_TO_VM_INTERFACE).get_mutator(tls) }
    }

    /// Return an iterator that includes all the mutators at the point of invocation.
    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut Mutator<SOMVM>> + 'a> {
        unsafe { (*MMTK_TO_VM_INTERFACE).get_all_mutators() }
    }

    fn number_of_mutators() -> usize {
        1 // TODO: is it always 1 right now though?
        // unimplemented!()
    }

    #[allow(unused)]
    fn vm_trace_object<Q: ObjectQueue>(
        queue: &mut Q,
        object: ObjectReference,
        _worker: &mut GCWorker<SOMVM>,
    ) -> ObjectReference {
        // I've had MMTk sometimes panic here. thus i reimplemented this one on our side, but only for debug purposes. 
        // this should never be invoked.
        // unsafe { dbg!(&(*UNIVERSE_RAW_PTR).globals);}
        
        // unsafe {
        //     dbg!(object);
        //     let addr = object.to_raw_address();
        //     let globals: &Vec<(Interned, Value)> = addr.as_ref();
        //     
        //     dbg!(globals);
        // }
        
        panic!("entering vm_trace_object for some reason: object {:?} not in mmtk space?", object)
        
        // info!("entering vm_trace_object");
        // queue.enqueue(object);
        // object
    }
}
