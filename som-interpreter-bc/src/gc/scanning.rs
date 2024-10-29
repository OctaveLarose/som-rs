use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::Frame;
use crate::gc::gc_interface::GCRef;
use crate::gc::object_model::{GCMagicId, VMObjectModel};
use crate::gc::SOMSlot;
use crate::gc::SOMVM;
use crate::instance::{Instance, InstanceAccess};
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::MMTK_TO_VM_INTERFACE;
use log::debug;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::SlotVisitor;
use mmtk::vm::{ObjectModel, Scanning};
use mmtk::vm::{ObjectTracer, RootsWorkFactory};
use mmtk::Mutator;

pub struct VMScanning {}

// Documentation: https://docs.mmtk.io/api/mmtk/vm/scanning/trait.Scanning.html
impl Scanning<SOMVM> for VMScanning {
    fn scan_object<SV: SlotVisitor<SOMSlot>>(
        _tls: VMWorkerThread,
        object: ObjectReference,
        slot_visitor: &mut SV,
    ) {
        unsafe {
            // let _ptr: *mut usize = unsafe { obj_addr.as_mut_ref() };
            let gc_id: &GCMagicId = VMObjectModel::ref_to_header(object).as_ref();

            // debug!("entering scan_object (type: {:?})", gc_id);

            match gc_id {
                GCMagicId::Frame => {
                    let frame: &mut Frame = object.to_raw_address().as_mut_ref();
                    debug!("(frame method is: {})", &frame.current_method.to_obj().signature);

                    if !frame.prev_frame.is_empty() {
                        let prev_frame_slot_addr = Address::from_ref(&frame.prev_frame);
                        slot_visitor.visit_slot(SOMSlot::from_address(prev_frame_slot_addr));
                    }

                    let method_slot_addr = Address::from_ref(&frame.current_method);
                    slot_visitor.visit_slot(SOMSlot::from_address(method_slot_addr));

                    for i in 0..frame.nbr_locals {
                        let val: &Value = frame.lookup_local(i);
                        visit_value(&val, slot_visitor)
                    }

                    for i in 0..frame.nbr_args {
                        let val: &Value = frame.lookup_argument(i);
                        visit_value(&val, slot_visitor)
                    }

                    // this should all really be done in the frame as a custom method. return an iter or something
                    let frame_stack_start_addr: Address = object.to_raw_address().add(size_of::<Frame>());
                    let mut stack_ptr = frame.stack_ptr;
                    while !std::ptr::eq(stack_ptr, frame_stack_start_addr.to_ptr()) {
                        stack_ptr = stack_ptr.sub(1);
                        let stack_val = *stack_ptr;
                        visit_value(&stack_val, slot_visitor)
                    }
                }
                GCMagicId::Method => {
                    let method: &mut Method = object.to_raw_address().as_mut_ref();

                    match &method.kind {
                        MethodKind::Defined(method_env) => {
                            for x in &method_env.literals {
                                match x {
                                    Literal::Block(blk) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(blk))),
                                    Literal::String(str) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(str))),
                                    Literal::BigInteger(bigint) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(bigint))),
                                    Literal::Array(arr) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(arr))),
                                    _ => {}
                                }
                            }
                        },
                        _ => {},
                    }

                    let holder_slot_addr = Address::from_ref(&method.holder);
                    slot_visitor.visit_slot(SOMSlot::from_address(holder_slot_addr))
                }
                GCMagicId::Class => {
                    let class: &mut Class = object.to_raw_address().as_mut_ref();

                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&class.class)));

                    if let Some(_) = class.super_class {
                        slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(class.super_class.as_ref().unwrap())));
                    }

                    for (_, method_ref) in class.methods.iter() {
                        slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(method_ref)))
                    }

                    for (_, field_ref) in class.locals.iter() {
                        visit_value(field_ref, slot_visitor)
                    }
                }
                GCMagicId::Block => {
                    let block: &mut Block = object.to_raw_address().as_mut_ref();
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&block.blk_info)));
                }
                GCMagicId::Instance => {
                    let instance: &mut Instance = object.to_raw_address().as_mut_ref();
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&instance.class)));

                    // not the cleanest, to be frank
                    let gcref_instance: GCRef<Instance> = GCRef::from_u64(object.to_raw_address().as_usize() as u64);
                    for i in 0..instance.nbr_fields {
                        let val: Value = gcref_instance.lookup_local(i);
                        visit_value(&val, slot_visitor)
                    }
                }
                GCMagicId::ArrayVal => {
                    let arr: &mut Vec<Value> = object.to_raw_address().as_mut_ref();
                    for val in arr {
                        visit_value(val, slot_visitor)
                    }
                }
                GCMagicId::BlockInfo | GCMagicId::String | GCMagicId::ArrayU8 | GCMagicId::BigInt => {
                    // leaf nodes: no children.
                }
            }
        }
    }

    fn scan_object_and_trace_edges<OT: ObjectTracer>(_tls: VMWorkerThread, _object: ObjectReference, _object_tracer: &mut OT) {
        todo!()
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {
        // do nothing.
    }

    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        mutator: &'static mut Mutator<SOMVM>,
        factory: impl RootsWorkFactory<SOMSlot>,
    ) {
        unsafe { (*MMTK_TO_VM_INTERFACE).scan_roots_in_mutator_thread(mutator, factory) }
    }
    fn scan_vm_specific_roots(_tls: VMWorkerThread, factory: impl RootsWorkFactory<SOMSlot>) {
        unsafe { (*MMTK_TO_VM_INTERFACE).scan_vm_specific_roots(factory) }
    }
    fn supports_return_barrier() -> bool {
        unimplemented!()
    }
    fn prepare_for_roots_re_scanning() {
        unimplemented!()
    }
}

fn visit_value<SV: SlotVisitor<SOMSlot>>(val: &Value, slot_visitor: &mut SV) {
    match val.is_ptr_type() {
        true => slot_visitor.visit_slot(SOMSlot::from_value(*val)),
        false => {}
    }
}