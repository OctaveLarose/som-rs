use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::Frame;
use crate::gc::object_model::{GCMagicId, VMObjectModel};
use crate::gc::SOMSlot;
use crate::gc::SOMVM;
use crate::instance::Instance;
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::MMTK_TO_VM_INTERFACE;
use log::debug;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::Slot;
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

            debug!("entering scan_object (type: {:?})", gc_id);
            
            match gc_id {
                GCMagicId::Frame => {
                    let frame: &mut Frame = object.to_raw_address().as_mut_ref();
                    debug!("(frame method is: {})", &frame.current_method.to_obj().signature);

                    debug_assert!(!frame.current_method.to_obj().signature.is_empty()); // rough way of checking with reasonable certainty that the cast to a frame succeeded
                    if !frame.prev_frame.is_empty() {
                        let prev_frame_slot_addr = Address::from_ref(&frame.prev_frame);
                        slot_visitor.visit_slot(SOMSlot::from_address(prev_frame_slot_addr));
                    }

                    let method_slot_addr = Address::from_ref(&frame.current_method);
                    slot_visitor.visit_slot(SOMSlot::from_address(method_slot_addr))
                }
                GCMagicId::Method => {
                    let method: &mut Method = object.to_raw_address().as_mut_ref();

                    // kind doesn't contain GCRefs, nothing to do.
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
                }
                GCMagicId::Block => {
                    let block: &mut Block = object.to_raw_address().as_mut_ref();
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&block.blk_info)));
                }
                GCMagicId::Instance => {
                    let instance: &mut Instance = object.to_raw_address().as_mut_ref();
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&instance.class)));
                }
                GCMagicId::ArrayVal => {
                    let arr: &mut Vec<Value> = object.to_raw_address().as_mut_ref();
                    for val in arr {
                        visit_value(val, slot_visitor)
                    }
                }
                GCMagicId::BlockInfo | GCMagicId::String | GCMagicId::ArrayU8 | GCMagicId::BigInt => {
                    // no children: dead end in dependency graph
                }
            }
        }
    }

    fn scan_object_and_trace_edges<OT: ObjectTracer>(_tls: VMWorkerThread, _object: ObjectReference, _object_tracer: &mut OT) {
        todo!()
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {
        unimplemented!()
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
    match value_to_slot(val) {
        Some(slot) => slot_visitor.visit_slot(slot),
        None => {}
    }
}

pub fn value_to_slot(val: &Value) -> Option<SOMSlot> {
    if let Some(gcref) = val.as_block() {
        Some(SOMSlot::from_address(Address::from_ref(&gcref)))
    } else if let Some(gcref) = val.as_class() {
        Some(SOMSlot::from_address(Address::from_ref(&gcref)))
    } else if let Some(gcref) = val.as_invokable() {
        Some(SOMSlot::from_address(Address::from_ref(&gcref)))
    } else if let Some(gcref) = val.as_instance() {
        Some(SOMSlot::from_address(Address::from_ref(&gcref)))
    } else if let Some(gcref) = val.as_big_integer() {
        Some(SOMSlot::from_address(Address::from_ref(&gcref)))
    } else if let Some(gcref) = val.as_string() {
        Some(SOMSlot::from_address(Address::from_ref(&gcref)))
    } else if let Some(gcref) = val.as_array() {
        Some(SOMSlot::from_address(Address::from_ref(&gcref)))
    } else {
        None
    }
}

#[allow(dead_code)]
// #[inline(always)]
/// Turns an address into a slot - and most importantly, optionally verify its validity. Inspired from the Julia MMTk code.
pub fn addr_to_slot(addr: Address) -> SOMSlot {
    let slot = SOMSlot::from_address(addr);

    #[cfg(debug_assertions)]
    {
        // println!("\tprocess slot = {:?} - {:?}\n", slot, slot.load());

        if let Some(objref) = slot.load() {
            debug_assert!(
                mmtk::memory_manager::is_in_mmtk_spaces(objref),
                "Object {:?} in slot {:?} is not mapped address",
                objref,
                slot
            );

            let raw_addr_usize = objref.to_raw_address().as_usize();
            debug_assert!(
                raw_addr_usize % 16 == 0 || raw_addr_usize % 8 == 0,
                "Object {:?} in slot {:?} is not aligned to 8 or 16",
                objref,
                slot
            );
        }
    }

    slot
}