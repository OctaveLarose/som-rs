use crate::compiler::Literal;
use crate::value::Value;
use crate::vm_objects::block::{Block, CacheEntry};
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::{Frame, FrameStackIter};
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use crate::{INTERPRETER_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use core::mem::size_of;
use log::{debug, trace};
use mmtk::util::ObjectReference;
use mmtk::vm::{ObjectModel, SlotVisitor};
use mmtk::Mutator;
use num_bigint::BigInt;
use som_gc::gc_interface::{HasTypeInfoForGC, MMTKtoVMCallbacks, SupportedSliceType, BIGINT_MAGIC_ID, STRING_MAGIC_ID};
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;
use som_gc::object_model::VMObjectModel;
use som_gc::slot::SOMSlot;
use som_gc::SOMVM;

// Mine. to put in GC headers
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BCObjMagicId {
    String = STRING_MAGIC_ID as isize,
    BigInt = BIGINT_MAGIC_ID as isize,
    Frame = 100,
    ArrayLiteral = GCSLICE_LITERAL_MAGIC_ID as isize,
    Block = 102,
    Class = 103,
    Instance = 104,
    Method = 105,
    ArrayVal = 106,
}

// TODO: HACK. this is to be able to define a magic id for it. what we REALLY need is a GCSlice<T> type.
// TODO: which now exists - implement properly with vecvalue.
pub struct VecValue(pub Vec<Value>);

pub const GCSLICE_LITERAL_MAGIC_ID: u8 = 101;
impl SupportedSliceType for Literal {
    fn get_magic_gc_slice_id() -> u8 {
        GCSLICE_LITERAL_MAGIC_ID
    }
}

impl HasTypeInfoForGC for VecValue {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::ArrayVal as u8
    }
}

impl HasTypeInfoForGC for Instance {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::Instance as u8
    }
}

impl HasTypeInfoForGC for Method {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::Method as u8
    }
}

impl HasTypeInfoForGC for Block {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::Block as u8
    }
}

impl HasTypeInfoForGC for Class {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::Class as u8
    }
}

impl HasTypeInfoForGC for Frame {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::Frame as u8
    }
}

// --- Scanning

/// Visits a value, via a specialized `SOMSlot` for value types.
/// # Safety
/// Values passed to this function MUST live on the GC heap, or the pointer generated from the reference will be invalid.
pub unsafe fn visit_value<'a>(val: &Value, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    if val.is_ptr_type() {
        slot_visitor.visit_slot(SOMSlot::from(val.as_mut_ptr()))
    }
}

pub fn visit_literal<'a>(lit: &Literal, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    match lit {
        Literal::Block(blk) => slot_visitor.visit_slot(SOMSlot::from(blk)),
        Literal::String(str) => slot_visitor.visit_slot(SOMSlot::from(str)),
        Literal::BigInteger(bigint) => slot_visitor.visit_slot(SOMSlot::from(bigint)),
        Literal::Array(arr) => slot_visitor.visit_slot(SOMSlot::from(arr)),
        _ => {}
    }
}

pub fn scan_object<'a>(object: ObjectReference, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    unsafe {
        let gc_id: &BCObjMagicId = VMObjectModel::ref_to_header(object).as_ref();

        trace!("entering scan_object (type: {:?})", gc_id);

        match gc_id {
            BCObjMagicId::Frame => {
                let frame: &Frame = object.to_raw_address().as_ref();

                if !frame.prev_frame.is_empty() {
                    slot_visitor.visit_slot(SOMSlot::from(&frame.prev_frame));
                }

                slot_visitor.visit_slot(SOMSlot::from(&frame.current_context));

                for i in 0..frame.get_nbr_locals() {
                    let val: &Value = frame.lookup_local(i);
                    visit_value(val, slot_visitor)
                }

                for i in 0..frame.get_nbr_args() {
                    let val: &Value = frame.lookup_argument(i);
                    visit_value(val, slot_visitor)
                }

                let stack_iter = FrameStackIter::from(frame);
                for stack_item in stack_iter.into_iter() {
                    visit_value(stack_item, slot_visitor);
                }
            }
            BCObjMagicId::Method => {
                let method: &Method = object.to_raw_address().as_ref();

                match method {
                    Method::Defined(method) => {
                        slot_visitor.visit_slot(SOMSlot::from(&method.base_method_info.holder));

                        for cache_entry in method.inline_cache.iter().flatten() {
                            match cache_entry {
                                CacheEntry::Send(cls_ptr, method_ptr) => {
                                    slot_visitor.visit_slot(SOMSlot::from(cls_ptr));
                                    slot_visitor.visit_slot(SOMSlot::from(method_ptr));
                                }
                                CacheEntry::Global(val) => {
                                    visit_value(val, slot_visitor);
                                }
                            }
                        }

                        for lit in &method.literals {
                            visit_literal(lit, slot_visitor)
                        }
                    }
                    Method::Primitive(_, met_info)
                    | Method::TrivialGlobal(_, met_info)
                    | Method::TrivialGetter(_, met_info)
                    | Method::TrivialSetter(_, met_info) => {
                        slot_visitor.visit_slot(SOMSlot::from(&met_info.holder));
                    }
                    Method::TrivialLiteral(trivial_lit, met_info) => {
                        visit_literal(&trivial_lit.literal, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&met_info.holder));
                    }
                }
            }
            BCObjMagicId::Class => {
                let class: &Class = object.to_raw_address().as_ref();

                slot_visitor.visit_slot(SOMSlot::from(&class.class));

                if class.super_class.is_some() {
                    slot_visitor.visit_slot(SOMSlot::from(class.super_class.as_ref().unwrap()));
                }

                for (_, method_ref) in class.methods.iter() {
                    slot_visitor.visit_slot(SOMSlot::from(method_ref))
                }

                for field_ref in class.fields.iter() {
                    visit_value(field_ref, slot_visitor)
                }
            }
            BCObjMagicId::Block => {
                let block: &Block = object.to_raw_address().as_ref();

                if let Some(frame) = block.frame.as_ref() {
                    slot_visitor.visit_slot(SOMSlot::from(frame));
                }

                slot_visitor.visit_slot(SOMSlot::from(&block.blk_info));
            }
            BCObjMagicId::Instance => {
                let instance: &Instance = object.to_raw_address().as_ref();
                slot_visitor.visit_slot(SOMSlot::from(&instance.class));

                let instance_as_gc: Gc<Instance> = object.to_raw_address().into();
                for i in 0..instance.class().get_nbr_fields() {
                    let val: &Value = Instance::lookup_field(instance_as_gc, i);
                    visit_value(val, slot_visitor)
                }
            }
            BCObjMagicId::ArrayVal => {
                let arr: &Vec<Value> = object.to_raw_address().as_ref();
                for val in arr {
                    visit_value(val, slot_visitor)
                }
            }
            BCObjMagicId::ArrayLiteral => {
                let literal_vec: GcSlice<Literal> = GcSlice::from(object.to_raw_address());
                for lit in literal_vec.iter() {
                    visit_literal(lit, slot_visitor)
                }
            }
            BCObjMagicId::String | BCObjMagicId::BigInt => {
                // leaf nodes: no children.
            }
        }
    }
}

fn get_roots_in_mutator_thread(_mutator: &mut Mutator<SOMVM>) -> Vec<SOMSlot> {
    debug!("calling scan_roots_in_mutator_thread");
    unsafe {
        let mut to_process: Vec<SOMSlot> = vec![];

        assert!(
            !(*UNIVERSE_RAW_PTR_CONST.as_ptr()).is_null() && !(*INTERPRETER_RAW_PTR_CONST.as_ptr()).is_null(),
            "GC triggered while the system wasn't finished initializing."
        );

        // walk the frame list.
        let current_frame_addr = &(**INTERPRETER_RAW_PTR_CONST.as_ptr()).current_frame;
        debug!(
            "scanning root: current_frame (method: {})",
            current_frame_addr.current_context.get_env().base_method_info.signature
        );
        to_process.push(SOMSlot::from(current_frame_addr));

        let frame_method_root = &(**INTERPRETER_RAW_PTR_CONST.as_ptr()).frame_method_root;
        if !frame_method_root.is_empty() {
            to_process.push(SOMSlot::from(frame_method_root));
        }

        // walk globals (includes core classes)
        debug!("scanning roots: globals");
        for (_name, val) in (**UNIVERSE_RAW_PTR_CONST.as_ptr()).globals.iter_mut() {
            if val.is_ptr_type() {
                to_process.push(SOMSlot::from(val.as_mut_ptr()));
            }
        }

        // we update the core classes in their class also though, to properly move them
        (**UNIVERSE_RAW_PTR_CONST.as_ptr()).core.iter().for_each(|(_, cls_ptr)| to_process.push(SOMSlot::from(cls_ptr)));

        debug!("scanning roots: finished");
        to_process
    }
}

fn get_object_size(object: ObjectReference) -> usize {
    let gc_id: &BCObjMagicId = unsafe { VMObjectModel::ref_to_header(object).as_ref() };

    let obj_size = {
        match gc_id {
            BCObjMagicId::String => size_of::<String>(),
            BCObjMagicId::BigInt => size_of::<BigInt>(),
            BCObjMagicId::ArrayLiteral => {
                let literals: GcSlice<Literal> = GcSlice::from(object.to_raw_address());
                literals.get_true_size()
            }
            BCObjMagicId::Frame => unsafe {
                let frame: &Frame = object.to_raw_address().as_ref();
                Frame::get_true_size(frame.get_max_stack_size(), frame.get_nbr_args(), frame.get_nbr_locals())
            },
            BCObjMagicId::ArrayVal => size_of::<Vec<Value>>(),
            BCObjMagicId::Method => size_of::<Method>(),
            BCObjMagicId::Block => size_of::<Block>(),
            BCObjMagicId::Class => size_of::<Class>(),
            BCObjMagicId::Instance => unsafe {
                let instance: &Instance = object.to_raw_address().as_ref();
                size_of::<Instance>() + instance.class.fields.len() * size_of::<Value>()
            },
        }
    };

    // debug!("get object size invoked ({:?}), and returning {}", gc_id, obj_size);

    obj_size
}

fn adapt_post_copy(object: ObjectReference, original_obj: ObjectReference) {
    let gc_id: &BCObjMagicId = unsafe { object.to_raw_address().as_ref() };

    unsafe {
        if gc_id == &BCObjMagicId::Frame {
            debug!("adapt_post_copy: frame");

            let frame: &mut Frame = object.to_raw_address().add(8).as_mut_ref();

            let frame_addr_usize: usize = object.to_raw_address().add(8).as_usize();
            let og_frame_ptr: *const Frame = original_obj.to_raw_address().to_ptr();

            let offset = frame_addr_usize as isize - og_frame_ptr as isize;
            // frame.stack_ptr = frame.stack_ptr.byte_offset(offset);
            frame.args_ptr = frame.args_ptr.byte_offset(offset);
            frame.locals_ptr = frame.locals_ptr.byte_offset(offset);

            debug_assert_eq!((*og_frame_ptr).lookup_argument(0), frame.lookup_argument(0));
            if frame.get_nbr_locals() >= 1 {
                debug_assert_eq!((*og_frame_ptr).lookup_local(0), frame.lookup_local(0));
            }
        }
    }
}

pub fn get_callbacks_for_gc() -> MMTKtoVMCallbacks {
    MMTKtoVMCallbacks {
        scan_object,
        get_roots_in_mutator_thread,
        get_object_size,
        adapt_post_copy,
    }
}
