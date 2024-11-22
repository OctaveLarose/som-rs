use crate::block::{Block, BlockInfo};
use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::{Frame, OFFSET_TO_STACK};
use crate::instance::Instance;
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::{INTERPRETER_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use core::mem::size_of;
use log::debug;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::{ObjectModel, SlotVisitor};
use mmtk::Mutator;
use num_bigint::BigInt;
use som_gc::gc_interface::{HasTypeInfoForGC, MMTKtoVMCallbacks, BIGINT_MAGIC_ID, STRING_MAGIC_ID, VECU8_MAGIC_ID};
use som_gc::gcref::Gc;
use som_gc::object_model::VMObjectModel;
use som_gc::slot::SOMSlot;
use som_gc::SOMVM;

// Mine. to put in GC headers
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BCObjMagicId {
    String = STRING_MAGIC_ID as isize,
    BigInt = BIGINT_MAGIC_ID as isize,
    ArrayU8 = VECU8_MAGIC_ID as isize,
    Frame = 100,
    BlockInfo = 101,
    Block = 102,
    Class = 103,
    Instance = 104,
    Method = 105,
    ArrayVal = 106,
}

// TODO: HACK. this is to be able to define a magic id for it. what we REALLY need is a GCSlice<T> type.
pub struct VecValue(pub Vec<Value>);

impl HasTypeInfoForGC for VecValue {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::ArrayVal as u8
    }
}

impl HasTypeInfoForGC for BlockInfo {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::BlockInfo as u8
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

pub fn visit_value<'a>(val: &Value, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    if val.is_ptr_type() {
        let val_ptr = unsafe { val.as_u64_ptr() };
        slot_visitor.visit_slot(SOMSlot::from_ref(val_ptr))
    }
}

pub fn visit_literal<'a>(lit: &Literal, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    match lit {
        Literal::Block(blk) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(blk))),
        Literal::String(str) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(str))),
        Literal::BigInteger(bigint) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(bigint))),
        Literal::Array(arr) => slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(arr))),
        _ => {}
    }
}

pub fn scan_object<'a>(object: ObjectReference, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    unsafe {
        // let _ptr: *mut usize = unsafe { obj_addr.as_mut_ref() };
        let gc_id: &BCObjMagicId = VMObjectModel::ref_to_header(object).as_ref();

        debug!("entering scan_object (type: {:?})", gc_id);

        match gc_id {
            BCObjMagicId::Frame => {
                let frame: &mut Frame = object.to_raw_address().as_mut_ref();
                eprintln!("frame (method: {})", &frame.current_method.signature);

                // debug!("(frame method is: {})", &frame.current_method.signature);

                if !frame.prev_frame.is_empty() {
                    let prev_frame_slot_addr = Address::from_ref(&frame.prev_frame);
                    slot_visitor.visit_slot(SOMSlot::from_address(prev_frame_slot_addr));
                }

                let method_slot_addr = Address::from_ref(&frame.current_method);
                slot_visitor.visit_slot(SOMSlot::from_address(method_slot_addr));

                for i in 0..frame.nbr_locals {
                    let val: &Value = frame.lookup_local(i);
                    visit_value(val, slot_visitor)
                }

                for i in 0..frame.nbr_args {
                    let val: &Value = frame.lookup_argument(i);
                    visit_value(val, slot_visitor)
                }

                // this should all really be done in the frame as a custom method. return an iter or something
                let frame_stack_start_addr: Address = object.to_raw_address().add(size_of::<Frame>());
                let mut stack_ptr = frame.stack_ptr;
                while !std::ptr::eq(stack_ptr, frame_stack_start_addr.to_ptr()) {
                    stack_ptr = stack_ptr.sub(1);
                    let stack_val = &*stack_ptr;
                    visit_value(stack_val, slot_visitor)
                }
            }
            BCObjMagicId::Method => {
                let method: &mut Method = object.to_raw_address().as_mut_ref();

                if let MethodKind::Defined(method_env) = &method.kind {
                    for x in &method_env.literals {
                        visit_literal(x, slot_visitor)
                    }
                }

                let holder_slot_addr = Address::from_ref(&method.holder);
                slot_visitor.visit_slot(SOMSlot::from_address(holder_slot_addr))
            }
            BCObjMagicId::Class => {
                let class: &mut Class = object.to_raw_address().as_mut_ref();

                slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&class.class)));

                if class.super_class.is_some() {
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(class.super_class.as_ref().unwrap())));
                }

                for (_, method_ref) in class.methods.iter() {
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(method_ref)))
                }

                for field_ref in class.fields.iter() {
                    visit_value(field_ref, slot_visitor)
                }
            }
            BCObjMagicId::Block => {
                let block: &mut Block = object.to_raw_address().as_mut_ref();

                if let Some(frame) = block.frame.as_ref() {
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(frame)));
                }

                slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&block.blk_info)));
            }
            BCObjMagicId::Instance => {
                let instance: &mut Instance = object.to_raw_address().as_mut_ref();
                slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&instance.class)));

                // not the cleanest, to be frank
                let gcref_instance: Gc<Instance> = Gc::from(object.to_raw_address().as_usize() as u64);
                for i in 0..instance.class().get_nbr_fields() {
                    let val: &Value = gcref_instance.lookup_field(i);
                    visit_value(val, slot_visitor)
                }
            }
            BCObjMagicId::ArrayVal => {
                let arr: &mut Vec<Value> = object.to_raw_address().as_mut_ref();
                for val in arr {
                    visit_value(val, slot_visitor)
                }
            }
            BCObjMagicId::BlockInfo => {
                let block_info: &mut BlockInfo = object.to_raw_address().as_mut_ref();
                for lit in &block_info.literals {
                    visit_literal(lit, slot_visitor)
                }
            }
            BCObjMagicId::String | BCObjMagicId::ArrayU8 | BCObjMagicId::BigInt => {
                // leaf nodes: no children.
            }
        }
    }
}

fn get_roots_in_mutator_thread(_mutator: &mut Mutator<SOMVM>) -> Vec<SOMSlot> {
    debug!("calling scan_roots_in_mutator_thread");
    unsafe {
        let mut to_process: Vec<SOMSlot> = vec![];

        // walk the frame list.
        let current_frame_addr = &INTERPRETER_RAW_PTR_CONST.unwrap().as_ref().current_frame;
        debug!("scanning root: current_frame (method: {})", current_frame_addr.current_method.signature);
        to_process.push(SOMSlot::from_address(Address::from_ref(current_frame_addr)));

        // walk globals (includes core classes)
        debug!("scanning roots: globals");
        for (_name, val) in UNIVERSE_RAW_PTR_CONST.unwrap().as_mut().globals.iter_mut() {
            if val.is_ptr_type() {
                let val_ptr = val.as_u64_ptr();
                to_process.push(SOMSlot::from_ref(val_ptr));
            }
        }

        {
            let core_classes = &UNIVERSE_RAW_PTR_CONST.unwrap().as_mut().core;
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.class_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.object_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.metaclass_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.nil_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.integer_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.double_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.array_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.method_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.primitive_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.symbol_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.string_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.system_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.block_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.block1_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.block2_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.block3_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.boolean_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.true_class)));
            to_process.push(SOMSlot::from_address(Address::from_ref(&core_classes.false_class)));
        }

        debug!("scanning roots: finished");
        to_process
    }
}

fn get_object_size(object: ObjectReference) -> usize {
    let gc_id: &BCObjMagicId = unsafe { VMObjectModel::ref_to_header(object).as_ref() };
    // let gc_id: &BCObjMagicId = unsafe { object.to_raw_address().as_ref() };

    // dbg!(&gc_id);

    let obj_size = {
        match gc_id {
            BCObjMagicId::String => size_of::<String>(),
            BCObjMagicId::BigInt => size_of::<BigInt>(),
            BCObjMagicId::ArrayU8 => size_of::<Vec<u8>>(),
            BCObjMagicId::Frame => unsafe {
                let frame: &mut Frame = object.to_raw_address().as_mut_ref();

                let max_stack_size = match &frame.current_method.kind {
                    MethodKind::Defined(e) => e.max_stack_size as usize,
                    MethodKind::Primitive(_) => 0,
                };

                size_of::<Frame>() + (frame.nbr_locals + frame.nbr_args + max_stack_size) * size_of::<Value>()
            },
            BCObjMagicId::BlockInfo => size_of::<BlockInfo>(),
            BCObjMagicId::ArrayVal => size_of::<Vec<Value>>(),
            BCObjMagicId::Method => size_of::<Method>(),
            BCObjMagicId::Block => size_of::<Block>(),
            BCObjMagicId::Class => size_of::<Class>(),
            BCObjMagicId::Instance => unsafe {
                let instance: &mut Instance = object.to_raw_address().as_mut_ref();
                size_of::<Instance>() + instance.class.fields.len() * size_of::<Value>()
            },
        }
    };

    // debug!("get object size invoked ({:?}), and returning {}", gc_id, obj_size);

    obj_size
}

fn adapt_post_copy(object: ObjectReference, original_obj: ObjectReference) {
    let gc_id: &BCObjMagicId = unsafe { object.to_raw_address().as_ref() };

    match gc_id {
        BCObjMagicId::Frame => unsafe {
            debug!("adapt_post_copy: frame");

            let obj_addr = object.to_raw_address().add(8);
            let frame: &mut Frame = obj_addr.as_mut_ref();

            let old_stack_len = frame.stack_ptr.byte_sub(original_obj.to_raw_address().as_usize()).byte_sub(size_of::<Frame>()) as usize / 8;

            frame.stack_ptr = obj_addr.add(OFFSET_TO_STACK).add(old_stack_len * size_of::<Value>()).to_mut_ptr();

            let stack_size = match &frame.current_method.kind {
                MethodKind::Defined(e) => e.max_stack_size as usize,
                MethodKind::Primitive(_) => 0,
            };

            frame.args_ptr = frame.stack_ptr.add(stack_size);
            frame.locals_ptr = frame.args_ptr.add(frame.nbr_args);

            // TODO: inline cache, also?
        },
        BCObjMagicId::Instance => unsafe {
            debug!("adapt_post_copy: instance");
            let obj_addr = object.to_raw_address().add(8);
            let instance_ptr: &mut Instance = obj_addr.as_mut_ref();
            instance_ptr.fields_ptr = obj_addr.add(size_of::<Instance>()).to_mut_ptr();
        },
        _ => {}
    }
}

pub fn get_callbacks_for_gc() -> MMTKtoVMCallbacks {
    MMTKtoVMCallbacks {
        scan_object_fn: scan_object,
        get_roots_in_mutator_thread_fn: get_roots_in_mutator_thread,
        get_object_size_fn: get_object_size,
        adapt_post_copy,
    }
}
