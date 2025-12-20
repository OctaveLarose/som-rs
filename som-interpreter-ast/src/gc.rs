use crate::ast::{AstBlock, AstDispatchNode, AstExpression, AstLiteral, InlinedNode};
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use crate::{STACK_ARGS_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use log::debug;
use mmtk::util::ObjectReference;
use mmtk::vm::{ObjectModel, SlotVisitor};
use mmtk::Mutator;
use num_bigint::BigInt;
use som_gc::gc_interface::{GcType, MMTKtoVMCallbacks, SupportedSliceType, BIGINT_MAGIC_ID, STRING_MAGIC_ID};
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;
use som_gc::object_model::VMObjectModel;
use som_gc::slot::SOMSlot;
use som_gc::SOMVM;
use std::ops::{Deref, DerefMut};

// Mine. to put in GC headers
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AstObjMagicId {
    String = STRING_MAGIC_ID as isize,
    BigInt = BIGINT_MAGIC_ID as isize,
    Frame = 100,
    AstBlock = 101,
    ArrayVal = 102,
    Block = 103,
    Method = 104,
    VecAstLiteral = ASTLITERAL_SLICE_ID as isize,
    Class = 106,
    Instance = 107,
}

// we have to wrap it in our own type to be able to implement traits on it
#[derive(Clone)]
pub struct VecValue(pub GcSlice<Value>);

impl Deref for VecValue {
    type Target = GcSlice<Value>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for VecValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl SupportedSliceType for Value {
    fn get_magic_gc_slice_id() -> u8 {
        AstObjMagicId::ArrayVal as u8
    }
}

const ASTLITERAL_SLICE_ID: u8 = 105;
impl SupportedSliceType for AstLiteral {
    fn get_magic_gc_slice_id() -> u8 {
        ASTLITERAL_SLICE_ID
    }
}

impl GcType for VecValue {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::ArrayVal as u8
    }

    fn scan_object(_self: Gc<Self>, _visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        todo!()
    }
}

// --- Scanning

fn get_roots_in_mutator_thread(_mutator: &mut Mutator<SOMVM>) -> Vec<SOMSlot> {
    debug!("calling scan_roots_in_mutator_thread");
    unsafe {
        let mut to_process: Vec<SOMSlot> = vec![];
        let mut to_process_fn = |slot: SOMSlot| {
            to_process.push(slot);
        };

        assert!(
            !(*UNIVERSE_RAW_PTR_CONST.as_ptr()).is_null(),
            "GC triggered while the system wasn't finished initializing."
        );

        // walk the frame list.
        let current_frame_addr = &(**UNIVERSE_RAW_PTR_CONST.as_ptr()).current_frame;
        debug!("scanning root: current_frame");
        to_process_fn(SOMSlot::from(current_frame_addr));

        // walk globals (includes core classes, but we also need to move the refs in the CoreClasses class)
        debug!("scanning roots: globals");
        for (_name, val) in (**UNIVERSE_RAW_PTR_CONST.as_ptr()).globals.iter() {
            visit_value(val, &mut to_process_fn)
        }

        debug!("scanning roots: core classes");
        for (_, cls_ptr) in (**UNIVERSE_RAW_PTR_CONST.as_ptr()).core.iter() {
            to_process_fn(SOMSlot::from(cls_ptr))
        }

        debug!("scanning roots: global argument stack");
        for val in (**STACK_ARGS_RAW_PTR_CONST.as_ptr()).iter() {
            visit_value(val, &mut to_process_fn)
        }

        debug!("scanning roots: finished");
        to_process
    }
}

pub fn scan_object<'a>(object: ObjectReference, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    unsafe {
        let gc_id: &AstObjMagicId = VMObjectModel::ref_to_header(object).as_ref();

        debug!("entering scan_object (type: {:?})", gc_id);

        let mut visit_fn = |slot: SOMSlot| {
            slot_visitor.visit_slot(slot);
        };

        match gc_id {
            AstObjMagicId::Frame => Frame::scan_object(object.to_raw_address().into(), &mut visit_fn),
            AstObjMagicId::Class => Class::scan_object(object.to_raw_address().into(), &mut visit_fn),
            AstObjMagicId::Method => Method::scan_object(object.to_raw_address().into(), &mut visit_fn),
            AstObjMagicId::Instance => Instance::scan_object(object.to_raw_address().into(), &mut visit_fn),
            AstObjMagicId::Block => Block::scan_object(object.to_raw_address().into(), &mut visit_fn),
            AstObjMagicId::AstBlock => AstBlock::scan_object(object.to_raw_address().into(), &mut visit_fn),
            AstObjMagicId::VecAstLiteral => {
                let literal_vec: GcSlice<AstLiteral> = GcSlice::from(object.to_raw_address());
                for lit in literal_vec.iter() {
                    visit_literal(lit, &mut visit_fn)
                }
            }
            AstObjMagicId::ArrayVal => {
                let array_val: GcSlice<Value> = GcSlice::from(object.to_raw_address());
                for val in array_val.iter() {
                    visit_value(val, &mut visit_fn)
                }
            }
            // leaf nodes
            AstObjMagicId::String | AstObjMagicId::BigInt => {}
        }
    }
}

/// Visits a value, via a specialized `SOMSlot` for value types.
/// # Safety
/// Values passed to this function MUST live on the GC heap, or the pointer generated from the reference will be invalid.
pub(crate) fn visit_value(val: &Value, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
    unsafe {
        if val.is_ptr_type() {
            if let Some(slice) = val.as_array() {
                // large object storage means no copying needed, but we still check the values stored
                // if slice.get_true_size() >= 65535 {
                for val in slice.iter() {
                    visit_value(val, visit_slot_fn)
                }
                // return;
                // }
            }
            visit_slot_fn(SOMSlot::from(val.as_mut_ptr()))
        }
    }
}

pub(crate) fn visit_expr(expr: &AstExpression, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
    fn visit_dispatch_node(dispatch_node: &AstDispatchNode, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        visit_expr(&dispatch_node.receiver, visit_slot_fn);
        if let Some(cache) = &dispatch_node.inline_cache {
            visit_slot_fn(SOMSlot::from(&cache.0));
            visit_slot_fn(SOMSlot::from(&cache.1));
        }
    }

    match expr {
        AstExpression::Block(blk) => visit_slot_fn(SOMSlot::from(blk)),
        AstExpression::Literal(lit) => visit_literal(lit, visit_slot_fn),
        AstExpression::InlinedCall(inlined_node) => match inlined_node.as_ref() {
            InlinedNode::IfInlined(if_inlined) => {
                visit_expr(&if_inlined.cond_expr, visit_slot_fn);
                for expr in &if_inlined.body_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
            }
            InlinedNode::IfNilInlined(if_nil_inlined) => {
                visit_expr(&if_nil_inlined.cond_expr, visit_slot_fn);
                for expr in &if_nil_inlined.body_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
            }
            InlinedNode::IfTrueIfFalseInlined(if_true_if_false_inlined) => {
                visit_expr(&if_true_if_false_inlined.cond_expr, visit_slot_fn);
                for expr in &if_true_if_false_inlined.body_1_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
                for expr in &if_true_if_false_inlined.body_2_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
            }
            InlinedNode::IfNilIfNotNilInlined(if_nil_if_not_nil_inlined) => {
                visit_expr(&if_nil_if_not_nil_inlined.cond_expr, visit_slot_fn);
                for expr in &if_nil_if_not_nil_inlined.body_1_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
                for expr in &if_nil_if_not_nil_inlined.body_2_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
            }
            InlinedNode::WhileInlined(while_inlined) => {
                for expr in &while_inlined.cond_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
                for expr in &while_inlined.body_instrs.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
            }
            InlinedNode::OrInlined(or_inlined) => {
                visit_expr(&or_inlined.first, visit_slot_fn);
                for expr in &or_inlined.second.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
            }
            InlinedNode::AndInlined(and_inlined) => {
                visit_expr(&and_inlined.first, visit_slot_fn);
                for expr in &and_inlined.second.exprs {
                    visit_expr(expr, visit_slot_fn)
                }
            }
            InlinedNode::ToDoInlined(to_do_inlined) => {
                visit_expr(&to_do_inlined.start, visit_slot_fn);
                visit_expr(&to_do_inlined.end, visit_slot_fn);
                for expr in &to_do_inlined.body.exprs {
                    visit_expr(expr, visit_slot_fn);
                }
            }
        },
        AstExpression::LocalExit(expr)
        | AstExpression::NonLocalExit(expr, _)
        | AstExpression::LocalVarWrite(_, expr)
        | AstExpression::ArgWrite(_, _, expr)
        | AstExpression::FieldWrite(_, expr)
        | AstExpression::NonLocalVarWrite(_, _, expr) => visit_expr(expr, visit_slot_fn),
        AstExpression::UnaryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, visit_slot_fn);
        }
        AstExpression::BinaryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, visit_slot_fn);
            visit_expr(&dispatch.arg, visit_slot_fn)
        }
        AstExpression::TernaryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, visit_slot_fn);
            visit_expr(&dispatch.arg1, visit_slot_fn);
            visit_expr(&dispatch.arg2, visit_slot_fn);
        }
        AstExpression::NAryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, visit_slot_fn);
            for arg in &dispatch.values {
                visit_expr(arg, visit_slot_fn);
            }
        }
        AstExpression::SuperMessage(super_message) => {
            visit_slot_fn(SOMSlot::from(&super_message.super_class));
            for arg in &super_message.values {
                visit_expr(arg, visit_slot_fn);
            }
        }
        AstExpression::GlobalRead(global_node) => {
            if let Some(cached_entry) = global_node.cached_entry.as_ref() {
                visit_value(cached_entry, visit_slot_fn)
            }
        }
        AstExpression::LocalVarRead(..)
        | AstExpression::NonLocalVarRead(..)
        | AstExpression::IncLocal(..)
        | AstExpression::DecLocal(..)
        | AstExpression::ArgRead(..)
        | AstExpression::FieldRead(..) => {} // leaf nodes
    }
}

/// Visits a value, via a specialized `SOMSlot` for value types.
/// # Safety
/// Literals passed to this function MUST live on the GC heap, but that's always the case for literals (at the moment).
pub(crate) fn visit_literal(literal: &AstLiteral, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
    match &literal {
        AstLiteral::String(s) => visit_slot_fn(SOMSlot::from(s)),
        AstLiteral::BigInteger(big_int) => visit_slot_fn(SOMSlot::from(big_int)),
        AstLiteral::Array(arr) => visit_slot_fn(SOMSlot::from(arr)),
        AstLiteral::Symbol(_) | AstLiteral::Double(_) | AstLiteral::Integer(_) => {}
    }
}

fn get_object_size(object: ObjectReference) -> usize {
    let gc_id: &AstObjMagicId = unsafe { VMObjectModel::ref_to_header(object).as_ref() };

    match gc_id {
        AstObjMagicId::Frame => unsafe {
            let frame: &Frame = object.to_raw_address().as_ref();
            Frame::get_true_size(frame.nbr_args, frame.nbr_locals)
        },
        AstObjMagicId::Instance => unsafe {
            let instance: &Instance = object.to_raw_address().as_ref();
            size_of::<Instance>() + instance.get_nbr_fields() * size_of::<Value>()
        },
        AstObjMagicId::String => size_of::<String>(),
        AstObjMagicId::BigInt => size_of::<BigInt>(),
        AstObjMagicId::AstBlock => size_of::<AstBlock>(),
        AstObjMagicId::VecAstLiteral => {
            let literals: GcSlice<AstLiteral> = GcSlice::from(object.to_raw_address());
            literals.get_true_size()
        }
        AstObjMagicId::ArrayVal => {
            let values: GcSlice<Value> = GcSlice::from(object.to_raw_address());
            values.get_true_size()
        }
        AstObjMagicId::Method => size_of::<Method>(),
        AstObjMagicId::Block => size_of::<Block>(),
        AstObjMagicId::Class => size_of::<Class>(),
    }
}

pub fn get_callbacks_for_gc() -> MMTKtoVMCallbacks {
    MMTKtoVMCallbacks {
        scan_object,
        get_roots_in_mutator_thread,
        //adapt_post_copy,
        get_object_size,
    }
}
