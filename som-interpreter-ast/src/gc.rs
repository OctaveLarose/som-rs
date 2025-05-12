use crate::ast::{AstBlock, AstDispatchNode, AstExpression, AstLiteral, InlinedNode};
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::{Frame, FrameAccess};
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::{Method, MethodKind};
use crate::{STACK_ARGS_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use log::debug;
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
    AstExpression = 108,
}

// we have to wrap it in our own type to be able to implement traits on it
#[derive(Clone, Copy)]
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

impl HasTypeInfoForGC for VecValue {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::ArrayVal as u8
    }
}

impl HasTypeInfoForGC for AstBlock {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::AstBlock as u8
    }
}

impl HasTypeInfoForGC for Block {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::Block as u8
    }
}

impl HasTypeInfoForGC for Frame {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::Frame as u8
    }
}

impl HasTypeInfoForGC for Class {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::Class as u8
    }
}

impl HasTypeInfoForGC for Instance {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::Instance as u8
    }
}

impl HasTypeInfoForGC for Method {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::Method as u8
    }
}

impl HasTypeInfoForGC for AstExpression {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::AstExpression as u8
    }
}

// --- Scanning

fn get_roots_in_mutator_thread(_mutator: &mut Mutator<SOMVM>) -> Vec<SOMSlot> {
    debug!("calling scan_roots_in_mutator_thread");
    unsafe {
        let mut to_process: Vec<SOMSlot> = vec![];

        assert!(
            !(*UNIVERSE_RAW_PTR_CONST.as_ptr()).is_null(),
            "GC triggered while the system wasn't finished initializing."
        );

        // walk the frame list.
        let current_frame_addr = &(**UNIVERSE_RAW_PTR_CONST.as_ptr()).current_frame;
        debug!("scanning root: current_frame");
        to_process.push(SOMSlot::from(current_frame_addr));

        // walk globals (includes core classes, but we also need to move the refs in the CoreClasses class)
        debug!("scanning roots: globals");
        for (_name, val) in (**UNIVERSE_RAW_PTR_CONST.as_ptr()).globals.iter() {
            visit_value_maybe_process(val, &mut to_process)
        }

        debug!("scanning roots: core classes");
        for (_, cls_ptr) in (**UNIVERSE_RAW_PTR_CONST.as_ptr()).core.iter() {
            to_process.push(SOMSlot::from(cls_ptr))
        }

        debug!("scanning roots: global argument stack");
        for val in (**STACK_ARGS_RAW_PTR_CONST.as_ptr()).iter() {
            visit_value_maybe_process(val, &mut to_process)
        }

        debug!("scanning roots: finished");
        to_process
    }
}

pub fn scan_object<'a>(object: ObjectReference, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    unsafe {
        let gc_id: &AstObjMagicId = VMObjectModel::ref_to_header(object).as_ref();

        debug!("entering scan_object (type: {:?})", gc_id);

        match gc_id {
            AstObjMagicId::Frame => {
                let frame: &Frame = object.to_raw_address().as_ref();

                if !frame.prev_frame.is_empty() {
                    slot_visitor.visit_slot(SOMSlot::from(&frame.prev_frame));
                }

                // kinda ew
                let gcref_frame: Gc<Frame> = Gc::from(object.to_raw_address());

                for i in 0..frame.nbr_locals {
                    let val: &Value = gcref_frame.lookup_local(i);
                    visit_value(val, slot_visitor)
                }

                for i in 0..frame.nbr_args {
                    let val: &Value = gcref_frame.lookup_argument(i);
                    visit_value(val, slot_visitor)
                }
            }
            AstObjMagicId::Class => {
                let class: &Class = object.to_raw_address().as_ref();

                slot_visitor.visit_slot(SOMSlot::from(&class.class));

                if let Some(scls) = class.super_class.as_ref() {
                    slot_visitor.visit_slot(SOMSlot::from(scls));
                }

                for (_, method_ref) in class.methods.iter() {
                    slot_visitor.visit_slot(SOMSlot::from(method_ref))
                }

                for field_ref in class.fields.iter() {
                    visit_value(field_ref, slot_visitor)
                }
            }
            AstObjMagicId::Method => {
                let method: &Method = object.to_raw_address().as_ref();

                slot_visitor.visit_slot(SOMSlot::from(&method.holder));

                match &method.kind {
                    MethodKind::Defined(method_def) => {
                        for expr in &method_def.body.exprs {
                            visit_expr(expr, slot_visitor)
                        }
                    }
                    MethodKind::TrivialLiteral(trivial_lit) => visit_literal(&trivial_lit.literal, slot_visitor),
                    MethodKind::TrivialGlobal(trivial_global) => {
                        if let Some(cached_entry) = trivial_global.global_name.cached_entry.as_ref() {
                            visit_value(cached_entry, slot_visitor)
                        }
                    }
                    MethodKind::Primitive(_) | MethodKind::TrivialGetter(_) | MethodKind::TrivialSetter(_) => {}
                }
            }
            AstObjMagicId::Instance => {
                let instance: &Instance = object.to_raw_address().as_ref();

                slot_visitor.visit_slot(SOMSlot::from(&instance.class));

                for val in &instance.fields {
                    visit_value(val, slot_visitor)
                }
            }
            AstObjMagicId::Block => {
                let block: &Block = object.to_raw_address().as_ref();
                slot_visitor.visit_slot(SOMSlot::from(&block.frame));
                slot_visitor.visit_slot(SOMSlot::from(&block.block));
            }
            AstObjMagicId::AstBlock => {
                let ast_block: &AstBlock = object.to_raw_address().as_ref();
                for expr in &ast_block.body.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            AstObjMagicId::VecAstLiteral => {
                let literal_vec: GcSlice<AstLiteral> = GcSlice::from(object.to_raw_address());
                for lit in literal_vec.iter() {
                    visit_literal(lit, slot_visitor)
                }
            }
            AstObjMagicId::ArrayVal => {
                let array_val: GcSlice<Value> = GcSlice::from(object.to_raw_address());
                for val in array_val.iter() {
                    visit_value(val, slot_visitor)
                }
            }
            AstObjMagicId::AstExpression => {
                let expr: &AstExpression = object.to_raw_address().as_ref();
                visit_expr(expr, slot_visitor);
            }
            AstObjMagicId::String | AstObjMagicId::BigInt => {} // leaf nodes
        }
    }
}

/// Visits a value, via a specialized `SOMSlot` for value types.
/// # Safety
/// Values passed to this function MUST live on the GC heap, or the pointer generated from the reference will be invalid.
unsafe fn visit_value<'a>(val: &Value, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    if val.is_ptr_type() {
        if let Some(slice) = val.as_array() {
            // large object storage means no copying needed, but we still check the values stored
            if slice.get_true_size() >= 65535 {
                for val in slice.iter() {
                    visit_value(val, slot_visitor)
                }
                return;
            }
        }
        slot_visitor.visit_slot(SOMSlot::from(val.as_mut_ptr()))
    }
}

/// Visits a value and potentially adds a slot made out of it to an array.
/// # Safety
/// Same as `visit_value`.
unsafe fn visit_value_maybe_process(val: &Value, to_process: &mut Vec<SOMSlot>) {
    if val.is_ptr_type() {
        if let Some(slice) = val.as_array() {
            // large object storage means no copying needed, but we still check the values stored
            if slice.get_true_size() >= 65535 {
                for val2 in slice.iter() {
                    visit_value_maybe_process(val2, to_process);
                }
                return;
            }
        }
        to_process.push(SOMSlot::from(val.as_mut_ptr()))
    }
}

fn visit_expr(expr: &AstExpression, slot_visitor: &mut dyn SlotVisitor<SOMSlot>) {
    fn visit_dispatch_node(dispatch_node: &AstDispatchNode, slot_visitor: &mut dyn SlotVisitor<SOMSlot>) {
        visit_expr(&dispatch_node.receiver, slot_visitor);
        if let Some(cache) = &dispatch_node.inline_cache {
            slot_visitor.visit_slot(SOMSlot::from(&cache.0));
            slot_visitor.visit_slot(SOMSlot::from(&cache.1));
        }
    }

    match expr {
        AstExpression::Block(blk) => slot_visitor.visit_slot(SOMSlot::from(blk)),
        AstExpression::Literal(lit) => visit_literal(lit, slot_visitor),
        AstExpression::InlinedCall(inlined_node) => match inlined_node.as_ref() {
            InlinedNode::IfInlined(if_inlined) => {
                visit_expr(&if_inlined.cond_expr, slot_visitor);
                for expr in &if_inlined.body_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            InlinedNode::IfNilInlined(if_nil_inlined) => {
                visit_expr(&if_nil_inlined.cond_expr, slot_visitor);
                for expr in &if_nil_inlined.body_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            InlinedNode::IfTrueIfFalseInlined(if_true_if_false_inlined) => {
                visit_expr(&if_true_if_false_inlined.cond_expr, slot_visitor);
                for expr in &if_true_if_false_inlined.body_1_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
                for expr in &if_true_if_false_inlined.body_2_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            InlinedNode::IfNilIfNotNilInlined(if_nil_if_not_nil_inlined) => {
                visit_expr(&if_nil_if_not_nil_inlined.cond_expr, slot_visitor);
                for expr in &if_nil_if_not_nil_inlined.body_1_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
                for expr in &if_nil_if_not_nil_inlined.body_2_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            InlinedNode::WhileInlined(while_inlined) => {
                for expr in &while_inlined.cond_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
                for expr in &while_inlined.body_instrs.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            InlinedNode::OrInlined(or_inlined) => {
                visit_expr(&or_inlined.first, slot_visitor);
                for expr in &or_inlined.second.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            InlinedNode::AndInlined(and_inlined) => {
                visit_expr(&and_inlined.first, slot_visitor);
                for expr in &and_inlined.second.exprs {
                    visit_expr(expr, slot_visitor)
                }
            }
            InlinedNode::ToDoInlined(to_do_inlined) => {
                visit_expr(&to_do_inlined.start, slot_visitor);
                visit_expr(&to_do_inlined.end, slot_visitor);
                for expr in &to_do_inlined.body.exprs {
                    visit_expr(expr, slot_visitor);
                }
            }
        },
        AstExpression::LocalExit(expr)
        | AstExpression::NonLocalExit(expr, _)
        | AstExpression::LocalVarWrite(_, expr)
        | AstExpression::ArgWrite(_, _, expr)
        | AstExpression::FieldWrite(_, expr)
        | AstExpression::NonLocalVarWrite(_, _, expr) => {
            slot_visitor.visit_slot(SOMSlot::from(expr));
            visit_expr(expr, slot_visitor)
        }
        AstExpression::UnaryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, slot_visitor);
        }
        AstExpression::BinaryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, slot_visitor);
            visit_expr(&dispatch.arg, slot_visitor)
        }
        AstExpression::TernaryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, slot_visitor);
            visit_expr(&dispatch.arg1, slot_visitor);
            visit_expr(&dispatch.arg2, slot_visitor);
        }
        AstExpression::NAryDispatch(dispatch) => {
            visit_dispatch_node(&dispatch.dispatch_node, slot_visitor);
            for arg in &dispatch.values {
                visit_expr(arg, slot_visitor);
            }
        }
        AstExpression::SuperMessage(super_message) => {
            slot_visitor.visit_slot(SOMSlot::from(&super_message.super_class));
            for arg in &super_message.values {
                visit_expr(arg, slot_visitor);
            }
        }
        AstExpression::GlobalRead(global_node) => {
            if let Some(cached_entry) = global_node.cached_entry.as_ref() {
                unsafe { visit_value(cached_entry, slot_visitor) }
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
fn visit_literal(literal: &AstLiteral, slot_visitor: &mut dyn SlotVisitor<SOMSlot>) {
    match &literal {
        AstLiteral::String(s) => slot_visitor.visit_slot(SOMSlot::from(s)),
        AstLiteral::BigInteger(big_int) => slot_visitor.visit_slot(SOMSlot::from(big_int)),
        AstLiteral::Array(arr) => slot_visitor.visit_slot(SOMSlot::from(arr)),
        AstLiteral::Symbol(_) | AstLiteral::Double(_) | AstLiteral::Integer(_) => {}
    }
}

fn adapt_post_copy(_object: ObjectReference, _original_obj: ObjectReference) {}

fn get_object_size(object: ObjectReference) -> usize {
    let gc_id: &AstObjMagicId = unsafe { VMObjectModel::ref_to_header(object).as_ref() };

    match gc_id {
        AstObjMagicId::Frame => unsafe {
            let frame: &Frame = object.to_raw_address().as_ref();
            Frame::get_true_size(frame.nbr_args, frame.nbr_locals)
        },
        AstObjMagicId::Instance => size_of::<Instance>(),
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
        AstObjMagicId::AstExpression => size_of::<AstExpression>(),
    }
}

pub fn get_callbacks_for_gc() -> MMTKtoVMCallbacks {
    MMTKtoVMCallbacks {
        scan_object,
        get_roots_in_mutator_thread,
        adapt_post_copy,
        get_object_size,
    }
}
