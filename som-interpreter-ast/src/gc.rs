use crate::ast::{
    AstBinaryDispatch, AstBlock, AstDispatchNode, AstExpression, AstLiteral, AstNAryDispatch, AstSuperMessage, AstTernaryDispatch, AstUnaryDispatch,
    InlinedNode,
};
use crate::nodes::global_read::GlobalNode;
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
    SliceVal = 102,
    Block = 103,
    Method = 104,
    SliceLiteral = ASTLITERAL_SLICE_ID as isize,
    Class = 106,
    Instance = 107,
    AstExpression = 108,
    GlobalNode = 109,
    InlinedNode = 110,
    AstUnaryDispatch = 111,
    AstBinaryDispatch = 112,
    AstTernaryDispatch = 113,
    AstNaryDispatch = 114,
    AstSuperMsg = 115,
    SliceAstExpr = ASTEXPRESSION_SLICE_ID as isize,
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
        AstObjMagicId::SliceVal as u8
    }
}

const ASTLITERAL_SLICE_ID: u8 = 105;
impl SupportedSliceType for AstLiteral {
    fn get_magic_gc_slice_id() -> u8 {
        ASTLITERAL_SLICE_ID
    }
}

const ASTEXPRESSION_SLICE_ID: u8 = 116;
impl SupportedSliceType for AstExpression {
    fn get_magic_gc_slice_id() -> u8 {
        ASTEXPRESSION_SLICE_ID
    }
}

impl HasTypeInfoForGC for VecValue {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::SliceVal as u8
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

impl HasTypeInfoForGC for GlobalNode {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::GlobalNode as u8
    }
}

impl HasTypeInfoForGC for InlinedNode {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::InlinedNode as u8
    }
}

impl HasTypeInfoForGC for AstUnaryDispatch {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::AstUnaryDispatch as u8
    }
}

impl HasTypeInfoForGC for AstBinaryDispatch {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::AstBinaryDispatch as u8
    }
}

impl HasTypeInfoForGC for AstTernaryDispatch {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::AstTernaryDispatch as u8
    }
}

impl HasTypeInfoForGC for AstNAryDispatch {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::AstNaryDispatch as u8
    }
}

impl HasTypeInfoForGC for AstSuperMessage {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::AstSuperMsg as u8
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
    fn visit_dispatch_node(dispatch_node: &AstDispatchNode, slot_visitor: &mut dyn SlotVisitor<SOMSlot>) {
        visit_expr(&dispatch_node.receiver, slot_visitor);
        if let Some(cache) = &dispatch_node.inline_cache {
            slot_visitor.visit_slot(SOMSlot::from(&cache.0));
            slot_visitor.visit_slot(SOMSlot::from(&cache.1));
        }
    }

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
                        slot_visitor.visit_slot(SOMSlot::from(&method_def.body.exprs));
                        // for expr in method_def.body.exprs.iter() {
                        //     visit_expr(expr, slot_visitor)
                        // }
                    }
                    MethodKind::TrivialLiteral(trivial_lit) => visit_literal(&trivial_lit.literal, slot_visitor),
                    MethodKind::TrivialGlobal(trivial_global) => {
                        slot_visitor.visit_slot(SOMSlot::from(&trivial_global.global_name));
                    }
                    MethodKind::Primitive(_) | MethodKind::TrivialGetter(_) | MethodKind::TrivialSetter(_) => {}
                }
            }
            AstObjMagicId::Instance => {
                let instance: &Instance = object.to_raw_address().as_ref();

                slot_visitor.visit_slot(SOMSlot::from(&instance.class));

                let instance_as_gc: Gc<Instance> = object.to_raw_address().into();
                for i in 0..instance.class().get_nbr_fields() {
                    let val: &Value = Instance::lookup_field(&instance_as_gc, i as u8);
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
                slot_visitor.visit_slot(SOMSlot::from(&ast_block.body.exprs));
                // for expr in ast_block.body.exprs.iter() {
                //     visit_expr(expr, slot_visitor)
                // }
            }
            AstObjMagicId::SliceLiteral => {
                let literal_vec: GcSlice<AstLiteral> = GcSlice::from(object.to_raw_address());
                for lit in literal_vec.iter() {
                    visit_literal(lit, slot_visitor)
                }
            }
            AstObjMagicId::SliceVal => {
                let array_val: GcSlice<Value> = GcSlice::from(object.to_raw_address());
                for val in array_val.iter() {
                    visit_value(val, slot_visitor)
                }
            }
            AstObjMagicId::SliceAstExpr => {
                let array_expr: GcSlice<AstExpression> = GcSlice::from(object.to_raw_address());
                for val in array_expr.iter() {
                    visit_expr(val, slot_visitor)
                }
            }
            AstObjMagicId::AstExpression => {
                let expr: &AstExpression = object.to_raw_address().as_ref();
                visit_expr(expr, slot_visitor);
            }
            AstObjMagicId::AstUnaryDispatch => {
                let expr: &AstUnaryDispatch = object.to_raw_address().as_ref();
                visit_dispatch_node(&expr.dispatch_node, slot_visitor);
            }
            AstObjMagicId::AstBinaryDispatch => {
                let expr: &AstBinaryDispatch = object.to_raw_address().as_ref();
                visit_dispatch_node(&expr.dispatch_node, slot_visitor);
                visit_expr(&expr.arg, slot_visitor);
            }
            AstObjMagicId::AstTernaryDispatch => {
                let expr: &AstTernaryDispatch = object.to_raw_address().as_ref();
                visit_dispatch_node(&expr.dispatch_node, slot_visitor);
                visit_expr(&expr.arg1, slot_visitor);
                visit_expr(&expr.arg2, slot_visitor);
            }
            AstObjMagicId::AstNaryDispatch => {
                let expr: &AstNAryDispatch = object.to_raw_address().as_ref();
                visit_dispatch_node(&expr.dispatch_node, slot_visitor);
                slot_visitor.visit_slot(SOMSlot::from(&expr.values));
            }
            AstObjMagicId::AstSuperMsg => {
                let expr: &AstSuperMessage = object.to_raw_address().as_ref();
                slot_visitor.visit_slot(SOMSlot::from(&expr.super_class));
                slot_visitor.visit_slot(SOMSlot::from(&expr.values));
            }
            AstObjMagicId::GlobalNode => {
                let global_node: &GlobalNode = object.to_raw_address().as_ref();
                if let Some(val) = global_node.cached_entry.as_ref() {
                    visit_value(val, slot_visitor);
                }
            }
            AstObjMagicId::InlinedNode => {
                let inlined_node: &InlinedNode = object.to_raw_address().as_ref();
                match inlined_node {
                    InlinedNode::IfInlined(if_inlined) => {
                        visit_expr(&if_inlined.cond_expr, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&if_inlined.body_instrs.exprs));
                    }
                    InlinedNode::IfNilInlined(if_nil_inlined) => {
                        visit_expr(&if_nil_inlined.cond_expr, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&if_nil_inlined.body_instrs.exprs));
                    }
                    InlinedNode::IfTrueIfFalseInlined(if_true_if_false_inlined) => {
                        visit_expr(&if_true_if_false_inlined.cond_expr, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&if_true_if_false_inlined.body_1_instrs.exprs));
                        slot_visitor.visit_slot(SOMSlot::from(&if_true_if_false_inlined.body_2_instrs.exprs));
                    }
                    InlinedNode::IfNilIfNotNilInlined(if_nil_if_not_nil_inlined) => {
                        visit_expr(&if_nil_if_not_nil_inlined.cond_expr, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&if_nil_if_not_nil_inlined.body_1_instrs.exprs));
                        slot_visitor.visit_slot(SOMSlot::from(&if_nil_if_not_nil_inlined.body_2_instrs.exprs));
                    }
                    InlinedNode::WhileInlined(while_inlined) => {
                        slot_visitor.visit_slot(SOMSlot::from(&while_inlined.cond_instrs.exprs));
                        slot_visitor.visit_slot(SOMSlot::from(&while_inlined.body_instrs.exprs));
                    }
                    InlinedNode::OrInlined(or_inlined) => {
                        visit_expr(&or_inlined.first, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&or_inlined.second.exprs));
                    }
                    InlinedNode::AndInlined(and_inlined) => {
                        visit_expr(&and_inlined.first, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&and_inlined.second.exprs));
                    }
                    InlinedNode::ToDoInlined(to_do_inlined) => {
                        visit_expr(&to_do_inlined.start, slot_visitor);
                        visit_expr(&to_do_inlined.end, slot_visitor);
                        slot_visitor.visit_slot(SOMSlot::from(&to_do_inlined.body.exprs));
                    }
                }
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
    match expr {
        AstExpression::LocalExit(expr)
        | AstExpression::NonLocalExit(expr, _)
        | AstExpression::LocalVarWrite(_, expr)
        | AstExpression::ArgWrite(_, _, expr)
        | AstExpression::FieldWrite(_, expr)
        | AstExpression::NonLocalVarWrite(_, _, expr) => {
            slot_visitor.visit_slot(SOMSlot::from(expr));
            // visit_expr(expr, slot_visitor)
        }
        AstExpression::Block(blk) => slot_visitor.visit_slot(SOMSlot::from(blk)),
        AstExpression::Literal(lit) => visit_literal(lit, slot_visitor),
        AstExpression::InlinedCall(inlined_node) => slot_visitor.visit_slot(SOMSlot::from(inlined_node)),
        AstExpression::UnaryDispatch(dispatch) => {
            slot_visitor.visit_slot(SOMSlot::from(dispatch));
        }
        AstExpression::BinaryDispatch(dispatch) => {
            slot_visitor.visit_slot(SOMSlot::from(dispatch));
        }
        AstExpression::TernaryDispatch(dispatch) => {
            slot_visitor.visit_slot(SOMSlot::from(dispatch));
        }
        AstExpression::NAryDispatch(dispatch) => {
            slot_visitor.visit_slot(SOMSlot::from(dispatch));
        }
        AstExpression::SuperMessage(super_message) => {
            slot_visitor.visit_slot(SOMSlot::from(super_message));
        }
        AstExpression::GlobalRead(global_node) => {
            slot_visitor.visit_slot(SOMSlot::from(global_node));
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
        AstObjMagicId::SliceLiteral => {
            let literals: GcSlice<AstLiteral> = GcSlice::from(object.to_raw_address());
            literals.get_true_size()
        }
        AstObjMagicId::SliceVal => {
            let values: GcSlice<Value> = GcSlice::from(object.to_raw_address());
            values.get_true_size()
        }
        AstObjMagicId::SliceAstExpr => {
            let exprs: GcSlice<AstExpression> = GcSlice::from(object.to_raw_address());
            exprs.get_true_size()
        }
        AstObjMagicId::Method => size_of::<Method>(),
        AstObjMagicId::Block => size_of::<Block>(),
        AstObjMagicId::Class => size_of::<Class>(),
        AstObjMagicId::AstExpression => size_of::<AstExpression>(),
        AstObjMagicId::GlobalNode => size_of::<GlobalNode>(),
        AstObjMagicId::InlinedNode => size_of::<InlinedNode>(),
        AstObjMagicId::AstUnaryDispatch => size_of::<AstUnaryDispatch>(),
        AstObjMagicId::AstBinaryDispatch => size_of::<AstBinaryDispatch>(),
        AstObjMagicId::AstTernaryDispatch => size_of::<AstTernaryDispatch>(),
        AstObjMagicId::AstNaryDispatch => size_of::<AstNAryDispatch>(),
        AstObjMagicId::AstSuperMsg => size_of::<AstSuperMessage>(),
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
