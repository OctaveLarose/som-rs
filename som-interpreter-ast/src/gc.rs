use crate::ast::{AstBlock, AstExpression, AstLiteral, InlinedNode};
use crate::block::Block;
use crate::class::Class;
use crate::frame::{Frame, FrameAccess};
use crate::instance::Instance;
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::UNIVERSE_RAW_PTR;
use log::debug;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::{ObjectModel, SlotVisitor};
use mmtk::Mutator;
use som_gc::gc_interface::{HasTypeInfoForGC, MMTKtoVMCallbacks, BIGINT_MAGIC_ID, STRING_MAGIC_ID, VECU8_MAGIC_ID};
use som_gc::gcref::GCRef;
use som_gc::object_model::VMObjectModel;
use som_gc::slot::{SOMSlot, ValueSlot};
use som_gc::SOMVM;
use std::ops::Deref;

// Mine. to put in GC headers
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AstObjMagicId {
    String = STRING_MAGIC_ID as isize,
    BigInt = BIGINT_MAGIC_ID as isize,
    ArrayU8 = VECU8_MAGIC_ID as isize,
    Frame = 100,
    AstBlock = 101,
    ArrayVal = 102,
    Block = 103,
    Method = 104,
    VecAstLiteral = 105,
    Class = 106,
    Instance = 107,
}

// TODO: HACK. this is to be able to define a magic id for it. what we REALLY need is a GCSlice<T> type.
pub struct VecValue(pub Vec<Value>);

impl Deref for VecValue {
    type Target = Vec<Value>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// HACK: ditto.
#[derive(Debug)]
pub struct VecAstLiteral(pub Vec<AstLiteral>);

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

impl HasTypeInfoForGC for VecAstLiteral {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::VecAstLiteral as u8
    }
}

impl HasTypeInfoForGC for Frame {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::Frame  as u8
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

// --- Scanning

// When GC triggers while we're allocating a frame, the arguments we want to add to that frame are being passed as an argument to the frame allocation function.
// This means the GC does NOT know how to reach them, and we have to inform it ourselves... So whenever we allocate a frame, we store a pointer to its arguments before that.
// It's possible this isn't just an issue when allocating frames, and that we need argument pointers to other values being initialized when we trigger GC. But I assume, and hope, not.
// It's not very pretty. But I'm not sure how else to fix it at the moment.
pub static mut FRAME_ARGS_PTR: Option<*const Vec<Value>> = None;

fn get_roots_in_mutator_thread(_mutator: &mut Mutator<SOMVM>) -> Vec<SOMSlot> {
    debug!("calling scan_roots_in_mutator_thread");
    unsafe {
        let mut to_process: Vec<SOMSlot> = vec![];

        // walk the frame list.
        let current_frame_addr = &(*UNIVERSE_RAW_PTR).current_frame;
        debug!("scanning root: current_frame");
        to_process.push(SOMSlot::from_address(Address::from_ref(current_frame_addr)));

        // walk globals (includes core classes)
        debug!("scanning roots: globals");
        for (_name, val) in (*UNIVERSE_RAW_PTR).globals.iter() {
            if val.is_ptr_type() {
                to_process.push(SOMSlot::from_value(val.as_u64()));
            }
        }
        
        if let Some(frame_args) = FRAME_ARGS_PTR {
            debug!("scanning roots: frame arguments (frame allocation triggered a GC)");
            for arg in &*frame_args {
                if arg.is_ptr_type() {
                    to_process.push(SOMSlot::Value(ValueSlot::from_value(arg.as_u64())))
                }
            }
        }

        debug!("scanning roots: finished");
        to_process
    }
}

pub fn scan_object<'a>(
    object: ObjectReference,
    slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)
) {
    unsafe {
        let gc_id: &AstObjMagicId = VMObjectModel::ref_to_header(object).as_ref();

        debug!("entering scan_object (type: {:?})", gc_id);

        match gc_id {
            AstObjMagicId::Frame => {
                let frame: &mut Frame = object.to_raw_address().as_mut_ref();

                if !frame.prev_frame.is_empty() {
                    let prev_frame_slot_addr = Address::from_ref(&frame.prev_frame);
                    slot_visitor.visit_slot(SOMSlot::from_address(prev_frame_slot_addr));
                }

                // ew
                let gcref_frame: GCRef<Frame> = GCRef::from_u64(object.to_raw_address().as_usize() as u64);

                for i in 0..frame.nbr_locals {
                    let val: Value = gcref_frame.lookup_local(i);
                    visit_value(&val, slot_visitor)
                }

                for i in 0..frame.nbr_args {
                    let val: Value = gcref_frame.lookup_argument(i);
                    visit_value(&val, slot_visitor)
                }
            }
            AstObjMagicId::Class => {
                let class: &mut Class = object.to_raw_address().as_mut_ref();

                slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&class.class)));

                if let Some(_) = class.super_class {
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(class.super_class.as_ref().unwrap())));
                }

                for (_, method_ref) in class.methods.iter() {
                    slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(method_ref)))
                }

                for field_ref in class.fields.iter() {
                    visit_value(field_ref, slot_visitor)
                }
            }
            AstObjMagicId::Method => {
                let method: &mut Method = object.to_raw_address().as_mut_ref();

                // we shouldn't need to scan the holder. because we ASSUME that when we encounter a method, we did so through a class.
                // I'm not sure in what case this isn't valid.
                // slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&method.holder)));

                match &method.kind {
                    MethodKind::Defined(_method_def) => {
                        // I -think- we don't need to visit expressions here?
                        for expr in &_method_def.body.exprs {
                            visit_expr(expr, slot_visitor)
                        }
                    }
                    MethodKind::TrivialLiteral(trivial_lit) => {
                        visit_literal(&trivial_lit.literal, slot_visitor)
                    }
                    MethodKind::Primitive(_) | MethodKind::TrivialGlobal(_) | MethodKind::TrivialGetter(_) | MethodKind::TrivialSetter(_) | MethodKind::NotImplemented(_) => {}
                    MethodKind::Specialized(_) => {} // for now, specialized methods don't contain data that needs to be traced.
                }
            }
            AstObjMagicId::Instance => {
                let instance: &mut Instance = object.to_raw_address().as_mut_ref();

                // slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&instance.class)));

                for val in &instance.locals {
                    visit_value(&val, slot_visitor)
                }
            }
            AstObjMagicId::Block => {
                let block: &mut Block = object.to_raw_address().as_mut_ref();
                slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&block.frame)));
                slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&block.block)));
            }
            AstObjMagicId::AstBlock => {
                let ast_block: &mut AstBlock = object.to_raw_address().as_mut_ref();

                for expr in &ast_block.body.exprs {
                    visit_expr(expr, slot_visitor)
                }
                // I -think- you don't need to scan expressions, once again.
            }
            AstObjMagicId::VecAstLiteral => {
                let literal_vec: &mut Vec<AstLiteral> = object.to_raw_address().as_mut_ref();
                for lit in literal_vec {
                    visit_literal(lit, slot_visitor)
                }
            }
            AstObjMagicId::ArrayVal => {
                let array_val: &mut Vec<Value> = object.to_raw_address().as_mut_ref();
                for val in array_val {
                    visit_value(&val, slot_visitor)
                }
            }
            AstObjMagicId::String | AstObjMagicId::BigInt | AstObjMagicId::ArrayU8 => {} // leaf nodes
        }
    }
}

fn visit_value<'a>(val: &Value, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    match val.is_ptr_type() {
        true => slot_visitor.visit_slot(SOMSlot::from_value(val.payload())),
        false => {}
    }
}

fn visit_literal(literal: &AstLiteral, slot_visitor: &mut dyn SlotVisitor<SOMSlot>) {
    match &literal {
        AstLiteral::Symbol(s) | AstLiteral::String(s) => {
            slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(s)))
        }
        AstLiteral::BigInteger(big_int) => {
            slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(big_int)))
        }
        AstLiteral::Array(arr) => {
            slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(arr)))
        }
        AstLiteral::Double(_) | AstLiteral::Integer(_) => {}
    }
}

fn visit_expr(expr: &AstExpression, slot_visitor: &mut dyn SlotVisitor<SOMSlot>) {
    match expr {
        AstExpression::Block(blk) => {
            slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(blk)))
        }
        AstExpression::Literal(lit) => {
            visit_literal(lit, slot_visitor)
        }
        AstExpression::InlinedCall(inlined_node) => {
            match inlined_node.as_ref() {
                InlinedNode::IfInlined(if_inlined) => {
                    visit_expr(&if_inlined.cond_expr, slot_visitor);
                    for expr in &if_inlined.body_instrs.exprs {
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
            }
        }
        AstExpression::LocalExit(expr)
        | AstExpression::NonLocalExit(expr, _)
        | AstExpression::LocalVarWrite(_, expr)
        | AstExpression::ArgWrite(_, _, expr)
        | AstExpression::FieldWrite(_, expr)
        | AstExpression::NonLocalVarWrite(_, _, expr) => {
            visit_expr(expr, slot_visitor)
        }
        AstExpression::UnaryDispatch(dispatch) => {
            visit_expr(&dispatch.dispatch_node.receiver, slot_visitor);
            // if let Some(cache) = dispatch.dispatch_node.inline_cache {
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.0)));
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.1)));
            // }
        }
        AstExpression::BinaryDispatch(dispatch) => {
            visit_expr(&dispatch.dispatch_node.receiver, slot_visitor);
            // if let Some(cache) = dispatch.dispatch_node.inline_cache {
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.0)));
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.1)));
            // }
            visit_expr(&dispatch.arg, slot_visitor)
        }
        AstExpression::TernaryDispatch(dispatch) => {
            visit_expr(&dispatch.dispatch_node.receiver, slot_visitor);
            // if let Some(cache) = dispatch.dispatch_node.inline_cache {
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.0)));
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.1)));
            // }
            visit_expr(&dispatch.arg1, slot_visitor);
            visit_expr(&dispatch.arg2, slot_visitor);
        }
        AstExpression::NAryDispatch(dispatch) => {
            visit_expr(&dispatch.dispatch_node.receiver, slot_visitor);
            // if let Some(cache) = dispatch.dispatch_node.inline_cache {
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.0)));
            //     slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&cache.1)));
            // }
            for arg in &dispatch.values {
                visit_expr(arg, slot_visitor);
            }
        }
        AstExpression::SuperMessage(super_message) => {
            // slot_visitor.visit_slot(SOMSlot::from_address(Address::from_ref(&super_message.super_class)));
            for arg in &super_message.values {
                visit_expr(arg, slot_visitor);
            }
        }
        AstExpression::GlobalRead(..)
        | AstExpression::LocalVarRead(..)
        | AstExpression::NonLocalVarRead(..)
        | AstExpression::ArgRead(..)
        | AstExpression::FieldRead(..) => {} // leaf nodes
    }
}

pub fn get_callbacks_for_gc() -> MMTKtoVMCallbacks {
    MMTKtoVMCallbacks {
        scan_object_fn: scan_object,
        get_roots_in_mutator_thread_fn: get_roots_in_mutator_thread,
    }
}