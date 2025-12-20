use crate::compiler::Literal;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use crate::{INTERPRETER_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use core::mem::size_of;
use log::{debug, trace};
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

#[derive(Clone, Debug)]
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
        BCObjMagicId::ArrayVal as u8
    }
}

pub const GCSLICE_LITERAL_MAGIC_ID: u8 = 101;
impl SupportedSliceType for Literal {
    fn get_magic_gc_slice_id() -> u8 {
        GCSLICE_LITERAL_MAGIC_ID
    }
}

impl GcType for VecValue {
    fn get_magic_gc_id() -> u8 {
        BCObjMagicId::ArrayVal as u8
    }

    fn scan_object(_self: Gc<Self>, _scan_fn: &mut dyn FnMut(SOMSlot)) {
        todo!()
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

pub fn visit_literal(lit: &Literal, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
    match lit {
        Literal::Block(blk) => visit_slot_fn(SOMSlot::from(blk)),
        Literal::String(str) => visit_slot_fn(SOMSlot::from(str)),
        Literal::BigInteger(bigint) => visit_slot_fn(SOMSlot::from(bigint)),
        Literal::Array(arr) => visit_slot_fn(SOMSlot::from(arr)),
        Literal::Symbol(_) | Literal::Double(_) | Literal::Integer(_) => {}
    }
}

pub fn scan_object<'a>(object: ObjectReference, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    unsafe {
        let gc_id: &BCObjMagicId = VMObjectModel::ref_to_header(object).as_ref();

        let mut visit_fn = |slot: SOMSlot| {
            slot_visitor.visit_slot(slot);
        };

        trace!("entering scan_object (type: {:?})", gc_id);

        match gc_id {
            BCObjMagicId::Frame => Frame::scan_object(object.to_raw_address().into(), &mut visit_fn),
            BCObjMagicId::Method => Method::scan_object(object.to_raw_address().into(), &mut visit_fn),
            BCObjMagicId::Class => Class::scan_object(object.to_raw_address().into(), &mut visit_fn),
            BCObjMagicId::Block => Block::scan_object(object.to_raw_address().into(), &mut visit_fn),
            BCObjMagicId::Instance => Instance::scan_object(object.to_raw_address().into(), &mut visit_fn),
            BCObjMagicId::ArrayVal => {
                let arr: GcSlice<Value> = GcSlice::from(object.to_raw_address());
                for val in arr.iter() {
                    visit_value(val, &mut visit_fn)
                }
            }
            BCObjMagicId::ArrayLiteral => {
                let literal_vec: GcSlice<Literal> = GcSlice::from(object.to_raw_address());
                for lit in literal_vec.iter() {
                    visit_literal(lit, &mut visit_fn)
                }
            }
            // leaf nodes: no children.
            BCObjMagicId::String | BCObjMagicId::BigInt => {}
        }
    }
}

fn get_roots_in_mutator_thread(_mutator: &mut Mutator<SOMVM>) -> Vec<SOMSlot> {
    debug!("calling scan_roots_in_mutator_thread");
    unsafe {
        let mut to_process: Vec<SOMSlot> = vec![];
        let mut to_process_fn = |slot: SOMSlot| {
            to_process.push(slot);
        };

        assert!(
            !(*UNIVERSE_RAW_PTR_CONST.as_ptr()).is_null() && !(*INTERPRETER_RAW_PTR_CONST.as_ptr()).is_null(),
            "GC triggered while the system wasn't finished initializing."
        );

        // walk the frame list.
        let current_frame_addr = &*(**INTERPRETER_RAW_PTR_CONST.as_ptr()).current_frame.get();
        debug!(
            "scanning root: current_frame (method: {})",
            current_frame_addr.current_context.get_env().base_method_info.signature
        );
        to_process_fn(SOMSlot::from(current_frame_addr));

        let frame_method_root = &(**INTERPRETER_RAW_PTR_CONST.as_ptr()).frame_method_root;
        if !frame_method_root.is_empty() {
            to_process_fn(SOMSlot::from(frame_method_root));
        }

        let frame_args_root = &(**INTERPRETER_RAW_PTR_CONST.as_ptr()).frame_args_root;
        if let Some(frame_args) = frame_args_root {
            for arg in frame_args {
                visit_value(arg, &mut to_process_fn);
            }
        }

        // walk globals (includes core classes)
        debug!("scanning roots: globals");
        for (_name, val) in (**UNIVERSE_RAW_PTR_CONST.as_ptr()).globals.iter_mut() {
            visit_value(val, &mut to_process_fn);
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
            BCObjMagicId::ArrayVal => {
                let values: GcSlice<Value> = GcSlice::from(object.to_raw_address());
                values.get_true_size()
            }
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

//fn adapt_post_copy(_object: ObjectReference, _original_obj: ObjectReference) {}

pub fn get_callbacks_for_gc() -> MMTKtoVMCallbacks {
    MMTKtoVMCallbacks {
        scan_object,
        get_roots_in_mutator_thread,
        get_object_size,
        //adapt_post_copy,
    }
}
