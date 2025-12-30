use crate::compiler::Literal;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::{Method, MethodInfo};
use crate::{INTERPRETER_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use log::{debug, trace};
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::{ObjectModel, SlotVisitor};
use mmtk::Mutator;
use num_bigint::BigInt;
use som_gc::gc_interface::{GcType, MMTKtoVMCallbacks, BIGINT_MAGIC_ID, STRING_MAGIC_ID};
use som_gc::gcref::Gc;
use som_gc::gcslice::{GcSlice, SupportedSliceType};
use som_gc::object_model::VMObjectModel;
use som_gc::slot::SOMSlot;
use som_gc::SOMVM;
use std::ops::{Deref, DerefMut};

/// Every GC object starts with this ID to identify what its type is.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum GcIdentifier {
    String = STRING_MAGIC_ID as isize,
    BigInt = BIGINT_MAGIC_ID as isize,
    Frame = 100,
    Block = 101,
    Instance = 102,
    Method = 103,
    MethodInfo = 104,
    Class = 105,
    ArrayVal = 106,
    ArrayLiteral = 107,
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
        GcIdentifier::ArrayVal as u8
    }
}

impl GcType for VecValue {
    fn get_magic_gc_id() -> u8 {
        GcIdentifier::ArrayVal as u8
    }

    fn scan_object(_self: Gc<Self>, _scan_fn: &mut dyn FnMut(SOMSlot)) {
        let slice: GcSlice<Value> = GcSlice::new(Address::from_ptr(_self.ptr));
        for val in slice.iter() {
            visit_value(val, _scan_fn)
        }
    }

    fn get_size_in_memory(_self: Gc<Self>) -> usize {
        let slice: GcSlice<Value> = GcSlice::new(Address::from_ptr(_self.ptr));
        slice.get_true_size()
    }
}

impl SupportedSliceType for Literal {
    fn get_magic_gc_slice_id() -> u8 {
        GcIdentifier::ArrayLiteral as u8
    }
}

#[derive(Clone, Debug)]
pub struct VecLiteral(pub GcSlice<Literal>);

impl Deref for VecLiteral {
    type Target = GcSlice<Literal>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for VecLiteral {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl GcType for VecLiteral {
    fn get_magic_gc_id() -> u8 {
        GcIdentifier::ArrayLiteral as u8
    }

    fn scan_object(_self: Gc<Self>, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        let slice: GcSlice<Literal> = GcSlice::new(Address::from_ptr(_self.ptr));
        for val in slice.iter() {
            visit_literal(val, visit_slot_fn)
        }
    }

    fn get_size_in_memory(_self: Gc<Self>) -> usize {
        let slice: GcSlice<Literal> = GcSlice::new(Address::from_ptr(_self.ptr));
        slice.get_true_size()
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

/// Visit a literal type.
pub fn visit_literal(lit: &Literal, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
    match lit {
        Literal::Block(blk) => visit_slot_fn(SOMSlot::from(blk)),
        Literal::String(str) => visit_slot_fn(SOMSlot::from(str)),
        Literal::BigInteger(bigint) => visit_slot_fn(SOMSlot::from(bigint)),
        Literal::Array(arr) => visit_slot_fn(SOMSlot::from(arr.deref())),
        Literal::Symbol(_) | Literal::Double(_) | Literal::Integer(_) => {}
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
            current_frame_addr.current_context.base_method_info.signature
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

        debug!("scanning roots: stack");
        for val in (**INTERPRETER_RAW_PTR_CONST.as_ptr()).stack.iter() {
            visit_value(val, &mut to_process_fn);
        }

        // walk globals (includes core classes)
        debug!("scanning roots: globals");
        for (_name, val) in (**UNIVERSE_RAW_PTR_CONST.as_ptr()).globals.iter() {
            visit_value(val, &mut to_process_fn);
        }

        // we update the core classes in their class also though, to properly move them
        (**UNIVERSE_RAW_PTR_CONST.as_ptr()).core.iter().for_each(|(_, cls_ptr)| to_process.push(SOMSlot::from(cls_ptr)));

        debug!("scanning roots: finished");
        to_process
    }
}

pub fn scan_object<'a>(object: ObjectReference, slot_visitor: &'a mut (dyn SlotVisitor<SOMSlot> + 'a)) {
    unsafe {
        let gc_id: &GcIdentifier = VMObjectModel::ref_to_header(object).as_ref();

        let mut visit_fn = |slot: SOMSlot| {
            slot_visitor.visit_slot(slot);
        };

        trace!("entering scan_object (type: {:?})", gc_id);

        // FEAT: make a macro to avoid duplication with `get_object_size` function maybe? Or maybe leverage the Rust typesystem in some way
        match gc_id {
            GcIdentifier::Frame => Frame::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::Method => Method::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::MethodInfo => MethodInfo::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::Class => Class::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::Block => Block::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::Instance => Instance::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::String => String::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::BigInt => BigInt::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::ArrayVal => VecValue::scan_object(object.to_raw_address().into(), &mut visit_fn),
            GcIdentifier::ArrayLiteral => VecLiteral::scan_object(object.to_raw_address().into(), &mut visit_fn),
        }
    }
}

fn get_object_size(object: ObjectReference) -> usize {
    let gc_id: &GcIdentifier = unsafe { VMObjectModel::ref_to_header(object).as_ref() };

    match gc_id {
        GcIdentifier::String => String::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::BigInt => BigInt::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::Frame => Frame::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::Method => Method::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::MethodInfo => MethodInfo::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::Block => Block::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::Class => Class::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::Instance => Instance::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::ArrayVal => VecValue::get_size_in_memory(object.to_raw_address().into()),
        GcIdentifier::ArrayLiteral => VecLiteral::get_size_in_memory(object.to_raw_address().into()),
    }

    // debug!("get object size invoked ({:?}), and returning {}", gc_id, obj_size);
}

pub fn get_callbacks_for_gc() -> MMTKtoVMCallbacks {
    MMTKtoVMCallbacks {
        scan_object,
        get_roots_in_mutator_thread,
        get_object_size,
    }
}
