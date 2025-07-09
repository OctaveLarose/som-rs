use std::convert::{TryFrom, TryInto};

use super::PrimInfo;
use crate::gc::VecValue;
use crate::interpreter::Interpreter;
use crate::pop_args_from_stack;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::{IntoValue, Primitive};
use crate::value::Value;
use anyhow::{Context, Error};
use once_cell::sync::Lazy;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcslice::GcSlice;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("at:", self::at.into_func(), true),
        ("at:put:", self::at_put.into_func(), true),
        ("length", self::length.into_func(), true),
        ("copy:", self::copy.into_func(), true),
        //("putAll:", self::put_all.into_func(), true),
        //("do:", self::do.into_func(), true),
        //("doIndexes:", self::do_indexes.into_func(), true),
    ])
});

pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([("new:", self::new.into_func(), true)]));

fn at(receiver: VecValue, index: i32) -> Result<Value, Error> {
    const _: &str = "Array>>#at:";

    let index = usize::try_from(index - 1)?;

    receiver.get_checked(index).cloned().context("index out of bounds")
}

fn at_put(mut receiver: VecValue, index: i32, value: Value) -> Result<VecValue, Error> {
    const _: &str = "Array>>#at:put:";

    let index = usize::try_from(index - 1)?;

    if let Some(location) = receiver.get_checked_mut(index) {
        *location = value;
    }

    Ok(receiver)
}

fn length(receiver: VecValue) -> Result<i32, Error> {
    receiver.len().try_into().context("could not convert `usize` to `i32`")
}

fn new(interp: &mut Interpreter, universe: &mut Universe) -> Result<(), Error> {
    // this whole thing is an attempt at fixing a GC related bug when allocating an Array
    // I think it did work, to be fair... but TODO clean up.

    std::hint::black_box(&interp.current_frame);

    let count = usize::try_from(interp.get_current_frame().stack_pop().as_integer().unwrap())?;
    interp.get_current_frame().stack_pop(); // receiver is just an unneeded Array class

    let arr_ptr: VecValue = VecValue(universe.gc_interface.alloc_slice(&vec![Value::NIL; count], AllocSiteMarker::VecValue));

    interp.get_current_frame().stack_push(arr_ptr.into_value());
    Ok(())
}

fn copy(interp: &mut Interpreter, universe: &mut Universe) -> Result<VecValue, Error> {
    let arr: VecValue = interp.get_current_frame().stack_last().as_array().unwrap();
    std::hint::black_box(&arr); // paranoia, in case the compiler gets ideas about reusing that variable
    let slice_size = arr.0.get_true_size();
    let slice_addr = universe.gc_interface.request_bytes_for_slice(slice_size, AllocSiteMarker::VecValue);

    pop_args_from_stack!(interp, arr2 => VecValue);
    std::hint::black_box(&arr2);

    let copied_arr: Vec<Value> = arr2.iter().copied().collect();
    let allocated: GcSlice<Value> = universe.gc_interface.write_slice_to_addr(slice_addr, &copied_arr);

    Ok(VecValue(allocated))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
