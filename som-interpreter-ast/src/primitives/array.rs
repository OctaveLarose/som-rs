use super::PrimInfo;
use crate::gc::VecValue;
use crate::get_args_from_stack;
use crate::primitives::PrimitiveFn;
use crate::universe::{GlobalValueStack, Universe};
use crate::value::convert::FromArgs;
use crate::value::convert::Primitive;
use crate::value::Value;
use anyhow::{bail, Error};
use once_cell::sync::Lazy;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcslice::GcSlice;
use std::convert::TryFrom;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("at:", self::at.into_func(), true),
        ("at:put:", self::at_put.into_func(), true),
        ("length", self::length.into_func(), true),
        ("copy:", self::copy.into_func(), true),
    ])
});

pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([("new:", self::new.into_func(), true)]));

fn at(values: VecValue, index: i32) -> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#at:";

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    };
    let value = values.get_checked(index).cloned().unwrap_or(Value::NIL);

    Ok(value)
}

fn at_put(mut values: VecValue, index: i32, value: Value) -> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#at:put:";

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    };
    if let Some(location) = values.get_checked_mut(index) {
        *location = value;
    }
    Ok(Value::Array(values))
}

fn length(values: VecValue) -> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#length";

    let length = values.len();
    match i32::try_from(length) {
        Ok(length) => Ok(Value::Integer(length)),
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn new(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#new:";

    get_args_from_stack!(stack,
        _a => Value,
        count => i32
    );

    match usize::try_from(count) {
        Ok(length) => Ok(Value::Array(VecValue(
            universe.gc_interface.alloc_slice(&vec![Value::NIL; length], AllocSiteMarker::VecValue),
        ))),
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn copy(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<VecValue, Error> {
    get_args_from_stack! {stack, arr => VecValue};

    let copied_arr: Vec<Value> = arr.iter().copied().collect();
    let allocated: GcSlice<Value> = universe.gc_interface.alloc_slice(&copied_arr, AllocSiteMarker::VecValue);
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
