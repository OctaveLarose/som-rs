use std::convert::TryFrom;
use anyhow::{bail, Error};
use once_cell::sync::Lazy;
use som_core::gc::GCRef;
use crate::convert::Primitive;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("at:", self::at.into_func(), true),
        ("at:put:", self::at_put.into_func(), true),
        ("length", self::length.into_func(), true),
    ])
});

pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| Box::new([("new:", self::new.into_func(), true)]));


fn at(_: &mut Universe, values: GCRef<Vec<Value>>, index: i32) -> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#at:";

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    };
    let value = values.borrow().get(index).cloned().unwrap_or(Value::NIL);

    Ok(value)
}

fn at_put(_: &mut Universe, values: GCRef<Vec<Value>>, index: i32, value: Value) -> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#at:put:";

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    };
    if let Some(location) = values.borrow_mut().get_mut(index) {
        *location = value;
    }
    Ok(Value::Array(values))
}

fn length(_: &mut Universe, values: GCRef<Vec<Value>>)-> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#length";

    let length = values.borrow().len();
    match i32::try_from(length) {
        Ok(length) => Ok(Value::Integer(length)),
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn new(universe: &mut Universe, _: Value, count: i32)-> Result<Value, Error> {
    const SIGNATURE: &str = "Array>>#new:";
    
    match usize::try_from(count) {
        Ok(length) => Ok(Value::Array(GCRef::<Vec<Value>>::alloc(vec![
            Value::NIL;
            length
        ], &mut universe.gc_interface))),
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    }
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}
