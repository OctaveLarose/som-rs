use anyhow::Error;
use once_cell::sync::Lazy;

use crate::interpreter::Interpreter;
use crate::primitives::PrimInfo;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::Primitive;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("not", self::not.into_func(), true),
        ("or:", self::or.into_func(), true),
        ("||", self::or.into_func(), true),
        ("and:", self::and_if_true.into_func(), true),
        ("&&", self::and_if_true.into_func(), true),
        ("ifTrue:", self::and_if_true.into_func(), true),
        ("ifFalse:", self::if_false.into_func(), true),
    ])
});

pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

fn not(_: &mut Interpreter, _universe: &mut Universe, _: bool) -> Result<bool, Error> {
    Ok(false)
}

fn or(_: &mut Interpreter, _universe: &mut Universe, _self: Value, _other: Value) -> Result<bool, Error> {
    Ok(true)
}

/// See equivalent function for the false primitive.
fn and_if_true(interpreter: &mut Interpreter, universe: &mut Universe, _self: Value, other: Value) -> Result<(), Error> {
    if other.as_block().is_some() {
        interpreter.push_block_frame(1, universe.gc_interface);
    } else {
        interpreter.current_frame.remove_n_last_elements(2);
        interpreter.current_frame.stack_push(other);
    }
    Ok(())
}

fn if_false(_: &mut Interpreter, _universe: &mut Universe, _self: Value, _other: Value) -> Result<Value, Error> {
    Ok(Value::NIL)
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
