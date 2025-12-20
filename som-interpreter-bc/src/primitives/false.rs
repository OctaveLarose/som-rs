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
        ("and:", self::and.into_func(), true),
        ("&&", self::and.into_func(), true),
        ("or:", self::or_and_if_false.into_func(), true),
        ("||:", self::or_and_if_false.into_func(), true),
        ("ifFalse:", self::or_and_if_false.into_func(), true),
        ("ifTrue:", self::if_true.into_func(), true),
    ])
});

pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

fn not(_: Value) -> Result<bool, Error> {
    Ok(true)
}

fn and(_self: Value, _other: Value) -> Result<bool, Error> {
    Ok(false)
}

fn or_and_if_false(interpreter: &mut Interpreter, universe: &mut Universe) -> Result<(), Error> {
    let cond_val = *interpreter.get_current_frame().stack_last();

    if cond_val.as_block().is_some() {
        // if it's a block: we execute "other" by creating a new frame.
        interpreter.push_block_frame(1, &mut universe.gc_interface);
        interpreter.get_current_frame().prev_frame.remove_n_last_elements(1); // the "False". the "Block" was already consumed and put into the new frame
    } else {
        // if it's not a block... we remove the arguments off the stack, and add the result back to
        // it ourselves: that being the "other" value.
        interpreter.get_current_frame().remove_n_last_elements(2);
        interpreter.get_current_frame().stack_push(cond_val);
    }
    Ok(())
}

fn if_true(_self: Value, _other: Value) -> Result<Value, Error> {
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
