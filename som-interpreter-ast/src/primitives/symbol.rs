use std::rc::Rc;

use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseAST;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("asString", self::as_string, true),
    ("concatenate:", self::concatenate, true)
];

pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn as_string(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Symbol>>#asString";

    expect_args!(SIGNATURE, args, [
        Value::Symbol(sym) => sym,
    ]);

    Return::Local(Value::String(Rc::new(
        universe.lookup_symbol(sym).to_string(),
    )))
}

fn concatenate(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Symbol>>#concatenate:";

    expect_args!(SIGNATURE, args, [
        s1 => s1,
        s2 => s2,
    ]);

    let s1 = match s1 {
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };
    let s2 = match s2 {
        Value::String(ref value) => value.as_str(),
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong types", SIGNATURE),
    };

    let interned = universe.intern_symbol(format!("{}{}", s1, s2).as_str());
    Return::Local(Value::Symbol(interned))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}
