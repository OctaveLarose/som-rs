use std::cell::RefCell;
use std::rc::Rc;

use crate::expect_args;
use crate::instance::Instance;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseAST;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("new", self::new, true),
    ("name", self::name, true),
    ("fields", self::fields, true),
    ("methods", self::methods, true),
    ("superclass", self::superclass, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn superclass(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#superclass";

    expect_args!(SIGNATURE, args, [
        Value::Class(class) => class,
    ]);

    let super_class = class.borrow().super_class();
    Return::Local(super_class.map(Value::Class).unwrap_or(Value::Nil))
}

fn new(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#new";

    expect_args!(SIGNATURE, args, [
        Value::Class(class) => class,
    ]);

    let instance = Instance::from_class(class);
    let instance = Rc::new(RefCell::new(instance));
    Return::Local(Value::Instance(instance))
}

fn name(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#name";

    expect_args!(SIGNATURE, args, [
        Value::Class(class) => class,
    ]);

    let sym = universe.intern_symbol(class.borrow().name());
    Return::Local(Value::Symbol(sym))
}

fn methods(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#methods";

    expect_args!(SIGNATURE, args, [
        Value::Class(class) => class,
    ]);

    let methods = class
        .borrow()
        .methods
        .values()
        .map(|invokable| Value::Invokable(invokable.clone()))
        .collect();

    Return::Local(Value::Array(Rc::new(RefCell::new(methods))))
}

fn fields(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Class>>#fields";

    expect_args!(SIGNATURE, args, [
        Value::Class(class) => class,
    ]);

    let fields = class.borrow().local_names.iter()
        .map(|field_name| Value::String(Rc::new(field_name.clone())))
        .collect();

    Return::Local(Value::Array(Rc::new(RefCell::new(fields))))
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
