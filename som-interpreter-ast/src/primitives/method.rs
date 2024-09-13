use crate::expect_args;
use crate::invokable::{Invoke, Return};
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseAST;
use crate::value::Value;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("holder", self::holder, true),
    ("signature", self::signature, true),
    ("invokeOn:with:", self::invoke_on_with, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn holder(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Method>>#holder";

    expect_args!(SIGNATURE, args, [
        Value::Invokable(invokable) => invokable,
    ]);

    let holder = invokable.to_obj().holder();
    Return::Local(Value::Class(*holder))

    // match maybe_holder {
    //     Some(holder) => Return::Local(Value::Class(holder)),
    //     None => Return::Exception(format!(
    //         "'{}': method holder has been collected",
    //         SIGNATURE
    //     )),
    // }
}

fn signature(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Method>>#signature";

    expect_args!(SIGNATURE, args, [
        Value::Invokable(invokable) => invokable,
    ]);

    let sym = universe.intern_symbol(invokable.to_obj().signature());
    Return::Local(Value::Symbol(sym))
}

fn invoke_on_with(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &str = "Method>>#invokeOn:with:";

    expect_args!(SIGNATURE, args, [
        Value::Invokable(invokable) => invokable,
        receiver => receiver,
        Value::Array(args) => args,
    ]);

    let args = std::iter::once(receiver.clone())
        .chain(args.borrow().iter().cloned())
        .collect();
    
    let invoke_result = invokable.to_obj().invoke(universe, args);
    invoke_result
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
