use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseBC;
use crate::value::Value;
use crate::expect_args;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("class", self::class, true),
    ("objectSize", self::object_size, true),
    ("hashcode", self::hashcode, true),
    ("perform:", self::perform, true),
    ("perform:withArguments:", self::perform_with_arguments, true),
    ("perform:inSuperclass:", self::perform_in_super_class, true),
    (
        "perform:withArguments:inSuperclass:",
        self::perform_with_arguments_in_super_class,
        true,
    ),
    ("instVarAt:", self::inst_var_at, true),
    ("instVarAt:put:", self::inst_var_at_put, true),
    ("halt", self::halt, true),
    ("==", self::eq, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn class(interpreter: &mut Interpreter, args: Vec<Value>, universe: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#class";

    expect_args!(SIGNATURE, args, [
        object
    ]);

    interpreter.stack.push(Value::Class(object.class(universe)));
}

fn object_size(interpreter: &mut Interpreter, _: Vec<Value>, _: &mut UniverseBC) {
    const _: &'static str = "Object>>#objectSize";

    interpreter
        .stack
        .push(Value::Integer(std::mem::size_of::<Value>() as i64));
}

fn hashcode(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#hashcode";

    expect_args!(SIGNATURE, args, [
        value
    ]);

    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    let hash = (hasher.finish() as i64).abs();

    interpreter.stack.push(Value::Integer(hash));
}

fn eq(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#==";

    expect_args!(SIGNATURE, args, [
        a,
        b
    ]);

    interpreter.stack.push(Value::Boolean(a == b));
}

fn perform(interpreter: &mut Interpreter, args: Vec<Value>, universe: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#perform:";

    expect_args!(SIGNATURE, args, [
        object,
        Value::Symbol(sym)
    ]);

    let object: Value = object.clone();

    let signature = universe.lookup_symbol(*sym);
    let method = object.lookup_method(universe, *sym);

    match method {
        Some(invokable) => invokable.invoke(interpreter, universe, object, vec![]),
        None => {
            let signature = signature.to_string();
            universe
                .does_not_understand(interpreter, object.clone(), *sym, vec![object.clone()])
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe),
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments(interpreter: &mut Interpreter, args: Vec<Value>, universe: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:";

    expect_args!(SIGNATURE, args, [
        object,
        Value::Symbol(sym),
        Value::Array(arr)
    ]);

    let signature = universe.lookup_symbol(*sym);
    let method = object.lookup_method(universe, *sym);

    match method {
        Some(invokable) => {
            let args = arr.borrow().iter().cloned().collect();
            invokable.invoke(interpreter, universe, object.clone(), args)
        }
        None => {
            let signature = signature.to_string();
            let args = std::iter::once(object.clone())
                .chain(arr.borrow().iter().cloned())
                .collect();
            universe
                .does_not_understand(interpreter, object.clone(), *sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_in_super_class(interpreter: &mut Interpreter, args: Vec<Value>, universe: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#perform:inSuperclass:";

    expect_args!(SIGNATURE, args, [
        object,
        Value::Symbol(sym),
        Value::Class(class)
    ]);

    let signature = universe.lookup_symbol(*sym);
    let method = class.borrow().lookup_method(*sym);

    match method {
        Some(invokable) => invokable.invoke(interpreter, universe, object.clone(), vec![]),
        None => {
            let signature = signature.to_string();
            let args = vec![object.clone()];
            universe
                .does_not_understand(interpreter, Value::Class(class.clone()), *sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments_in_super_class(interpreter: &mut Interpreter, args: Vec<Value>, universe: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:inSuperclass:";

    expect_args!(SIGNATURE, args, [
        object,
        Value::Symbol(sym),
        Value::Array(arr),
        Value::Class(class)
    ]);

    let signature = universe.lookup_symbol(*sym);
    let method = class.borrow().lookup_method(*sym);

    match method {
        Some(invokable) => {
            let args = arr.borrow().iter().cloned().collect();
            invokable.invoke(interpreter, universe, object.clone(), args)
        }
        None => {
            let args = std::iter::once(object.clone())
                .chain(arr.replace(Vec::default()).into_iter())
                .collect();
            let signature = signature.to_string();
            universe
                .does_not_understand(interpreter, Value::Class(class.clone()), *sym, args)
                .unwrap_or_else(|| {
                    panic!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    )
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn inst_var_at(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#instVarAt:";

    expect_args!(SIGNATURE, args, [
        object,
        Value::Integer(index)
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };

    let local = object.lookup_local(index).unwrap_or(Value::Nil);

    interpreter.stack.push(local);
}

fn inst_var_at_put(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
    const SIGNATURE: &'static str = "Object>>#instVarAt:put:";

    expect_args!(SIGNATURE, args, [
        object,
        Value::Integer(index),
        value
    ]);

    let mut obj = object.clone();
    
    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    };

    let local = obj
        .assign_local(index, value.clone())
        .map(|_| obj)
        .unwrap_or(Value::Nil);

    interpreter.stack.push(local);
}

fn halt(_interpreter: &mut Interpreter, _args: Vec<Value>, _: &mut UniverseBC) {
    const _: &'static str = "Object>>#halt";
    println!("HALT"); // so a breakpoint can be put
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
