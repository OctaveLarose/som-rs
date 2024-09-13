use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

use crate::invokable::{Invoke, Return};
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseAST;
use crate::value::Value;
use crate::expect_args;
use crate::value::Value::Nil;

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("halt", self::halt, true),
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
    ("==", self::eq, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn halt(_universe: &mut UniverseAST, _args: Vec<Value>) -> Return{
    const _: &'static str = "Object>>#halt";
    println!("HALT"); // so a breakpoint can be put
    Return::Local(Nil)
}

fn class(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#class";

    expect_args!(SIGNATURE, args, [
        object => object,
    ]);

    Return::Local(Value::Class(object.class(universe)))
}

fn object_size(_: &mut UniverseAST, _: Vec<Value>) -> Return {
    const _: &'static str = "Object>>#objectSize";

    Return::Local(Value::Integer(std::mem::size_of::<Value>() as i64))
}

fn hashcode(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#hashcode";

    expect_args!(SIGNATURE, args, [
        value => value,
    ]);

    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    let hash = (hasher.finish() as i64).abs();

    Return::Local(Value::Integer(hash))
}

fn eq(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#==";

    expect_args!(SIGNATURE, args, [
        a => a,
        b => b,
    ]);

    Return::Local(Value::Boolean(a == b))
}

fn perform(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = object.lookup_method(universe, signature);

    match method {
        Some(invokable) => invokable.to_obj().invoke(universe, vec![object]),
        None => {
            let signature = signature.to_string();
            universe
                .does_not_understand(object.clone(), signature.as_str(), vec![object.clone()])
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = object.lookup_method(universe, signature);

    match method {
        Some(invokable) => {
            // let args = std::iter::once(object)
            //     .chain(arr.replace(Vec::default()))
            //     .collect();
            let args = std::iter::once(object).chain(arr.to_obj().clone()).collect();
            invokable.to_obj().invoke(universe, args)
        }
        None => {
            let signature = signature.to_string();
            // let args = std::iter::once(object.clone())
            //     .chain(arr.to_obj().replace(Vec::default()))
            //     .collect();
            let args = std::iter::once(object.clone()).chain(arr.to_obj().clone()).collect();
            
            universe
                .does_not_understand(object.clone(), signature.as_str(), args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_in_super_class(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:inSuperclass:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Class(class) => class,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(signature);

    match method {
        Some(invokable) => invokable.to_obj().invoke(universe, vec![object]),
        None => {
            let signature = signature.to_string();
            let args = vec![object.clone()];
            universe
                .does_not_understand(Value::Class(class), signature.as_str(), args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn perform_with_arguments_in_super_class(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#perform:withArguments:inSuperclass:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Symbol(sym) => sym,
        Value::Array(arr) => arr,
        Value::Class(class) => class,
    ]);

    let signature = universe.lookup_symbol(sym);
    let method = class.borrow().lookup_method(signature);

    match method {
        Some(invokable) => {
            // let args = std::iter::once(object)
            //     .chain(arr.to_obj().replace(Vec::default()))
            //     .collect();
            let args = std::iter::once(object).chain(arr.to_obj().clone()).collect();
            
            invokable.to_obj().invoke(universe, args)
        }
        None => {
            // let args = std::iter::once(object.clone())
            //     .chain(arr.to_obj().replace(Vec::default()))
            //     .collect();
            let args = std::iter::once(object.clone()).chain(arr.to_obj().clone()).collect();
            
            let signature = signature.to_string();
            universe
                .does_not_understand(Value::Class(class), signature.as_str(), args)
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "'{}': method '{}' not found for '{}'",
                        SIGNATURE,
                        signature,
                        object.to_string(universe)
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

fn inst_var_at(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#instVarAt:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Integer(index) => index,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    };

    let local = match object {
        Value::Instance(c) => {
            c.borrow().locals.get(index).cloned().unwrap_or(Value::Nil)
        }
        Value::Class(c) => {
            c.clone().borrow().fields.get(index).cloned().unwrap_or(Value::Nil)
        },
        _ => unreachable!("instVarAt called not on an instance or a class")
    };

    Return::Local(local)
}

fn inst_var_at_put(_: &mut UniverseAST, args: Vec<Value>) -> Return {
    const SIGNATURE: &'static str = "Object>>#instVarAt:put:";

    expect_args!(SIGNATURE, args, [
        object => object,
        Value::Integer(index) => index,
        value => value,
    ]);

    let index = match usize::try_from(index - 1) {
        Ok(index) => index,
        Err(err) => return Return::Exception(format!("'{}': {}", SIGNATURE, err)),
    };

    let does_have_local = match &object {
        Value::Instance(c) => { c.borrow().locals.len() > index }
        Value::Class(c) => { c.clone().borrow().fields.len() > index },
        _ => unreachable!("instVarAtPut called not on an instance or a class")
    };

    if does_have_local {
        match object {
            Value::Instance(instance) => instance.borrow_mut().assign_local(index, value.clone()),
            Value::Class(class) => class.borrow_mut().assign_field(index, value.clone()),
            v => unreachable!("Assigning a local binding in a {:?} value type?", v),
        }
    }

    Return::Local(value)
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
