use super::PrimInfo;
use crate::get_args_from_stack;
use crate::primitives::PrimitiveFn;
use crate::universe::{GlobalValueStack, Universe};
use crate::value::convert::FromArgs;
use crate::value::convert::{Primitive, StringLike};
use crate::value::Value;
use anyhow::Error;
use once_cell::sync::Lazy;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_value::value::BaseValue;
use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::Hasher;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("length", self::length.into_func(), true),
        ("hashcode", self::hashcode.into_func(), true),
        ("isLetters", self::is_letters.into_func(), true),
        ("isDigits", self::is_digits.into_func(), true),
        ("isWhiteSpace", self::is_whitespace.into_func(), true),
        ("asSymbol", self::as_symbol.into_func(), true),
        ("concatenate:", self::concatenate.into_func(), true),
        ("primSubstringFrom:to:", self::prim_substring_from_to.into_func(), true),
        ("=", self::eq.into_func(), true),
        ("charAt:", self::char_at.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

fn length(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, value => StringLike);
    // tragically, we do not allow strings to have over 2 billion characters and just cast as i32
    // i apologize to everyone for that. i will strive to be better
    match value {
        StringLike::String(ref value) => Ok(Value::Integer(value.len() as i32)),
        StringLike::Symbol(sym) => Ok(Value::Integer(universe.lookup_symbol(sym).len() as i32)),
        StringLike::Char(_) => Ok(Value::Integer(1)),
    }
}

fn hashcode(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, value => StringLike);
    let value = match value {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let mut hasher = DefaultHasher::new();

    hasher.write(value.as_bytes());

    // match i32::try_from(hasher.finish()) {
    //     Ok(hash) => Ok(Value::Integer(hash)),
    //     Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    // }

    Ok(Value::Integer((hasher.finish() as i32).abs()))
}

fn is_letters(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, value => StringLike);
    let value = match value {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    Ok(Value::Boolean(
        !value.is_empty() && !value.is_empty() && value.chars().all(char::is_alphabetic),
    ))
}

fn is_digits(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, value => StringLike);
    let value = match value {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    Ok(Value::Boolean(!value.is_empty() && value.chars().all(char::is_numeric)))
}

fn is_whitespace(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, value => StringLike);
    let value = match value {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    Ok(Value::Boolean(!value.is_empty() && value.chars().all(char::is_whitespace)))
}

fn concatenate(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, receiver => StringLike, other => StringLike);
    let s1 = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let s2 = match other {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    Ok(Value::String(
        universe.gc_interface.alloc(format!("{}{}", s1, s2), AllocSiteMarker::String),
    ))
}

fn as_symbol(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, value => StringLike);
    match value {
        StringLike::String(ref value) => Ok(Value::Symbol(universe.intern_symbol(value.as_str()))),
        StringLike::Char(char) => Ok(Value::Symbol(universe.intern_symbol(String::from(char).as_str()))),
        StringLike::Symbol(sym) => Ok(Value::Symbol(sym)),
    }
}

fn char_at(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, receiver => StringLike, idx => i32);
    let string = receiver.as_str(|sym| universe.lookup_symbol(sym));

    let char = *string.as_bytes().get((idx - 1) as usize).unwrap();
    let char_val = Value(BaseValue::Char(char.into()));
    Ok(char_val)
}

fn eq(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<bool, Error> {
    get_args_from_stack!(stack, a => Value, b => Value);
    let Ok(a) = StringLike::try_from(a.0) else {
        return Ok(false);
    };

    let Ok(b) = StringLike::try_from(b.0) else {
        return Ok(false);
    };

    Ok(a.eq_stringlike(&b, |sym| universe.lookup_symbol(sym)))
}

fn prim_substring_from_to(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, receiver => StringLike, from => i32, to => i32);
    let from = usize::try_from(from - 1)?;
    let to = usize::try_from(to)?;

    let string = match receiver {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let s = universe.gc_interface.alloc(string.chars().skip(from).take(to - from).collect(), AllocSiteMarker::String);

    Ok(Value::String(s))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
