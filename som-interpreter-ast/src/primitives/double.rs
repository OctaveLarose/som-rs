use super::PrimInfo;
use crate::get_args_from_stack;
use crate::primitives::PrimitiveFn;
use crate::universe::{GlobalValueStack, Universe};
use crate::value::convert::FromArgs;
use crate::value::convert::{DoubleLike, IntoValue, Primitive, StringLike};
use crate::value::Value;
use anyhow::{bail, Error};
use num_traits::ToPrimitive;
use once_cell::sync::Lazy;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("<", self::lt.into_func(), true),
        ("<=", self::lt_or_eq.into_func(), true),
        (">", self::gt.into_func(), true),
        (">=", self::gt_or_eq.into_func(), true),
        ("=", self::eq.into_func(), true),
        ("~=", self::uneq.into_func(), true),
        ("<>", self::uneq.into_func(), true),
        ("==", self::eq_eq.into_func(), true),
        // -----------------
        ("+", self::plus.into_func(), true),
        ("-", self::minus.into_func(), true),
        ("*", self::times.into_func(), true),
        ("//", self::divide.into_func(), true),
        ("%", self::modulo.into_func(), true),
        ("sqrt", self::sqrt.into_func(), true),
        ("round", self::round.into_func(), true),
        ("max:", self::max.into_func(), true),
        ("min:", self::min.into_func(), true),
        ("cos", self::cos.into_func(), true),
        ("sin", self::sin.into_func(), true),
        ("asString", self::as_string.into_func(), true),
        ("asInteger", self::as_integer.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("fromString:", self::from_string.into_func(), true),
        ("PositiveInfinity", self::positive_infinity.into_func(), true),
    ])
});

macro_rules! promote {
    ($signature:expr, $value:expr) => {
        match $value {
            DoubleLike::Double(value) => value,
            DoubleLike::Integer(value) => value as f64,
            DoubleLike::BigInteger(value) => match value.to_f64() {
                Some(value) => value,
                None => {
                    panic!("'{}': `Integer` too big to be converted to `Double`", $signature)
                }
            },
        }
    };
}

fn from_string(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#fromString:";

    get_args_from_stack!(stack,
        _a => Value,
        string => StringLike
    );

    let string = match string {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    match string.parse() {
        Ok(parsed) => Ok(Value::Double(parsed)),
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn as_string(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#asString";

    get_args_from_stack!(stack, receiver => DoubleLike);

    let value = promote!(SIGNATURE, receiver);

    Ok(Value::String(universe.gc_interface.alloc(value.to_string(), AllocSiteMarker::String)))
}

fn as_integer(receiver: f64) -> Result<Value, Error> {
    Ok(Value::Integer(receiver.trunc() as i32))
}

fn sqrt(receiver: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#sqrt";

    let value = promote!(SIGNATURE, receiver);

    Ok(Value::Double(value.sqrt()))
}

fn max(receiver: f64, other: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#max";

    let other_val = promote!(SIGNATURE, other);
    match other_val >= receiver {
        true => Ok(other_val.into_value()),
        false => Ok(receiver.into_value()),
    }
}

fn min(receiver: f64, other: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#min";

    let other_val = promote!(SIGNATURE, other);
    match other_val >= receiver {
        true => Ok(receiver.into_value()),
        false => Ok(other_val.into_value()),
    }
}

fn round(receiver: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#round";

    let value = promote!(SIGNATURE, receiver);

    Ok(Value::Double(value.round()))
}

fn cos(value: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#cos";

    let value = promote!(SIGNATURE, value);

    Ok(Value::Double(value.cos()))
}

fn sin(receiver: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#sin";

    let value = promote!(SIGNATURE, receiver);

    Ok(Value::Double(value.sin()))
}

// TODO: I'm not sure it's the fastest way to go about it. Maybe take in a f64 directly - not sure.
// Ditto for several primitives that are very frequently invoked: is it best to rely on `DoubleLike`, on `Value`, on `f64` directly? A mix of all?
fn eq(a: Value, b: Value) -> Result<Value, Error> {
    Ok(Value::Boolean(a == b))
}

fn eq_eq(a: Value, b: Value) -> Result<bool, Error> {
    let Ok(a) = DoubleLike::try_from(a.0) else {
        return Ok(false);
    };

    let Ok(b) = DoubleLike::try_from(b.0) else {
        return Ok(false);
    };

    match (a, b) {
        (DoubleLike::Double(a), DoubleLike::Double(b)) => Ok(a == b),
        _ => Ok(false),
    }
}

fn uneq(a: DoubleLike, b: DoubleLike) -> Result<bool, Error> {
    Ok(!DoubleLike::eq(&a, &b))
}

fn lt(a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#<";
    Ok(a < promote!(SIGNATURE, b))
}

fn lt_or_eq(a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#<=";
    Ok(a <= promote!(SIGNATURE, b))
}

fn gt(a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#>";
    Ok(a > promote!(SIGNATURE, b))
}

fn gt_or_eq(a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#>=";
    Ok(a >= promote!(SIGNATURE, b))
}

fn plus(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#+";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(Value::Double(a + b))
}

fn minus(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#-";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(Value::Double(a - b))
}

fn times(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#*";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(Value::Double(a * b))
}

fn divide(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#//";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(Value::Double(a / b))
}

fn modulo(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#%";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(Value::Double(a % b))
}

fn positive_infinity(_: Value) -> Result<Value, Error> {
    const _: &str = "Double>>#positiveInfinity";

    Ok(Value::Double(f64::INFINITY))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
