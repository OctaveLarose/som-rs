use crate::interpreter::Interpreter;
use crate::primitives::PrimInfo;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::{DoubleLike, IntoValue, Primitive};
use crate::value::{HeapValPtr, Value};
use anyhow::{Context, Error};
use num_traits::ToPrimitive;
use once_cell::sync::Lazy;
use som_gc::gcref::Gc;

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
        ("max:", self::max.into_func(), true),
        ("min:", self::min.into_func(), true),
        ("round", self::round.into_func(), true),
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

fn from_string(_: &mut Interpreter, _: &mut Universe, _: Value, string: HeapValPtr<String>) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#fromString:";

    string.deref().parse().with_context(|| format!("`{SIGNATURE}`: could not parse `f64` from string"))
}

fn as_string(_: &mut Interpreter, universe: &mut Universe, receiver: DoubleLike) -> Result<Gc<String>, Error> {
    const SIGNATURE: &str = "Double>>#asString";

    let receiver = promote!(SIGNATURE, receiver);

    Ok(universe.gc_interface.alloc(receiver.to_string()))
}

fn as_integer(_: &mut Interpreter, _: &mut Universe, receiver: f64) -> Result<i32, Error> {
    const _: &str = "Double>>#asInteger";

    Ok(receiver.trunc() as i32)
}

fn sqrt(_: &mut Interpreter, _: &mut Universe, receiver: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#sqrt";

    let receiver = promote!(SIGNATURE, receiver);

    Ok(receiver.sqrt())
}

fn max(_: &mut Interpreter, _: &mut Universe, receiver: f64, other: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#max";

    let other_val = promote!(SIGNATURE, other);
    match other_val >= receiver {
        true => Ok(other_val.into_value()),
        false => Ok(receiver.into_value()),
    }
}

fn min(_: &mut Interpreter, _: &mut Universe, receiver: f64, other: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Double>>#min";

    let other_val = promote!(SIGNATURE, other);
    match other_val >= receiver {
        true => Ok(receiver.into_value()),
        false => Ok(other_val.into_value()),
    }
}

fn round(_: &mut Interpreter, _: &mut Universe, receiver: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#round";

    let receiver = promote!(SIGNATURE, receiver);

    Ok(receiver.round())
}

fn cos(_: &mut Interpreter, _: &mut Universe, receiver: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#cos";

    let receiver = promote!(SIGNATURE, receiver);

    Ok(receiver.cos())
}

fn sin(_: &mut Interpreter, _: &mut Universe, receiver: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#sin";

    let receiver = promote!(SIGNATURE, receiver);

    Ok(receiver.sin())
}

fn eq(_: &mut Interpreter, _: &mut Universe, a: Value, b: Value) -> Result<bool, Error> {
    let Ok(a) = DoubleLike::try_from(a.0) else {
        return Ok(false);
    };

    let Ok(b) = DoubleLike::try_from(b.0) else {
        return Ok(false);
    };

    Ok(DoubleLike::eq(&a, &b))
}

fn eq_eq(_: &mut Interpreter, _: &mut Universe, a: Value, b: Value) -> Result<bool, Error> {
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

fn uneq(_: &mut Interpreter, _: &mut Universe, a: Value, b: Value) -> Result<bool, Error> {
    let Ok(a) = DoubleLike::try_from(a.0) else {
        return Ok(false);
    };

    let Ok(b) = DoubleLike::try_from(b.0) else {
        return Ok(false);
    };

    Ok(!DoubleLike::eq(&a, &b))
}

fn lt(_: &mut Interpreter, _: &mut Universe, a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#<";
    Ok(a < promote!(SIGNATURE, b))
}

fn lt_or_eq(_: &mut Interpreter, _: &mut Universe, a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#<=";
    Ok(a <= promote!(SIGNATURE, b))
}

fn gt(_: &mut Interpreter, _: &mut Universe, a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#>";
    Ok(a > promote!(SIGNATURE, b))
}

fn gt_or_eq(_: &mut Interpreter, _: &mut Universe, a: f64, b: DoubleLike) -> Result<bool, Error> {
    const SIGNATURE: &str = "Double>>#>=";
    Ok(a >= promote!(SIGNATURE, b))
}

fn plus(_: &mut Interpreter, _: &mut Universe, a: DoubleLike, b: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#+";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(a + b)
}

fn minus(_: &mut Interpreter, _: &mut Universe, a: DoubleLike, b: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#-";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(a - b)
}

fn times(_: &mut Interpreter, _: &mut Universe, a: DoubleLike, b: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#*";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(a * b)
}

fn divide(_: &mut Interpreter, _: &mut Universe, a: DoubleLike, b: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#//";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(a / b)
}

fn modulo(_: &mut Interpreter, _: &mut Universe, a: DoubleLike, b: DoubleLike) -> Result<f64, Error> {
    const SIGNATURE: &str = "Double>>#%";

    let a = promote!(SIGNATURE, a);
    let b = promote!(SIGNATURE, b);

    Ok(a % b)
}

fn positive_infinity(_: &mut Interpreter, _: &mut Universe, _: Value) -> Result<f64, Error> {
    const _: &str = "Double>>#positiveInfinity";

    Ok(f64::INFINITY)
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
