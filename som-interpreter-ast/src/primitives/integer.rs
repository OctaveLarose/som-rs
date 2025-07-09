use super::PrimInfo;
use crate::gc::VecValue;
use crate::get_args_from_stack;
use crate::primitives::PrimitiveFn;
use crate::universe::{GlobalValueStack, Universe};
use crate::value::convert::{DoubleLike, FromArgs, IntegerLike, IntoValue, Primitive, StringLike};
use crate::value::Value;
use anyhow::{bail, Error};
use num_bigint::{BigInt, BigUint, Sign, ToBigInt};
use num_traits::{Signed, ToPrimitive};
use once_cell::sync::Lazy;
use rand::Rng;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;

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
        ("/", self::divide.into_func(), true),
        ("//", self::divide_float.into_func(), true),
        ("%", self::modulo.into_func(), true),
        ("rem:", self::remainder.into_func(), true),
        ("&", self::bitand.into_func(), true),
        ("<<", self::shift_left.into_func(), true),
        (">>>", self::shift_right.into_func(), true),
        ("bitXor:", self::bitxor.into_func(), true),
        ("sqrt", self::sqrt.into_func(), true),
        ("max:", self::max.into_func(), true),
        ("min:", self::min.into_func(), true),
        ("abs:", self::abs.into_func(), true),
        ("asString", self::as_string.into_func(), true),
        ("asDouble", self::as_double.into_func(), true),
        ("atRandom", self::at_random.into_func(), true),
        ("as32BitSignedValue", self::as_32bit_signed_value.into_func(), true),
        ("as32BitUnsignedValue", self::as_32bit_unsigned_value.into_func(), true),
        ("to:", self::to.into_func(), true),
    ])
});

pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([("fromString:", self::from_string.into_func(), true)]));

macro_rules! demote {
    ($gc_interface:expr, $expr:expr) => {{
        let value = $expr;
        match value.to_i32() {
            Some(value) => Ok(Value::Integer(value)),
            None => Ok(Value::BigInteger($gc_interface.alloc(value, AllocSiteMarker::BigInt))),
        }
    }};
}

fn from_string(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, _a => Value, string => StringLike);
    let value = match string {
        StringLike::String(ref value) => value.as_str(),
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    match value.parse::<i32>() {
        Ok(a) => Ok(Value::Integer(a)),
        Err(_) => match value.parse::<BigInt>() {
            Ok(b) => Ok(Value::BigInteger(universe.gc_interface.alloc(b, AllocSiteMarker::BigInt))),
            _ => panic!("couldn't turn an int/bigint into a string"),
        },
    }
}

fn as_string(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, receiver => IntegerLike);
    let value = match receiver {
        IntegerLike::Integer(value) => value.to_string(),
        IntegerLike::BigInteger(value) => value.to_string(),
    };

    Ok(Value::String(universe.gc_interface.alloc(value, AllocSiteMarker::String)))
}

fn as_double(receiver: IntegerLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Integer>>#asDouble";

    match receiver {
        IntegerLike::Integer(value) => Ok(Value::Double(value as f64)),
        IntegerLike::BigInteger(value) => match value.to_i64() {
            Some(value) => Ok(Value::Double(value as f64)),
            None => bail!(format!("'{}': `Integer` too big to be converted to `Double`", SIGNATURE)),
        },
    }
}

fn at_random(receiver: IntegerLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Integer>>#atRandom";

    let chosen = match receiver {
        IntegerLike::Integer(value) => rand::rng().random_range(0..=value),
        IntegerLike::BigInteger(_) => {
            bail!(format!("'{}': the range is too big to pick a random value from", SIGNATURE,))
        }
    };

    Ok(Value::Integer(chosen))
}

fn as_32bit_signed_value(receiver: IntegerLike) -> Result<Value, Error> {
    let value = match receiver {
        IntegerLike::Integer(value) => value,
        IntegerLike::BigInteger(value) => match value.to_u32_digits() {
            (Sign::Minus, values) => -(values[0] as i32),
            (Sign::Plus, values) | (Sign::NoSign, values) => values[0] as i32,
        },
    };

    Ok(Value::Integer(value))
}

fn as_32bit_unsigned_value(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<IntegerLike, Error> {
    get_args_from_stack!(stack, receiver => IntegerLike);
    let value = match receiver {
        IntegerLike::Integer(value) => value as u32,
        IntegerLike::BigInteger(value) => {
            // We do this gymnastic to get the 4 lowest bytes from the two's-complement representation.
            let mut values = value.to_signed_bytes_le();
            values.resize(4, 0);
            u32::from_le_bytes(values.try_into().unwrap())
        }
    };

    let value = match value.try_into() {
        Ok(value) => IntegerLike::Integer(value),
        Err(_) => IntegerLike::BigInteger(universe.gc_interface.alloc(BigInt::from(value), AllocSiteMarker::BigInt)),
    };

    Ok(value)
}

fn plus(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "Integer>>#+";

    get_args_from_stack!(stack, a => DoubleLike, b => DoubleLike);

    let heap = &mut universe.gc_interface;
    match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_add(b) {
            Some(value) => Ok(Value::Integer(value)),
            None => demote!(heap, BigInt::from(a) + BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(heap, &*a + &*b)
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b)) | (DoubleLike::Integer(b), DoubleLike::BigInteger(a)) => {
            demote!(heap, &*a + BigInt::from(b))
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => Ok(Value::Double(a + b)),
        (DoubleLike::Integer(a), DoubleLike::Double(b)) | (DoubleLike::Double(b), DoubleLike::Integer(a)) => Ok(Value::Double((a as f64) + b)),
        (DoubleLike::BigInteger(a), DoubleLike::Double(b)) | (DoubleLike::Double(b), DoubleLike::BigInteger(a)) => match a.to_f64() {
            Some(a) => Ok(Value::Double(a + b)),
            None => bail!(format!("'{}': `Integer` too big to be converted to `Double`", SIGNATURE)),
        },
    }
}

fn minus(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "Integer>>#-";

    get_args_from_stack!(stack, a => DoubleLike, b => DoubleLike);
    let heap = &mut universe.gc_interface;

    match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_sub(b) {
            Some(value) => Ok(Value::Integer(value)),
            None => demote!(heap, BigInt::from(a) - BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(heap, &*a - &*b)
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b)) => {
            demote!(heap, &*a - BigInt::from(b))
        }
        (DoubleLike::Integer(a), DoubleLike::BigInteger(b)) => {
            demote!(heap, BigInt::from(a) - &*b)
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => Ok(Value::Double(a - b)),
        (DoubleLike::Integer(a), DoubleLike::Double(b)) | (DoubleLike::Double(b), DoubleLike::Integer(a)) => Ok(Value::Double((a as f64) - b)),
        (DoubleLike::BigInteger(a), DoubleLike::Double(b)) | (DoubleLike::Double(b), DoubleLike::BigInteger(a)) => match a.to_f64() {
            Some(a) => Ok(Value::Double(a - b)),
            None => bail!(format!("'{}': `Integer` too big to be converted to `Double`", SIGNATURE)),
        },
    }
}

fn times(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "Integer>>#*";

    get_args_from_stack!(stack, a => DoubleLike, b => DoubleLike);
    let heap = &mut universe.gc_interface;

    match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_mul(b) {
            Some(value) => Ok(Value::Integer(value)),
            None => demote!(heap, BigInt::from(a) * BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(heap, &*a * &*b)
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b)) | (DoubleLike::Integer(b), DoubleLike::BigInteger(a)) => {
            demote!(heap, &*a * BigInt::from(b))
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => Ok(Value::Double(a * b)),
        (DoubleLike::Integer(a), DoubleLike::Double(b)) | (DoubleLike::Double(b), DoubleLike::Integer(a)) => Ok(Value::Double((a as f64) * b)),
        _ => bail!(format!("'{}': wrong types", SIGNATURE)),
    }
}

fn divide(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "Integer>>#/";

    get_args_from_stack!(stack, a => DoubleLike, b => DoubleLike);

    let heap = &mut universe.gc_interface;

    match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => match a.checked_div(b) {
            Some(value) => Ok(Value::Integer(value)),
            None => demote!(heap, BigInt::from(a) / BigInt::from(b)),
        },
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => {
            demote!(heap, &*a / &*b)
        }
        (DoubleLike::BigInteger(a), DoubleLike::Integer(b)) | (DoubleLike::Integer(b), DoubleLike::BigInteger(a)) => {
            demote!(heap, &*a / BigInt::from(b))
        }
        (DoubleLike::Double(a), DoubleLike::Double(b)) => Ok(Value::Double(a / b)),
        (DoubleLike::Integer(a), DoubleLike::Double(b)) | (DoubleLike::Double(b), DoubleLike::Integer(a)) => Ok(Value::Double((a as f64) / b)),
        _ => bail!(format!("'{}': wrong types", SIGNATURE)),
    }
}

fn divide_float(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    const SIGNATURE: &str = "Integer>>#//";

    match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => Ok(Value::Double((a as f64) / (b as f64))),
        (DoubleLike::Integer(a), DoubleLike::Double(b)) | (DoubleLike::Double(b), DoubleLike::Integer(a)) => Ok(Value::Double((a as f64) / b)),
        (DoubleLike::Double(a), DoubleLike::Double(b)) => Ok(Value::Double(a / b)),
        _ => bail!(format!("'{}': wrong types", SIGNATURE)),
    }
}

fn modulo(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, a => IntegerLike, b => i32);

    match a {
        IntegerLike::Integer(a) => {
            let result = a % b;
            if result.signum() != b.signum() {
                Ok(Value::Integer((result + b) % b))
            } else {
                Ok(Value::Integer(result))
            }
        }
        IntegerLike::BigInteger(a) => {
            let result = &*a % b;
            if result.is_positive() != b.is_positive() {
                demote!(&mut universe.gc_interface, (result + b) % b)
            } else {
                demote!(&mut universe.gc_interface, result)
            }
        }
    }
}

fn remainder(a: i32, b: i32) -> Result<Value, Error> {
    let result = a % b;
    if result.signum() != a.signum() {
        Ok(Value::Integer((result + a) % a))
    } else {
        Ok(Value::Integer(result))
    }
}

fn sqrt(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, a => DoubleLike);
    match a {
        DoubleLike::Integer(a) => {
            let sqrt = (a as f64).sqrt();
            let trucated = sqrt.trunc();
            if sqrt == trucated {
                Ok(Value::Integer(trucated as i32))
            } else {
                Ok(Value::Double(sqrt))
            }
        }
        DoubleLike::BigInteger(a) => demote!(&mut universe.gc_interface, a.sqrt()),
        DoubleLike::Double(a) => Ok(Value::Double(a.sqrt())),
    }
}

fn max(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    match DoubleLike::gt(&a, &b) {
        true => Ok(a.into_value()),
        false => Ok(b.into_value()),
    }
}

fn min(a: DoubleLike, b: DoubleLike) -> Result<Value, Error> {
    match DoubleLike::gt(&a, &b) {
        true => Ok(b.into_value()),
        false => Ok(a.into_value()),
    }
}

fn abs(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, a => DoubleLike);
    match a {
        DoubleLike::Double(f) => match f < 0.0 {
            true => Ok((-f).into_value()),
            false => Ok(f.into_value()),
        },
        DoubleLike::Integer(i) => Ok(i.into_value()),
        DoubleLike::BigInteger(v) => {
            let bigint: Gc<BigInt> = universe.gc_interface.alloc(v.abs(), AllocSiteMarker::BigInt);
            Ok(bigint.into_value())
        }
    }
}

fn bitand(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, a => IntegerLike, b => IntegerLike);
    let heap = &mut universe.gc_interface;
    match (a, b) {
        (IntegerLike::Integer(a), IntegerLike::Integer(b)) => Ok(Value::Integer(a & b)),
        (IntegerLike::BigInteger(a), IntegerLike::BigInteger(b)) => {
            demote!(heap, &*a & &*b)
        }
        (IntegerLike::BigInteger(a), IntegerLike::Integer(b)) | (IntegerLike::Integer(b), IntegerLike::BigInteger(a)) => {
            demote!(heap, &*a & BigInt::from(b))
        }
    }
}

fn bitxor(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, a => IntegerLike, b => IntegerLike);
    let heap = &mut universe.gc_interface;
    match (a, b) {
        (IntegerLike::Integer(a), IntegerLike::Integer(b)) => Ok(Value::Integer(a ^ b)),
        (IntegerLike::BigInteger(a), IntegerLike::BigInteger(b)) => {
            demote!(heap, &*a ^ &*b)
        }
        (IntegerLike::BigInteger(a), IntegerLike::Integer(b)) | (IntegerLike::Integer(b), IntegerLike::BigInteger(a)) => {
            demote!(heap, &*a ^ BigInt::from(b))
        }
    }
}

fn lt(a: DoubleLike, b: DoubleLike) -> Result<bool, Error> {
    Ok(DoubleLike::lt(&a, &b))
}

fn lt_or_eq(a: DoubleLike, b: DoubleLike) -> Result<bool, Error> {
    Ok(DoubleLike::lt_or_eq(&a, &b))
}

fn gt(a: DoubleLike, b: DoubleLike) -> Result<bool, Error> {
    Ok(DoubleLike::gt(&a, &b))
}

fn gt_or_eq(a: DoubleLike, b: DoubleLike) -> Result<bool, Error> {
    Ok(DoubleLike::gt_or_eq(&a, &b))
}

fn eq(a: Value, b: Value) -> Result<bool, Error> {
    let Ok(a) = DoubleLike::try_from(a.0) else {
        return Ok(false);
    };

    let Ok(b) = DoubleLike::try_from(b.0) else {
        return Ok(false);
    };

    Ok(DoubleLike::eq(&a, &b))
}

fn uneq(a: Value, b: Value) -> Result<bool, Error> {
    let Ok(a) = DoubleLike::try_from(a.0) else {
        return Ok(false);
    };

    let Ok(b) = DoubleLike::try_from(b.0) else {
        return Ok(false);
    };

    Ok(!DoubleLike::eq(&a, &b))
}

fn eq_eq(a: Value, b: Value) -> Result<bool, Error> {
    let Ok(a) = DoubleLike::try_from(a.0) else {
        return Ok(false);
    };

    let Ok(b) = DoubleLike::try_from(b.0) else {
        return Ok(false);
    };

    match (a, b) {
        (DoubleLike::Integer(a), DoubleLike::Integer(b)) => Ok(a == b),
        (DoubleLike::BigInteger(a), DoubleLike::BigInteger(b)) => Ok(*a == *b),
        _ => Ok(false),
    }
}

fn shift_left(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    // old code pre integers being i32 because of nan boxing:

    // match a {
    //     Value::Integer(a) => match a.checked_shl(b as u32) {
    //         Some(value) => Return::Local(Value::Integer(value)),
    //         None => demote!(BigInt::from(a) << (b as usize)),
    //     },
    //     Value::BigInteger(a) => demote!(a << (b as usize)),
    //     _ => bail!(format!("'{}': wrong types", SIGNATURE)),
    // }

    get_args_from_stack!(stack, a => IntegerLike, b => i32);

    let heap = &mut universe.gc_interface;

    match a {
        IntegerLike::Integer(a) => match (a as u64).checked_shl(b as u32) {
            Some(value) => match value.try_into() {
                Ok(value) => Ok(Value::Integer(value)),
                Err(_) => Ok(Value::BigInteger(
                    universe.gc_interface.alloc(BigInt::from(value as i64), AllocSiteMarker::BigInt),
                )),
            },
            None => demote!(heap, BigInt::from(a) << (b as u32)),
        },
        IntegerLike::BigInteger(a) => demote!(heap, &*a << (b as u32)),
    }
}

fn shift_right(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    // match a {
    //     Value::Integer(a) => match a.checked_shr(b as u32) {
    //         Some(value) => Return::Local(Value::Integer(value)),
    //         None => demote!(BigInt::from(a) >> (b as usize)),
    //     },
    //     Value::BigInteger(a) => demote!(a >> (b as usize)),
    //     _ => bail!(format!("'{}': wrong types", SIGNATURE)),
    // }

    get_args_from_stack!(stack, a => IntegerLike, b => i32);
    let gc_interface = &mut universe.gc_interface;

    match a {
        IntegerLike::Integer(a) => match (a as u64).checked_shr(b as u32) {
            Some(value) => match value.try_into() {
                Ok(value) => Ok(Value::Integer(value)),
                Err(_) => Ok(Value::BigInteger(gc_interface.alloc(BigInt::from(value), AllocSiteMarker::BigInt))),
            },
            None => {
                let uint = BigUint::from_bytes_le(&a.to_bigint().unwrap().to_signed_bytes_le());
                let result = uint >> (b as u32);
                demote!(gc_interface, BigInt::from_signed_bytes_le(&result.to_bytes_le()))
            }
        },
        IntegerLike::BigInteger(a) => {
            let uint = BigUint::from_bytes_le(&a.to_signed_bytes_le());
            let result = uint >> (b as u32);
            demote!(gc_interface, BigInt::from_signed_bytes_le(&result.to_bytes_le()))
        }
    }
}

fn to(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, a => i32, b => i32);
    let vec: Vec<Value> = (a..=b).map(Value::Integer).collect();
    let alloc_vec: GcSlice<Value> = universe.gc_interface.alloc_slice(&vec, AllocSiteMarker::VecValue);
    Ok(Value::Array(VecValue(alloc_vec)))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
