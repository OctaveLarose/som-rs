use std::ops::Deref;

use anyhow::{Context, Error};
use num_bigint::BigInt;

use crate::{interner::Interned, value::BaseValue};

// Unfinished: using TryFrom to replace the convert.rs types FromArgs

impl TryFrom<BaseValue> for i32 {
    type Error = anyhow::Error;

    fn try_from(value: BaseValue) -> Result<Self, Self::Error> {
        value.as_integer().context("value was not an integer type")
    }
}

impl TryFrom<BaseValue> for f64 {
    type Error = anyhow::Error;

    fn try_from(value: BaseValue) -> Result<Self, Self::Error> {
        value.as_double().context("value was not a double type")
    }
}

#[derive(Debug, Clone)]
pub enum IntegerLike<BIGINTPTR> {
    Integer(i32),
    BigInteger(BIGINTPTR),
}

impl<BIGINTPTR> TryFrom<BaseValue> for IntegerLike<BIGINTPTR>
where
    BIGINTPTR: Deref<Target = BigInt> + From<u64> + Into<u64>,
    u64: From<BIGINTPTR>,
{
    type Error = Error;

    fn try_from(value: BaseValue) -> Result<Self, Self::Error> {
        value
            .as_integer()
            .map(Self::Integer)
            .or_else(|| value.as_big_integer::<BIGINTPTR>().map(Self::BigInteger))
            .context("could not resolve `Value` as `Integer`, or `BigInteger`")
    }
}

#[derive(Debug, Clone)]
pub enum DoubleLike<BIGINTPTR> {
    Double(f64),
    Integer(i32),
    BigInteger(BIGINTPTR),
}

impl<BIGINTPTR> TryFrom<BaseValue> for DoubleLike<BIGINTPTR>
where
    BIGINTPTR: Deref<Target = BigInt> + From<u64> + Into<u64>,
    u64: From<BIGINTPTR>,
{
    type Error = Error;

    fn try_from(value: BaseValue) -> Result<Self, Self::Error> {
        value
            .as_double()
            .map(Self::Double)
            .or_else(|| value.as_integer().map(Self::Integer))
            .or_else(|| value.as_big_integer().map(Self::BigInteger))
            .context("could not resolve `Value` as `Double`, `Integer`, or `BigInteger`")
    }
}

#[derive(Debug, Clone)]
pub enum StringLike<SPTR> {
    String(SPTR),
    Symbol(Interned),
}

impl<SPTR> TryFrom<BaseValue> for StringLike<SPTR>
where
    SPTR: Deref<Target = String> + From<u64> + Into<u64>,
{
    type Error = anyhow::Error;

    fn try_from(value: BaseValue) -> Result<Self, Self::Error> {
        value
            .as_string()
            .map(Self::String)
            .or_else(|| value.as_symbol().map(Self::Symbol))
            .context("could not resolve `Value` as `String`, or `Symbol`")
    }
}
