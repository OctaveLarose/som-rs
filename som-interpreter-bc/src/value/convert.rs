// This is all Nicolas Polomack (https://github.com/Hirevo)'s work, despite what the commit history says.
// Nicolas is the original dev for som-rs, and had this code in an unmerged PR about Nan boxing.
// I didn't merge with his commits directly because his version of som-rs and mine have diverged a lot. But the credit is his, my edits are minor so far

use std::convert::TryFrom;

use anyhow::{bail, Context, Error};

use crate::gc::VecValue;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::MethodOrPrim;
use num_bigint::BigInt;
use som_core::interner::Interned;
use som_gc::gc_interface::GCInterface;
use som_gc::gcref::Gc;

pub trait IntoValue {
    #[allow(clippy::wrong_self_convention)] // though i guess we could/should rename it
    fn into_value(&self, gc_interface: &mut GCInterface) -> Value;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Nil;

impl TryFrom<Value> for Nil {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.is_nil() {
            Ok(Self)
        } else {
            bail!("could not resolve `Value` as `Nil`");
        }
    }
}

impl FromArgs for Nil {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let value = interpreter.current_frame.stack_pop();

        Self::try_from(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct System;

impl TryFrom<Value> for System {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.is_nil() {
            // not is_system?
            Ok(Self)
        } else {
            bail!("could not resolve `Value` as `System`");
        }
    }
}

impl FromArgs for System {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let value = interpreter.current_frame.stack_pop();

        Self::try_from(value)
    }
}

#[derive(Debug, Clone)]
pub enum StringLike {
    String(Gc<String>),
    Symbol(Interned),
}

impl TryFrom<Value> for StringLike {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value
            .as_string()
            .map(Self::String)
            .or_else(|| value.as_symbol().map(Self::Symbol))
            .context("could not resolve `Value` as `String`, or `Symbol`")
    }
}

impl FromArgs for StringLike {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let value = interpreter.current_frame.stack_pop();

        Self::try_from(value)
    }
}

#[derive(Debug, Clone)]
pub enum DoubleLike {
    Double(f64),
    Integer(i32),
    BigInteger(Gc<BigInt>),
}

impl TryFrom<Value> for DoubleLike {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value
            .as_double()
            .map(Self::Double)
            .or_else(|| value.as_integer().map(Self::Integer))
            .or_else(|| value.as_big_integer().map(Self::BigInteger))
            .context("could not resolve `Value` as `Double`, `Integer`, or `BigInteger`")
    }
}

impl FromArgs for DoubleLike {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let value = interpreter.current_frame.stack_pop();

        Self::try_from(value)
    }
}

#[derive(Debug, Clone)]
pub enum IntegerLike {
    Integer(i32),
    BigInteger(Gc<BigInt>),
}

impl TryFrom<Value> for IntegerLike {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value
            .as_integer()
            .map(Self::Integer)
            .or_else(|| value.as_big_integer().map(Self::BigInteger))
            .context("could not resolve `Value` as `Integer`, or `BigInteger`")
    }
}

impl FromArgs for IntegerLike {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let value = interpreter.current_frame.stack_pop();

        Self::try_from(value)
    }
}

pub trait FromArgs: Sized {
    fn from_args(interpreter: &mut Interpreter, universe: &mut Universe) -> Result<Self, Error>;
}

impl FromArgs for Value {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        Ok(interpreter.current_frame.stack_pop())
    }
}

impl FromArgs for bool {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_boolean().context("could not resolve `Value` as `Boolean`")
    }
}

impl FromArgs for i32 {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_integer().context("could not resolve `Value` as `Integer`")
    }
}

impl FromArgs for f64 {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_double().context("could not resolve `Value` as `Double`")
    }
}

impl FromArgs for Interned {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_symbol().context("could not resolve `Value` as `Symbol`")
    }
}

impl FromArgs for Gc<String> {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_string().context("could not resolve `Value` as `String`")
    }
}

impl FromArgs for Gc<VecValue> {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_array().context("could not resolve `Value` as `Array`")
    }
}

impl FromArgs for Gc<Class> {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_class().context("could not resolve `Value` as `Class`")
    }
}

impl FromArgs for Gc<Instance> {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_instance().context("could not resolve `Value` as `Instance`")
    }
}

impl FromArgs for Gc<Block> {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_block().context("could not resolve `Value` as `Block`")
    }
}

impl FromArgs for Gc<MethodOrPrim> {
    fn from_args(interpreter: &mut Interpreter, _: &mut Universe) -> Result<Self, Error> {
        let arg = interpreter.current_frame.stack_pop();
        arg.as_invokable().context("could not resolve `Value` as `Method`")
    }
}

impl IntoValue for bool {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Boolean(*self)
    }
}

impl IntoValue for i32 {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Integer(*self)
    }
}

impl IntoValue for f64 {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Double(*self)
    }
}

impl IntoValue for Interned {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Symbol(*self)
    }
}

impl IntoValue for Gc<String> {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::String(*self)
    }
}

impl IntoValue for Gc<BigInt> {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::BigInteger(*self)
    }
}

impl IntoValue for Gc<VecValue> {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Array(*self)
    }
}

impl IntoValue for Gc<Class> {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Class(*self)
    }
}

impl IntoValue for Gc<Instance> {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Instance(*self)
    }
}

impl IntoValue for Gc<Block> {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Block(*self)
    }
}

impl IntoValue for Gc<MethodOrPrim> {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::Invokable(*self)
    }
}

pub trait Primitive<T>: Sized + Send + Sync + 'static {
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe) -> Result<(), Error>;

    fn into_func(self) -> &'static PrimitiveFn {
        let boxed = Box::new(move |interpreter: &mut Interpreter, universe: &mut Universe| self.invoke(interpreter, universe));
        Box::leak(boxed)
    }
}

pub trait IntoReturn {
    fn into_return(self, interpreter: &mut Interpreter, heap: &mut GCInterface) -> Result<(), Error>;
}

macro_rules! reverse {
    ($interpreter:expr, $universe:expr, [], [ $($ty:ident),* $(,)? ]) => {
        $(
            #[allow(non_snake_case)]
            let $ty = $ty::from_args($interpreter, $universe)?;
        )*
    };
    ($interpreter:expr, $universe:expr, [ $ty:ident $(,)? ], [ $($ty2:ident),* $(,)? ]) => {
        reverse!($interpreter, $universe, [], [ $ty , $($ty2),* ])
    };
    ($interpreter:expr, $universe:expr, [ $ty:ident , $($ty1:ident),* $(,)? ], [ $($ty2:ident),* $(,)? ]) => {
        reverse!($interpreter, $universe, [ $($ty1),* ], [ $ty , $($ty2),* ])
    };
}

macro_rules! derive_stuff {
    ($($ty:ident),* $(,)?) => {
        impl <$($ty: $crate::value::convert::IntoValue),*> $crate::value::convert::IntoValue for ($($ty),*,) {
            fn into_value(&self, gc_interface: &mut GCInterface) -> $crate::value::Value {
                #[allow(non_snake_case)]
                let ($($ty),*,) = self;
                let values = vec![
                $(
                    $crate::value::convert::IntoValue::into_value($ty, gc_interface),
                )*];
                let allocated = gc_interface.alloc(VecValue(values));
                $crate::value::Value::Array(allocated)
            }
        }

        impl <$($ty: $crate::value::convert::FromArgs),*> $crate::value::convert::FromArgs for ($($ty),*,) {
            fn from_args(interpreter: &mut $crate::interpreter::Interpreter, universe: &mut $crate::universe::Universe) -> Result<Self, Error> {
                $(
                    #[allow(non_snake_case)]
                    let $ty = $ty::from_args(interpreter, universe)?;
                )*
                Ok(($($ty),*,))
            }
        }

        impl <F, R, $($ty),*> $crate::value::convert::Primitive<($($ty),*,)> for F
        where
            F: Fn(&mut $crate::interpreter::Interpreter, &mut $crate::universe::Universe, $($ty),*) -> Result<R, Error> + Send + Sync + 'static,
            R: $crate::value::convert::IntoReturn,
            $($ty: $crate::value::convert::FromArgs),*,
        {
            fn invoke(&self, interpreter: &mut $crate::interpreter::Interpreter, universe: &mut $crate::universe::Universe) -> Result<(), Error> {
                reverse!(interpreter, universe, [$($ty),*], []);
                let result = (self)(interpreter, universe, $($ty),*,)?;
                result.into_return(interpreter, &mut universe.gc_interface)
            }
        }
    };
}

impl<T: IntoValue> IntoReturn for T {
    fn into_return(self, interpreter: &mut Interpreter, heap: &mut GCInterface) -> Result<(), Error> {
        interpreter.current_frame.stack_push(self.into_value(heap));
        Ok(())
    }
}

impl IntoValue for Value {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        *self
    }
}

impl IntoValue for Nil {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::NIL
    }
}

impl IntoValue for System {
    fn into_value(&self, _: &mut GCInterface) -> Value {
        Value::SYSTEM
    }
}

impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(&self, heap: &mut GCInterface) -> Value {
        self.as_ref().map_or(Value::NIL, |it| it.into_value(heap))
    }
}

impl IntoReturn for () {
    fn into_return(self, _: &mut Interpreter, _: &mut GCInterface) -> Result<(), Error> {
        Ok(())
    }
}

impl IntoValue for StringLike {
    fn into_value(&self, heap: &mut GCInterface) -> Value {
        match self {
            StringLike::String(value) => value.into_value(heap),
            StringLike::Symbol(value) => value.into_value(heap),
        }
    }
}

impl IntoValue for IntegerLike {
    fn into_value(&self, heap: &mut GCInterface) -> Value {
        match self {
            IntegerLike::Integer(value) => value.into_value(heap),
            IntegerLike::BigInteger(value) => value.into_value(heap),
        }
    }
}

impl IntoValue for DoubleLike {
    fn into_value(&self, heap: &mut GCInterface) -> Value {
        match self {
            DoubleLike::Double(value) => value.into_value(heap),
            DoubleLike::Integer(value) => value.into_value(heap),
            DoubleLike::BigInteger(value) => value.into_value(heap),
        }
    }
}

// impl<F> Primitive<()> for F
// where
//     F: Fn(&mut Interpreter, &mut Universe) -> Result<(), Error>
//     + Send
//     + Sync
//     + 'static,
// {
//     fn invoke(
//         &self,
//         interpreter: &mut Interpreter,
//         universe: &mut Universe,
//     ) -> Result<(), Error> {
//         (self)(interpreter, universe)
//     }
// }

derive_stuff!(_A);
derive_stuff!(_A, _B);
derive_stuff!(_A, _B, _C);
derive_stuff!(_A, _B, _C, _D);
derive_stuff!(_A, _B, _C, _D, _E);
derive_stuff!(_A, _B, _C, _D, _E, _F);