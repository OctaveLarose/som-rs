use anyhow::{bail, Context, Error};
use som_gc::gcslice::GcSlice;
use som_value::value_ptr::HasPointerTag;
use std::convert::TryFrom;

use crate::gc::VecValue;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use num_bigint::BigInt;
use som_gc::gcref::Gc;
use som_value::interned::Interned;

pub type DoubleLike = som_value::convert::DoubleLike<Gc<BigInt>>;
pub type IntegerLike = som_value::convert::IntegerLike<Gc<BigInt>>;
pub type StringLike = som_value::convert::StringLike<Gc<String>>;

pub trait IntoValue {
    #[allow(clippy::wrong_self_convention)] // though i guess we could/should rename it
    fn into_value(&self) -> Value;
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
    fn from_args(arg: Value) -> Result<Self, Error> {
        Self::try_from(arg)
    }
}

impl FromArgs for StringLike {
    fn from_args(arg: Value) -> Result<Self, Error> {
        Self::try_from(arg.0)
    }
}

impl FromArgs for DoubleLike {
    fn from_args(arg: Value) -> Result<Self, Error> {
        Self::try_from(arg.0)
    }
}
impl FromArgs for IntegerLike {
    fn from_args(arg: Value) -> Result<Self, Error> {
        Self::try_from(arg.0)
    }
}

pub trait FromArgs: Sized {
    fn from_args(arg: Value) -> Result<Self, Error>;
}

impl FromArgs for Value {
    fn from_args(arg: Value) -> Result<Self, Error> {
        Ok(arg)
    }
}

impl FromArgs for bool {
    fn from_args(arg: Value) -> Result<Self, Error> {
        arg.as_boolean().context("could not resolve `Value` as `Boolean`")
    }
}

impl FromArgs for i32 {
    fn from_args(arg: Value) -> Result<Self, Error> {
        arg.as_integer().context("could not resolve `Value` as `Integer`")
    }
}

impl FromArgs for f64 {
    fn from_args(arg: Value) -> Result<Self, Error> {
        arg.as_double().context("could not resolve `Value` as `Double`")
    }
}

impl FromArgs for Interned {
    fn from_args(arg: Value) -> Result<Self, Error> {
        arg.as_symbol().context("could not resolve `Value` as `Symbol`")
    }
}

impl FromArgs for VecValue {
    fn from_args(arg: Value) -> Result<Self, Error> {
        Ok(VecValue(GcSlice::from(arg.extract_pointer_bits())))
    }
}

impl<T: HasPointerTag> FromArgs for Gc<T> {
    fn from_args(arg: Value) -> Result<Self, Error> {
        Ok(arg.as_value_ptr().unwrap())
    }
}

impl IntoValue for bool {
    fn into_value(&self) -> Value {
        Value::Boolean(*self)
    }
}

impl IntoValue for i32 {
    fn into_value(&self) -> Value {
        Value::Integer(*self)
    }
}

impl IntoValue for f64 {
    fn into_value(&self) -> Value {
        Value::Double(*self)
    }
}

impl IntoValue for char {
    fn into_value(&self) -> Value {
        Value::Char(*self)
    }
}

impl IntoValue for Interned {
    fn into_value(&self) -> Value {
        Value::Symbol(*self)
    }
}

impl IntoValue for Gc<String> {
    fn into_value(&self) -> Value {
        Value::String(self.clone())
    }
}

impl IntoValue for Gc<BigInt> {
    fn into_value(&self) -> Value {
        Value::BigInteger(self.clone())
    }
}

impl IntoValue for VecValue {
    fn into_value(&self) -> Value {
        Value::Array(self.clone())
    }
}

impl IntoValue for Gc<Class> {
    fn into_value(&self) -> Value {
        Value::Class(self.clone())
    }
}

impl IntoValue for Gc<Instance> {
    fn into_value(&self) -> Value {
        Value::Instance(self.clone())
    }
}

impl IntoValue for Gc<Block> {
    fn into_value(&self) -> Value {
        Value::Block(self.clone())
    }
}

impl IntoValue for Gc<Method> {
    fn into_value(&self) -> Value {
        Value::Invokable(self.clone())
    }
}

pub trait Primitive<T>: Sized + Send + Sync + 'static {
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe, nbr_args: usize) -> Result<(), Error>;

    fn into_func(self) -> &'static PrimitiveFn {
        let boxed =
            Box::new(move |interpreter: &mut Interpreter, universe: &mut Universe, nbr_args: usize| self.invoke(interpreter, universe, nbr_args));
        Box::leak(boxed)
    }
}

pub trait IntoReturn {
    fn into_return(self, interpreter: &mut Interpreter, nbr_args: usize) -> Result<(), Error>;
}

impl<T: IntoValue> IntoReturn for T {
    fn into_return(self, interpreter: &mut Interpreter, nbr_args: usize) -> Result<(), Error> {
        interpreter.stack.truncate(interpreter.stack.len() - nbr_args);
        interpreter.stack.push(self.into_value());
        Ok(())
    }
}

impl IntoValue for Value {
    fn into_value(&self) -> Value {
        *self
    }
}

impl IntoValue for Nil {
    fn into_value(&self) -> Value {
        Value::NIL
    }
}

impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(&self) -> Value {
        self.as_ref().map_or(Value::NIL, |it| it.into_value())
    }
}

impl IntoReturn for () {
    fn into_return(self, _: &mut Interpreter, _: usize) -> Result<(), Error> {
        Ok(())
    }
}

impl IntoValue for StringLike {
    fn into_value(&self) -> Value {
        match self {
            StringLike::String(value) => value.into_value(),
            StringLike::Char(value) => value.into_value(),
            StringLike::Symbol(value) => value.into_value(),
        }
    }
}

impl IntoValue for IntegerLike {
    fn into_value(&self) -> Value {
        match self {
            IntegerLike::Integer(value) => value.into_value(),
            IntegerLike::BigInteger(value) => value.into_value(),
        }
    }
}

impl IntoValue for DoubleLike {
    fn into_value(&self) -> Value {
        match self {
            DoubleLike::Double(value) => value.into_value(),
            DoubleLike::Integer(value) => value.into_value(),
            DoubleLike::BigInteger(value) => value.into_value(),
        }
    }
}

macro_rules! derive_prims {
    ($($ty:ident),* $(,)?) => {

        impl <F, R, $($ty),*> $crate::value::convert::Primitive<($($ty),*,)> for F
        where
            F: Fn($($ty),*) -> Result<R, Error> + Send + Sync + 'static,
            R: $crate::value::convert::IntoValue,
            $($ty: $crate::value::convert::FromArgs),*,
        {
            fn invoke(&self, interpreter: &mut $crate::interpreter::Interpreter, _: &mut $crate::universe::Universe, nbr_args: usize) -> Result<(), Error> {
                let result = {
                    let args: &[Value] = interpreter.stack_n_last_elements(nbr_args);
                    let mut args_iter = args.iter();
                    $(
                        #[allow(non_snake_case)]
                        let $ty = $ty::from_args(*args_iter.next().unwrap()).unwrap();
                    )*

                   (self)($($ty),*,)?.into_value()
                };

                interpreter.stack.truncate(interpreter.stack.len() - nbr_args);
                interpreter.stack.push(result);
                Ok(())
            }
        }
    };
}

derive_prims!(_A);
derive_prims!(_A, _B);
derive_prims!(_A, _B, _C);
derive_prims!(_A, _B, _C, _D);

/// Primitives that need access to the universe may trigger GC, which can move variables.
/// Therefore, they take arguments from the stack (previous frame) themselves, and are responsible
/// for ensuring possible GC triggers can't invalidate their arguments, or the primitive's behavior.
impl<F, R> Primitive<R> for F
where
    F: Fn(&mut Interpreter, &mut Universe) -> Result<R, Error> + Send + Sync + 'static,
    R: crate::value::convert::IntoValue,
{
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe, _: usize) -> Result<(), Error> {
        let result = self(interpreter, universe)?.into_value();
        interpreter.stack.push(result);
        Ok(())
    }
}

/// For primitives who have no return values... or want complete control over their arguments and return value.
impl<F> Primitive<()> for F
where
    F: Fn(&mut Interpreter, &mut Universe) -> Result<(), Error> + Send + Sync + 'static,
{
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe, _: usize) -> Result<(), Error> {
        self(interpreter, universe)
    }
}
