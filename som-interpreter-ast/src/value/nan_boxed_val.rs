use crate::gc::VecValue;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use num_bigint::BigInt;
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;
use som_value::delegate_to_base_value;
use som_value::interned::Interned;
use som_value::value::*;
use som_value::value_ptr::{HasPointerTag, TypedPtrValue};
use std::convert::Into;
use std::fmt;
use std::ops::Deref;

/// Tag bits for the `Array` type.
pub(crate) const ARRAY_TAG: u64 = 0b010 | CELL_BASE_TAG;
/// Tag bits for the `Block` type.
pub(crate) const BLOCK_TAG: u64 = 0b100 | CELL_BASE_TAG;
/// Tag bits for the `Class` type.
pub(crate) const CLASS_TAG: u64 = 0b101 | CELL_BASE_TAG;
/// Tag bits for the `Instance` type.
pub(crate) const INSTANCE_TAG: u64 = 0b110 | CELL_BASE_TAG;
/// Tag bits for the `Invokable` type.
pub(crate) const INVOKABLE_TAG: u64 = 0b111 | CELL_BASE_TAG;

impl Deref for Value {
    type Target = BaseValue;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<BaseValue> for Value {
    fn from(value: BaseValue) -> Self {
        Value(value)
    }
}

impl Value {
    #[inline(always)]
    pub fn is_value_gc_ptr<T: HasPointerTag>(&self) -> bool {
        self.0.is_ptr::<T, Gc<T>>()
    }

    #[inline(always)]
    pub fn as_value_gc_ptr<T: HasPointerTag>(&self) -> Option<Gc<T>> {
        self.0.as_ptr::<T, Gc<T>>()
    }

    /// Returns this value as an array, if such is its type.
    #[inline(always)]
    pub fn as_array(self) -> Option<VecValue> {
        match self.tag() == ARRAY_TAG {
            true => Some(VecValue(GcSlice::from(self.extract_pointer_bits()))),
            false => None,
        }
    }
    /// Returns this value as a block, if such is its type.
    #[inline(always)]
    pub fn as_block(self) -> Option<Gc<Block>> {
        self.as_value_gc_ptr::<Block>()
    }
    /// Returns this value as a class, if such is its type.
    #[inline(always)]
    pub fn as_class(self) -> Option<Gc<Class>> {
        self.as_value_gc_ptr::<Class>()
    }
    /// Returns this value as an instance, if such is its type.
    #[inline(always)]
    pub fn as_instance(self) -> Option<Gc<Instance>> {
        self.as_value_gc_ptr::<Instance>()
    }
    /// Returns this value as an invokable, if such is its type.
    #[inline(always)]
    pub fn as_invokable(self) -> Option<Gc<Method>> {
        self.as_value_gc_ptr::<Method>()
    }

    // /// Used for debugging.
    // pub unsafe fn as_some_pointer<T>(self) -> Option<Gc<T>> {
    //     self.is_ptr_type().then(|| self.extract_gc_cell())
    // }

    #[allow(non_snake_case)]
    pub fn Array(value: VecValue) -> Self {
        // TODO use TypedPtrValue somehow instead
        Value(BaseValue::new(ARRAY_TAG, value.0.into()))
    }
    #[allow(non_snake_case)]
    #[inline(always)]
    pub fn Block(value: Gc<Block>) -> Self {
        TypedPtrValue::new(value).into()
    }

    #[allow(non_snake_case)]
    pub fn Class(value: Gc<Class>) -> Self {
        TypedPtrValue::new(value).into()
    }

    #[allow(non_snake_case)]
    pub fn Instance(value: Gc<Instance>) -> Self {
        TypedPtrValue::new(value).into()
    }

    #[allow(non_snake_case)]
    pub fn Invokable(value: Gc<Method>) -> Self {
        TypedPtrValue::new(value).into()
    }
}

#[allow(non_snake_case)]
impl Value {
    pub const TRUE: Self = Value(BaseValue::TRUE);
    pub const FALSE: Self = Value(BaseValue::FALSE);
    pub const NIL: Self = Value(BaseValue::NIL);
    pub const INTEGER_ZERO: Self = Value(BaseValue::INTEGER_ZERO);
    pub const INTEGER_ONE: Self = Value(BaseValue::INTEGER_ONE);

    delegate_to_base_value!(
        new_boolean(value: bool) -> Self,
        new_integer(value: i32) -> Self,
        new_double(value: f64) -> Self,
        new_symbol(value: Interned) -> Self,
        new_big_integer(value: Gc<BigInt>) -> Self,
        new_string(value: Gc<String>) -> Self,
        new_char(value: char) -> Self,
        Boolean(value: bool) -> Self,
        Integer(value: i32) -> Self,
        Double(value: f64) -> Self,
        Symbol(value: Interned) -> Self,
        BigInteger(value: Gc<BigInt>) -> Self,
        String(value: Gc<String>) -> Self,
    );

    /// Get the class of the current value.
    #[inline(always)]
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        match self.tag() {
            NIL_TAG => universe.core.nil_class(),
            BOOLEAN_TAG => {
                if self.as_boolean().unwrap() {
                    universe.core.true_class()
                } else {
                    universe.core.false_class()
                }
            }
            INTEGER_TAG | BIG_INTEGER_TAG => universe.core.integer_class(),
            SYMBOL_TAG => universe.core.symbol_class(),
            STRING_TAG => universe.core.string_class(),
            CHAR_TAG => universe.core.string_class(),
            ARRAY_TAG => universe.core.array_class(),
            BLOCK_TAG => self.as_block().unwrap().class(universe),
            INSTANCE_TAG => self.as_instance().unwrap().class(),
            CLASS_TAG => self.as_class().unwrap().class(),
            INVOKABLE_TAG => self.as_value_gc_ptr::<Method>().unwrap().class(universe),
            _ => {
                if self.is_double() {
                    universe.core.double_class()
                } else {
                    panic!("unknown tag");
                }
            }
        }
    }

    /// Search for a given method for this value.
    pub fn lookup_method(&self, universe: &Universe, signature: Interned) -> Option<Gc<Method>> {
        self.class(universe).lookup_method(signature)
    }

    /// Get the string representation of this value.
    pub fn to_string(&self, universe: &Universe) -> String {
        match self.tag() {
            NIL_TAG => "nil".to_string(),
            BOOLEAN_TAG => self.as_boolean().unwrap().to_string(),
            INTEGER_TAG => self.as_integer().unwrap().to_string(),
            BIG_INTEGER_TAG => self.as_big_integer::<Gc<BigInt>>().unwrap().to_string(),
            _ if self.is_double() => self.as_double().unwrap().to_string(),
            SYMBOL_TAG => {
                let symbol = universe.lookup_symbol(self.as_symbol().unwrap());
                if symbol.chars().any(|ch| ch.is_whitespace() || ch == '\'') {
                    format!("#'{}'", symbol.replace("'", "\\'"))
                } else {
                    format!("#{}", symbol)
                }
            }
            STRING_TAG => self.as_string::<Gc<String>>().unwrap().to_string(),
            ARRAY_TAG => {
                // TODO: I think we can do better here (less allocations).
                let strings: Vec<String> = self.as_array().unwrap().iter().map(|value| value.to_string(universe)).collect();
                format!("#({})", strings.join(" "))
            }
            BLOCK_TAG => {
                let block = self.as_block().unwrap();
                format!("instance of Block{}", block.nb_parameters() + 1)
            }
            INSTANCE_TAG => {
                let instance = self.as_instance().unwrap();
                format!("instance of {} class", instance.class().name(),)
            }
            CLASS_TAG => self.as_class().unwrap().name().to_string(),
            INVOKABLE_TAG => {
                let invokable = self.as_value_gc_ptr::<Method>().unwrap();
                format!("{}>>#{}", invokable.holder.name(), invokable.signature(),)
            }
            _ => {
                panic!("unknown tag")
            }
        }
    }
}

impl From<Value> for ValueEnum {
    fn from(value: Value) -> Self {
        if let Some(value) = value.as_double() {
            Self::Double(value)
        } else if value.is_nil() {
            Self::Nil
        } else if let Some(value) = value.as_integer() {
            Self::Integer(value)
        } else if let Some(value) = value.as_big_integer() {
            Self::BigInteger(value)
        } else if let Some(value) = value.as_boolean() {
            Self::Boolean(value)
        } else if let Some(value) = value.as_symbol() {
            Self::Symbol(value)
        } else if let Some(value) = value.as_string() {
            Self::String(value)
        } else if let Some(_value) = value.as_array() {
            // Self::Array(value)
            eprintln!("no From<NanBoxedVal> impl for arr. returning Nil.");
            Self::Nil
        } else if let Some(value) = value.as_block() {
            Self::Block(value)
        } else if let Some(value) = value.as_instance() {
            Self::Instance(value)
        } else if let Some(value) = value.as_class() {
            Self::Class(value)
        } else if let Some(value) = value.as_value_gc_ptr::<Method>() {
            Self::Invokable(value)
        } else if let Some(value) = value.as_char() {
            Self::Char(value)
        } else {
            todo!()
        }
    }
}

impl From<ValueEnum> for Value {
    fn from(value: ValueEnum) -> Self {
        match value {
            ValueEnum::Nil => Self::NIL,
            ValueEnum::Boolean(value) => Self::new_boolean(value),
            ValueEnum::Integer(value) => Self::new_integer(value),
            ValueEnum::BigInteger(value) => Self::new_big_integer(value),
            ValueEnum::Double(value) => Self::new_double(value),
            ValueEnum::Symbol(value) => Self::new_symbol(value),
            ValueEnum::String(value) => Self::new_string(value),
            ValueEnum::Char(value) => Self::new_char(value),
            // ValueEnum::Array(value) => Self::new_array(value),
            ValueEnum::Array(_value) => unimplemented!("no impl for arr, same as BC"),
            ValueEnum::Block(value) => TypedPtrValue::new(value).into(),
            ValueEnum::Instance(value) => TypedPtrValue::new(value).into(),
            ValueEnum::Class(value) => TypedPtrValue::new(value).into(),
            ValueEnum::Invokable(value) => TypedPtrValue::new(value).into(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.as_u64() == other.as_u64() {
            // this encapsulates every comparison between values of the same primitive type, e.g. comparing two i32s or two booleans, and pointer comparisons
            true
        } else if let (Some(a), Some(b)) = (self.as_double(), other.as_double()) {
            a == b
        } else if let (Some(a), Some(b)) = (self.as_integer(), other.as_double()) {
            (a as f64) == b
        } else if let (Some(a), Some(b)) = (self.as_double(), other.as_integer()) {
            (b as f64) == a
        } else if let (Some(a), Some(b)) = (self.as_big_integer::<Gc<BigInt>>(), other.as_big_integer()) {
            a == b
        } else if let (Some(a), Some(b)) = (self.as_big_integer::<Gc<BigInt>>(), other.as_integer()) {
            (*a).eq(&BigInt::from(b))
        } else if let (Some(a), Some(b)) = (self.as_integer(), other.as_big_integer::<Gc<BigInt>>()) {
            BigInt::from(a).eq(&*b)
        } else if let (Some(a), Some(b)) = (self.as_string::<Gc<String>>(), other.as_string::<Gc<String>>()) {
            a == b
        } else {
            false
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ValueEnum::from(*self).fmt(f)
    }
}

/// Represents an SOM value.
#[derive(Clone)]
pub enum ValueEnum {
    /// The **nil** value.
    Nil,
    /// A boolean value (**true** or **false**).
    Boolean(bool),
    /// An integer value.
    Integer(i32),
    /// A big integer value (arbitrarily big).
    BigInteger(Gc<BigInt>),
    /// An floating-point value.
    Double(f64),
    /// An interned symbol value.
    Symbol(Interned),
    /// A string value.
    String(Gc<String>),
    /// An array of values.
    Array(Gc<Vec<Value>>),
    /// A block value, ready to be evaluated.
    Block(Gc<Block>),
    /// A generic (non-primitive) class instance.
    Instance(Gc<Instance>),
    /// A bare class object.
    Class(Gc<Class>),
    /// A bare invokable.
    Invokable(Gc<Method>),
    /// A single character
    Char(char),
}

impl ValueEnum {
    /// Get the class of the current value.
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        match self {
            Self::Nil => universe.core.nil_class(),
            Self::Boolean(true) => universe.core.true_class(),
            Self::Boolean(false) => universe.core.false_class(),
            Self::Integer(_) => universe.core.integer_class(),
            Self::BigInteger(_) => universe.core.integer_class(),
            Self::Double(_) => universe.core.double_class(),
            Self::Symbol(_) => universe.core.symbol_class(),
            Self::String(_) => universe.core.string_class(),
            Self::Char(_) => universe.core.string_class(),
            Self::Array(_) => universe.core.array_class(),
            Self::Block(block) => block.class(universe),
            Self::Instance(instance_ptr) => instance_ptr.class(),
            Self::Class(class) => class.class(),
            Self::Invokable(invokable) => invokable.class(universe),
        }
    }

    /// Search for a given method for this value.
    #[inline(always)]
    pub fn lookup_method(&self, universe: &Universe, signature: Interned) -> Option<Gc<Method>> {
        self.class(universe).lookup_method(signature)
    }

    /// Search for a local binding within this value.
    #[inline(always)]
    pub fn lookup_local(&self, idx: u8) -> Self {
        match self {
            Self::Instance(instance_ptr) => (*Instance::lookup_field(instance_ptr, idx)).into(),
            Self::Class(class) => class.lookup_field(idx).into(),
            v => unreachable!("Attempting to look up a local in {:?}", v),
        }
    }

    /// Assign a value to a local binding within this value.
    pub fn assign_local(&mut self, idx: u8, value: Self) {
        match self {
            Self::Instance(instance_ptr) => Instance::assign_field(instance_ptr, idx, value.into()),
            Self::Class(class) => class.assign_field(idx, value.into()),
            v => unreachable!("Attempting to assign a local in {:?}", v),
        }
    }

    /// Get the string representation of this value.
    pub fn to_string(&self, universe: &Universe) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::Boolean(value) => value.to_string(),
            Self::Integer(value) => value.to_string(),
            Self::BigInteger(value) => value.to_string(),
            Self::Double(value) => value.to_string(),
            Self::Symbol(value) => {
                let symbol = universe.lookup_symbol(*value);
                if symbol.chars().any(|ch| ch.is_whitespace() || ch == '\'') {
                    format!("#'{}'", symbol.replace("'", "\\'"))
                } else {
                    format!("#{}", symbol)
                }
            }
            Self::String(value) => value.as_str().to_string(),
            Self::Char(value) => String::from(*value),
            Self::Array(values) => {
                // TODO (from nicolas): I think we can do better here (less allocations).
                let strings: Vec<String> = values.iter().map(|value| value.to_string(universe)).collect();
                format!("#({})", strings.join(" "))
            }
            Self::Block(block) => format!("instance of Block{}", block.nb_parameters() + 1),
            Self::Instance(instance_ptr) => {
                format!("instance of {} class", instance_ptr.class().name(),)
            }
            Self::Class(class) => class.name().to_string(),
            Self::Invokable(invokable) => {
                format!("{}>>#{}", invokable.holder().name(), invokable.signature())
            }
        }
    }
}

impl PartialEq for ValueEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a.eq(b),
            (Self::Integer(a), Self::Integer(b)) => a.eq(b),
            (Self::Integer(a), Self::Double(b)) | (Self::Double(b), Self::Integer(a)) => (*a as f64).eq(b),
            (Self::Double(a), Self::Double(b)) => a.eq(b),
            (Self::BigInteger(a), Self::BigInteger(b)) => a.eq(b),
            (Self::BigInteger(a), Self::Integer(b)) | (Self::Integer(b), Self::BigInteger(a)) => (**a).eq(&BigInt::from(*b)),
            (Self::Symbol(a), Self::Symbol(b)) => a.eq(b),
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,
            (Self::Instance(a), Self::Instance(b)) => a == b,
            (Self::Class(a), Self::Class(b)) => a == b,
            (Self::Block(a), Self::Block(b)) => a == b,
            (Self::Invokable(a), Self::Invokable(b)) => a == b,
            _ => false,
        }
    }
}

impl fmt::Debug for ValueEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => f.debug_tuple("Nil").finish(),
            Self::Boolean(val) => f.debug_tuple("Boolean").field(val).finish(),
            Self::Integer(val) => f.debug_tuple("Integer").field(val).finish(),
            Self::BigInteger(val) => f.debug_tuple("BigInteger").field(val).finish(),
            Self::Double(val) => f.debug_tuple("Double").field(val).finish(),
            Self::Symbol(val) => f.debug_tuple("Symbol").field(val).finish(),
            Self::String(val) => f.debug_tuple("String").field(val).finish(),
            Self::Char(val) => f.debug_tuple("Char").field(val).finish(),
            Self::Array(val) => f.debug_tuple("Array").field(&val).finish(),
            Self::Block(val) => f.debug_tuple("Block").field(val).finish(),
            Self::Instance(val) => f.debug_tuple("Instance").field(&val).finish(),
            Self::Class(val) => f.debug_tuple("Class").field(&val).finish(),
            Self::Invokable(val) => {
                let signature = format!("{}>>#{}", val.holder.name(), val.signature());
                f.debug_tuple("Invokable").field(&signature).finish()
            }
        }
    }
}
