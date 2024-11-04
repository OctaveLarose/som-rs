use std::convert::TryInto;
use std::fmt;
use std::fmt::{Debug, Formatter};
use crate::block::Block;
use crate::class::Class;
use crate::gc::VecValue;
use crate::instance::{Instance, InstanceAccess};
use crate::method::Method;
use crate::universe::Universe;
use som_core::interner::Interned;
use som_core::value::{CELL_BASE_TAG, NIL_TAG, SYSTEM_TAG, STRING_TAG, INTEGER_TAG, BOOLEAN_TAG, BIG_INTEGER_TAG, SYMBOL_TAG};
use som_core::value::{CANON_NAN_BITS, IS_PTR_PATTERN, TAG_SHIFT, TAG_EXTRACTION};
use num_bigint::BigInt;
use som_core::nan_boxed_val_base_impl;
use som_gc::gcref::GCRef;

pub type Value = BCNaNBoxedVal;

/// Tag bits for the `Array` type.
const ARRAY_TAG: u64 = 0b010 | CELL_BASE_TAG;
/// Tag bits for the `Block` type.
const BLOCK_TAG: u64 = 0b100 | CELL_BASE_TAG;
/// Tag bits for the `Class` type.
const CLASS_TAG: u64 = 0b101 | CELL_BASE_TAG;
/// Tag bits for the `Instance` type.
const INSTANCE_TAG: u64 = 0b110 | CELL_BASE_TAG;
/// Tag bits for the `Invokable` type.
const INVOKABLE_TAG: u64 = 0b111 | CELL_BASE_TAG;

/// Represents an SOM value.
#[derive(Clone, Copy, Eq, Hash)]
#[repr(transparent)]
pub struct BCNaNBoxedVal {
    /// The 64-bit value that is used to store SOM values using NaN-boxing.
    pub encoded: u64,
}

nan_boxed_val_base_impl!(BCNaNBoxedVal);

impl BCNaNBoxedVal {
    /// Returns a new array value.
    #[inline(always)]
    pub fn new_array(value: GCRef<VecValue>) -> Self {
        Self::new(ARRAY_TAG, value.ptr.as_usize().try_into().unwrap())
    }
    /// Returns a new block value.
    #[inline(always)]
    pub fn new_block(value: GCRef<Block>) -> Self {
        Self::new(BLOCK_TAG, value.ptr.as_usize().try_into().unwrap())
    }
    /// Returns a new class value.
    #[inline(always)]
    pub fn new_class(value: GCRef<Class>) -> Self {
        Self::new(CLASS_TAG, value.ptr.as_usize().try_into().unwrap())
    }
    /// Returns a new instance value.
    #[inline(always)]
    pub fn new_instance(value: GCRef<Instance>) -> Self {
        Self::new(INSTANCE_TAG, value.ptr.as_usize().try_into().unwrap())
    }
    /// Returns a new invocable value.
    #[inline(always)]
    pub fn new_invokable(value: GCRef<Method>) -> Self {
        Self::new(INVOKABLE_TAG, value.ptr.as_usize().try_into().unwrap())
    }

    /// Returns whether this value is an array.
    #[inline(always)]
    pub fn is_array(self) -> bool {
        self.tag() == ARRAY_TAG
    }
    /// Returns whether this value is a block.
    #[inline(always)]
    pub fn is_block(self) -> bool {
        self.tag() == BLOCK_TAG
    }
    /// Returns whether this value is a class.
    #[inline(always)]
    pub fn is_class(self) -> bool {
        self.tag() == CLASS_TAG
    }
    /// Returns whether this value is an instance.
    #[inline(always)]
    pub fn is_instance(self) -> bool {
        self.tag() == INSTANCE_TAG
    }
    /// Returns whether this value is an invocable.
    #[inline(always)]
    pub fn is_invocable(self) -> bool {
        self.tag() == INVOKABLE_TAG
    }

    /// Returns this value as an array, if such is its type.
    #[inline(always)]
    pub fn as_array(self) -> Option<GCRef<VecValue>> {
        self.is_array().then(|| self.extract_gc_cell())
    }
    /// Returns this value as a block, if such is its type.
    #[inline(always)]
    pub fn as_block(self) -> Option<GCRef<Block>> {
        self.is_block().then(|| self.extract_gc_cell())
    }

    /// Returns this value as a class, if such is its type.
    #[inline(always)]
    pub fn as_class(self) -> Option<GCRef<Class>> {
        self.is_class().then(|| self.extract_gc_cell())
    }
    /// Returns this value as an instance, if such is its type.
    #[inline(always)]
    pub fn as_instance(self) -> Option<GCRef<Instance>> {
        self.is_instance().then(|| self.extract_gc_cell())
    }
    /// Returns this value as an invocable, if such is its type.
    #[inline(always)]
    pub fn as_invokable(self) -> Option<GCRef<Method>> {
        self.is_invocable().then(|| self.extract_gc_cell())
    }

    /// Get the class of the current value.
    #[inline(always)]
    pub fn class(&self, universe: &Universe) -> GCRef<Class> {
        match self.tag() {
            NIL_TAG => universe.nil_class(),
            SYSTEM_TAG => universe.system_class(),
            BOOLEAN_TAG => {
                if self.as_boolean().unwrap() {
                    universe.true_class()
                } else {
                    universe.false_class()
                }
            }
            INTEGER_TAG | BIG_INTEGER_TAG => universe.integer_class(),
            SYMBOL_TAG => universe.symbol_class(),
            STRING_TAG => universe.string_class(),
            ARRAY_TAG => universe.array_class(),
            BLOCK_TAG => self.as_block().unwrap().class(universe),
            INSTANCE_TAG => self.as_instance().unwrap().class(),
            CLASS_TAG => self.as_class().unwrap().class(),
            INVOKABLE_TAG => self.as_invokable().unwrap().class(universe),
            _ => {
                if self.is_double() {
                    universe.double_class()
                } else {
                    panic!("unknown tag");
                }
            }
        }
    }

    /// Search for a given method for this value.
    pub fn lookup_method(&self, universe: &Universe, signature: Interned) -> Option<GCRef<Method>> {
        self.class(universe).lookup_method(signature)
    }

    /// Search for a local binding within this value.
    pub fn lookup_local(&self, idx: usize) -> Value {
        if let Some(instance) = self.as_instance() {
            instance.lookup_local(idx)
        } else if let Some(class) = self.as_class() {
            class.lookup_local(idx)
        } else {
            panic!("looking up a local not from an instance or a class")
        }
    }

    /// Assign a value to a local binding within this value.
    pub fn assign_local(&mut self, idx: usize, value: Value) -> Option<()> {
        if let Some(mut instance) = self.as_instance() {
            Some(instance.assign_local(idx, value))
        } else if let Some(mut class) = self.as_class() {
            Some(class.assign_local(idx, value))
        } else {
            None
        }
    }

    /// Checks if a value has a local variable (field) at the given index. Used by the instVarAt and instVarAtPut primitives.
    /// Basically, we want normal field lookups/assignments to not be able to fail (through unsafe) to be fast, since we know the bytecode we emitted that needs them is sound.
    /// But those prims are free to be used and abused by devs, so they CAN fail, and we need to check that they won't fail before we invoke them. Hence this `has_local`.
    pub fn has_local(&self, idx: usize) -> bool {
        if let Some(instance) = self.as_instance() {
            instance.has_local(idx)
        } else if let Some(class) = self.as_class() {
            class.has_local(idx)
        } else {
            false
        }
    }

    /// Get the string representation of this value.
    pub fn to_string(&self, universe: &Universe) -> String {
        match self.tag() {
            NIL_TAG => "nil".to_string(),
            SYSTEM_TAG => "system".to_string(),
            BOOLEAN_TAG => self.as_boolean().unwrap().to_string(),
            INTEGER_TAG => self.as_integer().unwrap().to_string(),
            BIG_INTEGER_TAG => self.as_big_integer().unwrap().to_string(),
            _ if self.is_double() => self.as_double().unwrap().to_string(),
            SYMBOL_TAG => {
                let symbol = universe.lookup_symbol(self.as_symbol().unwrap());
                if symbol.chars().any(|ch| ch.is_whitespace() || ch == '\'') {
                    format!("#'{}'", symbol.replace("'", "\\'"))
                } else {
                    format!("#{}", symbol)
                }
            }
            STRING_TAG => self.as_string().unwrap().to_string(),
            ARRAY_TAG => {
                // TODO: I think we can do better here (less allocations).
                let strings: Vec<String> = self
                    .as_array()
                    .unwrap()
                    .0
                    .iter()
                    .map(|value| value.to_string(universe))
                    .collect();
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
                let invokable = self.as_invokable().unwrap();
                format!("{}>>#{}", invokable.holder.name(), invokable.signature(),)
            }
            _ => {
                panic!("unknown tag")
            }
        }
    }

}

// for backwards compatibility with current code... and maybe easy replacement with ValueEnum?
#[allow(non_snake_case)]
impl BCNaNBoxedVal {
    #[inline(always)]
    pub fn Array(value: GCRef<VecValue>) -> Self {
        BCNaNBoxedVal::new_array(value)
    }

    #[inline(always)]
    pub fn Block(value: GCRef<Block>) -> Self {
        BCNaNBoxedVal::new_block(value)
    }

    #[inline(always)]
    pub fn Class(value: GCRef<Class>) -> Self {
        BCNaNBoxedVal::new_class(value)
    }

    #[inline(always)]
    pub fn Instance(value: GCRef<Instance>) -> Self {
        BCNaNBoxedVal::new_instance(value)
    }

    #[inline(always)]
    pub fn Invokable(value: GCRef<Method>) -> Self {
        BCNaNBoxedVal::new_invokable(value)
    }
}

impl Debug for BCNaNBoxedVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        ValueEnum::from(*self).fmt(f)
    }
}

impl From<BCNaNBoxedVal> for ValueEnum {
    fn from(value: BCNaNBoxedVal) -> Self {
        if let Some(value) = value.as_double() {
            Self::Double(value)
        } else if value.is_nil() {
            Self::Nil
        } else if value.is_system() {
            Self::System
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
            // to work, would need mutator to be passed as an argument to create a new GCRef. not hard, but we'd ditch the From trait
            eprintln!("no From<NanBoxedVal> impl for arr. returning Nil.");
            Self::NIL
        } else if let Some(value) = value.as_block() {
            Self::Block(value)
        } else if let Some(value) = value.as_instance() {
            Self::Instance(value)
        } else if let Some(value) = value.as_class() {
            Self::Class(value)
        } else if let Some(value) = value.as_invokable() {
            Self::Invokable(value)
        } else {
            todo!()
        }
    }
}

impl From<ValueEnum> for BCNaNBoxedVal {
    fn from(value: ValueEnum) -> Self {
        match value {
            ValueEnum::Nil => Self::NIL,
            ValueEnum::System => Self::SYSTEM,
            ValueEnum::Boolean(value) => Self::new_boolean(value),
            ValueEnum::Integer(value) => Self::new_integer(value),
            ValueEnum::BigInteger(value) => Self::new_big_integer(value),
            ValueEnum::Double(value) => Self::new_double(value),
            ValueEnum::Symbol(value) => Self::new_symbol(value),
            ValueEnum::String(value) => Self::new_string(value),
            ValueEnum::Array(_value) => unimplemented!("no impl for arr. would need mutator to be passed as an argument to create a new GCRef. not hard, but we'd ditch the From trait"),
            ValueEnum::Block(value) => Self::new_block(value),
            ValueEnum::Instance(value) => Self::new_instance(value),
            ValueEnum::Class(value) => Self::new_class(value),
            ValueEnum::Invokable(value) => Self::new_invokable(value),
        }
    }
}

/// Represents an SOM value.
#[derive(Clone)]
pub enum ValueEnum {
    /// The **nil** value.
    Nil,
    /// The **system** value.
    System,
    /// A boolean value (**true** or **false**).
    Boolean(bool),
    /// An integer value.
    Integer(i32),
    /// A big integer value (arbitrarily big).
    BigInteger(GCRef<BigInt>),
    /// An floating-point value.
    Double(f64),
    /// An interned symbol value.
    Symbol(Interned),
    /// A string value.
    String(GCRef<String>),
    /// An array of values.
    Array(GCRef<Vec<ValueEnum>>),
    /// A block value, ready to be evaluated.
    Block(GCRef<Block>),
    /// A generic (non-primitive) class instance.
    Instance(GCRef<Instance>),
    /// A bare class object.
    Class(GCRef<Class>),
    /// A bare invokable.
    Invokable(GCRef<Method>),
}

impl ValueEnum {
    /// Get the class of the current value.
    pub fn class(&self, universe: &Universe) -> GCRef<Class> {
        match self {
            Self::Nil => universe.nil_class(),
            Self::System => universe.system_class(),
            Self::Boolean(true) => universe.true_class(),
            Self::Boolean(false) => universe.false_class(),
            Self::Integer(_) => universe.integer_class(),
            Self::BigInteger(_) => universe.integer_class(),
            Self::Double(_) => universe.double_class(),
            Self::Symbol(_) => universe.symbol_class(),
            Self::String(_) => universe.string_class(),
            Self::Array(_) => universe.array_class(),
            Self::Block(block) => block.class(universe),
            Self::Instance(instance_ptr) => instance_ptr.class(),
            Self::Class(class) => class.class(),
            Self::Invokable(invokable) => invokable.class(universe),
        }
    }

    /// Search for a given method for this value.
    #[inline(always)]
    pub fn lookup_method(&self, universe: &Universe, signature: Interned) -> Option<GCRef<Method>> {
        self.class(universe).lookup_method(signature)
    }

    /// Search for a local binding within this value.
    #[inline(always)]
    pub fn lookup_local(&self, idx: usize) -> Self {
        match self {
            Self::Instance(instance_ptr) => instance_ptr.lookup_local(idx).into(),
            Self::Class(class) => class.lookup_local(idx).into(),
            v => unreachable!("Attempting to look up a local in {:?}", v),
        }
    }

    /// Assign a value to a local binding within this value.
    pub fn assign_local(&mut self, idx: usize, value: Self) {
        match self {
            Self::Instance(instance_ptr) => instance_ptr.assign_local(idx, value.into()),
            Self::Class(class) => class.assign_local(idx, value.into()),
            v => unreachable!("Attempting to assign a local in {:?}", v),
        }
    }

    /// Get the string representation of this value.
    pub fn to_string(&self, universe: &Universe) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::System => "system".to_string(),
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
            Self::Array(values) => {
                // TODO (from nicolas): I think we can do better here (less allocations).
                let strings: Vec<String> = values
                    .iter()
                    .map(|value| value.to_string(universe))
                    .collect();
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
            (Self::Nil, Self::Nil) | (Self::System, Self::System) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a.eq(b),
            (Self::Integer(a), Self::Integer(b)) => a.eq(b),
            (Self::Integer(a), Self::Double(b)) | (Self::Double(b), Self::Integer(a)) => {
                (*a as f64).eq(b)
            }
            (Self::Double(a), Self::Double(b)) => a.eq(b),
            (Self::BigInteger(a), Self::BigInteger(b)) => a.eq(b),
            (Self::BigInteger(a), Self::Integer(b)) | (Self::Integer(b), Self::BigInteger(a)) => {
                // a.eq(&BigInt::from(*b))
                (&**a).eq(&BigInt::from(*b)) // not sure that's entirely correct
            }
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
            Self::System => f.debug_tuple("System").finish(),
            Self::Boolean(val) => f.debug_tuple("Boolean").field(val).finish(),
            Self::Integer(val) => f.debug_tuple("Integer").field(val).finish(),
            Self::BigInteger(val) => f.debug_tuple("BigInteger").field(val).finish(),
            Self::Double(val) => f.debug_tuple("Double").field(val).finish(),
            Self::Symbol(val) => f.debug_tuple("Symbol").field(val).finish(),
            Self::String(val) => f.debug_tuple("String").field(val).finish(),
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

impl ValueEnum {
    /// Returns whether this value is a big integer.
    #[inline(always)]
    pub fn is_big_integer(&self) -> bool {
        matches!(self, ValueEnum::BigInteger(_))
    }
    /// Returns whether this value is a string.
    #[inline(always)]
    pub fn is_string(&self) -> bool {
        matches!(self, ValueEnum::String(_))
    }
    /// Returns whether this value is an array.
    #[inline(always)]
    pub fn is_array(&self) -> bool {
        matches!(self, ValueEnum::Array(_))
    }
    /// Returns whether this value is a block.
    #[inline(always)]
    pub fn is_block(&self) -> bool {
        matches!(self, ValueEnum::Block(_))
    }
    /// Returns whether this value is a class.
    #[inline(always)]
    pub fn is_class(&self) -> bool {
        matches!(self, ValueEnum::Class(_))
    }
    /// Returns whether this value is an instance.
    #[inline(always)]
    pub fn is_instance(&self) -> bool {
        matches!(self, ValueEnum::Instance(_))
    }
    /// Returns whether this value is an invocable.
    #[inline(always)]
    pub fn is_invocable(&self) -> bool {
        matches!(self, ValueEnum::Invokable(_))
    }

    // `is_*` methods for pointer types

    /// Returns whether this value is `nil``.
    #[inline(always)]
    pub fn is_nil(&self) -> bool {
        matches!(self, ValueEnum::Nil)
    }
    /// Returns whether this value is `system`.
    #[inline(always)]
    pub fn is_system(&self) -> bool {
        matches!(self, ValueEnum::System)
    }
    /// Returns whether this value is an integer.
    #[inline(always)]
    pub fn is_integer(&self) -> bool {
        matches!(self, ValueEnum::Integer(_))
    }
    /// Returns whether this value is a boolean.
    #[inline(always)]
    pub fn is_boolean(&self) -> bool {
        matches!(self, ValueEnum::Boolean(_))
    }

    /// Returns whether or not it's a boolean corresponding to true. NB: does NOT check if the type actually is a boolean.
    #[inline(always)]
    pub fn is_boolean_true(&self) -> bool {
        matches!(self, ValueEnum::Boolean(true))
    }

    /// Returns whether or not it's a boolean corresponding to false. NB: does NOT check if the type actually is a boolean.
    #[inline(always)]
    pub fn is_boolean_false(&self) -> bool {
        matches!(self, ValueEnum::Boolean(false))
    }

    /// Returns whether this value is a symbol.
    #[inline(always)]
    pub fn is_symbol(&self) -> bool {
        matches!(self, ValueEnum::Symbol(_))
    }

    /// Returns whether this value is a double.
    #[inline(always)]
    pub fn is_double(&self) -> bool {
        matches!(self, ValueEnum::Double(_))
    }

    // `as_*` for pointer types

    /// Returns this value as a big integer, if such is its type.
    #[inline(always)]
    pub fn as_big_integer(&self) -> Option<GCRef<BigInt>> {
        if let ValueEnum::BigInteger(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    /// Returns this value as a string, if such is its type.
    #[inline(always)]
    pub fn as_string(&self) -> Option<GCRef<String>> {
        if let ValueEnum::String(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    /// Returns this value as an array, if such is its type.
    #[inline(always)]
    pub fn as_array(&self) -> Option<GCRef<Vec<ValueEnum>>> {
        if let ValueEnum::Array(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    /// Returns this value as a block, if such is its type.
    #[inline(always)]
    pub fn as_block(&self) -> Option<GCRef<Block>> {
        if let ValueEnum::Block(blk) = self {
            Some(*blk)
        } else {
            None
        }
    }

    /// Returns this value as a class, if such is its type.
    #[inline(always)]
    pub fn as_class(&self) -> Option<GCRef<Class>> {
        if let ValueEnum::Class(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    /// Returns this value as an instance, if such is its type.
    #[inline(always)]
    pub fn as_instance(&self) -> Option<GCRef<Instance>> {
        if let Self::Instance(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    /// Returns this value as an invocable, if such is its type.
    #[inline(always)]
    pub fn as_invokable(&self) -> Option<GCRef<Method>> {
        if let Self::Invokable(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    // `as_*` for non pointer types

    /// Returns this value as an integer, if such is its type.
    #[inline(always)]
    pub fn as_integer(&self) -> Option<i32> {
        if let ValueEnum::Integer(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    /// Returns this value as a boolean, if such is its type.
    #[inline(always)]
    pub fn as_boolean(&self) -> Option<bool> {
        if let ValueEnum::Boolean(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    /// Returns this value as a symbol, if such is its type.
    #[inline(always)]
    pub fn as_symbol(&self) -> Option<Interned> {
        if let ValueEnum::Symbol(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns this value as a double, if such is its type.
    #[inline(always)]
    pub fn as_double(&self) -> Option<f64> {
        if let ValueEnum::Double(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// The `nil` value.
    pub const NIL: Self = ValueEnum::Nil;
    /// The `system` value.
    pub const SYSTEM: Self = ValueEnum::System;
    /// The boolean `true` value.
    pub const TRUE: Self = ValueEnum::Boolean(true);
    /// The boolean `false` value.
    pub const FALSE: Self = ValueEnum::Boolean(false);

    /// The integer `0` value.
    pub const INTEGER_ZERO: Self = ValueEnum::Integer(0);
    /// The integer `1` value.
    pub const INTEGER_ONE: Self = ValueEnum::Integer(1);

    /// Returns a new boolean value.
    #[inline(always)]
    pub fn new_boolean(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    /// Returns a new integer value.
    #[inline(always)]
    pub fn new_integer(value: i32) -> Self {
        ValueEnum::Integer(value)
    }

    /// Returns a new double value.
    #[inline(always)]
    pub fn new_double(value: f64) -> Self {
        ValueEnum::Double(value)
    }

    /// Returns a new symbol value.
    #[inline(always)]
    pub fn new_symbol(value: Interned) -> Self {
        ValueEnum::Symbol(value)
    }

    // `new_*` for pointer types

    /// Returns a new big integer value.
    #[inline(always)]
    pub fn new_big_integer(value: GCRef<BigInt>) -> Self {
        ValueEnum::BigInteger(value)
    }
    /// Returns a new string value.
    #[inline(always)]
    pub fn new_string(value: GCRef<String>) -> Self {
        ValueEnum::String(value)
    }
    /// Returns a new array value.
    #[inline(always)]
    pub fn new_array(value: GCRef<Vec<ValueEnum>>) -> Self {
        ValueEnum::Array(value)
    }
    /// Returns a new block value.
    #[inline(always)]
    pub fn new_block(value: GCRef<Block>) -> Self {
        ValueEnum::Block(value)
    }
    /// Returns a new class value.
    #[inline(always)]
    pub fn new_class(value: GCRef<Class>) -> Self {
        ValueEnum::Class(value)
    }
    /// Returns a new instance value.
    #[inline(always)]
    pub fn new_instance(value: GCRef<Instance>) -> Self {
        ValueEnum::Instance(value)
    }
    /// Returns a new invocable value.
    #[inline(always)]
    pub fn new_invokable(value: GCRef<Method>) -> Self {
        ValueEnum::Invokable(value)
    }

    /// Checks if a value has a local variable (field) at the given index. Used by the instVarAt and instVarAtPut primitives.
    /// Basically, we want normal field lookups/assignments to not be able to fail (through unsafe) to be fast, since we know the bytecode we emitted that needs them is sound.
    /// But those prims are free to be used and abused by devs, so they CAN fail, and we need to check that they won't fail before we invoke them. Hence this `has_local`.
    pub fn has_local(&self, idx: usize) -> bool {
        if let Some(instance) = self.as_instance() {
            instance.has_local(idx)
        } else if let Some(class) = self.as_class() {
            class.has_local(idx)
        } else {
            false
        }
    }
}