use super::Value;
use crate::gc::{BCObjMagicId, VecValue};
use crate::universe::Universe;
use crate::value::value_enum::ValueEnum;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use num_bigint::BigInt;
use som_gc::debug_assert_valid_semispace_ptr_value;
use som_gc::gcref::Gc;
use som_value::delegate_to_base_value;
use som_value::interned::Interned;
use som_value::value::*;
use som_value::value_ptr::HasPointerTag;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;

pub(crate) const PTR_TAG: u64 = 0b100 | CELL_BASE_TAG;

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

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value(BaseValue::from(value))
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
        new_char(value: char) -> Self,
        new_big_integer(value: Gc<BigInt>) -> Self,
        new_string(value: Gc<String>) -> Self,
        Boolean(value: bool) -> Self,
        Char(value: char) -> Self,
        Integer(value: i32) -> Self,
        Double(value: f64) -> Self,
        Symbol(value: Interned) -> Self,
        BigInteger(value: Gc<BigInt>) -> Self,
        String(value: Gc<String>) -> Self,
    );

    #[inline(always)]
    pub fn is_value_ptr<T: HasPointerTag>(&self) -> bool {
        // TODO: invalid at the moment: will not properly check which type we're dealing with,
        // since all pointers have the same tag. easy fix but see next function comment, we need to
        // adjust the interface in the first place
        //self.0.is_ptr::<T, Gc<T>>()
        self.is_ptr_type()
    }

    #[inline(always)]
    pub fn as_value_ptr<T: HasPointerTag>(&self) -> Option<Gc<T>> {
        // TODO: removed all checks! need to adapt the typedptr interface instead to work with
        // arbitrary pointer tags better
        Some(self.extract_pointer_bits().into())
        //self.0.as_ptr::<T, Gc<T>>()
    }

    /// Returns this value as an array, if such is its type.
    #[inline(always)]
    pub fn as_array(self) -> Option<VecValue> {
        if !self.is_ptr_type() {
            return None;
        }
        let ptr = self.extract_pointer_bits();
        unsafe {
            let header: &BCObjMagicId = &*((ptr - 8) as *const BCObjMagicId);
            match header {
                BCObjMagicId::ArrayVal => Some(VecValue(ptr.into())),
                _ => None,
            }
        }
    }

    #[inline(always)]
    pub fn as_block(self) -> Option<Gc<Block>> {
        if !self.is_ptr_type() {
            return None;
        }
        let ptr = self.extract_pointer_bits();
        unsafe {
            let header: &BCObjMagicId = &*((ptr - 8) as *const BCObjMagicId);
            match header {
                BCObjMagicId::Block => Some(ptr.into()),
                _ => None,
            }
        }
    }

    /// Returns this value as a class, if such is its type.
    #[inline(always)]
    pub fn as_class(self) -> Option<Gc<Class>> {
        if !self.is_ptr_type() {
            return None;
        }
        let ptr = self.extract_pointer_bits();
        unsafe {
            let header: &BCObjMagicId = &*((ptr - 8) as *const BCObjMagicId);
            match header {
                BCObjMagicId::Class => Some(ptr.into()),
                _ => None,
            }
        }
    }
    /// Returns this value as an instance, if such is its type.
    #[inline(always)]
    pub fn as_instance(self) -> Option<Gc<Instance>> {
        if !self.is_ptr_type() {
            return None;
        }
        let ptr = self.extract_pointer_bits();
        unsafe {
            let header: &BCObjMagicId = &*((ptr - 8) as *const BCObjMagicId);
            match header {
                BCObjMagicId::Instance => Some(ptr.into()),
                _ => None,
            }
        }
    }
    /// Returns this value as an invokable, if such is its type.
    #[inline(always)]
    pub fn as_invokable(self) -> Option<Gc<Method>> {
        if !self.is_ptr_type() {
            return None;
        }
        let ptr = self.extract_pointer_bits();
        unsafe {
            let header: &BCObjMagicId = &*((ptr - 8) as *const BCObjMagicId);
            match header {
                BCObjMagicId::Method => Some(ptr.into()),
                _ => None,
            }
        }
    }

    /// Get the class of the current value.
    #[inline(always)]
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        debug_assert_valid_semispace_ptr_value!(self);
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
            _ => {
                if self.is_ptr_type() {
                    if let Some(blk) = self.as_block() {
                        return blk.class(universe);
                    } else if let Some(cls) = self.as_class() {
                        return cls.class();
                    } else if let Some(instance) = self.as_instance() {
                        return instance.class();
                    } else if let Some(invokable) = self.as_invokable() {
                        return invokable.class(universe);
                    } else if self.as_array().is_some() {
                        return universe.core.array_class();
                    }
                }
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
            _ => {
                if let Some(block) = self.as_block() {
                    format!("instance of Block{}", block.nb_parameters() + 1)
                } else if let Some(class) = self.as_class() {
                    class.name().to_string()
                } else if let Some(instance) = self.as_instance() {
                    format!("instance of {} class", instance.class().name(),)
                } else if let Some(invokable) = self.as_invokable() {
                    format!("{}>>#{}", invokable.holder().name(), invokable.signature(),)
                } else if let Some(arr) = self.as_array() {
                    // TODO: I think we can do better here (less allocations).
                    let strings: Vec<String> = arr.iter().map(|value| value.to_string(universe)).collect();
                    format!("#({})", strings.join(" "))
                } else {
                    panic!("unknown tag")
                }
            }
        }
    }
}

// for backwards compatibility with current code... and maybe easy replacement with ValueEnum?
#[allow(non_snake_case)]
impl Value {
    #[inline(always)]
    pub fn Array(value: VecValue) -> Self {
        // TODO use TypedPtrValue somehow instead
        Value(BaseValue::new(PTR_TAG, value.0.into()))
    }

    #[inline(always)]
    pub fn Block(value: Gc<Block>) -> Self {
        Value(BaseValue::new(PTR_TAG, value.into()))
    }

    #[inline(always)]
    pub fn Class(value: Gc<Class>) -> Self {
        Value(BaseValue::new(PTR_TAG, value.into()))
    }
    #[inline(always)]
    pub fn Instance(value: Gc<Instance>) -> Self {
        Value(BaseValue::new(PTR_TAG, value.into()))
    }

    #[inline(always)]
    pub fn Invokable(value: Gc<Method>) -> Self {
        Value(BaseValue::new(PTR_TAG, value.into()))
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

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(arr) = self.as_array() {
            f.write_fmt(format_args!("VecValue({:?})", arr.0))
        } else {
            ValueEnum::from(*self).fmt(f)
        }
    }
}
