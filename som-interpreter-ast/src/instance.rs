use crate::class::Class;
use crate::value::Value;
use som_gc::gcref::GCRef;
use std::fmt;

/// Represents a generic (non-primitive) class instance.
#[derive(Clone)]
pub struct Instance {
    /// The class of which this is an instance from.
    pub class: GCRef<Class>,
    /// This instance's locals.
    pub locals: Vec<Value>,
}

impl Instance {
    /// Construct an instance for a given class.
    pub fn from_class(class: GCRef<Class>) -> Self {
        let locals = class.fields.iter().map(|_| Value::NIL).collect();

        Self {
            class,
            locals,
        }
    }

    /// Get the class of which this is an instance from.
    pub fn class(&self) -> GCRef<Class> {
        self.class
    }

    /// Get the superclass of this instance's class.
    pub fn super_class(&self) -> Option<GCRef<Class>> {
        self.class.super_class()
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: u8) -> Value {
        match cfg!(debug_assertions) {
            true => self.locals.get(idx as usize).unwrap().clone(),
            false => unsafe { self.locals.get_unchecked(idx as usize).clone() }
        }
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, idx: u8, value: Value) {
        // dbg!(&idx);
        *self.locals.get_mut(idx as usize).unwrap() = value;
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instance")
            .field("name", &self.class.name())
            .field("fields", &self.locals.len())
            .field("methods", &self.class().methods.len())
            .finish()
    }
}
