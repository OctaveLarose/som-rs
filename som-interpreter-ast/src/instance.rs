use std::fmt;

use crate::class::Class;
use crate::value::Value;
use crate::SOMRef;

/// Represents a generic (non-primitive) class instance.
#[derive(Clone)]
pub struct Instance {
    /// The class of which this is an instance from.
    pub class: SOMRef<Class>,
    /// This instance's locals.
    pub locals: Vec<Value>,
}

impl Instance {
    /// Construct an instance for a given class.
    pub fn from_class(class: SOMRef<Class>) -> Self {
        let mut locals = vec![];

        fn collect_locals(class: &SOMRef<Class>, locals: &mut Vec<Value>) {
            if let Some(class) = class.borrow().super_class() {
                collect_locals(&class, locals);
            }

            locals.extend(vec![Value::Nil; class.borrow().locals.len()]);
                // class
                //     .borrow()
                //     .locals
                //     .iter()
                //     .zip(std::iter::repeat(Value::Nil)),
            // );
        }

        collect_locals(&class, &mut locals);

        Self { class, locals }
    }

    /// Get the class of which this is an instance from.
    pub fn class(&self) -> SOMRef<Class> {
        self.class.clone()
    }

    /// Get the superclass of this instance's class.
    pub fn super_class(&self) -> Option<SOMRef<Class>> {
        self.class.borrow().super_class()
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: usize) -> Value {
        match cfg!(debug_assertions) {
            true => self.locals.get(idx).unwrap().clone(),
            false => unsafe { self.locals.get_unchecked(idx).clone() }
        }
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: Value) {
        *self.locals.get_mut(idx).unwrap() = value;
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instance")
            .field("name", &self.class.borrow().name())
            .field("fields", &self.locals.len())
            .finish()
    }
}
