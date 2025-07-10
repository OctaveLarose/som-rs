use std::fmt;

use crate::value::Value;
use crate::vm_objects::method::Method;
use indexmap::IndexMap;
use som_gc::gcref::Gc;
use som_value::interned::Interned;
// /// A reference that may be either weak or owned/strong.
// #[derive(Debug, Clone)]
// pub enum MaybeWeak<A> {
//     /// An owned reference.
//     Strong(SOMRef<A>),
//     /// A weak reference.
//     Weak(SOMWeakRef<A>),
// }

/// Represents a loaded class.
#[derive(Clone)]
pub struct Class {
    /// The class' name.
    pub name: String,
    /// The class of this class.
    pub class: Gc<Class>,
    /// The superclass of this class.
    pub super_class: Option<Gc<Class>>,
    /// The class' fields.
    pub fields: Vec<Value>,
    /// The class' fields' names, in the same order as the fields array
    pub field_names: Vec<Interned>,
    /// The class' methods/invokables.
    pub methods: IndexMap<Interned, Gc<Method>>,
    /// Is this class a static one ? Unused argument.
    pub is_static: bool,
}

impl Class {
    /// Get the class' name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the class of this class.
    pub fn class(&self) -> Gc<Self> {
        self.class.clone()
    }

    /// Set the class of this class (as a weak reference).
    pub fn set_class(&mut self, class: &Gc<Self>) {
        self.class = class.clone();
    }

    /// Get the superclass of this class.
    pub fn super_class(&self) -> Option<Gc<Self>> {
        self.super_class.clone()
    }

    /// Set the superclass of this class (as a weak reference).
    pub fn set_super_class(&mut self, class: &Gc<Self>) {
        self.super_class = Some(class.clone());
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: Interned) -> Option<Gc<Method>> {
        self.methods.get(&signature).cloned().or_else(|| self.super_class.as_ref()?.lookup_method(signature))
    }

    /// Search for a local binding.
    pub fn lookup_field(&self, idx: usize) -> Value {
        self.fields.get(idx).copied().unwrap_or_else(|| {
            let super_class = self.super_class().unwrap();
            super_class.lookup_field(idx)
        })
    }

    /// Assign a value to a local binding.
    pub fn assign_field(&mut self, idx: usize, value: Value) {
        match self.fields.get_mut(idx) {
            Some(local) => {
                *local = value;
            }
            None => {
                let mut super_class = self.super_class().unwrap();
                super_class.assign_field(idx, value);
            }
        }
    }

    /// Checks whether there exists a local binding of a given index.
    pub fn has_local(&self, idx: usize) -> bool {
        idx < self.fields.len()
    }

    /// Get the total number of fields, counting the superclasses.
    pub fn get_nbr_fields(&self) -> usize {
        //let nbr_locals = self.fields.len();

        // TODO: the AST compounds fields like this, but the BC doesn't. TODO unify logic
        // if let Some(super_class) = self.super_class() {
        //     nbr_locals += super_class.get_nbr_fields()
        // }

        self.fields.len()
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            // .field("locals", &self.locals.keys())
            // .field("class", &self.class)
            // .field("super_class", &self.super_class)
            .finish()
    }
}
