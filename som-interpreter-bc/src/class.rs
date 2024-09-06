use std::fmt;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::interner::Interned;
use crate::method::Method;
use crate::value::Value;
use crate::{SOMRef, SOMWeakRef};

/// A reference that may be either weak or owned/strong.
#[derive(Debug, Clone)]
pub enum MaybeWeak<A> {
    /// An owned reference.
    Strong(SOMRef<A>),
    /// A weak reference.
    Weak(SOMWeakRef<A>),
}

/// Represents a loaded class.
#[derive(Clone)]
pub struct Class {
    /// The class' name.
    pub name: String,
    /// The class of this class.
    pub class: MaybeWeak<Class>,
    /// The superclass of this class.
    pub super_class: Option<SOMRef<Class>>,
    /// The class' locals.
    pub locals: IndexMap<Interned, Value>,
    /// The class' methods/invokables.
    pub methods: IndexMap<Interned, Rc<Method>>,
    /// Is this class a static one ?
    pub is_static: bool,
}

impl Class {
    /// Get the class' name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the class of this class.
    pub fn class(&self) -> SOMRef<Self> {
        match self.class {
            MaybeWeak::Weak(ref weak) => weak.upgrade().unwrap_or_else(|| {
                panic!("superclass dropped, cannot upgrade ref ({})", self.name())
            }),
            MaybeWeak::Strong(ref owned) => owned.clone(),
        }
    }

    /// Set the class of this class (as a weak reference).
    pub fn set_class(&mut self, class: &SOMRef<Self>) {
        self.class = MaybeWeak::Weak(Rc::downgrade(class));
    }

    /// Set the class of this class (as a strong reference).
    pub fn set_class_owned(&mut self, class: &SOMRef<Self>) {
        self.class = MaybeWeak::Strong(class.clone());
    }

    /// Get the superclass of this class.
    pub fn super_class(&self) -> Option<SOMRef<Self>> {
        self.super_class.clone()
    }

    /// Set the superclass of this class (as a weak reference).
    pub fn set_super_class(&mut self, class: &SOMRef<Self>) {
        self.super_class = Some(class.clone());
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: Interned) -> Option<Rc<Method>> {
        self.methods.get(&signature).cloned().or_else(|| {
            self.super_class.as_ref()?
                .borrow()
                .lookup_method(signature)
        })
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: usize) -> Value {
        self.locals.values().nth(idx).cloned().unwrap_or_else(|| {
            let super_class = self.super_class().unwrap();
            let super_class_ref = super_class.borrow_mut();
            super_class_ref.lookup_local(idx)
        })
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: Value) {
        match self.locals.values_mut().nth(idx) {
            Some(local) => {
                *local = value;
            },
            None => {
                let super_class = self.super_class().unwrap();
                super_class.borrow_mut().assign_local(idx, value);
            }
        }
    }

    /// Checks whether there exists a local binding of a given index.
    pub fn has_local(&self, idx: usize) -> bool {
        idx < self.locals.len()
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
