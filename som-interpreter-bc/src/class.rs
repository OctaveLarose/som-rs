use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

use indexmap::IndexMap;
use mmtk::{AllocationSemantics, Mutator};
use som_gc::api::{mmtk_alloc, mmtk_post_alloc};
use som_gc::SOMVM;
use crate::interner::Interned;
use crate::method::Method;
use crate::value::Value;
use crate::{SOMRef, SOMWeakRef};
use crate::gc::{Alloc, GCRef};
use core::mem::size_of;

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
    pub class: GCRef<Class>,
    /// The superclass of this class.
    pub super_class: Option<GCRef<Class>>,
    /// The class' locals.
    pub locals: IndexMap<Interned, Value>,
    /// The class' methods/invokables.
    pub methods: IndexMap<Interned, Rc<Method>>,
    /// Is this class a static one ?
    pub is_static: bool,
}

impl Alloc<Class> for Class {
    fn alloc(class: Class, mutator: &mut Mutator<SOMVM>) -> GCRef<Self> {
        let size = size_of::<Class>();
        let align= 8;
        let offset= 0;
        let semantics = AllocationSemantics::Default;

        let class_addr = mmtk_alloc(mutator, size, align, offset, semantics);
        debug_assert!(!class_addr.is_zero());

        mmtk_post_alloc(mutator, SOMVM::object_start_to_ref(class_addr), size, semantics);

        unsafe {
            *class_addr.as_mut_ref() = class;
        }

        // println!("class allocation OK");

        GCRef {
            ptr: class_addr,
            _phantom: PhantomData
        }
    }
}

impl Class {
    /// Get the class' name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the class of this class.
    pub fn class(&self) -> GCRef<Self> {
        self.class
    }

    /// Set the class of this class (as a weak reference).
    pub fn set_class(&mut self, class: &GCRef<Self>) {
        self.class = *class;
    }

    /// Set the class of this class (as a strong reference). TODO now useless
    pub fn set_class_owned(&mut self, class: &GCRef<Self>) {
        self.class = *class;
    }

    /// Get the superclass of this class.
    pub fn super_class(&self) -> Option<GCRef<Self>> {
        self.super_class
    }

    /// Set the superclass of this class (as a weak reference).
    pub fn set_super_class(&mut self, class: &GCRef<Self>) {
        self.super_class = Some(*class);
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: Interned) -> Option<Rc<Method>> {
        self.methods.get(&signature).cloned().or_else(|| {
            self.super_class.as_ref()?
                .to_obj()
                .lookup_method(signature)
        })
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: usize) -> Value {
        self.locals.values().nth(idx).cloned().unwrap_or_else(|| {
            let super_class = self.super_class().unwrap();
            super_class.to_obj().lookup_local(idx)
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
                super_class.to_obj().assign_local(idx, value);
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
