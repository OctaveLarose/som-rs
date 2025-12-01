use std::{fmt, vec};

use crate::compiler::AstMethodCompilerCtxt;
use crate::primitives;
use crate::value::Value;
use crate::vm_objects::method::{Method, MethodKind};
use indexmap::IndexMap;
use som_core::ast::ClassDef;
use som_core::interner::Interner;
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};
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
    /// The class' fields names.
    pub field_names: Vec<String>,
    /// The class' methods/invokables.
    pub methods: IndexMap<Interned, Gc<Method>>,
}

// I don't test every field, but this should be good enough, AFAIK.
impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.fields == other.fields && self.field_names == other.field_names && self.methods == other.methods
    }
}

impl Class {
    /// Load up a class from its class definition from the AST.
    /// NB: super_class is only ever None for one class: the core Object class, which all other classes inherit from.
    /// NB: while it takes the super_class as argument, it's not in charge of hooking it up to the class itself. That's `set_super_class`. Might need changing for clarity.
    pub fn from_class_def(
        defn: ClassDef,
        super_class: Option<Gc<Class>>,
        gc_interface: &mut GCInterface,
        interner: &mut Interner,
    ) -> Result<Gc<Class>, String> {
        let static_locals = {
            let mut static_locals = vec![];

            if let Some(super_class) = &super_class {
                // NB: only need to check the one superclass, and not walk the superclass tree, since the superclass already contains all the fields of its parents
                for scls_field in &super_class.class.field_names {
                    static_locals.push(scls_field.to_string())
                }
            }

            for field in defn.static_locals.iter() {
                static_locals.push(field.clone());
            }
            static_locals
        };

        let instance_locals = {
            let mut instance_locals = vec![];
            if let Some(super_class) = &super_class {
                for scls_field in &super_class.field_names {
                    instance_locals.push(scls_field.to_string());
                }
            }

            for field in defn.instance_locals.iter() {
                instance_locals.push(field.clone());
            }

            instance_locals
        };

        let maybe_static_superclass = super_class.clone().map(|cls| cls.class.clone());

        let static_class = Self {
            name: format!("{} class", defn.name),
            class: Gc::default(),
            super_class: maybe_static_superclass,
            fields: vec![Value::NIL; static_locals.len()],
            field_names: static_locals,
            methods: IndexMap::new(),
        };

        let mut static_class_gc_ptr = gc_interface.alloc(static_class, AllocSiteMarker::Class);

        let instance_class = Self {
            name: defn.name.clone(),
            class: static_class_gc_ptr.clone(),
            super_class,
            fields: vec![Value::NIL; instance_locals.len()],
            field_names: instance_locals,
            methods: IndexMap::new(),
        };

        let mut instance_class_gc_ptr = gc_interface.alloc(instance_class, AllocSiteMarker::Class);

        let mut static_methods: IndexMap<Interned, Gc<Method>> = defn
            .static_methods
            .iter()
            .map(|method| {
                let signature = method.signature.clone();
                let kind = AstMethodCompilerCtxt::get_method_kind(method, Some(static_class_gc_ptr.clone()), gc_interface, interner);
                let method = Method {
                    kind,
                    signature: signature.clone(),
                    holder: static_class_gc_ptr.clone(),
                };
                (interner.intern(signature.as_str()), gc_interface.alloc(method, AllocSiteMarker::Method))
            })
            .collect();

        if let Some(primitives) = primitives::get_class_primitives(&defn.name) {
            for (signature, primitive, _warning) in primitives {
                let interned_signature = interner.intern(signature);

                //if *warning && !static_methods.contains_key(&interned_signature) {
                //    eprintln!("Warning: Primitive '{}' is not in class definition for class '{}'", signature, defn.name);
                //}

                let method = Method {
                    kind: MethodKind::Primitive(*primitive),
                    signature: signature.to_string(),
                    holder: static_class_gc_ptr.clone(),
                };
                static_methods.insert(interned_signature, gc_interface.alloc(method, AllocSiteMarker::Method));
            }
        }

        let mut instance_methods: IndexMap<Interned, Gc<Method>> = defn
            .instance_methods
            .iter()
            .map(|method| {
                let interned_signature = interner.intern(&method.signature);
                let kind = AstMethodCompilerCtxt::get_method_kind(method, Some(instance_class_gc_ptr.clone()), gc_interface, interner);
                let method = Method {
                    kind,
                    signature: method.signature.clone(),
                    holder: instance_class_gc_ptr.clone(),
                };
                (interned_signature, gc_interface.alloc(method, AllocSiteMarker::Method))
            })
            .collect();

        if let Some(primitives) = primitives::get_instance_primitives(&defn.name) {
            for (signature, primitive, _warning) in primitives {
                let interned_signature = interner.intern(signature);

                //if *warning && !instance_methods.contains_key(&interned_signature) {
                //    eprintln!("Warning: Primitive '{}' is not in class definition for class '{}'", signature, defn.name);
                //}

                let method = Method {
                    kind: MethodKind::Primitive(*primitive),
                    signature: signature.to_string(),
                    holder: instance_class_gc_ptr.clone(),
                };
                instance_methods.insert(interned_signature, gc_interface.alloc(method, AllocSiteMarker::Method));
            }
        }

        static_class_gc_ptr.methods = static_methods;
        instance_class_gc_ptr.methods = instance_methods; // todo does this work? remove if runs ok

        Ok(instance_class_gc_ptr)
    }

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
        self.class = class.clone()
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
        self.methods.get(&signature).cloned().or_else(|| self.super_class()?.lookup_method(signature))
    }

    /// Search for a local binding.
    pub fn lookup_field(&self, idx: u8) -> Value {
        self.fields.get(idx as usize).cloned().unwrap_or_else(|| {
            let super_class = self.super_class().unwrap();
            super_class.lookup_field(idx)
        })
    }

    /// Assign a value to a local binding.
    pub fn assign_field(&mut self, idx: u8, value: Value) {
        if let Some(local) = self.fields.get_mut(idx as usize) {
            *local = value;
            return;
        }
        let mut super_class = self.super_class().unwrap();
        super_class.assign_field(idx, value);
    }

    /// Used during parsing, to generate a FieldRead or a FieldWrite.
    /// Iterates through superclasses to find the index of the field in a given class when it's originally defined in a superclass.
    pub fn get_field_offset_by_name(&self, name: &str) -> Option<usize> {
        self.field_names
            .iter()
            .position(|field_name| field_name == name)
            .map(|pos| pos + self.super_class().map(|scls| scls.get_nbr_fields()).unwrap_or(0))
            .or_else(|| match self.super_class() {
                Some(super_class) => super_class.get_field_offset_by_name(name),
                _ => None,
            })
    }

    pub fn get_nbr_fields(&self) -> usize {
        self.field_names.len()
    }

    /// Used by the `fields` primitive. Could be made faster (strings get cloned, then put on the GC heap in the primitive), but it's also basically never used.
    pub fn get_all_field_names(&self) -> Vec<String> {
        self.field_names.clone()
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            .field("fields", &self.fields.len())
            .field("methods", &self.methods.len())
            // .field("class", &self.class)
            // .field("super_class", &self.super_class)
            .finish()
    }
}
