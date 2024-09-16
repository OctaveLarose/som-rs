use std::fmt;

use indexmap::IndexMap;
use som_core::ast::ClassDef;
use som_core::gc::{GCInterface, GCRef};
use crate::method::{Method, MethodKind};
use crate::primitives;
use crate::value::Value;
use crate::compiler::AstMethodCompilerCtxt;

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
    pub class: GCRef<Class>,
    /// The superclass of this class.
    pub super_class: Option<GCRef<Class>>,
    /// The class' fields.
    pub fields: Vec<Value>,
    /// The class' fields names.
    pub field_names: Vec<String>,
    /// The class' methods/invokables.
    pub methods: IndexMap<String, GCRef<Method>>,
    /// Is this class a static one ?
    pub is_static: bool,
}

// I don't test every field, but this should be good enough, AFAIK.
impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.fields == other.fields && self.field_names == other.field_names && self.methods == other.methods && self.is_static == other.is_static
    }
}

impl Class {
    /// Load up a class from its class definition from the AST.
    /// NB: super_class is only ever None for one class: the core Object class, which all other classes inherit from.
    /// NB: while it takes the super_class as argument, it's not in charge of hooking it up to the class itself. That's `set_super_class`. Might need changing for clarity.
    pub fn from_class_def(defn: ClassDef, super_class: Option<GCRef<Class>>, gc_interface: &mut GCInterface) -> Result<GCRef<Class>, String> {
        let static_locals = {
            let mut static_locals = IndexMap::new();
            for field in defn.static_locals.iter() {
                if static_locals.insert(field.clone(), Value::Nil).is_some() {
                    return Err(format!(
                        "{}: the field named '{}' is already defined in this class",
                        defn.name, field,
                    ));
                }
            }
            static_locals
        };

        let instance_locals = {
            let mut instance_locals = IndexMap::new();
            for field in defn.instance_locals.iter() {
                if instance_locals.insert(field.clone(), Value::Nil).is_some() {
                    return Err(format!(
                        "{}: the field named '{}' is already defined in this class",
                        defn.name, field,
                    ));
                }
            }
            instance_locals
        };

        let static_class = Self {
            name: format!("{} class", defn.name),
            class: GCRef::default(),
            super_class: None,
            fields: vec![Value::Nil; static_locals.len()],
            field_names: defn.static_locals,
            methods: IndexMap::new(),
            is_static: true,
        };

        let static_class_gc_ptr = GCRef::<Class>::alloc(static_class, gc_interface);
        
        let instance_class = Self {
            name: defn.name.clone(),
            class: static_class_gc_ptr,
            super_class: None,
            fields: vec![Value::Nil; instance_locals.len()],
            field_names: defn.instance_locals,
            methods: IndexMap::new(),
            is_static: false,
        };

        let instance_class_gc_ptr = GCRef::<Class>::alloc(instance_class, gc_interface);
        
        let maybe_static_superclass = super_class.as_ref().map(|super_class| super_class.to_obj().class());
        
        let mut static_methods: IndexMap<String, GCRef<Method>> = defn
            .static_methods
            .iter()
            .map(|method| {
                let signature = method.signature.clone();
                let kind = AstMethodCompilerCtxt::get_method_kind(method, maybe_static_superclass.clone(), gc_interface);
                let method = Method {
                    kind,
                    signature: signature.clone(),
                    holder: static_class_gc_ptr,
                };
                (signature, GCRef::<Method>::alloc(method, gc_interface))
            })
            .collect();

        if let Some(primitives) = primitives::get_class_primitives(&defn.name) {
            for (signature, primitive, warning) in primitives {
                if *warning && !static_methods.contains_key(*signature) {
                    eprintln!(
                        "Warning: Primitive '{}' is not in class definition for class '{}'",
                        signature, defn.name
                    );
                }

                let method = Method {
                    kind: MethodKind::Primitive(*primitive),
                    signature: signature.to_string(),
                    holder: static_class_gc_ptr,
                };
                static_methods.insert(signature.to_string(), GCRef::<Method>::alloc(method, gc_interface));
            }
        }

        let mut instance_methods: IndexMap<String, GCRef<Method>> = defn
            .instance_methods
            .iter()
            .map(|method| {
                let signature = method.signature.clone();
                let kind = AstMethodCompilerCtxt::get_method_kind(method, super_class.clone(), gc_interface);
                let method = Method {
                    kind,
                    signature: signature.clone(),
                    holder: instance_class_gc_ptr,
                };
                (signature, GCRef::<Method>::alloc(method, gc_interface))
            })
            .collect();

        if let Some(primitives) = primitives::get_instance_primitives(&defn.name) {
            for (signature, primitive, warning) in primitives {
                if *warning && !instance_methods.contains_key(*signature) {
                    eprintln!(
                        "Warning: Primitive '{}' is not in class definition for class '{}'",
                        signature, defn.name
                    );
                }

                let method = Method {
                    kind: MethodKind::Primitive(*primitive),
                    signature: signature.to_string(),
                    holder: instance_class_gc_ptr,
                };
                instance_methods.insert(signature.to_string(), GCRef::<Method>::alloc(method, gc_interface));
            }
        }

        static_class_gc_ptr.to_obj().methods = static_methods;
        instance_class_gc_ptr.to_obj().methods = instance_methods; // todo does this work? remove if runs ok

        Ok(instance_class_gc_ptr)
    }

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

    /// Get the superclass of this class.
    pub fn super_class(&self) -> Option<GCRef<Self>> {
        self.super_class
    }

    /// Set the superclass of this class (as a weak reference).
    pub fn set_super_class(&mut self, class: &GCRef<Self>) {
        for local_name in class.borrow().field_names.iter().rev() {
            self.field_names.insert(0, local_name.clone());
        }
        for local in class.borrow().fields.iter().rev() {
            self.fields.insert(0, local.clone());
        }

        self.super_class = Some(*class);
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: impl AsRef<str>) -> Option<GCRef<Method>> {
        let signature = signature.as_ref();
        self.methods.get(signature).cloned().or_else(|| {
            self.super_class.clone()?
                .borrow()
                .lookup_method(signature)
        })
    }

    /// Search for a local binding.
    pub fn lookup_field(&self, idx: usize) -> Value {
        self.fields.get(idx).cloned().unwrap_or_else(|| {
            let super_class = self.super_class().unwrap();
            let super_class_ref = super_class.borrow_mut();
            super_class_ref.lookup_field(idx)
        })
    }

    /// Assign a value to a local binding.
    pub fn assign_field(&mut self, idx: usize, value: Value) {
        if let Some(local) = self.fields.get_mut(idx) {
            *local = value;
            return;
        }
        let super_class = self.super_class().unwrap();
        super_class.borrow_mut().assign_field(idx, value);
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
