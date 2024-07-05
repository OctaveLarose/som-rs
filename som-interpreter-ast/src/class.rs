use std::cell::RefCell;
use std::fmt;
use std::rc::{Rc, Weak};

use indexmap::IndexMap;

use som_core::ast::{ClassDef, MethodBody, MethodDef};

use crate::method::{Method, MethodKind};
use crate::primitives;
use crate::value::Value;
use crate::{SOMRef, SOMWeakRef};
use crate::specialized::down_to_do_node::DownToDoNode;
use crate::specialized::if_node::IfNode;
use crate::specialized::if_true_if_false_node::IfTrueIfFalseNode;
use crate::specialized::to_by_do_node::ToByDoNode;
use crate::specialized::to_do_node::ToDoNode;
use crate::specialized::while_node::WhileNode;

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
    // TODO: Should probably be `Option<SOMRef<Class>>`.
    pub super_class: SOMWeakRef<Class>,
    /// The class' locals.
    pub locals: Vec<Value>,
    /// The class' locals (fields) names. todo rename locals to fields
    pub local_names: Vec<String>,
    /// The class' methods/invokables.
    pub methods: IndexMap<String, Rc<Method>>,
    /// Is this class a static one ?
    pub is_static: bool,
}

impl Class {
    /// Load up a class from its class definition from the AST.
    pub fn from_class_def(defn: ClassDef) -> Result<SOMRef<Class>, String> {
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

        let static_class = Rc::new(RefCell::new(Self {
            name: format!("{} class", defn.name),
            class: MaybeWeak::Weak(Weak::new()),
            super_class: Weak::new(),
            locals: vec![Value::Nil; static_locals.len()],
            local_names: defn.static_locals,
            methods: IndexMap::new(),
            is_static: true,
        }));

        let instance_class = Rc::new(RefCell::new(Self {
            name: defn.name.clone(),
            class: MaybeWeak::Strong(static_class.clone()),
            super_class: Weak::new(),
            locals: vec![Value::Nil; instance_locals.len()],
            local_names: defn.instance_locals,
            methods: IndexMap::new(),
            is_static: false,
        }));

        let mut static_methods: IndexMap<String, Rc<Method>> = defn
            .static_methods
            .iter()
            .map(|method| {
                match method {
                    MethodDef::Generic(method) => {
                        let signature = method.signature.clone();
                        let kind = match method.body {
                            MethodBody::Primitive => MethodKind::NotImplemented(signature.clone()),
                            MethodBody::Body { .. } => MethodKind::Defined(method.clone()),
                        };
                        let method = Method {
                            kind,
                            signature: signature.clone(),
                            holder: Rc::downgrade(&static_class),
                        };
                        (signature, Rc::new(method))
                    }
                    _ => panic!("Unreachable, I believe?") // inlinedwhile, inlinedif, etc.
                }
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
                    holder: Rc::downgrade(&static_class),
                };
                static_methods.insert(signature.to_string(), Rc::new(method));
            }
        }

        let mut instance_methods: IndexMap<String, Rc<Method>> = defn
            .instance_methods
            .iter()
            .map(|method_def| {
                match method_def {
                    // todo find a way to no longer store the method for those inlined method cases...
                    MethodDef::Generic(method) | MethodDef::InlinedWhile(method, _) |
                    MethodDef::InlinedIf(method, _) | MethodDef::InlinedIfTrueIfFalse(method) | 
                    MethodDef::InlinedToDo(method) | MethodDef::InlinedToByDo(method) | MethodDef::InlinedDownToDo(method) => {
                        let signature = method.signature.clone();
                        let kind = match method_def {
                            MethodDef::Generic(_) => {
                                match method.body {
                                    MethodBody::Primitive => MethodKind::NotImplemented(signature.clone()),
                                    MethodBody::Body { .. } => MethodKind::Defined(method.clone())}
                            },
                            MethodDef::InlinedWhile(_, exp_bool) => MethodKind::WhileInlined(WhileNode { expected_bool: *exp_bool }),
                            MethodDef::InlinedIf(_, exp_bool) => MethodKind::IfInlined(IfNode { expected_bool: *exp_bool }),
                            MethodDef::InlinedIfTrueIfFalse(_) => MethodKind::IfTrueIfFalseInlined(IfTrueIfFalseNode {}),
                            MethodDef::InlinedToDo(_) => MethodKind::ToDoInlined(ToDoNode {}),
                            MethodDef::InlinedToByDo(_) => MethodKind::ToByDoInlined(ToByDoNode {}),
                            MethodDef::InlinedDownToDo(_) => MethodKind::DownToDoInlined(DownToDoNode {}),
                        };
                        let method = Method {
                            kind,
                            signature: signature.clone(),
                            holder: Rc::downgrade(&instance_class),
                        };
                        (signature, Rc::new(method))
                    }
                }
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
                    holder: Rc::downgrade(&instance_class),
                };
                instance_methods.insert(signature.to_string(), Rc::new(method));
            }
        }

        static_class.borrow_mut().methods = static_methods;
        instance_class.borrow_mut().methods = instance_methods;

        Ok(instance_class)
    }

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
        self.super_class.upgrade()
    }

    /// Set the superclass of this class (as a weak reference).
    pub fn set_super_class(&mut self, class: &SOMRef<Self>) {
        for local_name in class.borrow().local_names.iter().rev() {
            self.local_names.insert(0, local_name.clone());
        }
        for local in class.borrow().locals.iter().rev() {
            self.locals.insert(0, local.clone());
        }

        self.super_class = Rc::downgrade(class);
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: impl AsRef<str>) -> Option<Rc<Method>> {
        let signature = signature.as_ref();
        self.methods.get(signature).cloned().or_else(|| {
            self.super_class
                .upgrade()?
                .borrow()
                .lookup_method(signature)
        })
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, idx: usize) -> Value {
        self.locals.get(idx).cloned().unwrap_or_else(|| {
            let super_class = self.super_class().unwrap();
            let super_class_ref = super_class.borrow_mut();
            super_class_ref.lookup_local(idx)
        })
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, idx: usize, value: &Value) {
        if let Some(local) = self.locals.get_mut(idx) {
            *local = value.clone();
            return;
        }
        let super_class = self.super_class().unwrap();
        super_class.borrow_mut().assign_local(idx, value);
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            .field("fields", &self.locals.len())
            .field("methods", &self.methods.len())
            // .field("class", &self.class)
            // .field("super_class", &self.super_class)
            .finish()
    }
}
