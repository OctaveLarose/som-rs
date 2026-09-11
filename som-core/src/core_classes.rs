/// The core classes of the SOM interpreter.
///
/// This struct allows to always keep a reference to important classes,
/// even in case of modifications to global bindings by user-defined code.
#[derive(Debug)]
pub struct CoreClasses<ClassPtr> {
    /// The **Object** class.
    pub object_class: ClassPtr,
    /// The **Class** class.
    pub class_class: ClassPtr,
    /// The **Class** class.
    pub metaclass_class: ClassPtr,

    /// The **Nil** class.
    pub nil_class: ClassPtr,
    /// The **Integer** class.
    pub integer_class: ClassPtr,
    /// The **Double** class.
    pub double_class: ClassPtr,
    /// The **Array** class.
    pub array_class: ClassPtr,
    /// The **Method** class.
    pub method_class: ClassPtr,
    /// The **Primitive** class.
    pub primitive_class: ClassPtr,
    /// The **Symbol** class.
    pub symbol_class: ClassPtr,
    /// The **String** class.
    pub string_class: ClassPtr,
    /// The **System** class.
    pub system_class: ClassPtr,

    /// The **Block** class.
    pub block_class: ClassPtr,
    /// The **Block1** class.
    pub block1_class: ClassPtr,
    /// The **Block2** class.
    pub block2_class: ClassPtr,
    /// The **Block3** class.
    pub block3_class: ClassPtr,

    /// The **Boolean** class.
    pub boolean_class: ClassPtr,
    /// The **True** class.
    pub true_class: ClassPtr,
    /// The **False** class.
    pub false_class: ClassPtr,
}

impl<ClassPtr: Clone> CoreClasses<ClassPtr> {
    /// Loads core classes given a closure that returns a pointer to a class.
    /// FEAT: also take a closure to set_class and set_super_class to do the rest of the hooking up.
    pub fn from_load_cls_fn<F>(mut load_system_cls: F) -> Self
    where
        F: FnMut(&str, Option<&ClassPtr>) -> ClassPtr,
        // F1: FnMut(&ClassPtr, &ClassPtr),
        // F2: FnMut(&ClassPtr, &ClassPtr)
    {
        let object_class = load_system_cls("Object", None);
        let class_class = load_system_cls("Class", Some(&object_class));
        let boolean_class = load_system_cls("Boolean", Some(&object_class));
        let block_class = load_system_cls("Block", Some(&object_class));
        let string_class = load_system_cls("String", Some(&object_class));

        Self {
            metaclass_class: load_system_cls("Metaclass", Some(&class_class)),
            nil_class: load_system_cls("Nil", Some(&object_class)),
            integer_class: load_system_cls("Integer", Some(&object_class)),
            double_class: load_system_cls("Double", Some(&object_class)),
            array_class: load_system_cls("Array", Some(&object_class)),
            method_class: load_system_cls("Method", Some(&object_class)),
            primitive_class: load_system_cls("Primitive", Some(&object_class)),
            symbol_class: load_system_cls("Symbol", Some(&string_class)),
            system_class: load_system_cls("System", Some(&object_class)),
            block1_class: load_system_cls("Block1", Some(&block_class)),
            block2_class: load_system_cls("Block2", Some(&block_class)),
            block3_class: load_system_cls("Block3", Some(&block_class)),
            true_class: load_system_cls("True", Some(&boolean_class)),
            false_class: load_system_cls("False", Some(&boolean_class)),
            string_class,
            block_class,
            boolean_class,
            object_class,
            class_class,
        }
    }
}

impl<ClassPtr: Clone> CoreClasses<ClassPtr> {
    /// Get the **Object** class.
    pub fn object_class(&self) -> ClassPtr {
        self.object_class.clone()
    }

    /// Get the **Nil** class.
    pub fn nil_class(&self) -> ClassPtr {
        self.nil_class.clone()
    }
    /// Get the **System** class.
    pub fn system_class(&self) -> ClassPtr {
        self.system_class.clone()
    }

    /// Get the **Symbol** class.
    pub fn symbol_class(&self) -> ClassPtr {
        self.symbol_class.clone()
    }
    /// Get the **String** class.
    pub fn string_class(&self) -> ClassPtr {
        self.string_class.clone()
    }
    /// Get the **Array** class.
    pub fn array_class(&self) -> ClassPtr {
        self.array_class.clone()
    }

    /// Get the **Integer** class.
    pub fn integer_class(&self) -> ClassPtr {
        self.integer_class.clone()
    }
    /// Get the **Double** class.
    pub fn double_class(&self) -> ClassPtr {
        self.double_class.clone()
    }

    /// Get the **Block** class.
    pub fn block_class(&self) -> ClassPtr {
        self.block_class.clone()
    }
    /// Get the **Block1** class.
    pub fn block1_class(&self) -> ClassPtr {
        self.block1_class.clone()
    }
    /// Get the **Block2** class.
    pub fn block2_class(&self) -> ClassPtr {
        self.block2_class.clone()
    }
    /// Get the **Block3** class.
    pub fn block3_class(&self) -> ClassPtr {
        self.block3_class.clone()
    }

    /// Get the **True** class.
    pub fn true_class(&self) -> ClassPtr {
        self.true_class.clone()
    }
    /// Get the **False** class.
    pub fn false_class(&self) -> ClassPtr {
        self.false_class.clone()
    }

    /// Get the **Metaclass** class.
    pub fn metaclass_class(&self) -> ClassPtr {
        self.metaclass_class.clone()
    }

    /// Get the **Method** class.
    pub fn method_class(&self) -> ClassPtr {
        self.method_class.clone()
    }
    /// Get the **Primitive** class.
    pub fn primitive_class(&self) -> ClassPtr {
        self.primitive_class.clone()
    }
}

impl<Class: Clone> CoreClasses<Class> {
    pub fn iter(&self) -> CoreClassesIter<'_, Class> {
        CoreClassesIter {
            fields: vec![
                ("Object", &self.object_class),
                ("Class", &self.class_class),
                ("Metaclass", &self.metaclass_class),
                ("Nil", &self.nil_class),
                ("Integer", &self.integer_class),
                ("Double", &self.double_class),
                ("Array", &self.array_class),
                ("Method", &self.method_class),
                ("Primitive", &self.primitive_class),
                ("Symbol", &self.symbol_class),
                ("String", &self.string_class),
                ("System", &self.system_class),
                ("Block", &self.block_class),
                ("Block1", &self.block1_class),
                ("Block2", &self.block2_class),
                ("Block3", &self.block3_class),
                ("Boolean", &self.boolean_class),
                ("True", &self.true_class),
                ("False", &self.false_class),
            ]
            .into_iter(),
        }
    }
}

pub struct CoreClassesIter<'a, ClassPtr> {
    fields: std::vec::IntoIter<(&'static str, &'a ClassPtr)>,
}

impl<'a, ClassPtr> Iterator for CoreClassesIter<'a, ClassPtr> {
    type Item = (&'static str, &'a ClassPtr);

    fn next(&mut self) -> Option<Self::Item> {
        self.fields.next()
    }
}
