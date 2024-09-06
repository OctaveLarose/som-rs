use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anyhow::{anyhow, Error};
use som_core::universe::UniverseForParser;
use som_gc::api::mmtk_destroy_mutator;
use som_gc::SOMVM;
use crate::block::Block;
use crate::class::Class;
use crate::compiler;
use crate::frame::Frame;
use crate::interner::{Interned, Interner};
use crate::interpreter::Interpreter;
use crate::value::Value;
use crate::SOMRef;

/// The core classes of the SOM interpreter.
///
/// This struct allows to always keep a reference to important classes,
/// even in case of modifications to global bindings by user-defined code.
#[derive(Debug)]
pub struct CoreClasses {
    /// The **Object** class.
    pub object_class: SOMRef<Class>,
    /// The **Class** class.
    pub class_class: SOMRef<Class>,
    /// The **Class** class.
    pub metaclass_class: SOMRef<Class>,

    /// The **Nil** class.
    pub nil_class: SOMRef<Class>,
    /// The **Integer** class.
    pub integer_class: SOMRef<Class>,
    /// The **Double** class.
    pub double_class: SOMRef<Class>,
    /// The **Array** class.
    pub array_class: SOMRef<Class>,
    /// The **Method** class.
    pub method_class: SOMRef<Class>,
    /// The **Primitive** class.
    pub primitive_class: SOMRef<Class>,
    /// The **Symbol** class.
    pub symbol_class: SOMRef<Class>,
    /// The **String** class.
    pub string_class: SOMRef<Class>,
    /// The **System** class.
    pub system_class: SOMRef<Class>,

    /// The **Block** class.
    pub block_class: SOMRef<Class>,
    /// The **Block1** class.
    pub block1_class: SOMRef<Class>,
    /// The **Block2** class.
    pub block2_class: SOMRef<Class>,
    /// The **Block3** class.
    pub block3_class: SOMRef<Class>,

    /// The **Boolean** class.
    pub boolean_class: SOMRef<Class>,
    /// The **True** class.
    pub true_class: SOMRef<Class>,
    /// The **False** class.
    pub false_class: SOMRef<Class>,
}

/// The central data structure for the interpreter.
///
/// It represents the complete state of the interpreter, like the known class definitions,
/// the string interner and the stack frames.
pub struct UniverseBC {
    /// The string interner for symbols.
    pub interner: Interner,
    /// The known global bindings.
    pub globals: HashMap<Interned, Value>,
    /// The path to search in for new classes.
    pub classpath: Vec<PathBuf>,
    /// The interpreter's core classes.
    pub core: CoreClasses,
    /// mutator thread for GC.
    pub mutator: Box<mmtk::Mutator<SOMVM>>
}

impl Drop for UniverseBC {
    fn drop(&mut self) {
        mmtk_destroy_mutator(self.mutator.as_mut())
    }
}

impl UniverseForParser for UniverseBC {
    fn load_class_and_get_all_fields(&mut self, class_name: &str) -> (Vec<String>, Vec<String>) {
        fn parse_and_get_field_names(universe: &mut UniverseBC, class_name: &str) -> (Vec<String>, Vec<String>) {
            let cls = universe.load_class(class_name).expect(&format!("Failed to parse class: {}", class_name));
            let instance_field_names = cls.borrow().locals.keys().map(|s| universe.interner.lookup(*s).to_string()).collect();
            let static_field_names = cls.borrow().class().borrow().locals.keys().map(|s| universe.interner.lookup(*s).to_string()).collect();
            (instance_field_names, static_field_names)
        }
        
        match self.interner.reverse_lookup(class_name) {
            None => {
                parse_and_get_field_names(self, class_name)
            }
            Some(interned) => {
                match self.lookup_global(interned) {
                    Some(Value::Class(cls)) => {
                        let instance_field_names = cls.borrow().locals.keys().map(|s| self.interner.lookup(*s).to_string()).collect();
                        let static_field_names = cls.borrow().class().borrow().locals.keys().map(|s| self.interner.lookup(*s).to_string()).collect();
                        (instance_field_names, static_field_names)
                    },
                    Some(val) => unreachable!("superclass accessed from parser is not actually a class, but {:?}", val),
                    None => {
                        // this case is weird: you have encountered the superclass name, but not parsed it as a global. 
                        // it can happen and i'm not convinced at all it's indicative of a design flaw. if it is, it's likely not a major one or one that could have an impact on performance
                        parse_and_get_field_names(self, class_name)
                    }
                }
            }
        }
    }
}

impl UniverseBC {
    /// Initialize the universe from the given classpath.
    pub fn with_classpath(classpath: Vec<PathBuf>, mutator: Box<mmtk::Mutator<SOMVM>>) -> Result<Self, Error> {
        let mut interner = Interner::with_capacity(100);
        let mut globals = HashMap::new();

        let object_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Object")?;
        let class_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Class")?;
        let metaclass_class =
            Self::load_system_class(&mut interner, classpath.as_slice(), "Metaclass")?;

        let nil_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Nil")?;
        let integer_class =
            Self::load_system_class(&mut interner, classpath.as_slice(), "Integer")?;
        let array_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Array")?;
        let method_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Method")?;
        let symbol_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Symbol")?;
        let primitive_class =
            Self::load_system_class(&mut interner, classpath.as_slice(), "Primitive")?;
        let string_class = Self::load_system_class(&mut interner, classpath.as_slice(), "String")?;
        let system_class = Self::load_system_class(&mut interner, classpath.as_slice(), "System")?;
        let double_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Double")?;

        let block_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Block")?;
        let block1_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Block1")?;
        let block2_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Block2")?;
        let block3_class = Self::load_system_class(&mut interner, classpath.as_slice(), "Block3")?;

        let boolean_class =
            Self::load_system_class(&mut interner, classpath.as_slice(), "Boolean")?;
        let true_class = Self::load_system_class(&mut interner, classpath.as_slice(), "True")?;
        let false_class = Self::load_system_class(&mut interner, classpath.as_slice(), "False")?;

        // initializeSystemClass(objectClass, null, "Object");
        // set_super_class(&object_class, &nil_class, &metaclass_class);
        object_class
            .borrow()
            .class()
            .borrow_mut()
            .set_class(&metaclass_class);
        object_class
            .borrow()
            .class()
            .borrow_mut()
            .set_super_class(&class_class);
        // initializeSystemClass(classClass, objectClass, "Class");
        set_super_class(&class_class, &object_class, &metaclass_class);
        // initializeSystemClass(metaclassClass, classClass, "Metaclass");
        set_super_class(&metaclass_class, &class_class, &metaclass_class);
        // initializeSystemClass(nilClass, objectClass, "Nil");
        set_super_class(&nil_class, &object_class, &metaclass_class);
        // initializeSystemClass(arrayClass, objectClass, "Array");
        set_super_class(&array_class, &object_class, &metaclass_class);
        // initializeSystemClass(methodClass, arrayClass, "Method");
        set_super_class(&method_class, &array_class, &metaclass_class);
        // initializeSystemClass(stringClass, objectClass, "String");
        set_super_class(&string_class, &object_class, &metaclass_class);
        // initializeSystemClass(symbolClass, stringClass, "Symbol");
        set_super_class(&symbol_class, &string_class, &metaclass_class);
        // initializeSystemClass(integerClass, objectClass, "Integer");
        set_super_class(&integer_class, &object_class, &metaclass_class);
        // initializeSystemClass(primitiveClass, objectClass, "Primitive");
        set_super_class(&primitive_class, &object_class, &metaclass_class);
        // initializeSystemClass(doubleClass, objectClass, "Double");
        set_super_class(&double_class, &object_class, &metaclass_class);

        set_super_class(&system_class, &object_class, &metaclass_class);

        set_super_class(&block_class, &object_class, &metaclass_class);
        set_super_class(&block1_class, &block_class, &metaclass_class);
        set_super_class(&block2_class, &block_class, &metaclass_class);
        set_super_class(&block3_class, &block_class, &metaclass_class);

        set_super_class(&boolean_class, &object_class, &metaclass_class);
        set_super_class(&true_class, &boolean_class, &metaclass_class);
        set_super_class(&false_class, &boolean_class, &metaclass_class);

        #[rustfmt::skip] {
            globals.insert(interner.intern("Object"), Value::Class(object_class.clone()));
            globals.insert(interner.intern("Class"), Value::Class(class_class.clone()));
            globals.insert(interner.intern("Metaclass"), Value::Class(metaclass_class.clone()));
            globals.insert(interner.intern("Nil"), Value::Class(nil_class.clone()));
            globals.insert(interner.intern("Integer"), Value::Class(integer_class.clone()));
            globals.insert(interner.intern("Array"), Value::Class(array_class.clone()));
            globals.insert(interner.intern("Method"), Value::Class(method_class.clone()));
            globals.insert(interner.intern("Symbol"), Value::Class(symbol_class.clone()));
            globals.insert(interner.intern("Primitive"), Value::Class(primitive_class.clone()));
            globals.insert(interner.intern("String"), Value::Class(string_class.clone()));
            globals.insert(interner.intern("System"), Value::Class(system_class.clone()));
            globals.insert(interner.intern("Double"), Value::Class(double_class.clone()));
            globals.insert(interner.intern("Boolean"), Value::Class(boolean_class.clone()));
            globals.insert(interner.intern("True"), Value::Class(true_class.clone()));
            globals.insert(interner.intern("False"), Value::Class(false_class.clone()));
            globals.insert(interner.intern("Block"), Value::Class(block_class.clone()));
            globals.insert(interner.intern("Block1"), Value::Class(block1_class.clone()));
            globals.insert(interner.intern("Block2"), Value::Class(block2_class.clone()));
            globals.insert(interner.intern("Block3"), Value::Class(block3_class.clone()));

            globals.insert(interner.intern("true"), Value::Boolean(true));
            globals.insert(interner.intern("false"), Value::Boolean(false));
            globals.insert(interner.intern("nil"), Value::Nil);
            globals.insert(interner.intern("system"), Value::System);
        };

        Ok(Self {
            globals,
            interner,
            classpath,
            core: CoreClasses {
                object_class,
                class_class,
                metaclass_class,
                nil_class,
                integer_class,
                array_class,
                method_class,
                symbol_class,
                primitive_class,
                string_class,
                system_class,
                double_class,
                block_class,
                block1_class,
                block2_class,
                block3_class,
                boolean_class,
                true_class,
                false_class,
            },
            mutator
        })
    }

    /// Load a class from its name into this universe.
    pub fn load_class(&mut self, class_name: impl Into<String>) -> Result<SOMRef<Class>, Error> {
        let class_name = class_name.into();
        let paths: Vec<PathBuf> = self.classpath.iter().map(|path| path.clone()).collect(); // ugly. see original code for how it should be done instead. TODO change - maybe a .map() call fixes it?

        for mut path in paths {
            path.push(class_name.as_str());
            path.set_extension("som");

            // Read file contents.
            let contents = match fs::read_to_string(path.as_path()) {
                Ok(contents) => contents,
                Err(_) => continue,
            };

            // Collect all tokens from the file.
            let tokens: Vec<_> = som_lexer::Lexer::new(contents.as_str())
                .skip_comments(true)
                .skip_whitespace(true)
                .collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file(tokens.as_slice(), self) {
                Some(defn) => defn,
                None => continue,
            };

            if defn.name != class_name {
                return Err(anyhow!(
                    "{}: class name is different from file name.",
                    path.display(),
                ));
            }

            let super_class = if let Some(ref super_class) = defn.super_class {
                let symbol = self.intern_symbol(super_class.as_str());
                match self.lookup_global(symbol) {
                    Some(Value::Class(super_class)) => super_class,
                    _ => self.load_class(super_class)?,
                }
            } else {
                self.core.object_class.clone()
            };

            let class = compiler::compile_class(&mut self.interner, &defn, Some(&super_class))
                .ok_or_else(|| Error::msg(format!("")))?;
            set_super_class(&class, &super_class, &self.core.metaclass_class);

            let symbol = self.intern_symbol(class.borrow().name());
            self.globals.insert(symbol, Value::Class(class.clone()));

            return Ok(class);
        }

        Err(anyhow!("could not find the '{}' class", class_name))
    }

    /// Load a system class (with an incomplete hierarchy).
    pub fn load_system_class(
        interner: &mut Interner,
        classpath: &[impl AsRef<Path>],
        class_name: impl Into<String>,
    ) -> Result<SOMRef<Class>, Error> {
        let class_name = class_name.into();
        for path in classpath {
            let mut path = path.as_ref().join(class_name.as_str());
            path.set_extension("som");

            // Read file contents.
            let contents = match fs::read_to_string(path.as_path()) {
                Ok(contents) => contents,
                Err(err) if err.kind() == io::ErrorKind::NotFound => continue,
                Err(err) => return Err(Error::from(err)),
            };

            // Collect all tokens from the file.
            let tokens: Vec<_> = som_lexer::Lexer::new(contents.as_str())
                .skip_comments(true)
                .skip_whitespace(true)
                .collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file_no_universe(tokens.as_slice()) {
                Some(defn) => defn,
                None => return Err(anyhow!("could not parse the '{}' system class", class_name)),
            };

            if defn.name != class_name {
                return Err(anyhow!(
                    "{}: class name is different from file name.",
                    path.display(),
                ));
            }
            let class = compiler::compile_class(interner, &defn, None)
                .ok_or_else(|| Error::msg(format!("")))?;

            return Ok(class);
        }

        Err(anyhow!("could not find the '{}' system class", class_name))
    }

    /// Get the **Nil** class.
    pub fn nil_class(&self) -> SOMRef<Class> {
        self.core.nil_class.clone()
    }
    /// Get the **System** class.
    pub fn system_class(&self) -> SOMRef<Class> {
        self.core.system_class.clone()
    }

    /// Get the **Object** class.
    pub fn object_class(&self) -> SOMRef<Class> {
        self.core.object_class.clone()
    }

    /// Get the **Symbol** class.
    pub fn symbol_class(&self) -> SOMRef<Class> {
        self.core.symbol_class.clone()
    }
    /// Get the **String** class.
    pub fn string_class(&self) -> SOMRef<Class> {
        self.core.string_class.clone()
    }
    /// Get the **Array** class.
    pub fn array_class(&self) -> SOMRef<Class> {
        self.core.array_class.clone()
    }

    /// Get the **Integer** class.
    pub fn integer_class(&self) -> SOMRef<Class> {
        self.core.integer_class.clone()
    }
    /// Get the **Double** class.
    pub fn double_class(&self) -> SOMRef<Class> {
        self.core.double_class.clone()
    }

    /// Get the **Block** class.
    pub fn block_class(&self) -> SOMRef<Class> {
        self.core.block_class.clone()
    }
    /// Get the **Block1** class.
    pub fn block1_class(&self) -> SOMRef<Class> {
        self.core.block1_class.clone()
    }
    /// Get the **Block2** class.
    pub fn block2_class(&self) -> SOMRef<Class> {
        self.core.block2_class.clone()
    }
    /// Get the **Block3** class.
    pub fn block3_class(&self) -> SOMRef<Class> {
        self.core.block3_class.clone()
    }

    /// Get the **True** class.
    pub fn true_class(&self) -> SOMRef<Class> {
        self.core.true_class.clone()
    }
    /// Get the **False** class.
    pub fn false_class(&self) -> SOMRef<Class> {
        self.core.false_class.clone()
    }

    /// Get the **Metaclass** class.
    pub fn metaclass_class(&self) -> SOMRef<Class> {
        self.core.metaclass_class.clone()
    }

    /// Get the **Method** class.
    pub fn method_class(&self) -> SOMRef<Class> {
        self.core.method_class.clone()
    }
    /// Get the **Primitive** class.
    pub fn primitive_class(&self) -> SOMRef<Class> {
        self.core.primitive_class.clone()
    }

    /// Intern a symbol.
    pub fn intern_symbol(&mut self, symbol: &str) -> Interned {
        self.interner.intern(symbol)
    }

    pub fn has_global(&self, idx: Interned) -> bool {
        self.globals.contains_key(&idx)
    }

    /// Lookup a symbol.
    pub fn lookup_symbol(&self, symbol: Interned) -> &str {
        self.interner.lookup(symbol)
    }

    /// Search for a global binding.
    pub fn lookup_global(&self, idx: Interned) -> Option<Value> {
        self.globals.get(&idx).cloned()
    }

    /// Assign a value to a global binding.
    pub fn assign_global(&mut self, name: Interned, value: Value) -> Option<()> {
        self.globals.insert(name, value)?;
        Some(())
    }
}

impl UniverseBC {
    /// Call `escapedBlock:` on the given value, if it is defined.
    pub fn escaped_block(
        &mut self,
        interpreter: &mut Interpreter,
        value: Value,
        block: Rc<Block>,
    ) -> Option<()> {
        let method_name = self.intern_symbol("escapedBlock:");
        let method = value.lookup_method(self, method_name)?;
        interpreter.push_method_frame(method, vec![value, Value::Block(block)]);
        Some(())
    }

    /// Call `doesNotUnderstand:` on the given value, if it is defined.
    #[allow(unreachable_code, unused_variables)]
    pub fn does_not_understand(
        &mut self,
        interpreter: &mut Interpreter,
        value: Value,
        symbol: Interned,
        args: Vec<Value>,
    ) -> Option<()> {
        // dbg!(&interpreter.stack);
        // panic!("does not understand: {:?}, called on {:?}", self.interner.lookup(symbol), &value);
        
        let method_name = self.intern_symbol("doesNotUnderstand:arguments:");
        let method = value.lookup_method(self, method_name)?;

        interpreter.push_method_frame(method, vec![value, Value::Symbol(symbol), Value::Array(Rc::new(RefCell::new(args)))]);

        Some(())
    }

    /// Call `unknownGlobal:` on the given value, if it is defined.
    pub fn unknown_global(
        &mut self,
        interpreter: &mut Interpreter,
        value: Value,
        name: Interned,
    ) -> Option<()> {
        let method_name = self.intern_symbol("unknownGlobal:");
        let method = value.lookup_method(self, method_name)?;

        interpreter.current_frame.borrow_mut().bytecode_idx = interpreter.bytecode_idx;
        interpreter.push_method_frame(method, vec![value, Value::Symbol(name)]);

        Some(())
    }

    /// Call `System>>#initialize:` with the given name, if it is defined.
    pub fn initialize(&mut self, args: Vec<Value>) -> Option<Interpreter> {
        let method_name = self.interner.intern("initialize:");
        let method = Value::System.lookup_method(self, method_name)?;


        let frame = Rc::new(RefCell::new(Frame::from_method(method, vec![Value::System, Value::Array(Rc::new(RefCell::new(args)))])));
        let interpreter = Interpreter::new(Rc::clone(&frame));

        Some(interpreter)
    }
}

fn set_super_class(
    class: &SOMRef<Class>,
    super_class: &SOMRef<Class>,
    metaclass_class: &SOMRef<Class>,
) {
    class.borrow_mut().set_super_class(super_class);
    class
        .borrow()
        .class()
        .borrow_mut()
        .set_super_class(&super_class.borrow().class());
    class
        .borrow()
        .class()
        .borrow_mut()
        .set_class(metaclass_class);
}
