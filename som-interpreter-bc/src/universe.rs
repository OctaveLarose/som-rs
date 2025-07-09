use crate::compiler::compile::compile_class;
use crate::gc::{get_callbacks_for_gc, VecValue};
use crate::interpreter::Interpreter;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::instance::Instance;
use anyhow::{anyhow, Error};
use som_core::core_classes::CoreClasses;
use som_core::interner::Interner;
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};
use som_gc::gcref::Gc;
use som_value::interned::Interned;
use std::fs;
use std::io;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};

/// GC default heap size
pub const DEFAULT_HEAP_SIZE: usize = 1024 * 1024 * 256;

/// The central data structure for the interpreter.
///
/// It represents the complete state of the interpreter, like the known class definitions,
/// the string interner and the stack frames.
pub struct Universe {
    /// The string interner for symbols.
    pub interner: Interner,
    /// The known global bindings.
    // pub globals: HashMap<Interned, Value>,
    pub globals: Vec<(Interned, Value)>,
    /// The path to search in for new classes.
    pub classpath: Vec<PathBuf>,
    /// The interpreter's core classes.
    pub core: CoreClasses<Gc<Class>>,
    /// GC interface for GC operations
    pub gc_interface: &'static mut GCInterface,
}

impl Universe {
    /// Initialize the universe from the given classpath (and default heap size).
    pub fn with_classpath(classpath: Vec<PathBuf>) -> Result<Self, Error> {
        Self::with_classpath_and_heap_size(classpath, DEFAULT_HEAP_SIZE)
    }

    /// Initialize the universe from the given classpath and heap size.
    pub fn with_classpath_and_heap_size(classpath: Vec<PathBuf>, heap_size: usize) -> Result<Self, Error> {
        let mut interner = Interner::with_capacity(200);
        let mut globals = vec![];

        let gc_interface = GCInterface::init(heap_size, get_callbacks_for_gc());

        // TODO: really, we should take and set the superclass, like the AST does.
        let mut core: CoreClasses<Gc<Class>> = CoreClasses::from_load_cls_fn(|name: &str, _super_cls: Option<&Gc<Class>>| {
            Self::load_system_class(&mut interner, classpath.as_slice(), name, gc_interface).unwrap()
        });

        core.object_class.class().set_class(&core.metaclass_class);
        core.object_class.class().set_super_class(&core.class_class);
        set_super_class(&mut core.class_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.metaclass_class.clone(), &core.class_class, &core.metaclass_class);
        set_super_class(&mut core.nil_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.array_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.method_class, &core.array_class, &core.metaclass_class);
        set_super_class(&mut core.string_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.symbol_class, &core.string_class, &core.metaclass_class);
        set_super_class(&mut core.integer_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.primitive_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.double_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.system_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.block_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.block1_class, &core.block_class, &core.metaclass_class);
        set_super_class(&mut core.block2_class, &core.block_class, &core.metaclass_class);
        set_super_class(&mut core.block3_class, &core.block_class, &core.metaclass_class);
        set_super_class(&mut core.boolean_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.true_class, &core.boolean_class, &core.metaclass_class);
        set_super_class(&mut core.false_class, &core.boolean_class, &core.metaclass_class);

        for (cls_name, global_cls) in core.iter() {
            globals.push((interner.intern(cls_name), Value::Class(global_cls.clone())));
        }

        globals.push((interner.intern("true"), Value::Boolean(true)));
        globals.push((interner.intern("false"), Value::Boolean(false)));
        globals.push((interner.intern("nil"), Value::NIL));

        let system_instance = Value::Instance(gc_interface.alloc(
            Instance {
                class: core.system_class(),
                fields_marker: PhantomData,
            },
            AllocSiteMarker::Instance,
        ));
        globals.push((interner.intern("system"), system_instance));

        Ok(Self {
            globals,
            interner,
            classpath,
            core,
            gc_interface,
        })
    }

    /// Load a class from its name into this universe.
    pub fn load_class(&mut self, class_name: impl Into<String>) -> Result<Gc<Class>, Error> {
        let class_name = class_name.into();

        for path in self.classpath.iter() {
            let mut path = path.join(class_name.as_str());
            path.set_extension("som");

            // Read file contents.
            let contents = match fs::read_to_string(path.as_path()) {
                Ok(contents) => contents,
                Err(_) => continue,
            };

            // Collect all tokens from the file.
            let tokens: Vec<_> = som_lexer::Lexer::new(contents.as_str()).skip_comments(true).skip_whitespace(true).collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file(tokens.as_slice()) {
                Some(defn) => defn,
                None => continue,
            };

            if defn.name != class_name {
                return Err(anyhow!("{}: class name is different from file name.", path.display(),));
            }

            let super_class = if let Some(ref super_class) = defn.super_class {
                let symbol = self.intern_symbol(super_class.as_str());
                match self.lookup_global(symbol) {
                    v if v.is_some() && v.unwrap().is_value_ptr::<Class>() => v.unwrap().as_class().unwrap(),
                    _ => self.load_class(super_class)?,
                }
            } else {
                self.core.object_class.clone()
            };

            let mut class =
                compile_class(&mut self.interner, &defn, Some(&super_class), self.gc_interface).ok_or_else(|| Error::msg(String::new()))?;
            set_super_class(&mut class, &super_class, &self.core.metaclass_class);

            let symbol = self.intern_symbol(class.name());
            self.globals.push((symbol, Value::Class(class.clone())));

            return Ok(class);
        }

        Err(anyhow!("could not find the '{}' class", class_name))
    }

    /// Load a system class (with an incomplete hierarchy).
    pub fn load_system_class(
        interner: &mut Interner,
        classpath: &[impl AsRef<Path>],
        class_name: impl Into<String>,
        allocator: &mut GCInterface,
    ) -> Result<Gc<Class>, Error> {
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
            let tokens: Vec<_> = som_lexer::Lexer::new(contents.as_str()).skip_comments(true).skip_whitespace(true).collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file_no_universe(tokens.as_slice()) {
                Some(defn) => defn,
                None => return Err(anyhow!("could not parse the '{}' system class", class_name)),
            };

            if defn.name != class_name {
                return Err(anyhow!("{}: class name is different from file name.", path.display(),));
            }
            let class = compile_class(interner, &defn, None, allocator).ok_or_else(|| Error::msg(String::new()))?;

            return Ok(class);
        }

        Err(anyhow!("could not find the '{}' system class", class_name))
    }

    /// Intern a symbol.
    pub fn intern_symbol(&mut self, symbol: &str) -> Interned {
        self.interner.intern(symbol)
    }

    pub fn has_global(&self, idx: Interned) -> bool {
        self.globals.iter().any(|(interned, _)| *interned == idx)
    }

    /// Lookup a symbol.
    pub fn lookup_symbol(&self, symbol: Interned) -> &str {
        self.interner.lookup(symbol)
    }

    /// Search for a global binding.
    pub fn lookup_global(&self, idx: Interned) -> Option<Value> {
        self.globals.iter().find(|(interned, _)| *interned == idx).map(|(_, value)| *value)
    }

    /// Assign a value to a global binding.
    pub fn assign_global(&mut self, name: Interned, value: Value) {
        self.globals.push((name, value));
    }
}

impl Universe {
    /// Call `escapedBlock:` on the given value, if it is defined.
    pub fn escaped_block(&mut self, interpreter: &mut Interpreter, value: Value, block: Gc<Block>) -> Option<()> {
        let method_name = self.intern_symbol("escapedBlock:");
        let method = value.lookup_method(self, method_name)?;
        interpreter.push_method_frame_with_args(method, vec![value, Value::Block(block)], self.gc_interface);
        Some(())
    }

    /// Call `doesNotUnderstand:` on the given value, if it is defined.
    #[allow(unreachable_code, unused_variables)]
    pub fn does_not_understand(&mut self, interpreter: &mut Interpreter, value: Value, symbol: Interned, args: Vec<Value>) -> Option<()> {
        // dbg!(&interpreter.stack);
        // panic!("does not understand: {:?}, called on {:?}", self.interner.lookup(symbol), &value);

        let method_name = self.intern_symbol("doesNotUnderstand:arguments:");
        let method = value.lookup_method(self, method_name)?;

        // #[cfg(debug_assertions)]
        // {
        //     let stack_trace_fn = crate::primitives::system::get_instance_primitive("printStackTrace")?;
        //     stack_trace_fn(interpreter, self).expect("couldn't print stack trace");
        //     std::process::exit(1);
        // }

        interpreter.push_method_frame_with_args(
            method,
            vec![
                value,
                Value::Symbol(symbol),
                Value::Array(VecValue(self.gc_interface.alloc_slice(&args, AllocSiteMarker::VecValue))),
            ],
            self.gc_interface,
        );

        Some(())
    }

    /// Call `unknownGlobal:` on the given value, if it is defined.
    pub fn unknown_global(&mut self, interpreter: &mut Interpreter, value: Value, name: Interned) -> Option<()> {
        let method_name = self.intern_symbol("unknownGlobal:");
        let method = value.lookup_method(self, method_name)?;

        interpreter.get_current_frame().bytecode_idx = interpreter.bytecode_idx;
        interpreter.push_method_frame_with_args(method, vec![value, Value::Symbol(name)], self.gc_interface);

        Some(())
    }

    /// Call `System>>#initialize:` with the given name, if it is defined.
    pub fn initialize(&mut self, args: Vec<Value>) -> Option<Interpreter> {
        let method_name = self.interner.intern("initialize:");
        let initialize = self.core.system_class().lookup_method(method_name)?;
        let system_value = self.lookup_global(self.interner.reverse_lookup("system")?)?;

        let args_vec = VecValue(self.gc_interface.alloc_slice(&args, AllocSiteMarker::VecValue));
        let frame_ptr = Frame::alloc_initial_method(initialize, &[system_value, Value::Array(args_vec)], self.gc_interface);
        let interpreter = Interpreter::new(frame_ptr);

        Some(interpreter)
    }
}

fn set_super_class(class: &mut Gc<Class>, super_class: &Gc<Class>, metaclass_class: &Gc<Class>) {
    class.set_super_class(super_class);
    class.class().set_super_class(&super_class.class());
    class.class().set_class(metaclass_class);
}
