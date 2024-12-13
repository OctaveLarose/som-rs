use std::collections::HashMap;
use std::fs;
use std::io;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::slice::Iter;
use std::time::Instant;

use crate::evaluate::Evaluate;
use crate::gc::{get_callbacks_for_gc, VecValue};
use crate::invokable::{Invoke, Return};
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::{Frame, FrameAccess};
use anyhow::{anyhow, Error};
use som_core::core_classes::CoreClasses;
use som_core::interner::{Interned, Interner};
use som_gc::gc_interface::GCInterface;
use som_gc::gcref::Gc;
use som_gc::{debug_assert_valid_semispace_ptr, debug_assert_valid_semispace_ptr_value};

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
    pub globals: HashMap<Interned, Value>,
    /// The path to search in for new classes.
    pub classpath: Vec<PathBuf>,
    /// The current frame for the operation
    pub current_frame: Gc<Frame>,
    /// The interpreter's core classes.
    pub core: CoreClasses<Gc<Class>>,
    /// The time record of the universe's creation.
    pub start_time: Instant,
    /// GC interface
    pub gc_interface: &'static mut GCInterface,
}

impl Drop for Universe {
    fn drop(&mut self) {
        let _box: Box<GCInterface> = unsafe { Box::from_raw(self.gc_interface) };
        drop(_box)
    }
}

impl Universe {
    /// Initialize the universe from the given classpath.
    pub fn with_classpath(classpath: Vec<PathBuf>) -> Result<Self, Error> {
        Self::with_classpath_and_heap_size(classpath, DEFAULT_HEAP_SIZE)
    }

    /// Initialize the universe from the given classpath, and given a heap size
    pub fn with_classpath_and_heap_size(classpath: Vec<PathBuf>, heap_size: usize) -> Result<Self, Error> {
        let mut interner = Interner::with_capacity(200);
        let mut globals: HashMap<Interned, Value> = HashMap::new();

        let gc_interface = GCInterface::init(heap_size, get_callbacks_for_gc());

        let mut core: CoreClasses<Gc<Class>> = CoreClasses::from_load_cls_fn(|name: &str, super_cls: Option<Gc<Class>>| {
            Self::load_system_class(classpath.as_slice(), name, super_cls, gc_interface, &mut interner).unwrap()
        });

        // TODO: these can be removed for the most part - in the AST at least, we set a lot of super class relationships when loading system classes directly.
        core.object_class.class().set_class(&core.metaclass_class);
        core.object_class.class().set_super_class(&core.class_class);

        set_super_class(&mut core.class_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.metaclass_class.clone(), &core.class_class, &core.metaclass_class);
        // initializeSystemClass(nilClass, objectClass, "Nil");
        set_super_class(&mut core.nil_class, &core.object_class, &core.metaclass_class);
        // initializeSystemClass(arrayClass, objectClass, "Array");
        set_super_class(&mut core.array_class, &core.object_class, &core.metaclass_class);
        // initializeSystemClass(methodClass, arrayClass, "Method");
        set_super_class(&mut core.method_class, &core.array_class, &core.metaclass_class);
        // initializeSystemClass(stringClass, objectClass, "String");
        set_super_class(&mut core.string_class, &core.object_class, &core.metaclass_class);
        // initializeSystemClass(symbolClass, stringClass, "Symbol");
        set_super_class(&mut core.symbol_class, &core.string_class, &core.metaclass_class);
        // initializeSystemClass(integerClass, objectClass, "Integer");
        set_super_class(&mut core.integer_class, &core.object_class, &core.metaclass_class);
        // initializeSystemClass(primitiveClass, objectClass, "Primitive");
        set_super_class(&mut core.primitive_class, &core.object_class, &core.metaclass_class);
        // initializeSystemClass(doubleClass, objectClass, "Double");
        set_super_class(&mut core.double_class, &core.object_class, &core.metaclass_class);

        set_super_class(&mut core.system_class, &core.object_class, &core.metaclass_class);

        set_super_class(&mut core.block_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.block1_class, &core.block_class, &core.metaclass_class);
        set_super_class(&mut core.block2_class, &core.block_class, &core.metaclass_class);
        set_super_class(&mut core.block3_class, &core.block_class, &core.metaclass_class);

        set_super_class(&mut core.boolean_class, &core.object_class, &core.metaclass_class);
        set_super_class(&mut core.true_class, &core.boolean_class, &core.metaclass_class);
        set_super_class(&mut core.false_class, &core.boolean_class, &core.metaclass_class);

        for (cls_name, core_cls) in core.iter() {
            globals.insert(interner.intern(cls_name), Value::Class(*core_cls));
        }

        globals.insert(interner.intern("true"), Value::Boolean(true));
        globals.insert(interner.intern("false"), Value::Boolean(false));
        globals.insert(interner.intern("nil"), Value::NIL);
        globals.insert(interner.intern("system"), Value::SYSTEM);

        Ok(Self {
            globals,
            interner,
            classpath,
            current_frame: Gc::default(),
            start_time: Instant::now(),
            core,
            gc_interface,
        })
    }

    /// Load a class from its name into this universe.
    pub fn load_class(&mut self, class_name: impl Into<String>) -> Result<Gc<Class>, Error> {
        let class_name = class_name.into();

        for path in &self.classpath {
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
                self.lookup_global(symbol).and_then(Value::as_class).unwrap_or_else(|| self.load_class(super_class).unwrap())
            } else {
                self.core.object_class
            };

            let mut class = Class::from_class_def(defn, Some(super_class), self.gc_interface, &mut self.interner).map_err(Error::msg)?;
            set_super_class(&mut class, &super_class, &self.core.metaclass_class);

            let symbol = self.intern_symbol(class.name());
            self.globals.insert(symbol, Value::Class(class));

            return Ok(class);
        }

        Err(anyhow!("could not find the '{}' class", class_name))
    }

    /// Load a system class (with an incomplete hierarchy).
    pub fn load_system_class(
        classpath: &[impl AsRef<Path>],
        class_name: impl Into<String>,
        super_class: Option<Gc<Class>>,
        gc_interface: &mut GCInterface,
        interner: &mut Interner,
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

            return Class::from_class_def(defn, super_class, gc_interface, interner).map_err(Error::msg);
        }

        Err(anyhow!("could not find the '{}' system class", class_name))
    }
}

impl Universe {
    /// Evaluates a method or other after pushing a new frame onto the stack.
    /// The frame assumes the arguments it needs are on the global argument stack.
    pub fn eval_with_frame<T: Evaluate>(&mut self, value_stack: &mut GlobalValueStack, nbr_locals: u8, nbr_args: usize, invokable: &mut T) -> Return {
        let frame = Frame::alloc_new_frame(nbr_locals, nbr_args, self, value_stack);
        frame.debug_check_frame_addresses();
        self.current_frame = frame;
        let ret = invokable.evaluate(self, value_stack);
        self.current_frame = self.current_frame.prev_frame;
        ret
    }

    /// Evaluates a block after pushing a new block frame.
    pub fn eval_block_with_frame(&mut self, value_stack: &mut GlobalValueStack, nbr_locals: u8, nbr_args: usize) -> Return {
        let frame = Frame::alloc_new_frame(nbr_locals, nbr_args, self, value_stack);
        self.current_frame = frame;
        debug_assert_valid_semispace_ptr!(self.current_frame);
        let mut invokable = frame.lookup_argument(0).as_block().unwrap();
        debug_assert_valid_semispace_ptr!(invokable);
        debug_assert_valid_semispace_ptr!(invokable.block);
        let ret = invokable.evaluate(self, value_stack);
        self.current_frame = self.current_frame.prev_frame;
        ret
    }

    /// Intern a symbol.
    pub fn intern_symbol(&mut self, symbol: &str) -> Interned {
        self.interner.intern(symbol)
    }

    /// Lookup a symbol.
    pub fn lookup_symbol(&self, symbol: Interned) -> &str {
        self.interner.lookup(symbol)
    }

    /// Returns whether a global binding of the specified name exists.
    pub fn has_global(&self, name: Interned) -> bool {
        self.globals.contains_key(&name)
    }

    /// Search for a global binding.
    pub fn lookup_global(&self, name: Interned) -> Option<Value> {
        self.globals.get(&name).cloned()
    }

    /// Assign a value to a global binding.
    pub fn assign_global(&mut self, name: Interned, value: &Value) -> Option<()> {
        self.globals.insert(name, *value).map(|_| ())
    }
}

#[repr(transparent)] // probably not needed but might as well make it explicit to the compiler
#[derive(Debug)]
pub struct GlobalValueStack(Vec<Value>);

impl From<Vec<Value>> for GlobalValueStack {
    fn from(value: Vec<Value>) -> Self {
        Self(value)
    }
}

impl Deref for GlobalValueStack {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GlobalValueStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl GlobalValueStack {
    pub fn push(&mut self, value: Value) {
        debug_assert_valid_semispace_ptr_value!(value);
        self.0.push(value);
    }
    /// Remove N elements off the argument stack and return them as their own vector.
    /// The default way of getting elements off of the stack.
    pub fn stack_n_last_elems(&mut self, n: usize) -> Vec<Value> {
        let idx_split_off = self.0.len() - n;
        self.0.split_off(idx_split_off)
    }

    pub fn stack_borrow_n_last_elems(&self, n: usize) -> &[Value] {
        let idx_split_off = self.0.len() - n;
        &self.0.as_slice()[idx_split_off..]
    }

    /// Pop the last n elements of the stack.
    pub fn stack_pop_n(&mut self, n: usize) {
        let new_len = self.0.len() - n;
        self.0.truncate(new_len)
    }

    pub fn iter(&self) -> Iter<'_, Value> {
        self.0.iter()
    }
}

impl Universe {
    /// Call `escapedBlock:` on the given value, if it is defined.
    pub fn escaped_block(&mut self, value_stack: &mut GlobalValueStack, value: Value, block: Gc<Block>) -> Option<Return> {
        let method_name = self.intern_symbol("escapedBlock:");
        let mut initialize = value.lookup_method(self, method_name)?;

        value_stack.push(value);
        value_stack.push(Value::Block(block));
        let escaped_block_result = initialize.invoke(self, value_stack, 2);
        Some(escaped_block_result)
    }

    /// Call `doesNotUnderstand:` on the given value, if it is defined.
    pub fn does_not_understand(&mut self, value_stack: &mut GlobalValueStack, value: Value, sym: Interned, args: Vec<Value>) -> Option<Return> {
        let method_name = self.intern_symbol("doesNotUnderstand:arguments:");
        let mut initialize = value.lookup_method(self, method_name)?;
        let sym = Value::Symbol(sym);
        let args = Value::Array(self.gc_interface.alloc(VecValue(args)));

        // eprintln!("Couldn't invoke {}; exiting.", symbol.as_ref()); std::process::exit(1);

        value_stack.push(value);
        value_stack.push(sym);
        value_stack.push(args);

        let dnu_result = initialize.invoke(self, value_stack, 3);
        Some(dnu_result)
    }

    /// Call `unknownGlobal:` on the given value, if it is defined.
    pub fn unknown_global(&mut self, value_stack: &mut GlobalValueStack, value: Value, sym: Interned) -> Option<Return> {
        let method_name = self.intern_symbol("unknownGlobal:");
        let mut method = value.lookup_method(self, method_name)?;

        value_stack.push(value);
        value_stack.push(Value::Symbol(sym));

        let unknown_global_result = method.invoke(self, value_stack, 2);
        match unknown_global_result {
            Return::Local(value) | Return::NonLocal(value, _) => Some(Return::Local(value)),
            #[cfg(feature = "inlining-disabled")]
            Return::Restart => panic!("(from 'System>>#unknownGlobal:') incorrectly asked for a restart"),
        }
    }

    /// Call `System>>#initialize:` with the given name, if it is defined.
    pub fn initialize(&mut self, args: Vec<Value>, value_stack: &mut GlobalValueStack) -> Option<Return> {
        let method_name = self.interner.intern("initialize:");
        let mut initialize = Value::SYSTEM.lookup_method(self, method_name)?;
        let args = Value::Array(self.gc_interface.alloc(VecValue(args)));
        value_stack.push(Value::SYSTEM);
        value_stack.push(args);
        let program_result = initialize.invoke(self, value_stack, 2);
        Some(program_result)
    }
}

fn set_super_class(class: &mut Gc<Class>, super_class: &Gc<Class>, metaclass_class: &Gc<Class>) {
    class.set_super_class(super_class);

    class.class().set_super_class(&super_class.class());
    class.class().set_class(metaclass_class);
}
