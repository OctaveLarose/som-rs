use crate::compiler::Literal;
use crate::gc::{visit_literal, visit_value, GcIdentifier};
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::class::Class;
use som_core::bytecode::{BcEntry, Bytecode};
use som_gc::gc_interface::GcType;
use som_gc::slot::SOMSlot;
use std::fmt;
use std::fmt::{Debug, Formatter};

use som_gc::gcref::Gc;

use crate::vm_objects::block::BodyInlineCache;
use crate::vm_objects::trivial_methods::{TrivialGetterMethod, TrivialGlobalMethod, TrivialLiteralMethod, TrivialSetterMethod};

use super::block::CacheEntry;

/// The minimum for every kind of method: a signature and a holder.
#[derive(Debug, Clone)]
pub struct BasicMethodInfo {
    pub signature: String,
    pub holder: Gc<Class>,
}

impl BasicMethodInfo {
    pub fn new(signature: String, holder: Gc<Class>) -> Self {
        Self { signature, holder }
    }
}

/// Data for a method, or a block.
// TODO: not a fan of the name. But this *is* info/metadata about each method... Still, a rename would be nice.
#[derive(Clone)]
pub struct MethodInfo {
    pub basic_method_info: BasicMethodInfo,
    pub literals: Vec<Literal>,
    pub body: Vec<u8>,
    pub inline_cache: BodyInlineCache,
    pub nbr_locals: u8,
    pub nbr_args: u8,
}

/// Represents a class method.
#[derive(Clone)]
// repr(C) to support a Gc<MethodInfo> variant. I remember Rust doing optimizations way back when that could essentially conflate a Gc<Method> with a Gc<MethodInfo> in memory?
#[repr(C)]
pub enum Method {
    /// A user-defined method from the AST.
    Defined(Gc<MethodInfo>),
    /// An interpreter primitive.
    Primitive(&'static PrimitiveFn, BasicMethodInfo),
    /// A trivial literal read
    TrivialLiteral(TrivialLiteralMethod, BasicMethodInfo),
    /// A trivial global read
    TrivialGlobal(TrivialGlobalMethod, BasicMethodInfo),
    /// A trivial getter method
    TrivialGetter(TrivialGetterMethod, BasicMethodInfo),
    /// A trivial setter method
    TrivialSetter(TrivialSetterMethod, BasicMethodInfo),
}

impl Debug for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Method debug (TODO)")
    }
}

impl Method {
    /// Whether this invokable is a primitive.
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(..))
    }

    pub fn holder(&self) -> &Gc<Class> {
        match &self {
            Method::Defined(env) => &env.basic_method_info.holder,
            Method::Primitive(_, met_info)
            | Method::TrivialGlobal(_, met_info)
            | Method::TrivialGetter(_, met_info)
            | Method::TrivialSetter(_, met_info)
            | Method::TrivialLiteral(_, met_info) => &met_info.holder,
        }
    }

    /// Used during initialization.
    pub fn set_holder(&mut self, holder_ptr: &Gc<Class>) {
        match self {
            Method::Defined(env) => {
                env.basic_method_info.holder = holder_ptr.clone();
                for lit in &mut env.literals {
                    if let Literal::Block(blk) = lit {
                        blk.basic_method_info.holder = holder_ptr.clone();
                    }
                }
            }
            Method::Primitive(_, met_info)
            | Method::TrivialGlobal(_, met_info)
            | Method::TrivialLiteral(_, met_info)
            | Method::TrivialGetter(_, met_info)
            | Method::TrivialSetter(_, met_info) => met_info.holder = holder_ptr.clone(),
        }
    }

    pub fn as_method_info(&self) -> Gc<MethodInfo> {
        match self {
            Method::Defined(env) => env.clone(),
            _ => panic!("requesting method metadata from primitive/trivial method"),
        }
    }

    #[cfg(feature = "frame-debug-info")]
    pub fn get_block_debug_info(&self) -> &BlockDebugInfo {
        match &self {
            Method::Defined(env) => &env.block_debug_info,
            _ => panic!("requesting debug block info from primitive/trivial method"),
        }
    }
}

impl Method {
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        if self.is_primitive() {
            universe.core.primitive_class().clone()
        } else {
            universe.core.method_class().clone()
        }
    }

    pub fn signature(&self) -> &str {
        match &self {
            Method::Defined(gc) => &gc.basic_method_info.signature,
            Method::Primitive(_, met_info)
            | Method::TrivialGlobal(_, met_info)
            | Method::TrivialLiteral(_, met_info)
            | Method::TrivialGetter(_, met_info)
            | Method::TrivialSetter(_, met_info) => met_info.signature.as_str(),
        }
    }
}

pub trait Invoke {
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe, receiver: Value, args: Vec<Value>);
}

impl Invoke for Gc<Method> {
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe, receiver: Value, mut args: Vec<Value>) {
        match &**self {
            Method::Defined(method_info) => {
                let mut frame_args = vec![receiver];
                frame_args.append(&mut args);
                interpreter.push_method_frame_with_args(method_info.clone(), frame_args, &mut universe.gc_interface);
            }
            Method::Primitive(func, ..) => {
                let nbr_args = args.len() + 1;
                interpreter.stack.push(receiver);
                for arg in args {
                    interpreter.stack.push(arg)
                }
                func(interpreter, universe, nbr_args).unwrap_or_else(|_| panic!("invoking func {} failed", &self.signature()));
            }
            Method::TrivialGlobal(met, _) => met.invoke(universe, interpreter),
            Method::TrivialLiteral(met, _) => met.invoke(universe, interpreter),
            Method::TrivialGetter(met, _) => met.invoke(universe, interpreter),
            Method::TrivialSetter(met, _) => met.invoke(universe, interpreter),
        }
    }
}

impl GcType for MethodInfo {
    fn get_magic_gc_id() -> u8 {
        GcIdentifier::MethodInfo as u8
    }

    fn scan_object(method: Gc<Self>, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        visit_slot_fn(SOMSlot::from(&method.basic_method_info.holder));

        for cache_entry in method.inline_cache.iter().flatten() {
            match cache_entry {
                CacheEntry::Send(cls_ptr, method_ptr) => {
                    visit_slot_fn(SOMSlot::from(cls_ptr));
                    visit_slot_fn(SOMSlot::from(method_ptr));
                }
                CacheEntry::Global(val) => {
                    visit_value(val, visit_slot_fn);
                }
            }
        }

        for lit in &method.literals {
            visit_literal(lit, visit_slot_fn)
        }
    }

    fn get_size_in_memory(_self: Gc<Self>) -> usize {
        size_of::<MethodInfo>()
    }
}

impl GcType for Method {
    fn get_magic_gc_id() -> u8 {
        GcIdentifier::Method as u8
    }

    fn scan_object(method: Gc<Self>, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        match &*method {
            Method::Defined(method_info) => {
                visit_slot_fn(SOMSlot::from(method_info));
            }
            Method::Primitive(_, met_info)
            | Method::TrivialGlobal(_, met_info)
            | Method::TrivialGetter(_, met_info)
            | Method::TrivialSetter(_, met_info) => {
                visit_slot_fn(SOMSlot::from(&met_info.holder));
            }
            Method::TrivialLiteral(trivial_lit, met_info) => {
                visit_literal(&trivial_lit.literal, visit_slot_fn);
                visit_slot_fn(SOMSlot::from(&met_info.holder));
            }
        }
    }

    fn get_size_in_memory(_self: Gc<Self>) -> usize {
        size_of::<Method>()
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}>>#{} = ", self.holder().name(), self.signature())?;
        match &self {
            Method::Defined(env) => {
                writeln!(f, "(")?;
                write!(f, "    <{} locals>", env.nbr_locals)?;
                // TODO: unify with disassembler logic?
                let bc_iter = Bytecode::get_iter(&env.body);
                for bytecode in bc_iter {
                    writeln!(f)?;

                    match bytecode {
                        BcEntry::NoArg(bc) | BcEntry::OneArg(bc, _) | BcEntry::TwoArgs(bc, _, _) | BcEntry::U16Arg(bc, _) => {
                            write!(f, "    {}  ", Bytecode::padded_name(bc))?;
                        }
                    };

                    match bytecode {
                        BcEntry::NoArg(_) => {}
                        BcEntry::OneArg(Bytecode::PUSH_LOCAL, idx) => {
                            write!(f, "local: {}", idx)?;
                        }
                        BcEntry::TwoArgs(Bytecode::PUSH_NON_LOCAL, up_idx, idx) => {
                            write!(f, "local: {}, context: {}", idx, up_idx)?;
                        }
                        BcEntry::OneArg(Bytecode::PUSH_ARG, idx) => {
                            write!(f, "argument: {}", idx)?;
                        }
                        BcEntry::TwoArgs(Bytecode::PUSH_NON_LOCAL_ARG, up_idx, idx) => {
                            write!(f, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        BcEntry::OneArg(Bytecode::PUSH_FIELD, idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        BcEntry::OneArg(Bytecode::PUSH_BLOCK, idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        BcEntry::OneArg(Bytecode::PUSH_CONSTANT, idx) => {
                            write!(f, "index: {}, ", idx)?;
                            let constant = &env.literals[idx as usize];
                            match constant {
                                Literal::Symbol(_) => write!(f, "value: (#Symbol)"),
                                Literal::String(value) => write!(f, "value: (#String) {:?}", value),
                                Literal::Double(value) => write!(f, "value: (#Double) {}", value),
                                Literal::Integer(value) => write!(f, "value: (#Integer) {}", value),
                                Literal::BigInteger(value) => {
                                    write!(f, "value: (#Integer) {}", **value)
                                }
                                Literal::Array(_) => write!(f, "value: (#Array)"),
                                Literal::Block(_) => write!(f, "value: (#Block)"),
                            }?;
                        }
                        BcEntry::OneArg(Bytecode::PUSH_GLOBAL, idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        BcEntry::TwoArgs(Bytecode::POP_LOCAL, up_idx, idx) => {
                            write!(f, "local: {}, context: {}", idx, up_idx)?;
                        }
                        BcEntry::TwoArgs(Bytecode::POP_ARG, up_idx, idx) => {
                            write!(f, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        BcEntry::OneArg(Bytecode::POP_FIELD, idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        BcEntry::U16Arg(_, idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        a => panic!("Cannot print unhandled bytecode: {:?}", a),
                    }
                }
                Ok(())
            }
            Method::Primitive(..) => write!(f, "<primitive>"),
            Method::TrivialGlobal(..) => write!(f, "TrivialGlobal"),
            Method::TrivialLiteral(..) => write!(f, "TrivialLiteral"),
            Method::TrivialGetter(..) => write!(f, "TrivialGetter"),
            Method::TrivialSetter(..) => write!(f, "TrivialSetter"),
        }
    }
}
