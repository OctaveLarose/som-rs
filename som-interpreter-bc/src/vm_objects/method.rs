use std::fmt;

use som_core::bytecode::Bytecode;

use crate::compiler::Literal;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::class::Class;

#[cfg(feature = "frame-debug-info")]
use som_core::ast::BlockDebugInfo;
use som_gc::gcref::Gc;

use crate::vm_objects::block::BodyInlineCache;

/// Data for a method, or a block.
#[derive(Clone)]
pub struct MethodInfo {
    pub signature: String,
    pub holder: Gc<Class>,
    pub literals: Vec<Literal>,
    pub body: Vec<Bytecode>,
    pub inline_cache: BodyInlineCache,
    pub nbr_locals: usize,
    pub nbr_params: usize,
    pub max_stack_size: u8,
    #[cfg(feature = "frame-debug-info")]
    pub block_debug_info: BlockDebugInfo,
}

/// Represents a class method.
#[derive(Clone)]
pub enum Method {
    /// A user-defined method from the AST.
    Defined(MethodInfo),
    /// An interpreter primitive.
    Primitive(&'static PrimitiveFn, String, Gc<Class>),
}

impl Method {
    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(..))
    }

    pub fn holder(&self) -> &Gc<Class> {
        match &self {
            Method::Defined(env) => &env.holder,
            Method::Primitive(_, _, holder) => holder,
        }
    }

    /// Used during initialization.
    pub fn set_holder(&mut self, holder_ptr: Gc<Class>) {
        match self {
            Method::Defined(env) => env.holder = holder_ptr,
            Method::Primitive(_, _, c) => *c = holder_ptr,
        }
    }

    pub fn get_env(&self) -> &MethodInfo {
        match self {
            Method::Defined(env) => env,
            Method::Primitive(_, _, _) => panic!("requesting method metadata from primitive"),
        }
    }
}

impl Method {
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        if self.is_primitive() {
            universe.primitive_class()
        } else {
            universe.method_class()
        }
    }

    pub fn signature(&self) -> &str {
        match &self {
            Method::Defined(gc) => &gc.signature,
            Method::Primitive(_, name, _) => name.as_str(),
        }
    }
}

pub trait Invoke {
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe, receiver: Value, args: Vec<Value>);
}

impl Invoke for Gc<Method> {
    fn invoke(&self, interpreter: &mut Interpreter, universe: &mut Universe, receiver: Value, mut args: Vec<Value>) {
        match &**self {
            Method::Defined(_) => {
                let mut frame_args = vec![receiver];
                frame_args.append(&mut args);
                interpreter.push_method_frame_with_args(*self, frame_args.as_slice(), universe.gc_interface);
            }
            Method::Primitive(func, ..) => {
                interpreter.current_frame.stack_push(receiver);
                for arg in args {
                    interpreter.current_frame.stack_push(arg)
                }
                func(interpreter, universe).unwrap_or_else(|_| panic!("invoking func {} failed", &self.signature()))
            }
        }
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}>>#{} = ", self.holder().name(), self.signature())?;
        match &self {
            Method::Defined(env) => {
                writeln!(f, "(")?;
                write!(f, "    <{} locals>", env.nbr_locals)?;
                for bytecode in &env.body {
                    writeln!(f)?;
                    write!(f, "    {}  ", bytecode.padded_name())?;
                    match bytecode {
                        Bytecode::Dup | Bytecode::Dup2 => {}
                        Bytecode::PushLocal(idx) => {
                            write!(f, "local: {}", idx)?;
                        }
                        Bytecode::PushNonLocal(up_idx, idx) => {
                            write!(f, "local: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PushArg(idx) => {
                            write!(f, "argument: {}", idx)?;
                        }
                        Bytecode::PushNonLocalArg(up_idx, idx) => {
                            write!(f, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PushField(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::PushBlock(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::PushConstant0 | Bytecode::PushConstant1 | Bytecode::PushConstant2 => {}
                        Bytecode::PushConstant(idx) => {
                            write!(f, "index: {}, ", idx)?;
                            let constant = &env.literals[*idx as usize];
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
                        Bytecode::PushGlobal(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::Push0 | Bytecode::Push1 | Bytecode::PushNil => {}
                        Bytecode::PushSelf => {}
                        Bytecode::Inc | Bytecode::Dec | Bytecode::Pop => {}
                        Bytecode::PopLocal(up_idx, idx) => {
                            write!(f, "local: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PopArg(up_idx, idx) => {
                            write!(f, "argument: {}, context: {}", idx, up_idx)?;
                        }
                        Bytecode::PopField(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::Send1(idx) | Bytecode::Send2(idx) | Bytecode::Send3(idx) | Bytecode::SendN(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::SuperSend1(idx) | Bytecode::SuperSend2(idx) | Bytecode::SuperSend3(idx) | Bytecode::SuperSendN(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::ReturnLocal => {}
                        Bytecode::ReturnNonLocal(_) => {}
                        Bytecode::ReturnSelf => {}
                        Bytecode::Jump(idx)
                        | Bytecode::JumpBackward(idx)
                        | Bytecode::JumpOnTruePop(idx)
                        | Bytecode::JumpOnFalsePop(idx)
                        | Bytecode::JumpOnFalseTopNil(idx)
                        | Bytecode::JumpOnTrueTopNil(idx)
                        | Bytecode::JumpIfGreater(idx) => {
                            write!(f, "index: {}", idx)?;
                        }
                        Bytecode::Halt => {}
                    }
                }
                Ok(())
            }
            Method::Primitive(..) => write!(f, "<primitive>"),
        }
    }
}
