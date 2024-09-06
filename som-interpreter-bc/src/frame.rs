use std::cell::RefCell;
use std::rc::Rc;

use som_core::bytecode::Bytecode;

use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::gc::GCRef;
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::SOMRef;

#[cfg(feature = "frame-debug-info")]
/// The kind of a given frame.
#[derive(Clone)]
pub enum FrameKind {
    /// A frame created from a block evaluation.
    Block {
        /// The block instance for the current frame.
        block: GCRef<Block>,
    },
    /// A frame created from a method invocation.
    Method {
        /// The holder of the current method (used for lexical self/super).
        holder: GCRef<Class>,
        /// The current method.
        method: GCRef<Method>,
        /// The self value.
        self_value: Value,
    },
}

/// Represents a stack frame.
pub struct Frame {
    /// The bytecodes associated with the frame.
    pub bytecodes: *const Vec<Bytecode>,
    /// Literals/constants associated with the frame.
    pub literals: *const Vec<Literal>,
    /// The arguments within this frame.
    pub args: Vec<Value>,
    /// The bindings within this frame.
    pub locals: Vec<Value>,
    /// Bytecode index.
    pub bytecode_idx: usize,
    /// Inline cache associated with the frame.
    pub inline_cache: *const RefCell<Vec<Option<(*const Class, GCRef<Method>)>>>, // todo class can also be a GC ref, that's basically a pointer
    #[cfg(feature = "frame-debug-info")]
    /// This frame's kind.
    pub kind: FrameKind,
}

impl Frame {
    pub fn from_block(block: GCRef<Block>, args: Vec<Value>) -> Self {
        let block_obj = block.to_obj();
        Self {
            locals: (0..block_obj.blk_info.to_obj().nb_locals).map(|_| Value::Nil).collect(),
            args,
            literals: &block_obj.blk_info.to_obj().literals,
            bytecodes: &block_obj.blk_info.to_obj().body,
            bytecode_idx: 0,
            inline_cache: std::ptr::addr_of!(block_obj.blk_info.to_obj().inline_cache),
            #[cfg(feature = "frame-debug-info")]
            kind: FrameKind::Block { block }
        }
    }

    pub fn from_method(method: GCRef<Method>, args: Vec<Value>) -> Self {
        match method.to_obj().kind() {
            MethodKind::Defined(env) => {
                Self {
                    #[cfg(feature = "frame-debug-info")]
                    kind: {
                        let holder = method.holder.upgrade().unwrap();
                        FrameKind::Method {
                            self_value: args.get(0).unwrap().clone(),
                            method: Rc::clone(&method),
                            holder,
                        }
                    },
                    locals: (0..env.nbr_locals).map(|_| Value::Nil).collect(),
                    args,
                    literals: &env.literals,
                    bytecodes: &env.body,
                    bytecode_idx: 0,
                    inline_cache: std::ptr::addr_of!(env.inline_cache)
                }
            }
            _ => unreachable!()
        }
    }

    #[cfg(feature = "frame-debug-info")]
    /// Construct a new empty frame from its kind.
    pub fn from_kind(kind: FrameKind) -> Self {
        match &kind {
            FrameKind::Block { block } => {
                // let locals = block.blk_info.locals.iter().map(|_| Value::Nil).collect();
                let locals =  (0..block.blk_info.nb_locals).map(|_| Value::Nil).collect();
                let frame = Self {
                    locals,
                    args: vec![Value::Block(Rc::clone(&block))],
                    literals: &block.blk_info.literals,
                    bytecodes: &block.blk_info.body,
                    bytecode_idx: 0,
                    inline_cache: std::ptr::addr_of!(block.blk_info.inline_cache),
                    kind
                };
                frame
            }
            FrameKind::Method { method, .. } => {
                if let MethodKind::Defined(env) = method.kind() {
                    // let locals = env.locals.iter().map(|_| Value::Nil).collect();
                    let locals =  (0..env.nbr_locals).map(|_| Value::Nil).collect();
                    Self {
                        locals,
                        args: vec![],
                        literals: &env.literals,
                        bytecodes: &env.body,
                        bytecode_idx: 0,
                        inline_cache: std::ptr::addr_of!(env.inline_cache),
                        kind
                    }
                } else {
                    Self {
                        locals: vec![],
                        args: vec![],
                        literals: std::ptr::null(), // todo this is totally safe haha i think
                        bytecodes: std::ptr::null(), // yeah ditto
                        inline_cache: std::ptr::null(), // inline cache is never accessed in prims so this will never fail.. right?
                        bytecode_idx: 0,
                        kind
                    }
                }
            }
        }
    }

    #[cfg(feature = "frame-debug-info")]
    /// Get the frame's kind.
    pub fn kind(&self) -> &FrameKind {
        &self.kind
    }

    /// Get the self value for this frame.
    pub fn get_self(&self) -> Value {
        match self.args.first().unwrap() {
            Value::Block(b) => {
                let block_frame = b.to_obj().frame.as_ref().unwrap().clone();
                let x = block_frame.borrow().get_self();
                x
            },
            s => s.clone()
        }
    }

    #[cfg(feature = "frame-debug-info")]
    /// Get the current method itself.
    pub fn get_method(&self) -> GCRef<Method> {
        match &self.kind {
            FrameKind::Method { method, .. } => *method,
            FrameKind::Block { block, .. } => block.frame.as_ref().unwrap().borrow().get_method(),
        }
    }

    // Don't even need this function. We store a pointer to the bytecode in the interpreter directly.
    // pub fn get_bytecode(&self, idx: usize) -> Option<Bytecode> {
    //     self.bytecodes.get(idx).cloned()
    // }

    #[inline(always)]
    pub fn lookup_constant(&self, idx: usize) -> Literal {
        match cfg!(debug_assertions) {
            true => unsafe { (*self.literals).get(idx).unwrap().clone() },
            false => unsafe { (*self.literals).get_unchecked(idx).clone() }
        }
    }

    #[inline(always)]
    pub fn lookup_argument(&self, idx: usize) -> Value {
        match cfg!(debug_assertions) {
            true => self.args.get(idx).unwrap().clone(),
            false => unsafe { self.args.get_unchecked(idx).clone() }
        }
    }

    /// Search for a local binding.
    #[inline(always)]
    pub fn lookup_local(&self, idx: usize) -> Value {
        match cfg!(debug_assertions) {
            true => self.locals.get(idx).unwrap().clone(),
            false => unsafe { self.locals.get_unchecked(idx).clone() }
        }
    }

    /// Assign to a local binding.
    #[inline(always)]
    pub fn assign_local(&mut self, idx: usize, value: Value) {
        match cfg!(debug_assertions) {
            true => { *self.locals.get_mut(idx).unwrap() = value; },
            false => unsafe { *self.locals.get_unchecked_mut(idx) = value; }
        }
    }

    /// Assign to an argument.
    #[inline(always)]
    pub fn assign_arg(&mut self, idx: usize, value: Value) {
        match cfg!(debug_assertions) {
            true => { *self.args.get_mut(idx).unwrap() = value; },
            false => unsafe { *self.args.get_unchecked_mut(idx) = value; }
        }
    }

    pub fn nth_frame_back(current_frame: SOMRef<Frame>, n: u8) -> SOMRef<Frame> {
        if n == 0 {
            return current_frame;
        }

        let mut target_frame: Rc<RefCell<Frame>> = match current_frame.borrow().args.first().unwrap() {
            Value::Block(block) => {
                Rc::clone(block.to_obj().frame.as_ref().unwrap())
            }
            v => panic!("attempting to access a non local var/arg from a method instead of a block: self wasn't blockself but {:?}.", v)
        };
        for _ in 1..n {
            target_frame = match Rc::clone(&target_frame).borrow().args.first().unwrap() {
                Value::Block(block) => {
                    Rc::clone(block.to_obj().frame.as_ref().unwrap())
                }
                v => panic!("attempting to access a non local var/arg from a method instead of a block (but the original frame we were in was a block): self wasn't blockself but {:?}.", v)
            };
        }
        target_frame
    }
}
