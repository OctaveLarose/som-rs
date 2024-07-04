use std::cell::{RefCell};
use std::rc::Rc;

use crate::class::Class;
use crate::value::Value;
use crate::SOMRef;

/// The kind of a given frame.
// #[cfg(feature = "frame-debug-info")]
// #[derive(Debug, Clone)]
// pub enum FrameKind {
//     /// A frame created from a block evaluation.
//     Block {
//         /// The block instance for the current frame.
//         block: Rc<Block>,
//     },
//     /// A frame created from a method invocation.
//     Method {
//         /// The holder of the current method (used for lexical self/super).
//         holder: SOMRef<Class>,
//         /// The current method.
//         signature: Interned,
//         /// The self value.
//         self_value: Value,
//     },
// }

/// Represents a stack frame.
#[derive(Debug)]
pub struct Frame {
    /// This frame's kind.
    // #[cfg(feature = "frame-debug-info")]
    // pub kind: FrameKind,
    /// Local variables that get defined within this frame.
    pub locals: Vec<Value>,
    /// Parameters for this frame.
    pub params: Vec<Value>,
}

impl Frame {
    /// Construct a new empty frame from its kind.
    // pub fn from_kind(kind: FrameKind, nbr_locals: usize, self_value: Value) -> Self {
    //     let mut frame = Self {
    //         kind,
    //         locals: vec![Value::Nil; nbr_locals],
    //         params: vec![], // can we statically determine the length to not have to init it later? it's not straightforward as it turns out, but *should* be doable...
    //     };
    //     frame.params.push(self_value);
    //     frame
    // }

    pub fn new_frame(nbr_locals: usize, nbr_params: usize, self_value: Value) -> Self {
        let mut frame = Self {
            locals: vec![Value::Nil; nbr_locals],
            params: Vec::with_capacity(nbr_params + 1), // can we add all params now? it's not straightforward as it turns out, but adding some *should* be doable...
        };
        frame.params.push(self_value);
        frame
    }

    /// Get the frame's kind.
    // pub fn kind(&self) -> &FrameKind {
    //     &self.kind
    // }

    /// Get the self value for this frame.
    pub fn get_self(&self) -> Value {
        match self.params.get(0).unwrap() {
            Value::Block(b) => b.frame.borrow().get_self(),
            s => s.clone()
        }
    }

    /// Get the holder for this current method.
    pub fn get_method_holder(&self) -> SOMRef<Class> {
        let ours = match self.get_self() {
            Value::Class(c) => c,
            v => panic!("self value not a class, but {:?}", v)
        };

        ours.clone().borrow().class()
    }

    /// Get the signature of the current method.
    // #[cfg(feature = "frame-debug-info")]
    // pub fn get_method_signature(&self) -> Interned {
    //     match &self.kind {
    //         FrameKind::Method { signature, .. } => *signature,
    //         FrameKind::Block { block, .. } => block.frame.borrow().get_method_signature(),
    //     }
    // }

    #[inline] // not sure if necessary
    pub fn lookup_local(&self, idx: usize) -> Value {
        match cfg!(debug_assertions) {
            true => self.locals.get(idx).unwrap().clone(),
            false => unsafe { self.locals.get_unchecked(idx).clone() }
        }
    }

    pub fn lookup_non_local(&self, idx: usize, scope: usize) -> Value {
        self.nth_frame_back(scope).borrow().lookup_local(idx)
    }
    
    pub fn assign_local(&mut self, idx: usize, value: &Value) {
        let local = match cfg!(debug_assertions) {
            true => self.locals.get_mut(idx).unwrap(),
            false => unsafe { self.locals.get_unchecked_mut(idx) }
        };
        *local = value.clone();
    }

    pub fn assign_non_local(&mut self, idx: usize, scope: usize, value: &Value) {
        self.nth_frame_back(scope).borrow_mut().assign_local(idx, value)
    }

    pub fn lookup_arg(&self, idx: usize, scope: usize) -> Value {
        match (idx, scope) {
            (0, 0) => self.get_self(),
            (_, 0) => self.lookup_local_arg(idx),
            _ => self.lookup_non_local_arg(idx, scope),
        }
    }

    pub fn lookup_local_arg(&self, idx: usize) -> Value {
        match cfg!(debug_assertions) {
            true => self.params.get(idx).unwrap().clone(),
            false => unsafe { self.params.get_unchecked(idx).clone() }
        }
    }

    pub fn lookup_non_local_arg(&self, idx: usize, scope: usize) -> Value {
        self.nth_frame_back(scope).borrow().lookup_local_arg(idx)
    }

    pub fn assign_arg_local(&mut self, idx: usize, value: &Value) {
        let val = match cfg!(debug_assertions) {
            true => self.params.get_mut(idx).unwrap(),
            false => unsafe { self.params.get_unchecked_mut(idx) }
        };
        *val = value.clone();
    }

    pub fn assign_arg(&mut self, idx: usize, scope: usize, value: &Value) {
        match scope {
            0 => self.assign_arg_local(idx, value),
            _ => self.nth_frame_back(scope).borrow_mut().assign_arg_local(idx, value)
        }
    }

    pub fn lookup_field(&self, idx: usize) -> Value {
        match self.get_self() {
            Value::Instance(i) => { i.borrow_mut().lookup_local(idx) }
            Value::Class(c) => { c.borrow().class().borrow_mut().lookup_local(idx) }
            v => { panic!("{:?}", &v) }
        }
    }

    pub fn assign_field(&self, idx: usize, value: &Value) -> () {
        match self.get_self() {
            Value::Instance(i) => { i.borrow_mut().assign_local(idx, value.clone()) }
            Value::Class(c) => { c.borrow().class().borrow_mut().assign_local(idx, &value) }
            v => { panic!("{:?}", &v) }
        }
    }

    pub fn nth_frame_back(&self, n: usize) -> SOMRef<Frame> {
        let mut target_frame: Rc<RefCell<Frame>> = match self.params.get(0).unwrap() { // todo optimize that also
            Value::Block(block) => {
                Rc::clone(&block.frame)
            }
            v => panic!("attempting to access a non local var/arg from a method instead of a block: self wasn't blockself but {:?}.", v)
        };
        for _ in 1..n {
            target_frame = match Rc::clone(&target_frame).borrow().params.get(0).unwrap() {
                Value::Block(block) => {
                    Rc::clone(&block.frame)
                }
                v => panic!("attempting to access a non local var/arg from a method instead of a block (but the original frame we were in was a block): self wasn't blockself but {:?}.", v)
            };
        }
        target_frame
    }

        /// Get the method invocation frame for that frame.
    pub fn method_frame(frame: &SOMRef<Frame>) -> SOMRef<Frame> {
        if let Value::Block(b) = frame.borrow().params.get(0).unwrap() {
            Frame::method_frame(&b.frame)
        } else {
            Rc::clone(frame)
        }
    }
}