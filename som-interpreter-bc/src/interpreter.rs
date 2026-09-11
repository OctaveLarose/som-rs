use crate::compiler::{value_from_literal, Literal};
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::block::{Block, CacheEntry};
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::{Method, MethodInfo};
use anyhow::Context;
use std::cell::UnsafeCell;

#[cfg(feature = "profiler")]
use crate::debug::profiler::Profiler;

use num_bigint::BigInt;
use som_core::bytecode::{Bytecode, BC_SIZE_1_ARG, BC_SIZE_2_ARG, BC_SIZE_NO_ARGS, BC_SIZE_U16_ARG};
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};
use som_gc::gcref::Gc;
use som_value::interned::Interned;
use std::time::Instant;

macro_rules! resolve_method_and_send {
    ($self:expr, $universe:expr, $symbol:expr, $nbr_args:expr) => {{
        let receiver = $self.stack[$self.stack.len() - $nbr_args];
        let receiver_class = receiver.class($universe);
        let method = resolve_method(&mut $self.get_current_frame(), &receiver_class, $symbol, $self.bytecode_idx);
        $self.bytecode_idx += BC_SIZE_U16_ARG; // always this size for a send
        do_send($self, $universe, method, $symbol, $nbr_args);
    }};
}

macro_rules! profiler_maybe_start {
    ($bc_name:expr) => {{
        #[cfg(feature = "profiler")]
        let timing = Profiler::global().start_detached_event($bc_name, "bytecodes");

        #[cfg(feature = "profiler")]
        timing
    }};
}

macro_rules! profiler_maybe_stop {
    ($timing:expr) => {
        #[cfg(feature = "profiler")]
        Profiler::global().finish_detached_event($timing);
    };
}

// Safety: this assumes the bytecode is not malformed, and so that if a bytecode requires a pop, said pop is possible.
// If it were to not be possible somehow, bytecode gen would have messed up, and then our interpreter would be very unsound and pretty damn bad anyway.
// So might as well assume we didn't mess up and get some -potential- extra perf here.
//
// TODO: these macros are not used by any of the primitives at the moment (would require a macro_export which Rust discourages, but is likely fine in this context)
// But there might be a small perf benefit if we do that.
macro_rules! stack_fast_pop {
    ($stack:expr) => {
        unsafe { $stack.pop().unwrap_unchecked() }
    };
}

// Safety: same logic as `stack_fast_pop`.
macro_rules! stack_fast_last {
    ($stack:expr) => {
        unsafe { $stack.last().unwrap_unchecked() }
    };
}

// Safety: same logic as `stack_fast_pop`.
macro_rules! stack_fast_last_mut {
    ($stack:expr) => {
        unsafe { $stack.last_mut().unwrap_unchecked() }
    };
}

macro_rules! read_u16_fast {
    ($var_name:ident, $bytecodes:expr, $idx:expr) => {
        let $var_name: u16 = unsafe {
            u16::from_le_bytes(
                $bytecodes
                    .get_unchecked($idx..$idx + 2)
                    .try_into()
                    .unwrap_unchecked(),
            )
        };
    };
}

pub struct Interpreter {
    /// The time record of the interpreter's creation.
    pub start_time: Instant,
    /// The current bytecode index.
    pub bytecode_idx: u16,
    /// The current frame.
    pub current_frame: UnsafeCell<Gc<Frame>>,
    /// The interpreter is stack-based.
    pub stack: Vec<Value>,
    /// GC can trigger when the interpreter wants to allocate a new frame.
    /// We're then in a situation where we've looked up a `Method` (which is how we knew we were dealing with a non-primitive, and so that we had to create a frame)
    /// So this method can't be stored on the Rust stack, or GC would miss it. Therefore: we keep it reachable there.
    pub frame_method_root: Gc<MethodInfo>,
    pub frame_args_root: Option<Vec<Value>>,
}

impl Interpreter {
    pub fn new(base_frame: Gc<Frame>) -> Self {
        Self {
            start_time: Instant::now(),
            bytecode_idx: 0,
            current_frame: UnsafeCell::from(base_frame),
            frame_method_root: Gc::default(),
            frame_args_root: None,
            stack: vec![Value::STACK_MARKER],
        }
    }

    /// Return the current frame.
    /// It's in an `UnsafeCell` for moving GC reasons: you get many bugs by using Gc<Frame> by
    /// itself, since Rust assumes that it hasn't moved when it has
    pub fn get_current_frame(&self) -> Gc<Frame> {
        unsafe { (*self.current_frame.get()).clone() }
    }

    pub fn get_current_frame_mut(&mut self) -> &mut Gc<Frame> {
        self.current_frame.get_mut()
    }

    #[inline(always)]
    pub fn stack_n_last_elements(&self, n: usize) -> &[Value] {
        &self.stack[self.stack.len() - n..]
    }

    /// Creates and allocates a new frame corresponding to a method.
    /// nbr_args is the number of arguments, including the self value, which it takes from the stack.
    pub fn push_method_frame(&mut self, method: Gc<MethodInfo>, nbr_args: usize, mutator: &mut GCInterface) -> Gc<Frame> {
        self.frame_method_root = method.clone();
        std::hint::black_box(&self.frame_method_root); // paranoia

        let size = Frame::get_true_size(nbr_args as u8, method.nbr_locals);

        let mut frame_ptr: Gc<Frame> = mutator.request_memory_for_type(size, AllocSiteMarker::MethodFrame);

        *frame_ptr = Frame::new(self.frame_method_root.clone(), self.get_current_frame());

        Frame::init_frame_args_locals_from_stack(&mut frame_ptr, &mut self.stack, nbr_args);

        self.bytecode_idx = 0;
        self.current_frame = UnsafeCell::from(frame_ptr.clone());

        self.stack.push(Value::STACK_MARKER);
        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method, with arguments provided.
    /// Used in primitives and corner cases like DNU calls.
    pub fn push_method_frame_with_args(&mut self, method: Gc<MethodInfo>, args: Vec<Value>, mutator: &mut GCInterface) -> Gc<Frame> {
        self.frame_method_root = method.clone();
        std::hint::black_box(&self.frame_method_root); // paranoia

        let nbr_locals = method.nbr_locals;

        let size = Frame::get_true_size(args.len() as u8, nbr_locals);

        self.frame_args_root = Some(args);

        let mut frame_ptr: Gc<Frame> = mutator.request_memory_for_type(size, AllocSiteMarker::MethodFrameWithArgs);

        *frame_ptr = Frame::new(self.frame_method_root.clone(), self.get_current_frame());
        Frame::init_frame_args_locals(&mut frame_ptr, self.frame_args_root.as_ref().unwrap());

        self.bytecode_idx = 0;
        self.current_frame = UnsafeCell::from(frame_ptr.clone());
        self.frame_args_root = None;

        self.stack.push(Value::STACK_MARKER);

        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method.
    pub fn push_block_frame(&mut self, nbr_args: usize, mutator: &mut GCInterface) -> Gc<Frame> {
        // This used to be a function defined in the frame module, but its logic is tightly coupled with the interpreter. As in, it needs access to the stack, and the mutator.
        // NB: Also, inlining it here helps write moving-GC-proof code, since instead of passing references to the interpreter's internals (which would be moved by potential GC triggers), we can query those internals ourselves and ensure we get them from the source, where they would have been moved correctly.
        // A good example is querying the "current" (now parent) frame from the interpreter *after* allocating memory for the new frame, which was an issue when this was a non-inlined function since we then needed to pass a reference to the frame *before* allocating memory, as a function argument, and then we had to be very careful not to get bugs.
        let frame_ptr = {
            let size = {
                let nbr_locals = {
                    let block_value = self.stack[self.stack.len() - 1 - (nbr_args - 1)];
                    let block = block_value.as_block().unwrap();
                    block.blk_info.nbr_locals
                };
                Frame::get_true_size(nbr_args as u8, nbr_locals)
            };

            let mut frame_ptr: Gc<Frame> = mutator.request_memory_for_type(size, AllocSiteMarker::BlockFrame);

            let block_value = *self.stack.get(self.stack.len() - 1 - (nbr_args - 1)).unwrap();
            *frame_ptr = Frame::new(block_value.as_block().unwrap().blk_info.clone(), self.get_current_frame());

            Frame::init_frame_args_locals_from_stack(&mut frame_ptr, &mut self.stack, nbr_args);

            frame_ptr
        };

        self.stack.push(Value::STACK_MARKER);
        self.bytecode_idx = 0;
        self.current_frame = UnsafeCell::from(frame_ptr.clone());
        frame_ptr
    }

    pub fn pop_frame(&mut self) {
        let new_current_frame = &self.get_current_frame().prev_frame;

        while self.stack.pop().unwrap() != Value::STACK_MARKER {}

        self.current_frame = UnsafeCell::from(new_current_frame.clone());
        match new_current_frame.is_empty() {
            true => {}
            false => {
                self.bytecode_idx = new_current_frame.bytecode_idx;
            }
        }
    }

    pub fn pop_n_frames(&mut self, n: u8) {
        let mut new_current_frame = self.get_current_frame();
        for _ in 0..n {
            while self.stack.pop().unwrap() != Value::STACK_MARKER {}
            new_current_frame = new_current_frame.prev_frame.clone();
            if new_current_frame.is_empty() {
                panic!("found an empty target frame while walking the frame stack somehow");
            }
        }

        self.current_frame = UnsafeCell::from(new_current_frame.clone());
        match new_current_frame.is_empty() {
            true => {}
            false => {
                self.bytecode_idx = new_current_frame.bytecode_idx;
            }
        }
    }

    pub fn run(&mut self, universe: &mut Universe) -> Option<Value> {
        loop {
            let frame = self.get_current_frame();
            let bytecodes = frame.get_bytecodes();
            // Safety: there's always a reference to the current bytecodes. Need unsafe because we want to store a ref for quick access in perf-critical code (but probably doesn't matter)
            let bytecode = *(unsafe { bytecodes.get_unchecked(self.bytecode_idx as usize) });

            // dbg!(&bytecode);
            //dbg!(&self.bytecode_idx);

            // for the optional profiler macros not to be reported as warnings
            #[allow(clippy::let_unit_value)]
            match bytecode {
                Bytecode::SEND_1 => {
                    let _timing = profiler_maybe_start!("SEND");
                    read_u16_fast!(val, bytecodes, self.bytecode_idx as usize + 1);
                    let symbol: Interned = Interned(val);
                    resolve_method_and_send!(self, universe, symbol, 1);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::SEND_2 => {
                    let _timing = profiler_maybe_start!("SEND");
                    read_u16_fast!(val, bytecodes, self.bytecode_idx as usize + 1);
                    let symbol: Interned = Interned(val);
                    resolve_method_and_send!(self, universe, symbol, 2);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::SEND_3 => {
                    let _timing = profiler_maybe_start!("SEND");
                    read_u16_fast!(val, bytecodes, self.bytecode_idx as usize + 1);
                    let symbol: Interned = Interned(val);
                    resolve_method_and_send!(self, universe, symbol, 3);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::SEND_N => {
                    let _timing = profiler_maybe_start!("SEND");
                    read_u16_fast!(val, bytecodes, self.bytecode_idx as usize + 1);
                    let symbol: Interned = Interned(val);
                    let nbr_args = nbr_args(universe.lookup_symbol(symbol));
                    resolve_method_and_send!(self, universe, symbol, nbr_args);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_LOCAL => {
                    let _timing = profiler_maybe_start!("PUSH_LOCAL");
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let value = *self.get_current_frame().lookup_local(idx as usize);
                    self.stack.push(value);
                    self.bytecode_idx += BC_SIZE_1_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_NON_LOCAL => {
                    let _timing = profiler_maybe_start!("PUSHNONLOCAL");
                    let up_idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 2];
                    debug_assert_ne!(up_idx, 0);
                    let from = Frame::nth_frame_back(&self.get_current_frame(), up_idx);
                    let value = *from.lookup_local(idx as usize);
                    self.stack.push(value);
                    self.bytecode_idx += BC_SIZE_2_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_ARG => {
                    let _timing = profiler_maybe_start!("PUSH_ARG");
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    debug_assert_ne!(idx, 0); // that's a ReturnSelf case.
                    let value = *self.get_current_frame().lookup_argument(idx as usize);
                    self.stack.push(value);
                    self.bytecode_idx += BC_SIZE_1_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_NON_LOCAL_ARG => {
                    let _timing = profiler_maybe_start!("PUSH_NON_LOCAL_ARG");
                    let up_idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 2];
                    debug_assert_ne!(up_idx, 0);
                    debug_assert_ne!((up_idx, idx), (0, 0)); // that's a ReturnSelf case.
                    let from = Frame::nth_frame_back(&self.get_current_frame(), up_idx);
                    let value = from.lookup_argument(idx as usize);
                    self.stack.push(*value);
                    self.bytecode_idx += BC_SIZE_2_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_FIELD => {
                    let _timing = profiler_maybe_start!("PUSH_FIELD");
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let self_val = self.get_current_frame().get_self();
                    let val = {
                        if let Some(instance) = self_val.as_instance() {
                            *Instance::lookup_field(&instance, idx as usize)
                        } else if let Some(cls) = self_val.as_class() {
                            cls.class().lookup_field(idx as usize)
                        } else {
                            panic!("trying to read a field from a {:?}?", &self_val)
                        }
                    };
                    self.stack.push(val);
                    self.bytecode_idx += BC_SIZE_1_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::DUP => {
                    let _timing = profiler_maybe_start!("DUP");
                    let value = *stack_fast_last!(&mut self.stack);
                    self.stack.push(value);
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::INC => {
                    let _timing = profiler_maybe_start!("INC");
                    let last = stack_fast_last_mut!(&mut self.stack);

                    if let Some(int) = last.as_integer() {
                        *last = Value::new_integer(int + 1);
                    } else if let Some(double) = last.as_double() {
                        *last = Value::new_double(double + 1.0);
                    } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                        *big_int += 1;
                    } else {
                        panic!("Invalid type in Inc")
                    };
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::DEC => {
                    let _timing = profiler_maybe_start!("DEC");
                    let last = stack_fast_last_mut!(&mut self.stack);

                    if let Some(int) = last.as_integer() {
                        *last = Value::new_integer(int - 1);
                    } else if let Some(double) = last.as_double() {
                        *last = Value::new_double(double - 1.0);
                    } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                        *big_int -= 1;
                    } else {
                        panic!("Invalid type in DEC")
                    };
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_BLOCK => {
                    let _timing = profiler_maybe_start!("PUSH_BLOCK");
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];

                    // allocating ahead of time in case it triggers GC.
                    let mut new_blk =
                        universe.gc_interface.request_memory_for_type::<Block>(std::mem::size_of::<Block>(), AllocSiteMarker::RuntimeBlock);

                    let current_frame = self.get_current_frame();
                    match current_frame.lookup_constant(idx as usize) {
                        Literal::Block(blk) => {
                            new_blk.blk_info = (*blk).clone();
                            new_blk.frame = current_frame.clone();
                        }
                        _ => panic!("PushBlock expected a block, but got another invalid literal"),
                    }

                    self.stack.push(Value::Block(new_blk));
                    self.bytecode_idx += BC_SIZE_1_ARG;

                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_CONSTANT => {
                    let _timing = profiler_maybe_start!("PUSH_CONSTANT");
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];

                    let current_frame = self.get_current_frame();
                    let literal = current_frame.lookup_constant(idx as usize);
                    let value = value_from_literal(literal, &mut universe.gc_interface);
                    self.stack.push(value);
                    self.bytecode_idx += BC_SIZE_1_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_GLOBAL => {
                    let _timing = profiler_maybe_start!("PUSH_GLOBAL");

                    if let Some(CacheEntry::Global(value)) = unsafe { self.get_current_frame().get_inline_cache_entry(self.bytecode_idx as usize) } {
                        let value = *value;
                        self.stack.push(value);
                        self.bytecode_idx += BC_SIZE_1_ARG;
                        continue;
                    }

                    let current_frame = self.get_current_frame();
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let literal = current_frame.lookup_constant(idx as usize);
                    let symbol = match literal {
                        Literal::Symbol(sym) => sym,
                        _ => panic!("Global is not a symbol."),
                    };
                    if let Some(value) = universe.lookup_global(*symbol) {
                        self.stack.push(value);
                        unsafe { *self.get_current_frame().get_inline_cache_entry(self.bytecode_idx as usize) = Some(CacheEntry::Global(value)) }
                        self.bytecode_idx += BC_SIZE_1_ARG;
                    } else {
                        self.bytecode_idx += BC_SIZE_1_ARG;
                        let self_value = self.get_current_frame().get_self();
                        universe.unknown_global(self, self_value, *symbol)?;
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_0 => {
                    let _timing = profiler_maybe_start!("PUSH_0");
                    self.stack.push(Value::INTEGER_ZERO);
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_1 => {
                    let _timing = profiler_maybe_start!("PUSH_1");
                    self.stack.push(Value::INTEGER_ONE);
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_NIL => {
                    let _timing = profiler_maybe_start!("PUSH_NIL");
                    self.stack.push(Value::NIL);
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PUSH_SELF => {
                    let _timing = profiler_maybe_start!("PUSH_SELF");
                    let self_val = *self.get_current_frame().lookup_argument(0);
                    self.stack.push(self_val);
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::POP => {
                    let _timing = profiler_maybe_start!("POP");
                    stack_fast_pop!(&mut self.stack);
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::POP_LOCAL => {
                    let _timing = profiler_maybe_start!("POP_LOCAL");
                    let up_idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 2];
                    let value = stack_fast_pop!(&mut self.stack);
                    let mut from = Frame::nth_frame_back(self.get_current_frame_mut(), up_idx);
                    from.assign_local(idx as usize, value);
                    self.bytecode_idx += BC_SIZE_2_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::POP_ARG => {
                    let _timing = profiler_maybe_start!("POP_ARG");
                    let up_idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 2];
                    let value = stack_fast_pop!(&mut self.stack);
                    let mut from = Frame::nth_frame_back(self.get_current_frame_mut(), up_idx);
                    from.assign_arg(idx as usize, value);
                    self.bytecode_idx += BC_SIZE_2_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::POP_FIELD => {
                    let _timing = profiler_maybe_start!("POP_FIELD");
                    let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let value = stack_fast_pop!(&mut self.stack);
                    let self_val = self.get_current_frame().get_self();
                    if let Some(instance) = self_val.as_instance() {
                        Instance::assign_field(&instance, idx as usize, value);
                    } else if let Some(cls) = self_val.as_class() {
                        cls.class().assign_field(idx as usize, value)
                    } else {
                        panic!("trying to assign a field to a {:?}?", &self_val)
                    };
                    self.bytecode_idx += BC_SIZE_1_ARG;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::SUPER_SEND => {
                    let _timing = profiler_maybe_start!("SUPER_SEND");
                    read_u16_fast!(val, bytecodes, self.bytecode_idx as usize + 1);
                    let symbol: Interned = Interned(val);
                    let nbr_args = {
                        let signature = universe.lookup_symbol(symbol);
                        nbr_args(signature)
                    };

                    let method = {
                        let holder = self.get_current_frame().get_method_holder();
                        let super_class = holder.super_class().unwrap();
                        resolve_method(self.current_frame.get_mut(), &super_class, symbol, self.bytecode_idx)
                    };
                    self.bytecode_idx += BC_SIZE_2_ARG;
                    do_send(self, universe, method, symbol, nbr_args);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::RETURN_SELF => {
                    let _timing = profiler_maybe_start!("RETURN_SELF");
                    let self_val = *self.get_current_frame().lookup_argument(0);
                    self.pop_frame();
                    self.stack.push(self_val);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::RETURN_LOCAL => {
                    let _timing = profiler_maybe_start!("RETURN_LOCAL");
                    let value = stack_fast_pop!(&mut self.stack);
                    self.pop_frame();
                    if self.get_current_frame().is_empty() {
                        profiler_maybe_stop!(_timing);
                        return Some(value);
                    }
                    self.stack.push(value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::RETURN_NON_LOCAL => {
                    let _timing = profiler_maybe_start!("RETURN_NON_LOCAL");
                    let up_idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
                    let method_frame = Frame::nth_frame_back(&self.get_current_frame(), up_idx);

                    let escaped_frames_nbr = {
                        let mut current_frame = self.get_current_frame();
                        let mut count = 0;

                        loop {
                            if current_frame == method_frame {
                                break Some(count);
                            } else if current_frame.is_empty() {
                                break None;
                            } else {
                                current_frame = current_frame.prev_frame.clone();
                                count += 1;
                            }
                        }
                    };

                    if let Some(count) = escaped_frames_nbr {
                        let value = stack_fast_pop!(&mut self.stack);
                        self.pop_n_frames(count + 1);
                        self.stack.push(value);
                    } else {
                        // Block has escaped its method frame.
                        let instance = self.get_current_frame().get_self();
                        let block = match self.get_current_frame().lookup_argument(0).as_block() {
                            Some(block) => block,
                            _ => {
                                // Should never happen, because `universe.current_frame()` would
                                // have been equal to `universe.current_method_frame()`.
                                panic!("A method frame has escaped itself ??");
                            }
                        };

                        // we store the current bytecode idx to be able to correctly restore the bytecode state when we pop frames
                        self.get_current_frame().bytecode_idx = self.bytecode_idx + BC_SIZE_1_ARG;

                        universe
                            .escaped_block(self, instance, block)
                            .expect("A block has escaped and `escapedBlock:` is not defined on receiver");
                    };

                    profiler_maybe_stop!(_timing);
                }
                Bytecode::DUP_2 => {
                    let _timing = profiler_maybe_start!("DUP2");
                    let second_to_last = self.stack[self.stack.len() - 2];
                    self.stack.push(second_to_last);
                    self.bytecode_idx += BC_SIZE_NO_ARGS;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP => {
                    let _timing = profiler_maybe_start!("JUMP");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    self.bytecode_idx += offset;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_BACKWARD => {
                    let _timing = profiler_maybe_start!("JUMP_BACKWARD");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    self.bytecode_idx -= offset;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_ON_TRUE_TOP_NIL => {
                    let _timing = profiler_maybe_start!("JUMP_ON_TRUE_TOP_NIL");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_last_mut!(&mut self.stack);

                    if condition_result.is_boolean_true() {
                        self.bytecode_idx += offset;
                        *condition_result = Value::NIL;
                    } else if condition_result.is_boolean_false() {
                        stack_fast_pop!(&mut self.stack);
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                    } else {
                        panic!("JumpOnTrueTopNil condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_ON_FALSE_TOP_NIL => {
                    let _timing = profiler_maybe_start!("JUMP_ON_FALSE_TOP_NIL");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_last_mut!(&mut self.stack);

                    if condition_result.is_boolean_true() {
                        stack_fast_pop!(&mut self.stack);
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                    } else if condition_result.is_boolean_false() {
                        self.bytecode_idx += offset;
                        *condition_result = Value::NIL;
                    } else {
                        panic!("JumpOnFalseTopNil condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_ON_TRUE_POP => {
                    let _timing = profiler_maybe_start!("JUMP_ON_TRUE_POP");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_pop!(&mut self.stack);

                    if condition_result.is_boolean_true() {
                        self.bytecode_idx += offset;
                    } else if condition_result.is_boolean_false() {
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                        // pass
                    } else {
                        panic!("JumpOnTruePop condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_ON_FALSE_POP => {
                    let _timing = profiler_maybe_start!("JUMP_ON_FALSE_POP");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_pop!(&mut self.stack);

                    if condition_result.is_boolean_false() {
                        self.bytecode_idx += offset;
                    } else if condition_result.is_boolean_true() {
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                        // pass
                    } else {
                        panic!("JumpOnFalsePop condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_IF_GREATER => {
                    let _timing = profiler_maybe_start!("JUMP_IF_GREATER");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let top = stack_fast_last!(&self.stack);
                    let top2 = self.stack[self.stack.len() - 2];

                    let is_greater = {
                        if let (Some(a), Some(b)) = (top.as_integer(), top2.as_integer()) {
                            a > b
                        } else if let (Some(a), Some(b)) = (top.as_double(), top2.as_double()) {
                            a > b
                        } else {
                            panic!("JumpifGreater: we don't handle this case.")
                        }
                    };

                    if is_greater {
                        stack_fast_pop!(&mut self.stack);
                        stack_fast_pop!(&mut self.stack);
                        self.bytecode_idx += offset;
                    } else {
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                    }
                }
                Bytecode::JUMP_ON_NIL_TOP_TOP => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NIL_TOP_TOP");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_last!(&mut self.stack);

                    if condition_result.is_nil() {
                        self.bytecode_idx += offset;
                    } else {
                        stack_fast_pop!(&mut self.stack);
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                    }
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_ON_NOT_NIL_TOP_TOP => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_TOP_TOP");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_last!(&mut self.stack);

                    if !condition_result.is_nil() {
                        self.bytecode_idx += offset;
                    } else {
                        stack_fast_pop!(&mut self.stack);
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                    }
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_ON_NIL_POP => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NIL_POP");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_pop!(&mut self.stack);

                    if condition_result.is_nil() {
                        self.bytecode_idx += offset;
                    } else {
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                        // pass
                    }
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JUMP_ON_NOT_NIL_POP => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_POP");
                    read_u16_fast!(offset, bytecodes, self.bytecode_idx as usize + 1);
                    let condition_result = stack_fast_pop!(&mut self.stack);

                    if !condition_result.is_nil() {
                        self.bytecode_idx += offset;
                    } else {
                        self.bytecode_idx += BC_SIZE_U16_ARG;
                        // pass
                    }
                    profiler_maybe_stop!(_timing);
                }
                _ => {
                    unsafe { std::hint::unreachable_unchecked() }
                },
            }
        }

        pub fn do_send(interpreter: &mut Interpreter, universe: &mut Universe, method: Option<Gc<Method>>, symbol: Interned, nbr_args: usize) {
            // we store the current bytecode idx to be able to correctly restore the bytecode state when we pop frames
            interpreter.get_current_frame().bytecode_idx = interpreter.bytecode_idx; // TODO: should not be done before a primitive call. they should handle it if they need it

            let Some(method) = method else {
                let args = interpreter.stack.split_off(interpreter.stack.len() - nbr_args + 1);
                let self_value = interpreter.stack.pop().unwrap();

                universe
                    .does_not_understand(interpreter, self_value, symbol, args)
                    .expect("A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver");

                return;
            };

            match &*method {
                Method::Defined(method_info) => {
                    // let name = &method.holder().name.clone();
                    // eprintln!("--- Invoking {:?} (in {:?})", &method.signature(), &name);
                    interpreter.push_method_frame(method_info.clone(), nbr_args, &mut universe.gc_interface);
                }
                Method::Primitive(func, _met_info) => {
                    // eprintln!("--- Invoking prim {:?} (in {:?})", &_met_info.signature, &_met_info.holder.name);

                    func(interpreter, universe, nbr_args)
                        .with_context(|| anyhow::anyhow!("error calling primitive `{}`", universe.lookup_symbol(symbol)))
                        .unwrap();
                }
                Method::TrivialGlobal(met, _) => {
                    //eprintln!("--- Invoking trivial method");
                    met.invoke(universe, interpreter)
                }
                Method::TrivialLiteral(met, _) => {
                    //eprintln!("--- Invoking trivial method");
                    stack_fast_pop!(&mut interpreter.stack); // remove the receiver
                    met.invoke(universe, interpreter)
                }
                Method::TrivialGetter(met, _) => {
                    //eprintln!("--- Invoking trivial method");
                    met.invoke(universe, interpreter)
                }
                Method::TrivialSetter(met, _) => {
                    //eprintln!("--- Invoking trivial method");
                    met.invoke(universe, interpreter)
                }
            }
        }

        fn resolve_method(frame: &mut Gc<Frame>, class: &Gc<Class>, signature: Interned, bytecode_idx: u16) -> Option<Gc<Method>> {
            // SAFETY: this access is actually safe because the bytecode compiler
            // makes sure the cache has as many entries as there are bytecode instructions,
            // therefore we can avoid doing any redundant bounds checks here.
            let maybe_found = unsafe { frame.get_inline_cache_entry(bytecode_idx as usize) };

            match maybe_found {
                Some(CacheEntry::Send(receiver, method)) if receiver.as_ptr() == class.as_ptr() => Some(method.clone()),
                Some(CacheEntry::Global(_)) => panic!("global cache entry for a send?"),
                place @ None => {
                    let found = class.lookup_method(signature);
                    *place = found.clone().map(|method| CacheEntry::Send(class.clone(), method));
                    found
                }
                _ => class.lookup_method(signature),
            }
        }

        fn nbr_args(signature: &str) -> usize {
            match signature.chars().next() {
                Some(ch) if !ch.is_alphabetic() => 2,
                _ => signature.chars().filter(|ch| *ch == ':').count() + 1, // adding 1 to account for self
            }
        }
    }
}
