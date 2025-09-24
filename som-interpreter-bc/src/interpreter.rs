use crate::compiler::{value_from_literal, Literal};
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::block::{Block, CacheEntry};
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use anyhow::Context;
use std::cell::UnsafeCell;

#[cfg(feature = "profiler")]
use crate::debug::profiler::Profiler;

use num_bigint::BigInt;
use som_core::bytecode::Bytecode;
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};
use som_gc::gcref::Gc;
use som_value::interned::Interned;
use std::time::Instant;

#[macro_export]
macro_rules! cur_frame {
    ($interp:expr) => {
        $interp.get_current_frame()
    };
}

macro_rules! resolve_method_and_send {
    ($self:expr, $universe:expr, $symbol:expr, $nbr_args:expr) => {{
        let current_frame = $self.get_current_frame();
        let receiver = current_frame.stack_nth_back($nbr_args);
        let receiver_class = receiver.class($universe);
        let method = resolve_method(&mut $self.get_current_frame(), &receiver_class, $symbol, $self.bytecode_idx);
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

pub struct Interpreter {
    /// The time record of the interpreter's creation.
    pub start_time: Instant,
    /// The current bytecode index.
    pub bytecode_idx: u16,
    /// The current frame.
    pub current_frame: UnsafeCell<Gc<Frame>>,
    /// Pointer to the frame's bytecodes, to not have to read them from the frame directly
    pub current_bytecodes: *const Vec<Bytecode>,
    /// GC can trigger when the interpreter wants to allocate a new frame.
    /// We're then in a situation where we've looked up a `Method` (which is how we knew we were dealing with a non-primitive, and so that we had to create a frame)
    /// So this method can't be stored on the Rust stack, or GC would miss it. Therefore: we keep it reachable there.
    pub frame_method_root: Gc<Method>,
    pub frame_args_root: Option<Vec<Value>>,
}

impl Interpreter {
    pub fn new(base_frame: Gc<Frame>) -> Self {
        Self {
            start_time: Instant::now(),
            bytecode_idx: 0,
            current_bytecodes: base_frame.get_bytecode_ptr(),
            current_frame: UnsafeCell::from(base_frame),
            frame_method_root: Gc::default(),
            frame_args_root: None,
        }
    }

    /// Return the current frame.
    /// It's in an `UnsafeCell` for moving GC reasons: you get many bugs by using Gc<Frame> by
    /// itself, since Rust assumes that it hasn't moved when it in fact very much has
    pub fn get_current_frame(&self) -> Gc<Frame> {
        unsafe { (*self.current_frame.get()).clone() }
    }

    pub fn get_current_frame_mut(&mut self) -> &mut Gc<Frame> {
        self.current_frame.get_mut()
    }

    /// Creates and allocates a new frame corresponding to a method.
    /// nbr_args is the number of arguments, including the self value, which it takes from the previous frame.
    pub fn push_method_frame(&mut self, method: Gc<Method>, nbr_args: usize, mutator: &mut GCInterface) -> Gc<Frame> {
        self.frame_method_root = method.clone();
        std::hint::black_box(&self.frame_method_root); // paranoia

        let (max_stack_size, nbr_locals) = match &*method {
            Method::Defined(m_env) => (m_env.max_stack_size as usize, m_env.nbr_locals),
            _ => unreachable!("if we're allocating a method frame, it has to be defined."),
        };

        let size = Frame::get_true_size(max_stack_size, nbr_args as u8, nbr_locals);
        let mut frame_ptr: Gc<Frame> = mutator.request_memory_for_type(size, AllocSiteMarker::MethodFrame);
        mutator.total_other_memory_size -= (max_stack_size * size_of::<Value>()) as u128;

        *frame_ptr = Frame::from_method(self.frame_method_root.clone());

        let mut prev_frame = self.get_current_frame();
        let args = prev_frame.stack_n_last_elements(nbr_args);
        Frame::init_frame_post_alloc(frame_ptr.clone(), args, max_stack_size, prev_frame.clone());
        prev_frame.remove_n_last_elements(nbr_args);

        self.bytecode_idx = 0;
        self.current_bytecodes = frame_ptr.get_bytecode_ptr();
        self.current_frame = UnsafeCell::from(frame_ptr.clone());
        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method, with arguments provided.
    /// Used in primitives and corner cases like DNU calls.
    pub fn push_method_frame_with_args(&mut self, method: Gc<Method>, args: Vec<Value>, mutator: &mut GCInterface) -> Gc<Frame> {
        self.frame_method_root = method.clone();
        std::hint::black_box(&self.frame_method_root); // paranoia

        let (max_stack_size, nbr_locals) = match &*method {
            Method::Defined(m_env) => (m_env.max_stack_size as usize, m_env.nbr_locals),
            _ => unreachable!("if we're allocating a method frame, it has to be defined."),
        };

        let size = Frame::get_true_size(max_stack_size, args.len() as u8, nbr_locals);

        self.frame_args_root = Some(args);

        let mut frame_ptr: Gc<Frame> = mutator.request_memory_for_type(size, AllocSiteMarker::MethodFrameWithArgs);
        mutator.total_other_memory_size -= (max_stack_size * size_of::<Value>()) as u128;

        *frame_ptr = Frame::from_method(self.frame_method_root.clone());
        Frame::init_frame_post_alloc(
            frame_ptr.clone(),
            self.frame_args_root.as_ref().unwrap(),
            max_stack_size,
            self.get_current_frame(),
        );

        self.bytecode_idx = 0;
        self.current_bytecodes = frame_ptr.get_bytecode_ptr();
        self.current_frame = UnsafeCell::from(frame_ptr.clone());
        self.frame_args_root = None;

        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method.
    pub fn push_block_frame(&mut self, nbr_args: usize, mutator: &mut GCInterface) -> Gc<Frame> {
        let frame_ptr = Frame::alloc_from_block(nbr_args, self.get_current_frame_mut(), mutator);
        self.bytecode_idx = 0;
        self.current_bytecodes = frame_ptr.get_bytecode_ptr();
        self.current_frame = UnsafeCell::from(frame_ptr.clone());
        frame_ptr
    }

    pub fn pop_frame(&mut self) {
        // dbg!(self.get_current_frame().prev_frame.ptr);
        let new_current_frame = &self.get_current_frame().prev_frame;
        self.current_frame = UnsafeCell::from(new_current_frame.clone());
        match new_current_frame.is_empty() {
            true => {}
            false => {
                self.bytecode_idx = new_current_frame.bytecode_idx;
                self.current_bytecodes = new_current_frame.get_bytecode_ptr();
            }
        }
    }

    pub fn pop_n_frames(&mut self, n: u8) {
        let new_current_frame = &Frame::nth_frame_back_through_frame_list(&self.get_current_frame(), n + 1);
        self.current_frame = UnsafeCell::from(new_current_frame.clone());
        match new_current_frame.is_empty() {
            true => {}
            false => {
                self.bytecode_idx = new_current_frame.bytecode_idx;
                self.current_bytecodes = new_current_frame.get_bytecode_ptr();
            }
        }
    }

    pub fn run(&mut self, universe: &mut Universe) -> Option<Value> {
        loop {
            // Actually safe, there's always a reference to the current bytecodes. Need unsafe because we want to store a ref for quick access in perf-critical code
            let bytecode = *(unsafe { (*self.current_bytecodes).get_unchecked(self.bytecode_idx as usize) });

            // unsafe {
            //     dbg!(&(*self.current_frame.get()).current_context.class(universe).name);
            // }
            self.bytecode_idx += 1;

            // dbg!(&self.get_current_frame());
            // dbg!(&bytecode);

            // for the optional profiler macros not to be reported as warnings
            #[allow(clippy::let_unit_value)]
            match bytecode {
                Bytecode::Send1(symbol) => {
                    let _timing = profiler_maybe_start!("SEND");
                    resolve_method_and_send!(self, universe, symbol, 0);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Send2(symbol) => {
                    let _timing = profiler_maybe_start!("SEND");
                    resolve_method_and_send!(self, universe, symbol, 1);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Send3(symbol) => {
                    let _timing = profiler_maybe_start!("SEND");
                    resolve_method_and_send!(self, universe, symbol, 2);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::SendN(symbol) => {
                    let _timing = profiler_maybe_start!("SEND");
                    let nbr_args = nb_params(universe.lookup_symbol(symbol));
                    resolve_method_and_send!(self, universe, symbol, nbr_args);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushLocal(idx) => {
                    let _timing = profiler_maybe_start!("PUSH_LOCAL");
                    let value = *self.get_current_frame().lookup_local(idx as usize);
                    self.get_current_frame().stack_push(value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushNonLocal(up_idx, idx) => {
                    let _timing = profiler_maybe_start!("PUSHNONLOCAL");
                    debug_assert_ne!(up_idx, 0);
                    let from = Frame::nth_frame_back(&self.get_current_frame(), up_idx);
                    let value = *from.lookup_local(idx as usize);
                    self.get_current_frame().stack_push(value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushArg(idx) => {
                    let _timing = profiler_maybe_start!("PUSH_ARG");
                    debug_assert_ne!(idx, 0); // that's a ReturnSelf case.
                    let value = *self.get_current_frame().lookup_argument(idx as usize);
                    self.get_current_frame().stack_push(value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushNonLocalArg(up_idx, idx) => {
                    let _timing = profiler_maybe_start!("PUSH_NON_LOCAL_ARG");
                    debug_assert_ne!(up_idx, 0);
                    debug_assert_ne!((up_idx, idx), (0, 0)); // that's a ReturnSelf case.
                    let from = Frame::nth_frame_back(&self.get_current_frame(), up_idx);
                    let value = from.lookup_argument(idx as usize);
                    self.get_current_frame().stack_push(*value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushField(idx) => {
                    let _timing = profiler_maybe_start!("PUSH_FIELD");
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
                    self.get_current_frame().stack_push(val);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Dup => {
                    let _timing = profiler_maybe_start!("DUP");
                    let value = *self.get_current_frame().stack_last();
                    self.get_current_frame().stack_push(value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Inc => {
                    let _timing = profiler_maybe_start!("INC");
                    let mut current_frame = self.get_current_frame();
                    let last = current_frame.stack_last_mut();

                    if let Some(int) = last.as_integer() {
                        *last = Value::new_integer(int + 1);
                    } else if let Some(double) = last.as_double() {
                        *last = Value::new_double(double + 1.0);
                    } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                        *big_int += 1;
                    } else {
                        panic!("Invalid type in Inc")
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Dec => {
                    let _timing = profiler_maybe_start!("DEC");
                    let mut current_frame = self.get_current_frame();
                    let last = current_frame.stack_last_mut();

                    if let Some(int) = last.as_integer() {
                        *last = Value::new_integer(int - 1);
                    } else if let Some(double) = last.as_double() {
                        *last = Value::new_double(double - 1.0);
                    } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                        *big_int -= 1;
                    } else {
                        panic!("Invalid type in DEC")
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushBlock(idx) => {
                    let _timing = profiler_maybe_start!("PUSH_BLOCK");
                    let current_frame = self.get_current_frame();
                    let literal = current_frame.lookup_constant(idx as usize);
                    let mut block = match literal {
                        Literal::Block(blk) => {
                            let mut new_blk =
                                universe.gc_interface.request_memory_for_type::<Block>(std::mem::size_of::<Block>(), AllocSiteMarker::RuntimeBlock);
                            *new_blk = (**blk).clone();
                            new_blk
                        }
                        _ => panic!("PushBlock expected a block, but got another invalid literal"),
                    };
                    block.frame.replace(self.get_current_frame());
                    self.get_current_frame().stack_push(Value::Block(block));
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushConstant(idx) => {
                    let _timing = profiler_maybe_start!("PUSH_CONSTANT");
                    let current_frame = self.get_current_frame();
                    let literal = current_frame.lookup_constant(idx as usize);
                    let value = value_from_literal(literal, universe.gc_interface);
                    self.get_current_frame().stack_push(value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushGlobal(idx) => {
                    let _timing = profiler_maybe_start!("PUSH_GLOBAL");
                    if let Some(CacheEntry::Global(value)) = unsafe { self.get_current_frame().get_inline_cache_entry(self.bytecode_idx as usize) } {
                        let value = *value;
                        self.get_current_frame().stack_push(value);
                        continue;
                    }

                    let current_frame = self.get_current_frame();
                    let literal = current_frame.lookup_constant(idx as usize);
                    let symbol = match literal {
                        Literal::Symbol(sym) => sym,
                        _ => panic!("Global is not a symbol."),
                    };
                    if let Some(value) = universe.lookup_global(*symbol) {
                        self.get_current_frame().stack_push(value);
                        unsafe { *self.get_current_frame().get_inline_cache_entry(self.bytecode_idx as usize) = Some(CacheEntry::Global(value)) }
                    } else {
                        let self_value = self.get_current_frame().get_self();
                        universe.unknown_global(self, self_value, *symbol)?;
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Push0 => {
                    let _timing = profiler_maybe_start!("PUSH_0");
                    self.get_current_frame().stack_push(Value::INTEGER_ZERO);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Push1 => {
                    let _timing = profiler_maybe_start!("PUSH_1");
                    self.get_current_frame().stack_push(Value::INTEGER_ONE);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushNil => {
                    let _timing = profiler_maybe_start!("PUSH_NIL");
                    self.get_current_frame().stack_push(Value::NIL);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PushSelf => {
                    let _timing = profiler_maybe_start!("PUSH_SELF");
                    let self_val = *self.get_current_frame().lookup_argument(0);
                    self.get_current_frame().stack_push(self_val);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Pop => {
                    let _timing = profiler_maybe_start!("POP");
                    self.get_current_frame().stack_pop();
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PopLocal(up_idx, idx) => {
                    let _timing = profiler_maybe_start!("POP_LOCAL");
                    let value = self.get_current_frame().stack_pop();
                    let mut from = Frame::nth_frame_back(self.get_current_frame_mut(), up_idx);
                    from.assign_local(idx as usize, value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PopArg(up_idx, idx) => {
                    let _timing = profiler_maybe_start!("POP_ARG");
                    let value = self.get_current_frame().stack_pop();
                    let mut from = Frame::nth_frame_back(self.get_current_frame_mut(), up_idx);
                    from.assign_arg(idx as usize, value);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::PopField(idx) => {
                    let _timing = profiler_maybe_start!("POP_FIELD");
                    let value = self.get_current_frame().stack_pop();
                    let self_val = self.get_current_frame().get_self();
                    if let Some(instance) = self_val.as_instance() {
                        Instance::assign_field(&instance, idx as usize, value);
                    } else if let Some(cls) = self_val.as_class() {
                        cls.class().assign_field(idx as usize, value)
                    } else {
                        panic!("trying to assign a field to a {:?}?", &self_val)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::SuperSend(symbol) => {
                    let _timing = profiler_maybe_start!("SUPER_SEND");
                    let nb_params = {
                        let signature = universe.lookup_symbol(symbol);
                        nb_params(signature)
                    };

                    let method = {
                        // let method_with_holder = $frame.borrow().get_holding_method();
                        let holder = self.get_current_frame().get_method_holder();
                        //dbg!(&holder);
                        let super_class = holder.super_class().unwrap();
                        //dbg!(&super_class);
                        resolve_method(self.current_frame.get_mut(), &super_class, symbol, self.bytecode_idx)
                    };
                    do_send(self, universe, method, symbol, nb_params);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::ReturnSelf => {
                    let _timing = profiler_maybe_start!("RETURN_SELF");
                    let self_val = *self.get_current_frame().lookup_argument(0);
                    self.pop_frame();
                    self.get_current_frame().stack_push(self_val);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::ReturnLocal => {
                    let _timing = profiler_maybe_start!("RETURN_LOCAL");
                    let val = self.get_current_frame().stack_pop();
                    self.pop_frame();
                    if self.get_current_frame().is_empty() {
                        profiler_maybe_stop!(_timing);
                        return Some(val);
                    }
                    self.get_current_frame().stack_push(val);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::ReturnNonLocal(up_idx) => {
                    let _timing = profiler_maybe_start!("RETURN_NON_LOCAL");
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
                        let val = self.get_current_frame().stack_pop();
                        self.pop_n_frames(count + 1);
                        self.get_current_frame().stack_push(val);
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
                        self.get_current_frame().bytecode_idx = self.bytecode_idx;

                        universe
                            .escaped_block(self, instance, block)
                            .expect("A block has escaped and `escapedBlock:` is not defined on receiver");
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Dup2 => {
                    let _timing = profiler_maybe_start!("DUP2");
                    let second_to_last = *self.get_current_frame().stack_nth_back(1);
                    self.get_current_frame().stack_push(second_to_last);
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::Jump(offset) => {
                    let _timing = profiler_maybe_start!("JUMP");
                    self.bytecode_idx += offset - 1; // minus one because it gets incremented by one already every loop;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpBackward(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_BACKWARD");
                    self.bytecode_idx -= offset + 1;
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpOnTrueTopNil(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_TRUE_TOP_NIL");
                    let mut current_frame = self.get_current_frame();
                    let condition_result = current_frame.stack_last_mut();

                    if condition_result.is_boolean_true() {
                        self.bytecode_idx += offset - 1;
                        *condition_result = Value::NIL;
                    } else if condition_result.is_boolean_false() {
                        self.get_current_frame().stack_pop();
                    } else {
                        panic!("JumpOnTrueTopNil condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpOnFalseTopNil(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_FALSE_TOP_NIL");
                    let mut current_frame = self.get_current_frame();
                    let condition_result = current_frame.stack_last_mut();

                    if condition_result.is_boolean_true() {
                        self.get_current_frame().stack_pop();
                    } else if condition_result.is_boolean_false() {
                        self.bytecode_idx += offset - 1;
                        *condition_result = Value::NIL;
                    } else {
                        panic!("JumpOnFalseTopNil condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpOnTruePop(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_TRUE_POP");
                    let condition_result = self.get_current_frame().stack_pop();

                    if condition_result.is_boolean_true() {
                        self.bytecode_idx += offset - 1;
                    } else if condition_result.is_boolean_false() {
                        // pass
                    } else {
                        panic!("JumpOnTruePop condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpOnFalsePop(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_FALSE_POP");
                    let condition_result = self.get_current_frame().stack_pop();

                    if condition_result.is_boolean_false() {
                        self.bytecode_idx += offset - 1;
                    } else if condition_result.is_boolean_true() {
                        // pass
                    } else {
                        panic!("JumpOnFalsePop condition did not evaluate to boolean (was {:?})", condition_result)
                    };
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpIfGreater(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_IF_GREATER");
                    let current_frame = self.get_current_frame();
                    let top = current_frame.stack_last();
                    let top2 = current_frame.stack_nth_back(1);

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
                        self.get_current_frame().remove_n_last_elements(2);
                        self.bytecode_idx += offset - 1;
                    }
                }
                Bytecode::JumpOnNilTopTop(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NIL_TOP_TOP");
                    let current_frame = self.get_current_frame();
                    let condition_result = current_frame.stack_last();

                    if condition_result.is_nil() {
                        self.bytecode_idx += offset - 1;
                    } else {
                        self.get_current_frame().stack_pop();
                    }
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpOnNotNilTopTop(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_TOP_TOP");
                    let current_frame = self.get_current_frame();
                    let condition_result = current_frame.stack_last();

                    if !condition_result.is_nil() {
                        self.bytecode_idx += offset - 1;
                    } else {
                        self.get_current_frame().stack_pop();
                    }
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpOnNilPop(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NIL_POP");
                    let condition_result = self.get_current_frame().stack_pop();

                    if condition_result.is_nil() {
                        self.bytecode_idx += offset - 1;
                    } else {
                        // pass
                    }
                    profiler_maybe_stop!(_timing);
                }
                Bytecode::JumpOnNotNilPop(offset) => {
                    let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_POP");
                    let condition_result = self.get_current_frame().stack_pop();

                    if !condition_result.is_nil() {
                        self.bytecode_idx += offset - 1;
                    } else {
                        // pass
                    }
                    profiler_maybe_stop!(_timing);
                }
            }
        }

        pub fn do_send(interpreter: &mut Interpreter, universe: &mut Universe, method: Option<Gc<Method>>, symbol: Interned, nb_params: usize) {
            // we store the current bytecode idx to be able to correctly restore the bytecode state when we pop frames
            interpreter.get_current_frame().bytecode_idx = interpreter.bytecode_idx;

            let Some(method) = method else {
                let frame_copy = interpreter.get_current_frame();
                let args = frame_copy.stack_n_last_elements(nb_params);
                interpreter.get_current_frame().remove_n_last_elements(nb_params);
                let self_value = interpreter.get_current_frame().clone().stack_pop();

                // could be avoided by passing args slice directly...
                // ...but A) DNU is a very rare path and B) i guess we allocate a new args arr in the DNU call anyway
                let args = args.to_vec();

                universe
                    .does_not_understand(interpreter, self_value, symbol, args)
                    .expect("A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver");

                return;
            };

            match &*method {
                Method::Defined(_) => {
                    //let name = &method.holder().name.clone();
                    //eprintln!("Invoking {:?} (in {:?})", &method.signature(), &name);
                    interpreter.push_method_frame(method, nb_params + 1, universe.gc_interface);
                }
                Method::Primitive(func, _met_info) => {
                    //eprintln!("Invoking prim {:?} (in {:?})", &_met_info.signature, &_met_info.holder.name);

                    // dbg!(interpreter.current_frame);
                    func(interpreter, universe, nb_params + 1)
                        .with_context(|| anyhow::anyhow!("error calling primitive `{}`", universe.lookup_symbol(symbol)))
                        .unwrap();
                }
                Method::TrivialGlobal(met, _) => met.invoke(universe, interpreter),
                Method::TrivialLiteral(met, _) => {
                    interpreter.get_current_frame().stack_pop(); // remove the receiver
                    met.invoke(universe, interpreter)
                }
                Method::TrivialGetter(met, _) => met.invoke(universe, interpreter),
                Method::TrivialSetter(met, _) => met.invoke(universe, interpreter),
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

        fn nb_params(signature: &str) -> usize {
            match signature.chars().next() {
                Some(ch) if !ch.is_alphabetic() => 1,
                _ => signature.chars().filter(|ch| *ch == ':').count(),
            }
        }
    }
}
