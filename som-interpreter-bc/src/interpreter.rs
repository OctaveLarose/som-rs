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

use crate::{stack_drop, stack_last, stack_n_last_elements, stack_nth_back, stack_pop, stack_push, stack_truncate};
use num_bigint::BigInt;
use som_core::bytecode::{BC_SIZE_1_ARG, BC_SIZE_2_ARG, BC_SIZE_NO_ARGS, BC_SIZE_U16_ARG};
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};
use som_gc::gcref::Gc;
use som_value::interned::Interned;
use std::time::Instant;

macro_rules! resolve_method_and_send {
    ($self:expr, $universe:expr, $frame:expr, $symbol:expr, $nbr_args:expr, $bc_idx:expr) => {{
        let receiver = stack_nth_back!($self.sp, $nbr_args - 1);
        let receiver_class = receiver.class($universe);
        let method = resolve_method(&mut $frame, &receiver_class, $symbol, $bc_idx);
        $bc_idx += BC_SIZE_U16_ARG; // always this size for a send
        $self.bytecode_idx = $bc_idx as u16;
        do_send($self, $universe, method, $symbol, $nbr_args);
    }};
}

type HandlerFn = fn(&mut Interpreter, &mut Universe, &[u8], Gc<Frame>, usize) -> Option<Value>;

macro_rules! dispatch_next {
    ($JUMP_TABLE:expr, $interpreter:expr, $universe:expr, $bytecodes:expr, $frame:expr, $bc_idx:expr) => {
        become (*unsafe { $JUMP_TABLE.get_unchecked(*$bytecodes.get_unchecked($bc_idx) as usize) })(
            $interpreter,
            $universe,
            $bytecodes,
            $frame,
            $bc_idx,
        )
    };
}

macro_rules! log_bc {
    ($bytecodes:expr, $bc_idx:expr) => {
        // let _var = Bytecode::padded_name($bytecodes[$bc_idx]);
        //         dbg!(_var);
    };
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

macro_rules! read_u16_fast {
    ($var_name:ident, $bytecodes:expr, $idx:expr) => {
        let $var_name: u16 = unsafe { u16::from_le_bytes($bytecodes.get_unchecked($idx..$idx + 2).try_into().unwrap_unchecked()) };
    };
}

macro_rules! update_frame_and_bytecodes {
    ($interp:expr, $frame:expr, $bytecodes:expr) => {
        $frame = $interp.get_current_frame();
        $bytecodes = unsafe { &*($frame.get_bytecodes() as *const Vec<u8>) };
    };
}

pub struct Interpreter {
    /// The time record of the interpreter's creation.
    pub start_time: Instant,
    /// The current bytecode index.
    pub bytecode_idx: u16,
    /// The current frame.
    pub current_frame: UnsafeCell<Gc<Frame>>,
    /// Pointer to top of stack
    pub sp: *mut Value,
    /// Pointer to base of stack
    pub base_sp: *mut Value,
    /// GC can trigger when the interpreter wants to allocate a new frame.
    /// We're then in a situation where we've looked up a `Method` (which is how we knew we were dealing with a non-primitive, and so that we had to create a frame)
    /// So this method can't be stored on the Rust stack, or GC would miss it. Therefore: we keep it reachable there.
    pub frame_method_root: Gc<MethodInfo>,
    pub frame_args_root: Option<Vec<Value>>,
}

pub const STACK_SIZE: usize = 2 << 20; // 2 MiB

impl Interpreter {
    pub fn new(base_frame: Gc<Frame>) -> Self {
        let stack_ptr = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                STACK_SIZE,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut Value
        };

        let mut interp = Self {
            start_time: Instant::now(),
            bytecode_idx: 0,
            current_frame: UnsafeCell::from(base_frame),
            frame_method_root: Gc::default(),
            frame_args_root: None,
            sp: stack_ptr,
            base_sp: stack_ptr,
        };

        stack_push!(interp.sp, Value::STACK_MARKER);

        interp
    }

    /// Return the current frame.
    /// It's in an `UnsafeCell` for moving GC reasons: you get many bugs by using Gc<Frame> by
    /// itself, since Rust assumes that it hasn't moved when it has
    pub fn get_current_frame(&self) -> Gc<Frame> {
        unsafe { (*self.current_frame.get()).clone() }
    }

    /// Creates and allocates a new frame corresponding to a method.
    /// nbr_args is the number of arguments, including the self value, which it takes from the stack.
    pub fn push_method_frame(&mut self, method: Gc<MethodInfo>, nbr_args: usize, mutator: &mut GCInterface) -> Gc<Frame> {
        // we store the current bytecode idx to be able to correctly restore the bytecode state when we pop frames
        self.get_current_frame().bytecode_idx = self.bytecode_idx;

        self.frame_method_root = method.clone();
        std::hint::black_box(&self.frame_method_root); // paranoia

        let size = Frame::get_true_size(nbr_args as u8, method.nbr_locals);

        let mut frame_ptr: Gc<Frame> = mutator.request_memory_for_type(size, AllocSiteMarker::MethodFrame);

        *frame_ptr = Frame::new(self.frame_method_root.clone(), self.get_current_frame());

        Frame::init_frame_args_locals_from_stack(&mut frame_ptr, &mut self.sp, nbr_args);

        self.bytecode_idx = 0;
        self.current_frame = UnsafeCell::from(frame_ptr.clone());

        stack_push!(self.sp, Value::STACK_MARKER);
        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method, with arguments provided.
    /// Used in primitives and corner cases like DNU calls.
    pub fn push_method_frame_with_args(&mut self, method: Gc<MethodInfo>, args: Vec<Value>, mutator: &mut GCInterface) -> Gc<Frame> {
        self.get_current_frame().bytecode_idx = self.bytecode_idx;

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

        stack_push!(self.sp, Value::STACK_MARKER);

        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method.
    pub fn push_block_frame(&mut self, nbr_args: usize, mutator: &mut GCInterface) -> Gc<Frame> {
        self.get_current_frame().bytecode_idx = self.bytecode_idx;

        // This used to be a function defined in the frame module, but its logic is tightly coupled with the interpreter. As in, it needs access to the stack, and the mutator.
        // NB: Also, inlining it here helps write moving-GC-proof code, since instead of passing references to the interpreter's internals (which would be moved by potential GC triggers), we can query those internals ourselves and ensure we get them from the source, where they would have been moved correctly.
        // A good example is querying the "current" (now parent) frame from the interpreter *after* allocating memory for the new frame, which was an issue when this was a non-inlined function since we then needed to pass a reference to the frame *before* allocating memory, as a function argument, and then we had to be very careful not to get bugs.
        let frame_ptr = {
            let size = {
                let nbr_locals = {
                    let block_value = stack_nth_back!(self.sp, nbr_args - 1);
                    let block = block_value.as_block().unwrap();
                    block.blk_info.nbr_locals
                };
                Frame::get_true_size(nbr_args as u8, nbr_locals)
            };

            let mut frame_ptr: Gc<Frame> = mutator.request_memory_for_type(size, AllocSiteMarker::BlockFrame);

            let block_value = *stack_nth_back!(self.sp, nbr_args - 1);

            *frame_ptr = Frame::new(block_value.as_block().unwrap().blk_info.clone(), self.get_current_frame());

            Frame::init_frame_args_locals_from_stack(&mut frame_ptr, &mut self.sp, nbr_args);

            frame_ptr
        };

        stack_push!(self.sp, Value::STACK_MARKER);
        self.bytecode_idx = 0;
        self.current_frame = UnsafeCell::from(frame_ptr.clone());
        frame_ptr
    }

    pub fn pop_frame(&mut self) {
        let new_current_frame = &self.get_current_frame().prev_frame;

        while stack_pop!(self.sp) != Value::STACK_MARKER {}

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
            while stack_pop!(self.sp) != Value::STACK_MARKER {}
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

    #[allow(clippy::let_unit_value)] // for the profiling
    #[allow(unused_assignments)] // sometimes we modify bytecode
    pub fn run(&mut self, universe: &mut Universe) -> Option<Value> {
        static JUMP_TABLE: [HandlerFn; 39] = [
            handle_dup,
            handle_push_local,
            handle_push_non_local,
            handle_push_arg,
            handle_push_non_local_arg,
            handle_push_field,
            handle_push_block,
            handle_push_constant,
            handle_push_global,
            handle_inc,
            handle_dec,
            handle_push_0,
            handle_push_1,
            handle_push_nil,
            handle_push_self,
            handle_pop,
            handle_pop_local,
            handle_pop_arg,
            handle_pop_field,
            handle_send_1,
            handle_send_2,
            handle_send_3,
            handle_send_n,
            handle_super_send,
            handle_return_self,
            handle_return_local,
            handle_return_non_local,
            handle_jump,
            handle_jump_backward,
            handle_jump_on_true_top_nil,
            handle_jump_on_false_top_nil,
            handle_jump_on_nil_top_top,
            handle_jump_on_not_nil_top_top,
            handle_jump_on_nil_pop,
            handle_jump_on_not_nil_pop,
            handle_jump_on_true_pop,
            handle_jump_on_false_pop,
            handle_jump_if_greater,
            handle_dup_2,
        ];

        fn handle_send_1(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("SEND");
            read_u16_fast!(val, bytecodes, bc_idx + 1);
            let symbol: Interned = Interned(val);
            resolve_method_and_send!(interpreter, universe, frame, symbol, 1, bc_idx);
            update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_send_2(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("SEND");
            read_u16_fast!(val, bytecodes, bc_idx + 1);
            let symbol: Interned = Interned(val);
            resolve_method_and_send!(interpreter, universe, frame, symbol, 2, bc_idx);
            update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_send_3(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("SEND");
            read_u16_fast!(val, bytecodes, bc_idx + 1);
            let symbol: Interned = Interned(val);
            resolve_method_and_send!(interpreter, universe, frame, symbol, 3, bc_idx);
            update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_send_n(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("SEND");
            read_u16_fast!(val, bytecodes, bc_idx + 1);
            let symbol: Interned = Interned(val);
            let nbr_args = nbr_args(universe.lookup_symbol(symbol));
            resolve_method_and_send!(interpreter, universe, frame, symbol, nbr_args, bc_idx);
            update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_push_local(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_LOCAL");
            let idx: u8 = bytecodes[bc_idx + 1];
            let value = *frame.lookup_local(idx as usize);
            stack_push!(interpreter.sp, value);
            bc_idx += BC_SIZE_1_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_non_local(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSHNONLOCAL");
            let up_idx: u8 = bytecodes[bc_idx + 1];
            let idx: u8 = bytecodes[bc_idx + 2];
            debug_assert_ne!(up_idx, 0);
            let from = Frame::nth_frame_back(&frame, up_idx);
            let value = *from.lookup_local(idx as usize);
            stack_push!(interpreter.sp, value);
            bc_idx += BC_SIZE_2_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_arg(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_ARG");
            let idx: u8 = bytecodes[bc_idx + 1];
            debug_assert_ne!(idx, 0); // that's a ReturnSelf case.
            let value = *frame.lookup_argument(idx as usize);
            stack_push!(interpreter.sp, value);
            bc_idx += BC_SIZE_1_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_non_local_arg(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_NON_LOCAL_ARG");
            let up_idx: u8 = bytecodes[bc_idx + 1];
            let idx: u8 = bytecodes[bc_idx + 2];
            debug_assert_ne!(up_idx, 0);
            debug_assert_ne!((up_idx, idx), (0, 0)); // that's a ReturnSelf case.
            let from = Frame::nth_frame_back(&frame, up_idx);
            let value = from.lookup_argument(idx as usize);
            stack_push!(interpreter.sp, *value);
            bc_idx += BC_SIZE_2_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_field(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_FIELD");
            let idx: u8 = bytecodes[bc_idx + 1];
            let self_val = frame.get_self();
            let val = {
                if let Some(instance) = self_val.as_instance() {
                    *Instance::lookup_field(&instance, idx as usize)
                } else if let Some(cls) = self_val.as_class() {
                    cls.class().lookup_field(idx as usize)
                } else {
                    panic!("trying to read a field from a {:?}?", &self_val)
                }
            };
            stack_push!(interpreter.sp, val);
            bc_idx += BC_SIZE_1_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_dup(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("DUP");
            let value = *stack_last!(interpreter.sp);
            stack_push!(interpreter.sp, value);
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_inc(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("INC");
            let last = stack_last!(interpreter.sp);

            if let Some(int) = last.as_integer() {
                *last = Value::new_integer(int + 1);
            } else if let Some(double) = last.as_double() {
                *last = Value::new_double(double + 1.0);
            } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                *big_int += 1;
            } else {
                panic!("Invalid type in Inc")
            };
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_dec(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("DEC");
            let last = stack_last!(interpreter.sp);

            if let Some(int) = last.as_integer() {
                *last = Value::new_integer(int - 1);
            } else if let Some(double) = last.as_double() {
                *last = Value::new_double(double - 1.0);
            } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                *big_int -= 1;
            } else {
                panic!("Invalid type in DEC")
            };
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_block(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_BLOCK");
            let idx: u8 = bytecodes[bc_idx + 1];

            // allocating ahead of time in case it triggers GC.
            let mut new_blk = universe.gc_interface.request_memory_for_type::<Block>(std::mem::size_of::<Block>(), AllocSiteMarker::RuntimeBlock);

            match frame.lookup_constant(idx as usize) {
                Literal::Block(blk) => {
                    new_blk.blk_info = (*blk).clone();
                    new_blk.frame = frame.clone();
                }
                _ => panic!("PushBlock expected a block, but got another invalid literal"),
            }

            stack_push!(interpreter.sp, Value::Block(new_blk));
            bc_idx += BC_SIZE_1_ARG;

            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_constant(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_CONSTANT");
            let idx: u8 = bytecodes[bc_idx + 1];

            let literal = frame.lookup_constant(idx as usize);
            let value = value_from_literal(literal, &mut universe.gc_interface);
            stack_push!(interpreter.sp, value);
            bc_idx += BC_SIZE_1_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_global(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_GLOBAL");

            if let Some(CacheEntry::Global(value)) = unsafe { frame.get_inline_cache_entry(bc_idx) } {
                let value = *value;
                stack_push!(interpreter.sp, value);
                bc_idx += BC_SIZE_1_ARG;
                profiler_maybe_stop!(_timing);
            } else {
                let idx: u8 = bytecodes[bc_idx + 1];
                let literal = frame.lookup_constant(idx as usize);
                let symbol = match literal {
                    Literal::Symbol(sym) => sym,
                    _ => panic!("Global is not a symbol."),
                };
                if let Some(value) = universe.lookup_global(*symbol) {
                    stack_push!(interpreter.sp, value);
                    unsafe { *frame.get_inline_cache_entry(bc_idx) = Some(CacheEntry::Global(value)) }
                    bc_idx += BC_SIZE_1_ARG;
                } else {
                    bc_idx += BC_SIZE_1_ARG;
                    interpreter.bytecode_idx = bc_idx as u16;
                    let self_value = frame.get_self();
                    universe.unknown_global(interpreter, self_value, *symbol)?;
                    update_frame_and_bytecodes!(interpreter, frame, bytecodes);
                    bc_idx = interpreter.bytecode_idx as usize;
                };
            }
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_0(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_0");
            stack_push!(interpreter.sp, Value::INTEGER_ZERO);
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_1(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_1");
            stack_push!(interpreter.sp, Value::INTEGER_ONE);
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_nil(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_NIL");
            stack_push!(interpreter.sp, Value::NIL);
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_push_self(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("PUSH_SELF");
            let self_val = *frame.lookup_argument(0);
            stack_push!(interpreter.sp, self_val);
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_pop(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("POP");
            stack_drop!(interpreter.sp);
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_pop_local(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("POP_LOCAL");
            let up_idx: u8 = bytecodes[bc_idx + 1];
            let idx: u8 = bytecodes[bc_idx + 2];
            let value = stack_pop!(interpreter.sp);
            let mut from = Frame::nth_frame_back(&frame, up_idx);
            from.assign_local(idx as usize, value);
            bc_idx += BC_SIZE_2_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_pop_arg(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("POP_ARG");
            let up_idx: u8 = bytecodes[bc_idx + 1];
            let idx: u8 = bytecodes[bc_idx + 2];
            let value = stack_pop!(interpreter.sp);
            let mut from = Frame::nth_frame_back(&frame, up_idx);
            from.assign_arg(idx as usize, value);
            bc_idx += BC_SIZE_2_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_pop_field(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("POP_FIELD");
            let idx: u8 = bytecodes[bc_idx + 1];
            let value = stack_pop!(interpreter.sp);
            let self_val = frame.get_self();
            if let Some(instance) = self_val.as_instance() {
                Instance::assign_field(&instance, idx as usize, value);
            } else if let Some(cls) = self_val.as_class() {
                cls.class().assign_field(idx as usize, value)
            } else {
                panic!("trying to assign a field to a {:?}?", &self_val)
            };
            bc_idx += BC_SIZE_1_ARG;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_super_send(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("SUPER_SEND");
            read_u16_fast!(val, bytecodes, bc_idx + 1);
            let symbol: Interned = Interned(val);
            let nbr_args = {
                let signature = universe.lookup_symbol(symbol);
                nbr_args(signature)
            };

            let method = {
                let holder = frame.get_method_holder();
                let super_class = holder.super_class().unwrap();
                resolve_method(interpreter.current_frame.get_mut(), &super_class, symbol, bc_idx)
            };
            interpreter.bytecode_idx = (bc_idx + BC_SIZE_2_ARG) as u16;
            do_send(interpreter, universe, method, symbol, nbr_args);
            update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_return_self(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            _bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, _bc_idx);
            let _timing = profiler_maybe_start!("RETURN_SELF");
            let self_val = *frame.lookup_argument(0);
            interpreter.pop_frame();
            stack_push!(interpreter.sp, self_val);
            update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_return_local(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            _bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, _bc_idx);
            let _timing = profiler_maybe_start!("RETURN_LOCAL");
            let value = stack_pop!(interpreter.sp);
            interpreter.pop_frame();
            if interpreter.get_current_frame().is_empty() {
                profiler_maybe_stop!(_timing);
                return Some(value);
            }
            stack_push!(interpreter.sp, value);
            update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_return_non_local(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            mut bytecodes: &[u8],
            mut frame: Gc<Frame>,
            bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("RETURN_NON_LOCAL");
            let up_idx: u8 = bytecodes[bc_idx + 1];
            let method_frame = Frame::nth_frame_back(&frame, up_idx);

            let escaped_frames_nbr = {
                let mut current_frame = frame.clone();
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
                let value = stack_pop!(interpreter.sp);
                interpreter.pop_n_frames(count + 1);
                stack_push!(interpreter.sp, value);
                update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            } else {
                // Block has escaped its method frame.
                let instance = frame.get_self();
                let block = match frame.lookup_argument(0).as_block() {
                    Some(block) => block,
                    _ => {
                        // Should never happen, because `universe.current_frame()` would
                        // have been equal to `universe.current_method_frame()`.
                        panic!("A method frame has escaped itself ??");
                    }
                };

                interpreter.bytecode_idx = (bc_idx + BC_SIZE_1_ARG) as u16;

                universe
                    .escaped_block(interpreter, instance, block)
                    .expect("A block has escaped and `escapedBlock:` is not defined on receiver");
                update_frame_and_bytecodes!(interpreter, frame, bytecodes);
            };

            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, interpreter.bytecode_idx as usize);
        }

        fn handle_dup_2(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("DUP2");
            let second_to_last = stack_nth_back!(interpreter.sp, 1);
            stack_push!(interpreter.sp, *second_to_last);
            bc_idx += BC_SIZE_NO_ARGS;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            bc_idx += offset as usize;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_backward(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_BACKWARD");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            bc_idx -= offset as usize;
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_true_top_nil(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_TRUE_TOP_NIL");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_last!(interpreter.sp);

            if condition_result.is_boolean_true() {
                bc_idx += offset as usize;
                *condition_result = Value::NIL;
            } else if condition_result.is_boolean_false() {
                stack_drop!(interpreter.sp);
                bc_idx += BC_SIZE_U16_ARG;
            } else {
                panic!("JumpOnTrueTopNil condition did not evaluate to boolean (was {:?})", condition_result)
            };
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_false_top_nil(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_FALSE_TOP_NIL");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_last!(interpreter.sp);

            if condition_result.is_boolean_true() {
                stack_drop!(interpreter.sp);
                bc_idx += BC_SIZE_U16_ARG;
            } else if condition_result.is_boolean_false() {
                bc_idx += offset as usize;
                *condition_result = Value::NIL;
            } else {
                panic!("JumpOnFalseTopNil condition did not evaluate to boolean (was {:?})", condition_result)
            };
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_true_pop(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_TRUE_POP");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_pop!(interpreter.sp);

            if condition_result.is_boolean_true() {
                bc_idx += offset as usize;
            } else if condition_result.is_boolean_false() {
                bc_idx += BC_SIZE_U16_ARG;
                // pass
            } else {
                panic!("JumpOnTruePop condition did not evaluate to boolean (was {:?})", condition_result)
            };
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_false_pop(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_FALSE_POP");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_pop!(interpreter.sp);

            if condition_result.is_boolean_false() {
                bc_idx += offset as usize;
            } else if condition_result.is_boolean_true() {
                bc_idx += BC_SIZE_U16_ARG;
                // pass
            } else {
                panic!("JumpOnFalsePop condition did not evaluate to boolean (was {:?})", condition_result)
            };
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_if_greater(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let top = stack_last!(interpreter.sp);
            let top2 = stack_nth_back!(interpreter.sp, 1);

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
                stack_drop!(interpreter.sp);
                stack_drop!(interpreter.sp);
                bc_idx += offset as usize;
            } else {
                bc_idx += BC_SIZE_U16_ARG;
            }
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_nil_top_top(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_NIL_TOP_TOP");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_last!(interpreter.sp);

            if condition_result.is_nil() {
                bc_idx += offset as usize;
            } else {
                stack_drop!(interpreter.sp);
                bc_idx += BC_SIZE_U16_ARG;
            }
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_not_nil_top_top(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_TOP_TOP");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_last!(interpreter.sp);

            if !condition_result.is_nil() {
                bc_idx += offset as usize;
            } else {
                stack_drop!(interpreter.sp);
                bc_idx += BC_SIZE_U16_ARG;
            }
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_nil_pop(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_NIL_POP");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_pop!(interpreter.sp);

            if condition_result.is_nil() {
                bc_idx += offset as usize;
            } else {
                bc_idx += BC_SIZE_U16_ARG;
                // pass
            }
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        fn handle_jump_on_not_nil_pop(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            bytecodes: &[u8],
            frame: Gc<Frame>,
            mut bc_idx: usize,
        ) -> Option<Value> {
            log_bc!(bytecodes, bc_idx);
            let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_POP");
            read_u16_fast!(offset, bytecodes, bc_idx + 1);
            let condition_result = stack_pop!(interpreter.sp);

            if !condition_result.is_nil() {
                bc_idx += offset as usize;
            } else {
                bc_idx += BC_SIZE_U16_ARG;
                // pass
            }
            profiler_maybe_stop!(_timing);
            dispatch_next!(JUMP_TABLE, interpreter, universe, bytecodes, frame, bc_idx);
        }

        let frame = self.get_current_frame();
        let bytecodes = unsafe { &*(frame.get_bytecodes() as *const Vec<u8>) };
        let bc_idx = self.bytecode_idx as usize;

        return JUMP_TABLE[bytecodes[bc_idx] as usize](self, universe, bytecodes, frame, bc_idx);

        pub fn do_send(interpreter: &mut Interpreter, universe: &mut Universe, method: Option<Gc<Method>>, symbol: Interned, nbr_args: usize) {
            let Some(method) = method else {
                let args = stack_n_last_elements!(interpreter.sp, nbr_args - 1);
                stack_truncate!(interpreter.sp, nbr_args - 1);
                let self_value = stack_pop!(interpreter.sp);

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

                    // dbg!(stack_nth_back!(interpreter.sp, 1));
                    // dbg!(stack_nth_back!(interpreter.sp, 2));

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
                    stack_drop!(interpreter.sp); // remove the receiver
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

        fn resolve_method(frame: &mut Gc<Frame>, class: &Gc<Class>, signature: Interned, bytecode_idx: usize) -> Option<Gc<Method>> {
            // SAFETY: this access is actually safe because the bytecode compiler
            // makes sure the cache has as many entries as there are bytecode instructions,
            // therefore we can avoid doing any redundant bounds checks here.
            let maybe_found = unsafe { frame.get_inline_cache_entry(bytecode_idx) };

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
