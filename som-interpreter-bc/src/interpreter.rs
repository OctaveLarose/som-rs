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

#[allow(unused_imports)]
use som_core::bytecode::{Bytecode, BC_SIZE_1_ARG, BC_SIZE_2_ARG, BC_SIZE_NO_ARGS, BC_SIZE_U16_ARG};

use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcref::Gc;
use som_value::interned::Interned;

#[cfg(not(feature = "switch-dispatch"))]
macro_rules! resolve_method_and_send {
    ($universe:expr, $frame:expr, $symbol:expr, $nbr_args:expr, $bc_idx:expr) => {{
        let receiver = stack_nth_back!($universe.sp, $nbr_args - 1);
        let receiver_class = receiver.class($universe);
        let method = resolve_method(&mut $frame, &receiver_class, $symbol, $bc_idx);
        $bc_idx += BC_SIZE_U16_ARG; // always this size for a send
        $universe.bytecode_idx = $bc_idx as u16;
        do_send($universe, method, $symbol, $nbr_args);
    }};
}

#[cfg(not(feature = "switch-dispatch"))]
type HandlerFn = fn(&mut Universe, &[u8], Gc<Frame>, usize) -> Option<Value>;

#[cfg(not(feature = "switch-dispatch"))]
macro_rules! dispatch_next {
    ($JUMP_TABLE:expr, $universe:expr, $bytecodes:expr, $frame:expr, $bc_idx:expr) => {
        become (*unsafe { $JUMP_TABLE.get_unchecked(*$bytecodes.get_unchecked($bc_idx) as usize) })($universe, $bytecodes, $frame, $bc_idx)
    };
}

#[cfg(not(feature = "switch-dispatch"))]
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
    ($universe:expr, $frame:expr, $bytecodes:expr) => {
        $frame = $universe.get_current_frame();
        $bytecodes = unsafe { &*($frame.get_bytecodes() as *const Vec<u8>) };
    };
}

/// Creates and allocates a new frame corresponding to a method.
/// nbr_args is the number of arguments, including the self value, which it takes from the stack.
pub fn push_method_frame(universe: &mut Universe, method: Gc<MethodInfo>, nbr_args: usize) -> Gc<Frame> {
    // we store the current bytecode idx to be able to correctly restore the bytecode state when we pop frames
    universe.get_current_frame().bytecode_idx = universe.bytecode_idx;

    universe.frame_method_root = method.clone();
    std::hint::black_box(&universe.frame_method_root); // paranoia

    let size = Frame::get_true_size(nbr_args as u8, method.nbr_locals);

    let mut frame_ptr: Gc<Frame> = universe.gc_interface.request_memory_for_type(size, AllocSiteMarker::MethodFrame);

    *frame_ptr = Frame::new(universe.frame_method_root.clone(), universe.get_current_frame());

    Frame::init_frame_args_locals_from_stack(&mut frame_ptr, &mut universe.sp, nbr_args);

    universe.bytecode_idx = 0;
    universe.current_frame = UnsafeCell::from(frame_ptr.clone());

    stack_push!(universe.sp, Value::STACK_MARKER);
    frame_ptr
}

/// Creates and allocates a new frame corresponding to a method, with arguments provided.
/// Used in primitives and corner cases like DNU calls.
pub fn push_method_frame_with_args(universe: &mut Universe, method: Gc<MethodInfo>, args: Vec<Value>) -> Gc<Frame> {
    universe.get_current_frame().bytecode_idx = universe.bytecode_idx;

    universe.frame_method_root = method.clone();
    std::hint::black_box(&universe.frame_method_root); // paranoia

    let nbr_locals = method.nbr_locals;

    let size = Frame::get_true_size(args.len() as u8, nbr_locals);

    universe.frame_args_root = Some(args);

    let mut frame_ptr: Gc<Frame> = universe.gc_interface.request_memory_for_type(size, AllocSiteMarker::MethodFrameWithArgs);

    *frame_ptr = Frame::new(universe.frame_method_root.clone(), universe.get_current_frame());
    Frame::init_frame_args_locals(&mut frame_ptr, universe.frame_args_root.as_ref().unwrap());

    universe.bytecode_idx = 0;
    universe.current_frame = UnsafeCell::from(frame_ptr.clone());
    universe.frame_args_root = None;

    stack_push!(universe.sp, Value::STACK_MARKER);

    frame_ptr
}

/// Creates and allocates a new frame corresponding to a method.
pub fn push_block_frame(universe: &mut Universe, nbr_args: usize) -> Gc<Frame> {
    universe.get_current_frame().bytecode_idx = universe.bytecode_idx;

    // This used to be a function defined in the frame module, but its logic is tightly coupled with the interpreter. As in, it needs access to the stack, and the mutator.
    // NB: Also, inlining it here helps write moving-GC-proof code, since instead of passing references to the interpreter's internals (which would be moved by potential GC triggers), we can query those internals ourselves and ensure we get them from the source, where they would have been moved correctly.
    // A good example is querying the "current" (now parent) frame from the interpreter *after* allocating memory for the new frame, which was an issue when this was a non-inlined function since we then needed to pass a reference to the frame *before* allocating memory, as a function argument, and then we had to be very careful not to get bugs.
    let frame_ptr = {
        let size = {
            let nbr_locals = {
                let block_value = stack_nth_back!(universe.sp, nbr_args - 1);
                let block = block_value.as_block().unwrap();
                block.blk_info.nbr_locals
            };
            Frame::get_true_size(nbr_args as u8, nbr_locals)
        };

        let mut frame_ptr: Gc<Frame> = universe.gc_interface.request_memory_for_type(size, AllocSiteMarker::BlockFrame);

        let block_value = *stack_nth_back!(universe.sp, nbr_args - 1);

        *frame_ptr = Frame::new(block_value.as_block().unwrap().blk_info.clone(), universe.get_current_frame());

        Frame::init_frame_args_locals_from_stack(&mut frame_ptr, &mut universe.sp, nbr_args);

        frame_ptr
    };

    stack_push!(universe.sp, Value::STACK_MARKER);
    universe.bytecode_idx = 0;
    universe.current_frame = UnsafeCell::from(frame_ptr.clone());
    frame_ptr
}

pub fn pop_frame(universe: &mut Universe) {
    let new_current_frame = &universe.get_current_frame().prev_frame;

    while stack_pop!(universe.sp) != Value::STACK_MARKER {}

    universe.current_frame = UnsafeCell::from(new_current_frame.clone());
    match new_current_frame.is_empty() {
        true => {}
        false => {
            universe.bytecode_idx = new_current_frame.bytecode_idx;
        }
    }
}

pub fn pop_n_frames(universe: &mut Universe, n: u8) {
    let mut new_current_frame = universe.get_current_frame();
    for _ in 0..n {
        while stack_pop!(universe.sp) != Value::STACK_MARKER {}
        new_current_frame = new_current_frame.prev_frame.clone();
        if new_current_frame.is_empty() {
            panic!("found an empty target frame while walking the frame stack somehow");
        }
    }

    universe.current_frame = UnsafeCell::from(new_current_frame.clone());
    match new_current_frame.is_empty() {
        true => {}
        false => {
            universe.bytecode_idx = new_current_frame.bytecode_idx;
        }
    }
}

pub fn do_send(universe: &mut Universe, method: Option<Gc<Method>>, symbol: Interned, nbr_args: usize) {
    let Some(method) = method else {
        let args = stack_n_last_elements!(universe.sp, nbr_args - 1);
        stack_truncate!(universe.sp, nbr_args - 1);
        let self_value = stack_pop!(universe.sp);

        universe
            .does_not_understand(self_value, symbol, args)
            .expect("A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver");

        return;
    };

    match &*method {
        Method::Defined(method_info) => {
            // let name = &method.holder().name.clone();
            // eprintln!("--- Invoking {:?} (in {:?})", &method.signature(), &name);
            push_method_frame(universe, method_info.clone(), nbr_args);
        }
        Method::Primitive(func, _met_info) => {
            // eprintln!("--- Invoking prim {:?} (in {:?})", &_met_info.signature, &_met_info.holder.name);

            // dbg!(stack_nth_back!(universe.sp, 1));
            // dbg!(stack_nth_back!(universe.sp, 2));

            func(universe, nbr_args)
                .with_context(|| anyhow::anyhow!("error calling primitive `{}`", universe.lookup_symbol(symbol)))
                .unwrap();
        }
        Method::TrivialGlobal(met, _) => {
            //eprintln!("--- Invoking trivial method");
            met.invoke(universe)
        }
        Method::TrivialLiteral(met, _) => {
            //eprintln!("--- Invoking trivial method");
            stack_drop!(universe.sp); // remove the receiver
            met.invoke(universe)
        }
        Method::TrivialGetter(met, _) => {
            //eprintln!("--- Invoking trivial method");
            met.invoke(universe)
        }
        Method::TrivialSetter(met, _) => {
            //eprintln!("--- Invoking trivial method");
            met.invoke(universe)
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

#[cfg(not(feature = "switch-dispatch"))]
#[allow(clippy::let_unit_value)] // for the profiling
#[allow(unused_assignments)] // sometimes we modify bytecode
pub fn run(universe: &mut Universe) -> Option<Value> {
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

    fn handle_send_1(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("SEND");
        read_u16_fast!(val, bytecodes, bc_idx + 1);
        let symbol: Interned = Interned(val);
        resolve_method_and_send!(universe, frame, symbol, 1, bc_idx);
        update_frame_and_bytecodes!(universe, frame, bytecodes);
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_send_2(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("SEND");
        read_u16_fast!(val, bytecodes, bc_idx + 1);
        let symbol: Interned = Interned(val);
        resolve_method_and_send!(universe, frame, symbol, 2, bc_idx);
        update_frame_and_bytecodes!(universe, frame, bytecodes);
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_send_3(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("SEND");
        read_u16_fast!(val, bytecodes, bc_idx + 1);
        let symbol: Interned = Interned(val);
        resolve_method_and_send!(universe, frame, symbol, 3, bc_idx);
        update_frame_and_bytecodes!(universe, frame, bytecodes);
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_send_n(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("SEND");
        read_u16_fast!(val, bytecodes, bc_idx + 1);
        let symbol: Interned = Interned(val);
        let nbr_args = nbr_args(universe.lookup_symbol(symbol));
        resolve_method_and_send!(universe, frame, symbol, nbr_args, bc_idx);
        update_frame_and_bytecodes!(universe, frame, bytecodes);
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_push_local(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_LOCAL");
        let idx: u8 = bytecodes[bc_idx + 1];
        let value = *frame.lookup_local(idx as usize);
        stack_push!(universe.sp, value);
        bc_idx += BC_SIZE_1_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_non_local(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSHNONLOCAL");
        let up_idx: u8 = bytecodes[bc_idx + 1];
        let idx: u8 = bytecodes[bc_idx + 2];
        debug_assert_ne!(up_idx, 0);
        let from = Frame::nth_frame_back(&frame, up_idx);
        let value = *from.lookup_local(idx as usize);
        stack_push!(universe.sp, value);
        bc_idx += BC_SIZE_2_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_arg(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_ARG");
        let idx: u8 = bytecodes[bc_idx + 1];
        debug_assert_ne!(idx, 0); // that's a ReturnSelf case.
        let value = *frame.lookup_argument(idx as usize);
        stack_push!(universe.sp, value);
        bc_idx += BC_SIZE_1_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_non_local_arg(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_NON_LOCAL_ARG");
        let up_idx: u8 = bytecodes[bc_idx + 1];
        let idx: u8 = bytecodes[bc_idx + 2];
        debug_assert_ne!(up_idx, 0);
        debug_assert_ne!((up_idx, idx), (0, 0)); // that's a ReturnSelf case.
        let from = Frame::nth_frame_back(&frame, up_idx);
        let value = from.lookup_argument(idx as usize);
        stack_push!(universe.sp, *value);
        bc_idx += BC_SIZE_2_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_field(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
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
        stack_push!(universe.sp, val);
        bc_idx += BC_SIZE_1_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_dup(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("DUP");
        let value = *stack_last!(universe.sp);
        stack_push!(universe.sp, value);
        bc_idx += BC_SIZE_NO_ARGS;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_inc(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("INC");
        let last = stack_last!(universe.sp);

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
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_dec(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("DEC");
        let last = stack_last!(universe.sp);

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
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_block(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
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

        stack_push!(universe.sp, Value::Block(new_blk));
        bc_idx += BC_SIZE_1_ARG;

        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_constant(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_CONSTANT");
        let idx: u8 = bytecodes[bc_idx + 1];

        let literal = frame.lookup_constant(idx as usize);
        let value = value_from_literal(literal, &mut universe.gc_interface);
        stack_push!(universe.sp, value);
        bc_idx += BC_SIZE_1_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_global(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_GLOBAL");

        if let Some(CacheEntry::Global(value)) = unsafe { frame.get_inline_cache_entry(bc_idx) } {
            let value = *value;
            stack_push!(universe.sp, value);
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
                stack_push!(universe.sp, value);
                unsafe { *frame.get_inline_cache_entry(bc_idx) = Some(CacheEntry::Global(value)) }
                bc_idx += BC_SIZE_1_ARG;
            } else {
                bc_idx += BC_SIZE_1_ARG;
                universe.bytecode_idx = bc_idx as u16;
                let self_value = frame.get_self();
                universe.unknown_global(self_value, *symbol)?;
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                bc_idx = universe.bytecode_idx as usize;
            };
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_0(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_0");
        stack_push!(universe.sp, Value::INTEGER_ZERO);
        bc_idx += BC_SIZE_NO_ARGS;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_1(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_1");
        stack_push!(universe.sp, Value::INTEGER_ONE);
        bc_idx += BC_SIZE_NO_ARGS;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_nil(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_NIL");
        stack_push!(universe.sp, Value::NIL);
        bc_idx += BC_SIZE_NO_ARGS;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_push_self(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("PUSH_SELF");
        let self_val = *frame.lookup_argument(0);
        stack_push!(universe.sp, self_val);
        bc_idx += BC_SIZE_NO_ARGS;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_pop(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("POP");
        stack_drop!(universe.sp);
        bc_idx += BC_SIZE_NO_ARGS;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_pop_local(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("POP_LOCAL");
        let up_idx: u8 = bytecodes[bc_idx + 1];
        let idx: u8 = bytecodes[bc_idx + 2];
        let value = stack_pop!(universe.sp);
        let mut from = Frame::nth_frame_back(&frame, up_idx);
        from.assign_local(idx as usize, value);
        bc_idx += BC_SIZE_2_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_pop_arg(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("POP_ARG");
        let up_idx: u8 = bytecodes[bc_idx + 1];
        let idx: u8 = bytecodes[bc_idx + 2];
        let value = stack_pop!(universe.sp);
        let mut from = Frame::nth_frame_back(&frame, up_idx);
        from.assign_arg(idx as usize, value);
        bc_idx += BC_SIZE_2_ARG;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_pop_field(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("POP_FIELD");
        let idx: u8 = bytecodes[bc_idx + 1];
        let value = stack_pop!(universe.sp);
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
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_super_send(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, bc_idx: usize) -> Option<Value> {
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
            resolve_method(universe.current_frame.get_mut(), &super_class, symbol, bc_idx)
        };
        universe.bytecode_idx = (bc_idx + BC_SIZE_2_ARG) as u16;
        do_send(universe, method, symbol, nbr_args);
        update_frame_and_bytecodes!(universe, frame, bytecodes);
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_return_self(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, _bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, _bc_idx);
        let _timing = profiler_maybe_start!("RETURN_SELF");
        let self_val = *frame.lookup_argument(0);
        pop_frame(universe);
        stack_push!(universe.sp, self_val);
        update_frame_and_bytecodes!(universe, frame, bytecodes);
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_return_local(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, _bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, _bc_idx);
        let _timing = profiler_maybe_start!("RETURN_LOCAL");
        let value = stack_pop!(universe.sp);
        pop_frame(universe);
        if universe.get_current_frame().is_empty() {
            profiler_maybe_stop!(_timing);
            return Some(value);
        }
        stack_push!(universe.sp, value);
        update_frame_and_bytecodes!(universe, frame, bytecodes);
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_return_non_local(universe: &mut Universe, mut bytecodes: &[u8], mut frame: Gc<Frame>, bc_idx: usize) -> Option<Value> {
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
            let value = stack_pop!(universe.sp);
            pop_n_frames(universe, count + 1);
            stack_push!(universe.sp, value);
            update_frame_and_bytecodes!(universe, frame, bytecodes);
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

            universe.bytecode_idx = (bc_idx + BC_SIZE_1_ARG) as u16;

            universe.escaped_block(instance, block).expect("A block has escaped and `escapedBlock:` is not defined on receiver");
            update_frame_and_bytecodes!(universe, frame, bytecodes);
        };

        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, universe.bytecode_idx as usize);
    }

    fn handle_dup_2(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("DUP2");
        let second_to_last = stack_nth_back!(universe.sp, 1);
        stack_push!(universe.sp, *second_to_last);
        bc_idx += BC_SIZE_NO_ARGS;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        bc_idx += offset as usize;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_backward(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_BACKWARD");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        bc_idx -= offset as usize;
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_true_top_nil(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_TRUE_TOP_NIL");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_last!(universe.sp);

        debug_assert!(condition_result.is_boolean());
        if condition_result.is_boolean_true() {
            bc_idx += offset as usize;
            *condition_result = Value::NIL;
        } else {
            stack_drop!(universe.sp);
            bc_idx += BC_SIZE_U16_ARG;
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_false_top_nil(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_FALSE_TOP_NIL");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_last!(universe.sp);

        debug_assert!(condition_result.is_boolean());
        if condition_result.is_boolean_true() {
            stack_drop!(universe.sp);
            bc_idx += BC_SIZE_U16_ARG;
        } else {
            bc_idx += offset as usize;
            *condition_result = Value::NIL;
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_true_pop(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_TRUE_POP");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_pop!(universe.sp);

        debug_assert!(condition_result.is_boolean());
        if condition_result.is_boolean_true() {
            bc_idx += offset as usize;
        } else {
            bc_idx += BC_SIZE_U16_ARG;
            // pass
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_false_pop(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_FALSE_POP");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_pop!(universe.sp);

        debug_assert!(condition_result.is_boolean());
        if condition_result.is_boolean_false() {
            bc_idx += offset as usize;
        } else {
            bc_idx += BC_SIZE_U16_ARG;
            // pass
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_if_greater(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let top = stack_last!(universe.sp);
        let top2 = stack_nth_back!(universe.sp, 1);

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
            stack_drop!(universe.sp);
            stack_drop!(universe.sp);
            bc_idx += offset as usize;
        } else {
            bc_idx += BC_SIZE_U16_ARG;
        }
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_nil_top_top(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_NIL_TOP_TOP");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_last!(universe.sp);

        if condition_result.is_nil() {
            bc_idx += offset as usize;
        } else {
            stack_drop!(universe.sp);
            bc_idx += BC_SIZE_U16_ARG;
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_not_nil_top_top(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_TOP_TOP");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_last!(universe.sp);

        if !condition_result.is_nil() {
            bc_idx += offset as usize;
        } else {
            stack_drop!(universe.sp);
            bc_idx += BC_SIZE_U16_ARG;
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_nil_pop(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_NIL_POP");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_pop!(universe.sp);

        if condition_result.is_nil() {
            bc_idx += offset as usize;
        } else {
            bc_idx += BC_SIZE_U16_ARG;
            // pass
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    fn handle_jump_on_not_nil_pop(universe: &mut Universe, bytecodes: &[u8], frame: Gc<Frame>, mut bc_idx: usize) -> Option<Value> {
        log_bc!(bytecodes, bc_idx);
        let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_POP");
        read_u16_fast!(offset, bytecodes, bc_idx + 1);
        let condition_result = stack_pop!(universe.sp);

        if !condition_result.is_nil() {
            bc_idx += offset as usize;
        } else {
            bc_idx += BC_SIZE_U16_ARG;
            // pass
        }
        profiler_maybe_stop!(_timing);
        dispatch_next!(JUMP_TABLE, universe, bytecodes, frame, bc_idx);
    }

    let frame = universe.get_current_frame();
    let bytecodes = unsafe { &*(frame.get_bytecodes() as *const Vec<u8>) };
    let bc_idx = universe.bytecode_idx as usize;

    JUMP_TABLE[bytecodes[bc_idx] as usize](universe, bytecodes, frame, bc_idx)
}

#[cfg(feature = "switch-dispatch")]
macro_rules! resolve_method_and_send_legacy {
    ($universe:expr, $frame:expr, $symbol:expr, $nbr_args:expr) => {{
        let receiver = stack_nth_back!($universe.sp, $nbr_args - 1);
        let receiver_class = receiver.class($universe);
        let method = resolve_method(&mut $frame, &receiver_class, $symbol, $universe.bytecode_idx as usize);
        $universe.bytecode_idx += BC_SIZE_U16_ARG as u16; // always this size for a send
        do_send($universe, method, $symbol, $nbr_args);
    }};
}

#[cfg(feature = "switch-dispatch")]
pub fn run(universe: &mut Universe) -> Option<Value> {
    let mut frame = universe.get_current_frame();
    // Pointer to bytecodes for faster access, to presumably lead to better codegen.
    // Safety: all updates to it are alongside updates to `frame`. See `update_frame_and_bytecodes` macro.
    let mut bytecodes = unsafe { &*(frame.get_bytecodes() as *const Vec<u8>) };

    loop {
        let bytecode = *(unsafe { bytecodes.get_unchecked(universe.bytecode_idx as usize) });

        // dbg!(Bytecode::padded_name(bytecode));
        //dbg!(&self.bytecode_idx);

        // for the optional profiler macros not to be reported as warnings
        #[allow(clippy::let_unit_value)]
        match bytecode {
            Bytecode::SEND_1 => {
                let _timing = profiler_maybe_start!("SEND");
                read_u16_fast!(val, bytecodes, universe.bytecode_idx as usize + 1);
                let symbol: Interned = Interned(val);
                resolve_method_and_send_legacy!(universe, frame, symbol, 1);
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                profiler_maybe_stop!(_timing);
            }
            Bytecode::SEND_2 => {
                let _timing = profiler_maybe_start!("SEND");
                read_u16_fast!(val, bytecodes, universe.bytecode_idx as usize + 1);
                let symbol: Interned = Interned(val);
                resolve_method_and_send_legacy!(universe, frame, symbol, 2);
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                profiler_maybe_stop!(_timing);
            }
            Bytecode::SEND_3 => {
                let _timing = profiler_maybe_start!("SEND");
                read_u16_fast!(val, bytecodes, universe.bytecode_idx as usize + 1);
                let symbol: Interned = Interned(val);
                resolve_method_and_send_legacy!(universe, frame, symbol, 3);
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                profiler_maybe_stop!(_timing);
            }
            Bytecode::SEND_N => {
                let _timing = profiler_maybe_start!("SEND");
                read_u16_fast!(val, bytecodes, universe.bytecode_idx as usize + 1);
                let symbol: Interned = Interned(val);
                let nbr_args = nbr_args(universe.lookup_symbol(symbol));
                resolve_method_and_send_legacy!(universe, frame, symbol, nbr_args);
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_LOCAL => {
                let _timing = profiler_maybe_start!("PUSH_LOCAL");
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                let value = *frame.lookup_local(idx as usize);
                stack_push!(universe.sp, value);
                universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_NON_LOCAL => {
                let _timing = profiler_maybe_start!("PUSHNONLOCAL");
                let up_idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 2];
                debug_assert_ne!(up_idx, 0);
                let from = Frame::nth_frame_back(&frame, up_idx);
                let value = *from.lookup_local(idx as usize);
                stack_push!(universe.sp, value);
                universe.bytecode_idx += BC_SIZE_2_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_ARG => {
                let _timing = profiler_maybe_start!("PUSH_ARG");
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                debug_assert_ne!(idx, 0); // that's a Returnuniverse case.
                let value = *frame.lookup_argument(idx as usize);
                stack_push!(universe.sp, value);
                universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_NON_LOCAL_ARG => {
                let _timing = profiler_maybe_start!("PUSH_NON_LOCAL_ARG");
                let up_idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 2];
                debug_assert_ne!(up_idx, 0);
                debug_assert_ne!((up_idx, idx), (0, 0)); // that's a Returnuniverse case.
                let from = Frame::nth_frame_back(&frame, up_idx);
                let value = from.lookup_argument(idx as usize);
                stack_push!(universe.sp, *value);
                universe.bytecode_idx += BC_SIZE_2_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_FIELD => {
                let _timing = profiler_maybe_start!("PUSH_FIELD");
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
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
                stack_push!(universe.sp, val);
                universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::DUP => {
                let _timing = profiler_maybe_start!("DUP");
                let value = *stack_last!(&mut universe.sp);
                stack_push!(universe.sp, value);
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::INC => {
                let _timing = profiler_maybe_start!("INC");
                let last: &mut Value = stack_last!(universe.sp);

                if let Some(int) = last.as_integer() {
                    *last = Value::new_integer(int + 1);
                } else if let Some(double) = last.as_double() {
                    *last = Value::new_double(double + 1.0);
                } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                    *big_int += 1;
                } else {
                    panic!("Invalid type in Inc")
                };
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::DEC => {
                let _timing = profiler_maybe_start!("DEC");
                let last = stack_last!(&mut universe.sp);

                if let Some(int) = last.as_integer() {
                    *last = Value::new_integer(int - 1);
                } else if let Some(double) = last.as_double() {
                    *last = Value::new_double(double - 1.0);
                } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
                    *big_int -= 1;
                } else {
                    panic!("Invalid type in DEC")
                };
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_BLOCK => {
                let _timing = profiler_maybe_start!("PUSH_BLOCK");
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];

                // allocating ahead of time in case it triggers GC.
                let mut new_blk = universe.gc_interface.request_memory_for_type::<Block>(std::mem::size_of::<Block>(), AllocSiteMarker::RuntimeBlock);

                match frame.lookup_constant(idx as usize) {
                    Literal::Block(blk) => {
                        new_blk.blk_info = (*blk).clone();
                        new_blk.frame = frame.clone();
                    }
                    _ => panic!("PushBlock expected a block, but got another invalid literal"),
                }

                stack_push!(universe.sp, Value::Block(new_blk));
                universe.bytecode_idx += BC_SIZE_1_ARG as u16;

                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_CONSTANT => {
                let _timing = profiler_maybe_start!("PUSH_CONSTANT");
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];

                let literal = frame.lookup_constant(idx as usize);
                let value = value_from_literal(literal, &mut universe.gc_interface);
                stack_push!(universe.sp, value);
                universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_GLOBAL => {
                let _timing = profiler_maybe_start!("PUSH_GLOBAL");

                if let Some(CacheEntry::Global(value)) = unsafe { frame.get_inline_cache_entry(universe.bytecode_idx as usize) } {
                    let value = *value;
                    stack_push!(universe.sp, value);
                    universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                    continue;
                }

                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                let literal = frame.lookup_constant(idx as usize);
                let symbol = match literal {
                    Literal::Symbol(sym) => sym,
                    _ => panic!("Global is not a symbol."),
                };
                if let Some(value) = universe.lookup_global(*symbol) {
                    stack_push!(universe.sp, value);
                    unsafe { *frame.get_inline_cache_entry(universe.bytecode_idx as usize) = Some(CacheEntry::Global(value)) }
                    universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                } else {
                    universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                    let self_value = frame.get_self();
                    universe.unknown_global(self_value, *symbol)?;
                    update_frame_and_bytecodes!(universe, frame, bytecodes);
                };
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_0 => {
                let _timing = profiler_maybe_start!("PUSH_0");
                stack_push!(universe.sp, Value::INTEGER_ZERO);
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_1 => {
                let _timing = profiler_maybe_start!("PUSH_1");
                stack_push!(universe.sp, Value::INTEGER_ONE);
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_NIL => {
                let _timing = profiler_maybe_start!("PUSH_NIL");
                stack_push!(universe.sp, Value::NIL);
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::PUSH_SELF => {
                let _timing = profiler_maybe_start!("PUSH_SELF");
                let self_val = *frame.lookup_argument(0);
                stack_push!(universe.sp, self_val);
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::POP => {
                let _timing = profiler_maybe_start!("POP");
                stack_drop!(universe.sp);
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::POP_LOCAL => {
                let _timing = profiler_maybe_start!("POP_LOCAL");
                let up_idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 2];
                let value = stack_pop!(universe.sp);
                let mut from = Frame::nth_frame_back(&frame, up_idx);
                from.assign_local(idx as usize, value);
                universe.bytecode_idx += BC_SIZE_2_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::POP_ARG => {
                let _timing = profiler_maybe_start!("POP_ARG");
                let up_idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 2];
                let value = stack_pop!(universe.sp);
                let mut from = Frame::nth_frame_back(&frame, up_idx);
                from.assign_arg(idx as usize, value);
                universe.bytecode_idx += BC_SIZE_2_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::POP_FIELD => {
                let _timing = profiler_maybe_start!("POP_FIELD");
                let idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
                let value = stack_pop!(universe.sp);
                let self_val = frame.get_self();
                if let Some(instance) = self_val.as_instance() {
                    Instance::assign_field(&instance, idx as usize, value);
                } else if let Some(cls) = self_val.as_class() {
                    cls.class().assign_field(idx as usize, value)
                } else {
                    panic!("trying to assign a field to a {:?}?", &self_val)
                };
                universe.bytecode_idx += BC_SIZE_1_ARG as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::SUPER_SEND => {
                let _timing = profiler_maybe_start!("SUPER_SEND");
                read_u16_fast!(val, bytecodes, universe.bytecode_idx as usize + 1);
                let symbol: Interned = Interned(val);
                let nbr_args = {
                    let signature = universe.lookup_symbol(symbol);
                    nbr_args(signature)
                };

                let method = {
                    let holder = frame.get_method_holder();
                    let super_class = holder.super_class().unwrap();
                    resolve_method(universe.current_frame.get_mut(), &super_class, symbol, universe.bytecode_idx as usize)
                };
                universe.bytecode_idx += BC_SIZE_2_ARG as u16;
                do_send(universe, method, symbol, nbr_args);
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                profiler_maybe_stop!(_timing);
            }
            Bytecode::RETURN_SELF => {
                let _timing = profiler_maybe_start!("RETURN_SELF");
                let self_val = *frame.lookup_argument(0);
                pop_frame(universe);
                stack_push!(universe.sp, self_val);
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                profiler_maybe_stop!(_timing);
            }
            Bytecode::RETURN_LOCAL => {
                let _timing = profiler_maybe_start!("RETURN_LOCAL");
                let value = stack_pop!(universe.sp);
                pop_frame(universe);
                if universe.get_current_frame().is_empty() {
                    profiler_maybe_stop!(_timing);
                    return Some(value);
                }
                stack_push!(universe.sp, value);
                update_frame_and_bytecodes!(universe, frame, bytecodes);
                profiler_maybe_stop!(_timing);
            }
            Bytecode::RETURN_NON_LOCAL => {
                let _timing = profiler_maybe_start!("RETURN_NON_LOCAL");
                let up_idx: u8 = bytecodes[universe.bytecode_idx as usize + 1];
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
                    let value = stack_pop!(universe.sp);
                    pop_n_frames(universe, count + 1);
                    stack_push!(universe.sp, value);
                    update_frame_and_bytecodes!(universe, frame, bytecodes);
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

                    universe.bytecode_idx += BC_SIZE_1_ARG as u16;

                    universe.escaped_block(instance, block).expect("A block has escaped and `escapedBlock:` is not defined on receiver");
                    update_frame_and_bytecodes!(universe, frame, bytecodes);
                };

                profiler_maybe_stop!(_timing);
            }
            Bytecode::DUP_2 => {
                let _timing = profiler_maybe_start!("DUP2");
                let second_to_last = stack_nth_back!(universe.sp, 1);
                stack_push!(universe.sp, *second_to_last);
                universe.bytecode_idx += BC_SIZE_NO_ARGS as u16;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP => {
                let _timing = profiler_maybe_start!("JUMP");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                universe.bytecode_idx += offset;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_BACKWARD => {
                let _timing = profiler_maybe_start!("JUMP_BACKWARD");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                universe.bytecode_idx -= offset;
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_ON_TRUE_TOP_NIL => {
                let _timing = profiler_maybe_start!("JUMP_ON_TRUE_TOP_NIL");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_last!(&mut universe.sp);

                debug_assert!(condition_result.is_boolean());
                if condition_result.is_boolean_true() {
                    universe.bytecode_idx += offset;
                    *condition_result = Value::NIL;
                } else {
                    stack_drop!(universe.sp);
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                }
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_ON_FALSE_TOP_NIL => {
                let _timing = profiler_maybe_start!("JUMP_ON_FALSE_TOP_NIL");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_last!(&mut universe.sp);

                debug_assert!(condition_result.is_boolean());
                if condition_result.is_boolean_true() {
                    stack_drop!(universe.sp);
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                } else {
                    universe.bytecode_idx += offset;
                    *condition_result = Value::NIL;
                }
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_ON_TRUE_POP => {
                let _timing = profiler_maybe_start!("JUMP_ON_TRUE_POP");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_pop!(universe.sp);

                debug_assert!(condition_result.is_boolean());
                if condition_result.is_boolean_true() {
                    universe.bytecode_idx += offset;
                } else {
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                    // pass
                }
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_ON_FALSE_POP => {
                let _timing = profiler_maybe_start!("JUMP_ON_FALSE_POP");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_pop!(universe.sp);

                debug_assert!(condition_result.is_boolean());
                if condition_result.is_boolean_false() {
                    universe.bytecode_idx += offset;
                } else {
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                    // pass
                }
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_IF_GREATER => {
                let _timing = profiler_maybe_start!("JUMP_IF_GREATER");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let top = stack_last!(universe.sp);
                let top2 = stack_nth_back!(universe.sp, 1);

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
                    stack_drop!(universe.sp);
                    stack_drop!(universe.sp);
                    universe.bytecode_idx += offset;
                } else {
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                }
            }
            Bytecode::JUMP_ON_NIL_TOP_TOP => {
                let _timing = profiler_maybe_start!("JUMP_ON_NIL_TOP_TOP");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_last!(&mut universe.sp);

                if condition_result.is_nil() {
                    universe.bytecode_idx += offset;
                } else {
                    stack_drop!(universe.sp);
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                }
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_ON_NOT_NIL_TOP_TOP => {
                let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_TOP_TOP");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_last!(&mut universe.sp);

                if !condition_result.is_nil() {
                    universe.bytecode_idx += offset;
                } else {
                    stack_drop!(universe.sp);
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                }
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_ON_NIL_POP => {
                let _timing = profiler_maybe_start!("JUMP_ON_NIL_POP");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_pop!(universe.sp);

                if condition_result.is_nil() {
                    universe.bytecode_idx += offset;
                } else {
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                    // pass
                }
                profiler_maybe_stop!(_timing);
            }
            Bytecode::JUMP_ON_NOT_NIL_POP => {
                let _timing = profiler_maybe_start!("JUMP_ON_NOT_NIL_POP");
                read_u16_fast!(offset, bytecodes, universe.bytecode_idx as usize + 1);
                let condition_result = stack_pop!(universe.sp);

                if !condition_result.is_nil() {
                    universe.bytecode_idx += offset;
                } else {
                    universe.bytecode_idx += BC_SIZE_U16_ARG as u16;
                    // pass
                }
                profiler_maybe_stop!(_timing);
            }
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}
