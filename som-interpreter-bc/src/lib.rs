//!
//! This is the interpreter for the Simple Object Machine.
//!

// Ideally this would be preceded by a `#[cfg(not(feature = "switch-dispatch"))]`, but that's not something Rust allows.
#![feature(explicit_tail_calls)]

use crate::universe::Universe;
use std::sync::atomic::AtomicPtr;

/// VM objects.
pub mod vm_objects;

/// Facilities for compiling code into bytecode.
pub mod compiler;
/// Facilities for manipulating values.
pub mod hashcode;
/// The interpreter's main data structure.
pub mod interpreter;
/// Definitions for all supported primitives.
pub mod primitives;
/// The collection of all known SOM objects during execution.
pub mod universe;
/// Facilities for manipulating values.
pub mod value;

/// Structs and info related to interacting with the GC
pub mod gc;

/// Used for debugging.
pub mod debug;
pub mod jit;

// NB: I made them macros just in case the compiler wouldn't necessarily inline them with `inline(always)`, but really that's not a serious concern at all.
// So we can make them functions instead in the future. Same difference, I feel.
#[macro_export]
macro_rules! stack_push {
    ($stack:expr, $value:expr) => {{
        let value = $value;
        let stack = $stack;
        unsafe {
            *stack = value;
            $stack = stack.add(1);
        }
    }};
}

#[macro_export]
macro_rules! stack_pop {
    ($stack:expr) => {{
        let stack = $stack;
        unsafe {
            let tos = stack.sub(1);
            $stack = tos;
            (*tos).clone()
        }
    }};
}

#[macro_export]
macro_rules! stack_drop {
    ($stack:expr) => {{
        let stack = $stack;
        unsafe {
            $stack = stack.sub(1);
        }
    }};
}

#[macro_export]
macro_rules! stack_last {
    ($stack:expr) => {{
        let stack = $stack;
        unsafe { &mut *stack.sub(1) }
    }};
}

// NB: zero-indexed
#[macro_export]
macro_rules! stack_nth_back {
    ($stack:expr, $n:expr) => {{
        let stack = $stack;
        let n = $n;
        unsafe { &mut *stack.sub(n + 1) }
    }};
}

#[macro_export]
macro_rules! stack_n_last_elements {
    ($sp:expr, $n:expr) => {{
        let sp = $sp;
        let n = $n;
        unsafe { std::slice::from_raw_parts(sp.sub(n), n) }
    }};
}

#[macro_export]
macro_rules! stack_truncate {
    ($stack:expr, $n:expr) => {{
        let stack = $stack;
        let n = $n;
        $stack = unsafe { stack.sub(n) };
    }};
}

/// Raw pointer needed to trace GC roots. Meant to be accessed only non-mutably, hence the "CONST" in the name.
pub static UNIVERSE_RAW_PTR_CONST: AtomicPtr<Universe> = AtomicPtr::new(std::ptr::null_mut());
