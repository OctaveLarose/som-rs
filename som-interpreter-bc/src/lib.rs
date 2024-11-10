//!
//! This is the interpreter for the Simple Object Machine.
//!

use std::ptr::NonNull;

use crate::interpreter::Interpreter;
use crate::universe::Universe;

/// Facilities for manipulating blocks.
pub mod block;
/// Facilities for manipulating classes.
pub mod class;
/// Facilities for compiling code into bytecode.
pub mod compiler;
/// Facilities for disassembling bytecode.
pub mod disassembler;
/// Facilities for manipulating stack frames.
pub mod frame;
/// Facilities for manipulating values.
pub mod hashcode;
/// Facilities for manipulating class instances.
pub mod instance;
/// The interpreter's main data structure.
pub mod interpreter;
/// Facilities for manipulating class methods.
pub mod method;
/// Definitions for all supported primitives.
pub mod primitives;
/// The collection of all known SOM objects during execution.
pub mod universe;
/// Facilities for manipulating values.
pub mod value;

/// Inlining some calls to a select few builtin functions for sizeable perf gains.
pub mod inliner;

mod convert;
/// Facilities for profiling the SOM VM during execution.
#[cfg(feature = "profiler")]
pub mod profiler;

/// Structs and info related to interacting with the GC
pub mod gc;

// /// A strong and owning reference to an object.
// pub type SOMRef<T> = Rc<RefCell<T>>;
// /// A weak reference to an object.
// pub type SOMWeakRef<T> = Weak<RefCell<T>>;

/// Raw pointer needed to trace GC roots. Meant to be accessed only non-mutably, hence the "CONST" in the name.
pub static mut UNIVERSE_RAW_PTR_CONST: Option<NonNull<Universe>> = None;
/// See `UNIVERSE_RAW_PTR_CONST`.
pub static mut INTERPRETER_RAW_PTR_CONST: Option<NonNull<Interpreter>> = None;
