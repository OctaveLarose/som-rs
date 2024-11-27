//!
//! This is the interpreter for the Simple Object Machine.
//!

use crate::interpreter::Interpreter;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::block::Block;
use som_gc::gcref::Gc;
use std::ptr::NonNull;
use vm_objects::class::Class;
use vm_objects::method::Method;

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

/// Used to convert types, used by primitives.
pub mod convert;

/// Structs and info related to interacting with the GC
pub mod gc;

/// Used for debugging.
pub mod debug;

/// Raw pointer needed to trace GC roots. Meant to be accessed only non-mutably, hence the "CONST" in the name.
pub static mut UNIVERSE_RAW_PTR_CONST: Option<NonNull<Universe>> = None;
/// See `UNIVERSE_RAW_PTR_CONST`.
pub static mut INTERPRETER_RAW_PTR_CONST: Option<NonNull<Interpreter>> = None;

/// Hack! at the moment, we pass a copied reference to a class' method when allocating a frame. When GC triggers from a frame allocation, that pointer isn't a root and doesn't get moved.
/// So we're storing a reference to it. Better solution: just push it on the stack. Or TODO, we might be able to pass a pointer to it through functions.
pub static mut HACK_FRAME_CURRENT_METHOD_PTR: Option<Gc<Method>> = None;
/// that one's a true hack, and avoidable also.
pub static mut HACK_FRAME_CURRENT_BLOCK_PTR: Option<Gc<Block>> = None;
/// that one's the ugliest of hacks and we can definitely remove it somehow..
pub static mut HACK_FRAME_FRAME_ARGS_PTR: Option<Vec<Value>> = None;

/// For instance initializations... we really need to pass pointers by references to primitives...
pub static mut HACK_INSTANCE_CLASS_PTR: Option<Gc<Class>> = None;
