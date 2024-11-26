//!
//! This crate contains common types that are useful to be shared across multiple tools when manipulating SOM-related things.
//!

/// The SOM Abstract Syntax Tree definitions.
pub mod ast;
/// The SOM bytecode definitions.
pub mod bytecode;
pub mod core_classes;
/// Facilities for string interning.
pub mod interner;
/// Shared value representation logic (NaN boxing really)
pub mod value;
