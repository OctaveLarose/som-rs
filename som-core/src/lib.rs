//!
//! This crate contains common types that are useful to be shared across multiple tools when manipulating SOM-related things.
//!

/// The SOM Abstract Syntax Tree definitions.
pub mod ast;
/// The SOM bytecode definitions.
pub mod bytecode;
/// The Universe trait, shared by both AST and BC interpreters.
pub mod universe;
