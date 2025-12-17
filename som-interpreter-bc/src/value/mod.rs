use som_value::value::BaseValue;

/// Value type(s!), and value-related code.
/// Used to convert types, used by primitives.
pub mod convert;

/// Our default type: NaN boxed
pub mod nanboxed;

/// Our enum based type
pub mod value_enum;
mod value_ptr;

/// Represents an SOM value.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Value(pub BaseValue);

// TODO: we should be able to switch between Value (nanboxed) and ValueEnum at will. That used to be the case, but I broke those changes. Need to restore
