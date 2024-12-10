use som_core::value::BaseValue;
use som_gc::gcref::Gc;

/// The main value type.
pub mod nan_boxed_val;

/// Automatically convert values to their underlying type. Useful for primitives.
pub mod convert;

/// For values that are to pointer types.
pub mod value_ptr;

#[derive(Clone, Copy)]
pub struct Value(pub(crate) BaseValue);

/// A pointer to a Value on the GC heap.
pub type HeapValPtr<T> = som_core::value_ptr::ValStaticPtr<T, Gc<T>>;
