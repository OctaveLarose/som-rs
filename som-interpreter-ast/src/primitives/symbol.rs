use once_cell::sync::Lazy;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseAST;
use crate::value::Value;
use som_core::gc::GCRef;
use crate::convert::Primitive;
use crate::interner::Interned;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([("asString", self::as_string.into_func(), true)]));

pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([]));

fn as_string(universe: &mut UniverseAST, sym: Interned) -> Return {
    Return::Local(Value::String(GCRef::<String>::alloc(universe.lookup_symbol(sym).to_string(), &mut universe.gc_interface)))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}