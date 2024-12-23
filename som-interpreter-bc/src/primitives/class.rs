use crate::gc::VecValue;
use crate::interpreter::Interpreter;
use crate::primitives::PrimInfo;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::Primitive;
use crate::value::{HeapValPtr, Value};
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use anyhow::Error;
use once_cell::sync::Lazy;
use som_core::interner::Interned;
use som_gc::gcref::Gc;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new({
        [
            ("new", self::new.into_func(), true),
            ("name", self::name.into_func(), true),
            ("fields", self::fields.into_func(), true),
            ("methods", self::methods.into_func(), true),
            ("superclass", self::superclass.into_func(), true),
        ]
    })
});
pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

fn superclass(_: &mut Interpreter, _: &mut Universe, receiver: HeapValPtr<Class>) -> Result<Value, Error> {
    const _: &str = "Class>>#superclass";

    let super_class = receiver.deref().super_class();
    let super_class_val = super_class.map_or(Value::NIL, Value::Class);
    // interpreter.current_frame.stack_push(super_class);

    Ok(super_class_val)
}

fn new(_: &mut Interpreter, universe: &mut Universe, receiver: HeapValPtr<Class>) -> Result<Gc<Instance>, Error> {
    let instance = Instance::from_class(receiver.deref(), universe.gc_interface);
    Ok(instance)
}

fn name(_: &mut Interpreter, universe: &mut Universe, receiver: HeapValPtr<Class>) -> Result<Interned, Error> {
    const _: &str = "Class>>#name";

    Ok(universe.intern_symbol(receiver.deref().name()))
}

fn methods(_: &mut Interpreter, universe: &mut Universe, receiver: HeapValPtr<Class>) -> Result<Gc<VecValue>, Error> {
    const _: &str = "Class>>#methods";

    let methods = receiver.deref().methods.values().copied().map(Value::Invokable).collect();

    Ok(universe.gc_interface.alloc(VecValue(methods)))
}

fn fields(_: &mut Interpreter, universe: &mut Universe, receiver: HeapValPtr<Class>) -> Result<Gc<VecValue>, Error> {
    const _: &str = "Class>>#fields";

    let fields = receiver.deref().field_names.iter().copied().map(Value::Symbol).collect();

    Ok(universe.gc_interface.alloc(VecValue(fields)))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
