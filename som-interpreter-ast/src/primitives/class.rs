use super::PrimInfo;
use crate::gc::VecValue;
use crate::get_args_from_stack;
use crate::primitives::PrimitiveFn;
use crate::universe::{GlobalValueStack, Universe};
use crate::value::convert::FromArgs;
use crate::value::convert::Primitive;
use crate::value::Value;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use anyhow::Error;
use once_cell::sync::Lazy;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
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

fn superclass(receiver: Gc<Class>) -> Result<Value, Error> {
    let super_class = receiver.super_class();
    Ok(super_class.map(Value::Class).unwrap_or(Value::NIL))
}

fn new(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    let nbr_fields = stack.last().as_class().unwrap().get_nbr_fields();
    let size = size_of::<Instance>() + (nbr_fields * size_of::<Value>());

    let mut instance_ptr: Gc<Instance> = universe.gc_interface.request_memory_for_type(size, AllocSiteMarker::Instance);

    get_args_from_stack!(stack, receiver => Gc<Class>);

    *instance_ptr = Instance::from_class(receiver);
    for idx in 0..instance_ptr.class.get_nbr_fields() {
        Instance::assign_field(&instance_ptr, idx as u8, Value::NIL)
    }
    
    Ok(Value::Instance(instance_ptr))
}

fn name(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, receiver => Gc<Class>);
    let sym = universe.intern_symbol(receiver.name());
    Ok(Value::Symbol(sym))
}

fn methods(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, receiver => Gc<Class>);
    let methods: Vec<Value> = receiver.methods.values().map(|invokable| Value::Invokable(invokable.clone())).collect();

    Ok(Value::Array(VecValue(
        universe.gc_interface.alloc_slice(&methods, AllocSiteMarker::VecValue),
    )))
}

fn fields(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, receiver => Gc<Class>);
    let fields: Vec<Value> = receiver
        .get_all_field_names()
        .iter()
        .map(|field_name| Value::String(universe.gc_interface.alloc(field_name.clone(), AllocSiteMarker::String)))
        .collect();

    Ok(Value::Array(VecValue(
        universe.gc_interface.alloc_slice(&fields, AllocSiteMarker::VecValue),
    )))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
