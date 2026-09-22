use crate::gc::VecValue;
use crate::pop_args_from_stack;
use crate::primitives::PrimInfo;
use crate::primitives::PrimitiveFn;
use crate::stack_pop;
use crate::universe::Universe;
use crate::value::convert::Primitive;
use crate::value::Value;
use crate::vm_objects::class::Class;
use crate::vm_objects::method::{Invoke, Method};
use anyhow::Error;
use once_cell::sync::Lazy;
use som_gc::gcref::Gc;
use som_value::interned::Interned;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("holder", self::holder.into_func(), true),
        ("signature", self::signature.into_func(), true),
        ("invokeOn:with:", self::invoke_on_with.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

fn holder(invokable: Gc<Method>) -> Result<Gc<Class>, Error> {
    Ok(invokable.holder().clone())
}

fn signature(universe: &mut Universe) -> Result<Interned, Error> {
    pop_args_from_stack!(universe, invokable => Gc<Method>);
    Ok(universe.intern_symbol(invokable.signature()))
}

fn invoke_on_with(universe: &mut Universe) -> Result<(), Error> {
    pop_args_from_stack!(universe, invokable => Gc<Method>, receiver => Value, arguments => VecValue);

    // TODO: this should NOT pop. a frame allocation causes a GC bug here, as far as I know.

    invokable.invoke(
        universe,
        receiver,
        arguments.iter().copied().collect(), // todo lame to clone tbh
    );
    Ok(())
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
