use crate::gc::VecValue;
use crate::interpreter::Interpreter;
use crate::pop_args_from_stack;
use crate::primitives::PrimInfo;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::Primitive;
use crate::value::Value;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use anyhow::Error;
use once_cell::sync::Lazy;
use som_gc::gc_interface::AllocSiteMarker;
use som_gc::gc_interface::SOMAllocator;
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;
use som_value::interned::Interned;

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
    let super_class_val = super_class.map_or(Value::NIL, Value::Class);
    // interpreter.current_frame.stack_push(super_class);

    Ok(super_class_val)
}

fn new(interp: &mut Interpreter, universe: &mut Universe) -> Result<(), Error> {
    std::hint::black_box(&interp.current_frame);

    let nbr_fields = interp.get_current_frame().stack_last().as_class().unwrap().get_nbr_fields();
    let size = size_of::<Instance>() + (nbr_fields * size_of::<Value>());

    let mut instance_ptr: Gc<Instance> = universe.gc_interface.request_memory_for_type(size, AllocSiteMarker::Instance);
    *instance_ptr = Instance::from_class(interp.get_current_frame().stack_last().as_class().unwrap());

    for idx in 0..nbr_fields {
        Instance::assign_field(&instance_ptr, idx, Value::NIL)
    }

    interp.get_current_frame().stack_pop();
    interp.get_current_frame().stack_push(Value::Instance(instance_ptr));

    Ok(())
}

fn name(interp: &mut Interpreter, universe: &mut Universe) -> Result<Interned, Error> {
    pop_args_from_stack!(interp, receiver => Gc<Class>);
    Ok(universe.intern_symbol(receiver.name()))
}

fn methods(interp: &mut Interpreter, universe: &mut Universe) -> Result<VecValue, Error> {
    let cls: Gc<Class> = interp.get_current_frame().stack_last().as_class().unwrap();
    std::hint::black_box(&cls); // paranoia, in case the compiler gets ideas about reusing that variable
    let slice_size = cls.methods.len() * size_of::<Value>();
    let slice_addr = universe.gc_interface.request_bytes_for_slice(slice_size, AllocSiteMarker::VecValue);

    pop_args_from_stack!(interp, receiver => Gc<Class>);
    let methods: Vec<Value> = receiver.methods.values().cloned().map(Value::Invokable).collect();
    let allocated: GcSlice<Value> = universe.gc_interface.write_slice_to_addr(slice_addr, &methods);

    Ok(VecValue(allocated))
}

fn fields(interp: &mut Interpreter, universe: &mut Universe) -> Result<VecValue, Error> {
    pop_args_from_stack!(interp, receiver => Gc<Class>);
    let fields: Vec<Value> = receiver.field_names.iter().copied().map(Value::Symbol).collect();
    Ok(VecValue(universe.gc_interface.alloc_slice(&fields, AllocSiteMarker::VecValue)))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
