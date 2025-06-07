use crate::gc::VecValue;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use som_gc::gcref::Gc;
use som_value::value_ptr::{HasPointerTag, TypedPtrValue};

impl<T> From<Value> for TypedPtrValue<T, Gc<T>> {
    fn from(value: Value) -> Self {
        value.0.into()
    }
}

impl<T> From<TypedPtrValue<T, Gc<T>>> for Value {
    fn from(val: TypedPtrValue<T, Gc<T>>) -> Self {
        Value(val.into())
    }
}

impl HasPointerTag for VecValue {
    fn get_tag() -> u64 {
        unreachable!("we shouldn't need to access any tag for this type anymore")
    }
}

impl HasPointerTag for Block {
    fn get_tag() -> u64 {
        unreachable!("we shouldn't need to access any tag for this type anymore")
    }
}

impl HasPointerTag for Class {
    fn get_tag() -> u64 {
        unreachable!("we shouldn't need to access any tag for this type anymore")
    }
}

impl HasPointerTag for Method {
    fn get_tag() -> u64 {
        unreachable!("we shouldn't need to access any tag for this type anymore")
    }
}

impl HasPointerTag for Instance {
    fn get_tag() -> u64 {
        unreachable!("we shouldn't need to access any tag for this type anymore")
    }
}
