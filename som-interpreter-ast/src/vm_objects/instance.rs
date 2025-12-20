use crate::gc::{visit_value, AstObjMagicId};
use crate::value::Value;
use crate::vm_objects::class::Class;
use som_gc::gc_interface::GcType;
use som_gc::gcref::Gc;
use som_gc::slot::SOMSlot;
use std::fmt;
use std::marker::PhantomData;

/// Represents a generic (non-primitive) class instance.
#[derive(Clone)]
pub struct Instance {
    /// The class of which this is an instance from.
    pub class: Gc<Class>,
    /// We store the fields right after the instance in memory.
    pub fields_marker: PhantomData<[Value]>,
}

impl Instance {
    /// Construct an instance for a given class.
    pub fn from_class(class: Gc<Class>) -> Self {
        Self {
            class,
            fields_marker: PhantomData,
        }
    }

    /// Get the class of which this is an instance from.
    pub fn class(&self) -> Gc<Class> {
        self.class.clone()
    }

    /// Get the superclass of this instance's class.
    pub fn super_class(&self) -> Option<Gc<Class>> {
        self.class.super_class()
    }

    #[inline(always)]
    fn get_field_ptr(ptr: usize, n: u8) -> *mut Value {
        (ptr + size_of::<Instance>() + (n as usize) * size_of::<Value>()) as *mut Value
    }

    /// Lookup a field in an instance.
    pub fn lookup_field(_self: &Gc<Instance>, idx: u8) -> &'static Value {
        unsafe {
            let field_ptr = Self::get_field_ptr(_self.as_ptr() as usize, idx);
            &*field_ptr
        }
    }

    /// Assign a field to an instance.
    pub fn assign_field(_self: &Gc<Self>, idx: u8, value: Value) {
        unsafe {
            let field_ptr = Self::get_field_ptr(_self.as_ptr() as usize, idx);
            *field_ptr = value
        }
    }

    pub fn get_nbr_fields(&self) -> usize {
        self.class.get_nbr_fields()
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instance")
            .field("name", &self.class.name())
            //.field("fields", &self.fields.len())
            .field("methods", &self.class().methods.len())
            .finish()
    }
}

impl GcType for Instance {
    fn get_magic_gc_id() -> u8 {
        AstObjMagicId::Instance as u8
    }

    fn scan_object(instance: Gc<Self>, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        visit_slot_fn(SOMSlot::from(&instance.class));

        for i in 0..instance.class().get_nbr_fields() {
            let val: &Value = Instance::lookup_field(&instance, i as u8);
            visit_value(val, visit_slot_fn)
        }
    }
}
