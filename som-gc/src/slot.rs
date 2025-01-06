use crate::gcref::Gc;
use crate::gcslice::GcSlice;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::{SimpleSlot, Slot};
use som_core::value::BaseValue;
// pub type SOMSlot = mmtk::vm::slot::SimpleSlot;

// because of NaN boxing, we make a new slot specifically for accessing values, which contain internally a GCRef
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SOMSlot {
    Simple(SimpleSlot),
    RefValueSlot(RefValueSlot),
}

/// Most common case: turn any pointer to any heap address into a slot.
/// This pointer must be on the heap or in a static variable! Otherwise, it becomes invalid when Rust discards it.
impl<T> From<&Gc<T>> for SOMSlot {
    fn from(value: &Gc<T>) -> Self {
        SOMSlot::Simple(SimpleSlot::from_address(Address::from_ref(value)))
    }
}

impl<T> From<&GcSlice<T>> for SOMSlot {
    fn from(value: &GcSlice<T>) -> Self {
        SOMSlot::Simple(SimpleSlot::from_address(Address::from_ref(value)))
    }
}

impl From<*mut BaseValue> for SOMSlot {
    fn from(value: *mut BaseValue) -> Self {
        SOMSlot::RefValueSlot(RefValueSlot { value })
    }
}

impl Slot for SOMSlot {
    fn load(&self) -> Option<ObjectReference> {
        match self {
            SOMSlot::Simple(e) => e.load(),
            SOMSlot::RefValueSlot(e) => e.load(),
        }
    }

    fn store(&self, object: ObjectReference) {
        match self {
            SOMSlot::Simple(e) => e.store(object),
            SOMSlot::RefValueSlot(e) => e.store(object),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RefValueSlot {
    value: *mut BaseValue,
}

unsafe impl Send for RefValueSlot {}

impl Slot for RefValueSlot {
    fn load(&self) -> Option<ObjectReference> {
        unsafe {
            debug_assert!((*self.value).is_ptr_type());
            ObjectReference::from_raw_address(Address::from_usize((*self.value).extract_pointer_bits() as usize))
        }
    }

    fn store(&self, object: ObjectReference) {
        unsafe {
            debug_assert!((*self.value).is_ptr_type());
            *self.value = BaseValue::new((*self.value).tag(), object.to_raw_address().as_usize() as u64);
            debug_assert!((*self.value).is_ptr_type());
        }
    }
}
