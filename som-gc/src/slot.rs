use crate::gcref::Gc;
use crate::gcslice::GcSlice;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::{SimpleSlot, Slot};
use som_value::value::BaseValue;
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

// impl From<*mut BaseValue> for SOMSlot {
//     // we allow unsafe derefs since it's just for debugging
//     #[allow(clippy::not_unsafe_ptr_arg_deref)]
//     fn from(value: *mut BaseValue) -> Self {
//         SOMSlot::RefValueSlot(RefValueSlot {
//             value,
//             #[cfg(debug_assertions)]
//             expected_tag: (unsafe { *value }).tag(),
//         })
//     }
// }

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
    /// Reference to a value. Can be treated as a `*mut BaseValue`.
    value: Address,
    #[cfg(debug_assertions)]
    /// Tag of the value stored. Used in debugging to check that the data isn't malformed.
    expected_tag: u64,
}

impl From<*mut BaseValue> for SOMSlot {
    #[allow(clippy::not_unsafe_ptr_arg_deref)] // It's just used in debug assertions.
    fn from(value: *mut BaseValue) -> Self {
        SOMSlot::RefValueSlot(RefValueSlot {
            value: Address::from_mut_ptr(value),
            #[cfg(debug_assertions)]
            expected_tag: unsafe { (*value).tag() },
        })
    }
}

impl Slot for RefValueSlot {
    fn load(&self) -> Option<ObjectReference> {
        unsafe {
            let val_ref = self.value.as_mut_ref::<BaseValue>();
            #[cfg(debug_assertions)] // a bit silly, but otherwise rust complains release versions don't have expected_tag
            debug_assert!(
                val_ref.is_ptr_type(),
                "load failed, pointer 0x{:x} does not point to a value pointer type (value: {}, tag: {}, expected_tag: {})",
                self.value.as_usize(),
                val_ref.as_u64(),
                (*val_ref).tag(),
                self.expected_tag
            );
            ObjectReference::from_raw_address(Address::from_usize((*val_ref).extract_pointer_bits() as usize))
        }
    }

    fn store(&self, object: ObjectReference) {
        unsafe {
            let val_ref = self.value.as_mut_ref::<BaseValue>();
            debug_assert!((*val_ref).is_ptr_type());
            *val_ref = BaseValue::new((*val_ref).tag(), object.to_raw_address().as_usize() as u64);
            debug_assert!((*val_ref).is_ptr_type());
        }
    }
}
