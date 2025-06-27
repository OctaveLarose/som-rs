use mmtk::util::Address;
use std::{
    fmt::{Debug, Formatter},
    marker::PhantomPinned,
};

use crate::mmtk;
use std::ops::{Deref, DerefMut};

#[macro_export]
macro_rules! debug_assert_valid_semispace_ptr {
    ($self:expr) => {
        // #[cfg(all(feature = "semispace", debug_assertions))]
        #[cfg(debug_assertions)]
        assert!($self.is_pointer_to_valid_space(), "Pointer to invalid space: {}.", $self.ptr as usize);
    };
}

#[macro_export]
macro_rules! debug_assert_valid_semispace_ptr_value {
    ($value:expr) => {
        #[cfg(debug_assertions)]
        unsafe {
            if let Some(slice) = $value.as_array() {
                if slice.get_true_size() >= 65535 {
                    // pass
                } else {
                    assert!(slice.ptr.is_pointer_to_valid_space(), "Pointer to invalid space.");
                }
            } else if let Some(ptr) = $value.0.as_something::<Gc<()>>() {
                assert!(ptr.is_pointer_to_valid_space(), "Pointer to invalid space.");
            }
        }
    };
}

/// A pointer to the heap for GC.
///
/// To note: it could be a `NonNull` instead, and have places that need a "default" value rely on Option<Gc<T>>.
/// That might be a mild speedup for all I know.
pub struct Gc<T> {
    pub ptr: *mut T,
    _phantom: PhantomPinned,
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc::new(self.ptr)
    }
}

impl<T: Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match !self.is_empty() {
            true => (**self).fmt(f),
            false => f.write_str("(empty)"),
        }
    }
}

// Occasionally we want a placeholder. Code definitely refactorable to never need this (we could just use `Option<GCRef>`), but it would likely be a minor perf hit.
impl<T> Default for Gc<T> {
    fn default() -> Self {
        Gc {
            ptr: std::ptr::null_mut(),
            _phantom: PhantomPinned,
        }
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        debug_assert_valid_semispace_ptr!(self);
        unsafe { &*self.ptr }
    }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        debug_assert_valid_semispace_ptr!(self);
        unsafe { &mut *self.ptr }
    }
}

impl<T> From<Gc<T>> for u64 {
    fn from(value: Gc<T>) -> Self {
        debug_assert!(!value.is_empty());
        value.ptr as usize as u64
    }
}

impl<T> From<u64> for Gc<T> {
    fn from(ptr: u64) -> Self {
        debug_assert!(ptr != 0);
        Gc {
            ptr: ptr as usize as *mut T,
            _phantom: PhantomPinned,
        }
    }
}

/// Convert an MMTk address into a GC pointer.
impl<T> From<Address> for Gc<T> {
    fn from(ptr: Address) -> Self {
        unsafe {
            Gc {
                ptr: ptr.as_mut_ref(),
                _phantom: PhantomPinned,
            }
        }
    }
}

impl<T> Gc<T> {
    pub fn new(ptr: *mut T) -> Self {
        Gc {
            ptr,
            _phantom: PhantomPinned,
        }
    }

    /// Checks if a frame is "empty", i.e. contains the default value
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.ptr.is_null()
    }

    /// Get a const pointer to the underlying data.
    pub fn as_ptr(&self) -> *const T {
        self.ptr
    }

    // /// Return a mutable pointer to the underlying data as an arbitrary type.
    // /// Usage discouraged, and would be better off going unused entirely.
    // pub unsafe fn unsafe_cast<U>(&self) -> *mut U {
    //     self.ptr as *mut U
    // }

    #[cfg(feature = "semispace")]
    /// Checks if the pointer points to a valid space.
    /// Because of our semispace GC, pointers can move from one space to the other, and the number one bug cause is pointers not having been moved.
    /// So this function is tremendously useful for debugging.
    pub fn is_pointer_to_valid_space(&self) -> bool {
        //return true;

        fn leftmost_digit(mut number: usize) -> u8 {
            while number >= 10 {
                number /= 10;
            }
            number as u8
        }

        let gc_interface = unsafe { &**crate::MUTATOR_WRAPPER.get().unwrap() };

        // if we're collecting, we're handling both new and old pointers, so we just say they're all valid for simplicity.
        if gc_interface.is_currently_collecting() {
            return true;
        }

        match gc_interface.get_nbr_collections() % 2 == 0 {
            true => leftmost_digit(self.ptr as usize) == 2,
            false => leftmost_digit(self.ptr as usize) == 4,
        }
    }

    #[cfg(feature = "marksweep")]
    pub fn is_pointer_to_valid_space(&self) -> bool {
        true
    }
}
