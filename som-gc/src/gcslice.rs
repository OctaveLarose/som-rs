use std::marker::PhantomPinned;

use crate::gcref::Gc;
use mmtk::util::Address;

/// Special, subtype of GC ref that stores a list.
/// It's really just a `Vec<T>` replacement (though immutable), where Rust manages none of the memory itself.
/// Used because finalization might be a slowdown if we stored references to `Vec`s on the heap?
pub struct GcSlice<T> {
    pub ptr: Gc<T>,
    _phantom: PhantomPinned, // NB: not sure that's needed at all since Gc<T> is already !Unpin.
}

impl<T> Clone for GcSlice<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
            _phantom: PhantomPinned,
        }
    }
}

impl<T> GcSlice<T>
where
    T: std::fmt::Debug,
{
    pub fn new(ptr: Address) -> GcSlice<T> {
        debug_assert!(!ptr.is_zero());
        GcSlice {
            ptr: Gc::from(ptr),
            _phantom: PhantomPinned,
        }
    }

    pub fn iter(&self) -> GCSliceIter<'_, T> {
        GCSliceIter { gc_slice: self, cur_idx: 0 }
    }

    pub fn len(&self) -> usize {
        let len: &usize = unsafe { &*(self.ptr.as_ptr() as *const usize) };
        *len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get_true_size(&self) -> usize {
        // the usize for the length, and all the values.
        size_of::<usize>() + self.len() * size_of::<T>()
    }

    /// Get the address of the Nth element.
    /// # Safety
    /// Safe if checked ahead of time that n is within the slice's bounds.
    pub unsafe fn nth_addr(&self, n: usize) -> Address {
        Address::from_usize(self.ptr.as_ptr().byte_add(size_of::<usize>() + (n * std::mem::size_of::<T>())) as usize)
    }

    #[inline(always)]
    pub fn get(&self, idx: usize) -> &T {
        unsafe { self.nth_addr(idx).as_ref() }
    }

    pub fn get_checked(&self, idx: usize) -> Option<&T> {
        if idx >= self.len() {
            return None;
        }
        Some(self.get(idx))
    }

    #[inline(always)]
    pub fn get_mut(&mut self, idx: usize) -> &mut T {
        debug_assert!(idx < self.len());
        unsafe { self.nth_addr(idx).as_mut_ref() }
    }

    pub fn get_checked_mut(&mut self, idx: usize) -> Option<&mut T> {
        if idx >= self.len() {
            return None;
        }
        Some(self.get_mut(idx))
    }

    pub fn set(&mut self, idx: usize, val: T) {
        debug_assert!(idx < self.len());
        *self.get_mut(idx) = val
    }
}

impl<T> PartialEq for GcSlice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr // not correct i think? should compare each element individually instead.
    }
}

impl<T> From<&GcSlice<T>> for Address {
    fn from(ptr: &GcSlice<T>) -> Self {
        Address::from_ref(ptr)
    }
}

impl<T> From<GcSlice<T>> for u64 {
    fn from(val: GcSlice<T>) -> Self {
        val.ptr.into()
    }
}

impl<T> From<u64> for GcSlice<T> {
    fn from(value: u64) -> Self {
        Self {
            ptr: Gc::from(value),
            _phantom: PhantomPinned,
        }
    }
}

impl<T: std::fmt::Debug> From<Address> for GcSlice<T> {
    fn from(value: Address) -> Self {
        GcSlice::new(value)
    }
}

pub struct GCSliceIter<'a, T> {
    gc_slice: &'a GcSlice<T>,
    cur_idx: usize,
}

impl<'a, T: std::fmt::Debug> Iterator for GCSliceIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.gc_slice.get_checked(self.cur_idx);
        self.cur_idx += 1;
        item
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for GcSlice<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("GcSlice: ")?;
        let mut debug_list = f.debug_list();
        for idx in 0..self.len() {
            debug_list.entry(self.get(idx));
        }
        debug_list.finish()
    }
}

/// Used by VMs need to specify what type is allowed to be put inside of a slice
pub trait SupportedSliceType {
    fn get_magic_gc_slice_id() -> u8
    where
        Self: Sized;
}

// FEAT: I like the idea of a common GcType for slices like that, but it makes it more difficult to supply a scanning closure
// since the scanning logic depends on the stored type. So shelved for now.

//impl<T: SupportedSliceType + std::fmt::Debug> GcType for GcSlice<T> {
//    fn get_magic_gc_id() -> u8 {
//        T::get_magic_gc_slice_id()
//    }
//
//    fn scan_object(_self: Gc<Self>, _scan_fn: &mut dyn FnMut(SOMSlot)) {
//        // has to be implemented on the VM size, since this needs a function definition change to pass a scanning closure that takes a T and not just a SOMSlot
//        // (which could be implemented with a new trait probably! FEAT: doing that.)
//        unimplemented!("scanning slices implementation is currently done only on VM-specific code")
//    }
//
//    fn get_size_in_memory(_self: Gc<Self>) -> usize {
//        // conversion feels a bit hackish, but this is essentially just a downcast. With a new trait that would take a GcSlice<T> instead of Gc<GcSlice<T>>, we'd be better off
//        let slice: GcSlice<T> = GcSlice::new(Address::from_ptr(_self.ptr));
//        slice.get_true_size()
//    }
//}
