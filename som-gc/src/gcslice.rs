use std::marker::PhantomPinned;

use crate::gcref::Gc;
use mmtk::util::Address;

/// Special GC ref that stores a list.
/// It's really just a `Vec<T>` replacement (though immutable), where Rust manages none of the memory itself.
/// Used because finalization might be a slowdown if we stored references to `Vec`s on the heap?
///
/// TODO: there should be NO NEED for a PhantomPinned field since Gc<T> is already !Unpin.
/// But I'm paranoid. Will remove later when VM fully sound, it's zero cost anyway.
pub struct GcSlice<T> {
    pub ptr: Gc<T>,
    _phantom: PhantomPinned,
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
    pub fn new(ptr: Address, obj: &[T]) -> GcSlice<T> {
        debug_assert!(!ptr.is_zero());

        unsafe {
            *ptr.as_mut_ref() = obj.len();
            let obj_addr = ptr.add(size_of::<usize>());
            std::ptr::copy_nonoverlapping(obj.as_ptr(), obj_addr.as_mut_ref(), obj.len());
        }
        Self {
            ptr: ptr.into(),
            _phantom: PhantomPinned,
        }
    }

    pub fn iter(&self) -> GCSliceIter<T> {
        GCSliceIter { gc_slice: self, cur_idx: 0 }
    }

    //pub fn iter_mut(&mut self) -> GCSliceIterMut<T> {
    //    GCSliceIterMut { gc_slice: self, cur_idx: 0 }
    //}

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
        Self {
            ptr: value.into(),
            _phantom: PhantomPinned,
        }
    }
}

pub struct GCSliceIter<'a, T> {
    gc_slice: &'a GcSlice<T>,
    cur_idx: usize,
}

//pub struct GCSliceIterMut<'a, T> {
//    gc_slice: &'a mut GcSlice<T>,
//    cur_idx: usize,
//}

impl<'a, T: std::fmt::Debug> Iterator for GCSliceIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.gc_slice.get_checked(self.cur_idx);
        self.cur_idx += 1;
        item
    }
}

//impl<'a, T: std::fmt::Debug> Iterator for GCSliceIterMut<'a, T> {
//    type Item = &'a mut T;
//
//    fn next(&mut self) -> Option<Self::Item> {
//        let item = self.gc_slice.get_checked_mut(self.cur_idx);
//        self.cur_idx += 1;
//        item
//    }
//}

// impl<T: std::fmt::Debug> std::fmt::Debug for GcSlice<T> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_str("GcSlice [")?;
//         for idx in 0..self.len() {
//             f.write_str(&format!("{:?},\n", self.get(idx)))?;
//         }
//         f.write_str("]")
//     }
// }

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

#[test]
fn test_basic() {
    use std::alloc::{alloc, Layout};

    let slice_size = size_of::<usize>() + 4 * size_of::<i32>();
    let layout = Layout::from_size_align(slice_size, 8).unwrap();
    let addr = unsafe { alloc(layout) };

    let slice: GcSlice<i32> = GcSlice::new(Address::from_ptr(addr), &[1, 2, 3, 4]);

    assert_eq!(slice.len(), 4);
    assert_eq!(slice.get_true_size(), slice_size);

    assert_eq!(slice.get(0), &1);
    assert_eq!(slice.get_checked(1), Some(&2));
    assert_eq!(slice.get(2), &3);
    assert_eq!(slice.get(3), &4);
    assert_eq!(slice.get_checked(4), None);
}

#[test]
fn test_iter() {
    use std::alloc::{alloc, Layout};

    let slice_size = size_of::<usize>() + 4 * size_of::<char>();
    let layout = Layout::from_size_align(slice_size, 8).unwrap();
    let addr = unsafe { alloc(layout) };

    let og_slice = &['a', 'b', 'c', 'd'];
    let slice: GcSlice<char> = GcSlice::new(Address::from_ptr(addr), og_slice);

    for (idx, slice_elem) in slice.iter().enumerate() {
        assert_eq!(slice_elem, &og_slice[idx])
    }
}
