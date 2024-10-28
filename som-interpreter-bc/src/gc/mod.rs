extern crate libc;
extern crate mmtk;

use mmtk::vm::VMBinding;
use mmtk::MMTK;
use std::sync::OnceLock;

pub mod active_plan;
pub mod api;
pub mod collection;
pub mod object_model;
pub mod reference_glue;
pub mod scanning;

pub mod gc_interface;

pub type SOMSlot = mmtk::vm::slot::SimpleSlot;

#[derive(Default)]
pub struct SOMVM;

// Documentation: https://docs.mmtk.io/api/mmtk/vm/trait.VMBinding.html
impl VMBinding for SOMVM {
    type VMObjectModel = object_model::VMObjectModel;
    type VMScanning = scanning::VMScanning;
    type VMCollection = collection::VMCollection;
    type VMActivePlan = active_plan::VMActivePlan;
    type VMReferenceGlue = reference_glue::VMReferenceGlue;
    type VMSlot = SOMSlot;
    type VMMemorySlice = mmtk::vm::slot::UnimplementedMemorySlice;

    /// Allowed maximum alignment in bytes.
    // const MAX_ALIGNMENT: usize = 1 << 6;

    const ALIGNMENT_VALUE: usize = 0xdead_beef;
    /// Allowed minimal alignment in bytes.
    const MIN_ALIGNMENT: usize = 1 << 2;
    /// Allowed maximum alignment in bytes.
    const MAX_ALIGNMENT: usize = 1 << 3;
    const USE_ALLOCATION_OFFSET: bool = true;

    const ALLOC_END_ALIGNMENT: usize = 1;
}

use mmtk::util::{Address, ObjectReference};

impl SOMVM {
    pub fn object_start_to_ref(start: Address) -> ObjectReference {
        // Safety: start is the allocation result, and it should not be zero with an offset.
        unsafe {
            ObjectReference::from_raw_address_unchecked(
                start + object_model::OBJECT_REF_OFFSET,
            )
        }
    }
}

pub static MMTK_SINGLETON: OnceLock<MMTK<SOMVM>> = OnceLock::new();

fn mmtk() -> &'static MMTK<SOMVM> {
    &MMTK_SINGLETON.get().unwrap()
}