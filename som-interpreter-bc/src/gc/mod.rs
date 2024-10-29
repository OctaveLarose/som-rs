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

// pub type SOMSlot = mmtk::vm::slot::SimpleSlot;

// because of NaN boxing, we make a new slot specifically for accessing values, which contain internally a GCRef
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SOMSlot {
    Simple(SimpleSlot),
    Value(ValueSlot)
}

impl SOMSlot {
    pub fn from_address(addr: Address) -> SOMSlot {
        SOMSlot::Simple(SimpleSlot::from_address(addr))
    }

    pub fn from_value(value: Value) -> SOMSlot {
        SOMSlot::Value(ValueSlot::from_value(value))
    }
}

impl Slot for SOMSlot {
    fn load(&self) -> Option<ObjectReference> {
        match self {
            SOMSlot::Simple(e) => e.load(),
            SOMSlot::Value(e) => e.load(),
        }
    }

    fn store(&self, object: ObjectReference) {
        match self {
            SOMSlot::Simple(e) => e.store(object),
            SOMSlot::Value(e) => e.store(object),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ValueSlot {
    value: Value // should be a pointer instead, probably (but that might be slower?). using the non nan boxed val makes slots MASSIVE otherwise.
}

impl ValueSlot {
    pub fn from_value(value: Value) -> Self {
        Self {
            value
        }
    }
}

unsafe impl Send for ValueSlot {}

impl Slot for ValueSlot {
    fn load(&self) -> Option<ObjectReference> {
        debug_assert!(self.value.is_ptr_type());
        let gcref: GCRef<()> = self.value.extract_gc_cell();
        ObjectReference::from_raw_address(gcref.ptr)
    }

    fn store(&self, _object: ObjectReference) {
        unimplemented!()
    }
}

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
    type VMMemorySlice = mmtk::vm::slot::UnimplementedMemorySlice<SOMSlot>;

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

use crate::gc::gc_interface::GCRef;
use crate::value::Value;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::{SimpleSlot, Slot};

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