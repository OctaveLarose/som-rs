extern crate libc;
extern crate mmtk;

use mmtk::vm::VMBinding;
use mmtk::{MMTKBuilder, MMTK};
use std::sync::atomic::AtomicBool;
use std::sync::Mutex;
use structopt::lazy_static::lazy_static;

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

use crate::gc::api::mmtk_set_fixed_heap_size;
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


lazy_static! {
    pub static ref MMTK_HAS_RAN_INIT_COLLECTION: AtomicBool = AtomicBool::new(false);
    pub static ref BUILDER: Mutex<MMTKBuilder> = Mutex::new(MMTKBuilder::new());
    pub static ref MMTK_SINGLETON: MMTK<SOMVM> = {
        let mut builder = BUILDER.lock().unwrap();

        let heap_success = mmtk_set_fixed_heap_size(&mut builder, 1024 * 1024);
        assert!(heap_success, "Couldn't set MMTk fixed heap size");

        // let gc_success = builder.set_option("plan", "NoGC");
        let gc_success = builder.set_option("plan", "MarkSweep");
        // let gc_success = builder.set_option("plan", "SemiSpace");
        assert!(gc_success, "Couldn't set GC plan");

        // let ok = builder.set_option("stress_factor", DEFAULT_STRESS_FACTOR.to_string().as_str());
        // assert!(ok);
        // let ok = builder.set_option("analysis_factor", DEFAULT_STRESS_FACTOR.to_string().as_str());
        // assert!(ok);

        let ret = mmtk::memory_manager::mmtk_init::<SOMVM>(&builder);
        *ret
    };
}

fn mmtk() -> &'static MMTK<SOMVM> {
    &MMTK_SINGLETON
}