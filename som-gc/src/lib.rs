#![allow(static_mut_refs)]
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
pub mod gcref;
pub mod gcslice;
pub mod slot;

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

    const ALIGNMENT_VALUE: u8 = 0xff;
    /// Allowed minimal alignment in bytes.
    const MIN_ALIGNMENT: usize = 1 << 2;
    /// Allowed maximum alignment in bytes.
    const MAX_ALIGNMENT: usize = 1 << 3; // const MAX_ALIGNMENT: usize = 1 << 6;
    const USE_ALLOCATION_OFFSET: bool = true;

    const ALLOC_END_ALIGNMENT: usize = 1;
}

use crate::gc_interface::{GCInterface, MMTKtoVMCallbacks};
use crate::slot::SOMSlot;
use mmtk::util::{Address, ObjectReference};

impl SOMVM {
    pub fn object_start_to_ref(start: Address) -> ObjectReference {
        // Safety: start is the allocation result, and it should not be zero with an offset.
        unsafe { ObjectReference::from_raw_address_unchecked(start + object_model::OBJECT_REF_OFFSET) }
    }
}

pub static MMTK_SINGLETON: OnceLock<MMTK<SOMVM>> = OnceLock::new();

fn mmtk() -> &'static MMTK<SOMVM> {
    MMTK_SINGLETON.get().unwrap()
}

pub(crate) static mut VM_TO_MMTK_INTERFACE: OnceLock<*mut GCInterface> = OnceLock::new();
pub(crate) static MMTK_TO_VM_INTERFACE: OnceLock<MMTKtoVMCallbacks> = OnceLock::new();

/// Initialize globals to allow VM/MMTk communication.
/// TODO: Ideally, this would also take a closure that initializes the VM-specific globals,
/// namely the `UNIVERSE_RAW_POINTER` needed for the VM to report roots given the universe when MMTk requests them from the VM.
/// As such, this function only contains MOST of the handshake logic.
pub fn handshake_with_vm(gc_interface_ptr: &mut Box<GCInterface>, vm_callbacks: MMTKtoVMCallbacks) {
    unsafe {
        if VM_TO_MMTK_INTERFACE.get().is_none() {
            // # (Un)Safety: we duplicate a mutable reference to the GC interface ptr since MMTk must also hold a reference to it.
            // A better solution would be to just use an `Rc`, but then we'd have a minor - though likely negligible - perf cost every time we want to e.g. allocate
            // This operation is safe because the Box<GCInterface> never moves, and lasts throughout all of execution.
            let dup_ptr = gc_interface_ptr.as_mut() as *mut GCInterface;
            VM_TO_MMTK_INTERFACE.set(dup_ptr).unwrap_or_else(|_| panic!("couldn't set mutator wrapper?"));
        }

        if MMTK_TO_VM_INTERFACE.get().is_none() {
            MMTK_TO_VM_INTERFACE.get_or_init(|| vm_callbacks);
        }
    }
}
