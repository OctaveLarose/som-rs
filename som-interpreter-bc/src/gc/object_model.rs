use crate::frame::Frame;
use crate::gc::SOMVM;
use log::debug;
use mmtk::util::copy::{CopySemantics, GCWorkerCopyContext};
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::*;

pub struct VMObjectModel {}

/// This is the offset from the allocation result to the object reference for the object.
/// For bindings that this offset is not a constant, you can implement the calculation in the method `ref_to_object_start``, and
/// remove this constant.
pub const OBJECT_REF_OFFSET: usize = 8; // TODO: 8 bytes is overkill. though we need all that for alignment reasons... don't we?

/// This is the offset from the object reference to an in-object address. The binding needs
/// to guarantee the in-object address is inside the storage associated with the object.
/// It has to be a constant offset. See `ObjectModel::IN_OBJECT_ADDRESS_OFFSET`.
pub const IN_OBJECT_ADDRESS_OFFSET: isize = 0;

// This is the offset from the object reference to the object header.
// This value is used in `ref_to_header` where MMTk loads header metadata from.
pub const OBJECT_HEADER_OFFSET: usize = 8;

// Mine. to put in GC headers
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum GCMagicId {
    Frame = 100,
    BlockInfo = 101,
    Block = 102,
    Class = 103,
    Instance = 104,
    Method = 105,
    String = 106,
    ArrayVal = 107,
    ArrayU8 = 108,
    BigInt = 109,
}

// Documentation: https://docs.mmtk.io/api/mmtk/vm/object_model/trait.ObjectModel.html
impl ObjectModel<SOMVM> for VMObjectModel {
    // Global metadata

    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();

    // Local metadata

    // Forwarding pointers have to be in the header. It is okay to overwrite the object payload with a forwarding pointer.
    // FIXME: The bit offset needs to be set properly.
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        VMLocalForwardingPointerSpec::in_header(0);
    // The other metadata can be put in the side metadata.
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::side_first();
    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec =
        VMLocalMarkBitSpec::side_after(Self::LOCAL_FORWARDING_BITS_SPEC.as_spec());
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec =
        VMLocalLOSMarkNurserySpec::side_after(Self::LOCAL_MARK_BIT_SPEC.as_spec());

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = OBJECT_REF_OFFSET as isize;

    fn copy(
        from: ObjectReference,
        semantics: CopySemantics,
        copy_context: &mut GCWorkerCopyContext<SOMVM>,
    ) -> ObjectReference {
        debug!("invoking copy (unfinished...)");

        // dbg!(&from);
        // let _from_ptr: *mut usize = unsafe { from.to_raw_address().as_mut_ref() };

        let bytes = size_of::<Frame>(); // we only ever handle frames with GC at the moment!..
        let align = 8; // todo is that correct?
        let offset = 0;

        // let from_addr = from.to_raw_address();
        let from_start = Self::ref_to_object_start(from);
        // let _header_offset = from_addr - from_start;

        let from_and_header = unsafe {ObjectReference::from_raw_address_unchecked(from_start)};
        //dbg!(header_offset);

        let dst = copy_context.alloc_copy(from_and_header, bytes, align, offset, semantics);
        debug_assert!(!dst.is_zero());
        
        // dbg!(&dst);
        // unsafe {
        //     let frame: &mut Frame = dst.as_mut_ref();
        //     dbg!(&(*(frame.current_method)).signature);
        //     dbg!(&(*(frame.current_method)).signature);
        // }

        // let to_obj = unsafe { ObjectReference::from_raw_address_unchecked(dst + header_offset) };
        let to_obj = unsafe { ObjectReference::from_raw_address_unchecked(dst) };
        
        let _dst_addr: *mut usize = unsafe { dst.as_mut_ref() };

        copy_context.post_copy(to_obj, bytes, semantics);

        debug!("Copied object {} into {}", from, to_obj);
        
        to_obj
    }

    fn copy_to(_from: ObjectReference, _to: ObjectReference, _region: Address) -> Address {
        unimplemented!()
    }

    fn get_current_size(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        // FIXME: This assumes the object size is unchanged during copying.
        Self::get_current_size(object)
    }

    fn get_align_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_align_offset_when_copied(_object: ObjectReference) -> usize {
        unimplemented!()
    }

    fn get_reference_when_copied_to(_from: ObjectReference, _to: Address) -> ObjectReference {
        unimplemented!()
    }

    fn get_type_descriptor(_reference: ObjectReference) -> &'static [i8] {
        unimplemented!()
    }

    fn ref_to_object_start(object: ObjectReference) -> Address {
        object.to_raw_address().sub(OBJECT_REF_OFFSET)
    }

    fn ref_to_header(object: ObjectReference) -> Address {
        object.to_raw_address().sub(OBJECT_HEADER_OFFSET)
    }

    fn dump_object(_object: ObjectReference) {
        unimplemented!()
    }
}
