use crate::gc::GcIdentifier;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::method::{Method, MethodInfo};
use som_gc::debug_assert_valid_semispace_ptr;
use som_gc::gc_interface::GcType;
use som_gc::gcref::Gc;
use som_gc::slot::SOMSlot;
use std::fmt;

#[derive(Debug, Clone)]
pub enum CacheEntry {
    Send(Gc<Class>, Gc<Method>),
    Global(Value), // unused for now
}

pub type BodyInlineCache = Vec<Option<CacheEntry>>;

/// Represents an executable block.
#[derive(Clone)]
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: Option<Gc<Frame>>,
    /// Block environment needed for execution, e.g. the block's bytecodes, literals, number of locals...
    pub blk_info: Gc<MethodInfo>,
}

impl Block {
    /// Get the block's class.
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        match self.nb_parameters() {
            0 => universe.core.block1_class(),
            1 => universe.core.block2_class(),
            2 => universe.core.block3_class(),
            _ => panic!("no support for blocks with more than 2 parameters"),
        }
    }

    /// Retrieve the number of parameters this block accepts.
    pub fn nb_parameters(&self) -> u8 {
        debug_assert_valid_semispace_ptr!(self.blk_info);
        self.blk_info.nbr_params
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(&format!("Block{}", self.nb_parameters() + 1))
            .field("block", &self.blk_info)
            .field("frame", &self.frame.as_ref().map(|f| f.as_ptr()))
            .finish()
    }
}

impl fmt::Debug for MethodInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BlockInfo")
            .field("nbr_locals", &self.nbr_locals)
            .field("nbr_params", &self.nbr_params)
            .field("literals", &self.literals)
            .finish()
    }
}

impl GcType for Block {
    fn get_magic_gc_id() -> u8 {
        GcIdentifier::Block as u8
    }

    fn scan_object(block: Gc<Self>, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        if let Some(frame) = block.frame.as_ref() {
            visit_slot_fn(SOMSlot::from(frame));
        }

        visit_slot_fn(SOMSlot::from(&block.blk_info));
    }

    fn get_size_in_memory(_self: Gc<Self>) -> usize {
        size_of::<Block>()
    }
}
