use crate::ast::AstBlock;
use crate::gc::GcIdentifier;
use som_gc::gc_interface::GcType;
use som_gc::gcref::Gc;
use som_gc::slot::SOMSlot;
use std::fmt;

use crate::universe::Universe;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;

/// Represents an executable block.
#[derive(Clone)]
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: Gc<Frame>,
    /// Block definition from the AST.
    pub block: Gc<AstBlock>,
}

impl Block {
    /// Get the block's class.
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        match self.nbr_args() {
            0 => universe.core.block1_class(),
            1 => universe.core.block2_class(),
            2 => universe.core.block3_class(),
            _ => panic!("no support for blocks with more than 2 parameters"),
        }
    }

    /// Retrieve the number of parameters this block accepts.
    pub fn nbr_args(&self) -> u8 {
        self.block.nbr_args
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(&format!("Block{}", self.nbr_args() + 1))
            // .field("block", &self.block)
            .field("frame", &self.frame)
            .finish()
    }
}

impl GcType for Block {
    fn get_magic_gc_id() -> u8 {
        GcIdentifier::Block as u8
    }

    fn scan_object(block: Gc<Self>, visit_slot_fn: &mut dyn FnMut(SOMSlot)) {
        visit_slot_fn(SOMSlot::from(&block.frame));
        visit_slot_fn(SOMSlot::from(&block.block));
    }

    fn get_size_in_memory(_self: Gc<Self>) -> usize {
        size_of::<Block>()
    }
}
