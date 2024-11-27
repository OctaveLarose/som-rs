use crate::universe::Universe;
use crate::vm_objects::class::Class;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::method::{Method, MethodEnv};
use som_gc::gcref::Gc;
use std::fmt;

pub type BodyInlineCache = Vec<Option<(Gc<Class>, Gc<Method>)>>;

/// Represents an executable block.
#[derive(Clone)]
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: Option<Gc<Frame>>,
    /// Block environment needed for execution, e.g. the block's bytecodes, literals, number of locals...
    pub blk_info: Gc<MethodEnv>,
}

impl Block {
    /// Get the block's class.
    pub fn class(&self, universe: &Universe) -> Gc<Class> {
        match self.nb_parameters() {
            0 => universe.block1_class(),
            1 => universe.block2_class(),
            2 => universe.block3_class(),
            _ => panic!("no support for blocks with more than 2 parameters"),
        }
    }

    /// Retrieve the number of parameters this block accepts.
    pub fn nb_parameters(&self) -> usize {
        self.blk_info.nbr_params
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(&format!("Block{}", self.nb_parameters() + 1))
            .field("block", &self.blk_info)
            .field("frame", &self.frame.map(|f| f.ptr))
            .finish()
    }
}

impl fmt::Debug for MethodEnv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BlockInfo")
            .field("nbr_locals", &self.nbr_locals)
            .field("nbr_params", &self.nbr_params)
            .field("literals", &self.literals)
            .finish()
    }
}