use std::fmt;

use som_core::ast;

use crate::class::Class;
use crate::frame::Frame;
use crate::universe::Universe;
use crate::SOMRef;

/// Represents an executable block.
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: *mut Frame,
    /// Block definition from the AST.
    pub block: ast::Block
}

impl Clone for Block {
    fn clone(&self) -> Self {
        todo!()
    }

    fn clone_from(&mut self, _source: &Self) {
        todo!()
    }
}

impl Block {
    /// Get the block's class.
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        match self.nb_parameters() {
            0 => universe.block1_class(),
            1 => universe.block2_class(),
            2 => universe.block3_class(),
            _ => panic!("no support for blocks with more than 2 parameters"),
        }
    }

    /// Retrieve the number of parameters this block accepts.
    pub fn nb_parameters(&self) -> usize {
        self.block.nbr_params
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(&format!("Block{}", self.nb_parameters() + 1))
            .finish()
    }
}
