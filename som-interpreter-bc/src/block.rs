use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
#[cfg(feature = "frame-debug-info")]
use som_core::ast::BlockDebugInfo;

use som_core::bytecode::Bytecode;

use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::Frame;
// use crate::interner::Interned;
use crate::method::Method;
use crate::universe::UniverseBC;
use crate::SOMRef;

#[derive(Clone)]
pub struct BlockInfo {
    // pub locals: Vec<Interned>,
    pub literals: Vec<Literal>,
    pub body: Vec<Bytecode>,
    pub nb_locals: usize,
    pub nb_params: usize,
    pub inline_cache: RefCell<Vec<Option<(*const Class, Rc<Method>)>>>,
    #[cfg(feature = "frame-debug-info")]
    pub block_debug_info: BlockDebugInfo,
}

/// Represents an executable block.
#[derive(Clone)]
pub struct Block {
    /// Reference to the captured stack frame.
    pub frame: Option<SOMRef<Frame>>,
    pub blk_info: Rc<BlockInfo>,
}

impl Block {
    /// Get the block's class.
    pub fn class(&self, universe: &UniverseBC) -> SOMRef<Class> {
        match self.nb_parameters() {
            0 => universe.block1_class(),
            1 => universe.block2_class(),
            2 => universe.block3_class(),
            _ => panic!("no support for blocks with more than 2 parameters"),
        }
    }

    /// Retrieve the number of parameters this block accepts.
    pub fn nb_parameters(&self) -> usize {
        self.blk_info.nb_params
    }

    /// Creates a new block with POP bytecodes before each return (local and non local), so that the block returns with no effect on the stack
    pub fn make_equivalent_with_no_return(&self) -> Rc<Block> {
        fn patch_return_non_locals(new_body: &mut Vec<Bytecode>) {
            let non_local_rets_idx: Vec<usize> = new_body.iter().enumerate()
                .filter_map(|(idx, &bc)| if let Bytecode::ReturnNonLocal(_) = bc { Some(idx) } else { None })
                .collect();

            if !non_local_rets_idx.is_empty() {
                for (i, pop_insert_idx) in non_local_rets_idx.iter().enumerate() {
                    // we do "+i": +0, +1, +2, +... to adjust for the fact that we are inserting elements in succession (which changes the subsequent target indices)
                    // and we pop the SECOND TO LAST element of the stack (which is "self" in to:do: - an Integer) so we still have the return value of the ReturnNonLocal on the top of stack
                    new_body.insert(*pop_insert_idx + i, Bytecode::Pop2);
                    debug_assert!(matches!(new_body.get(*pop_insert_idx + i + 1).unwrap(), Bytecode::ReturnNonLocal(_)));
                }

                for (bc_idx, bc) in &mut new_body.iter_mut().enumerate() {
                    match bc {
                        Bytecode::Jump(jump_idx)
                        | Bytecode::JumpOnTruePop(jump_idx) | Bytecode::JumpOnFalsePop(jump_idx)
                        | Bytecode::JumpOnTrueTopNil(jump_idx) | Bytecode::JumpOnFalseTopNil(jump_idx) => {
                            let nbr_new_bc_in_range = non_local_rets_idx.iter()
                                .filter(|new_bc_idx| (bc_idx < **new_bc_idx) && (**new_bc_idx <= *jump_idx + bc_idx))
                                .count();

                            if nbr_new_bc_in_range > 0 {
                                *jump_idx += nbr_new_bc_in_range
                            }
                        },
                        Bytecode::JumpBackward(jump_idx) => {
                            let nbr_new_bc_in_range = non_local_rets_idx.iter()
                                .filter(|new_bc_idx| (**new_bc_idx < bc_idx) && (**new_bc_idx >= bc_idx - *jump_idx))
                                .count();

                            if nbr_new_bc_in_range > 0 {
                                *jump_idx += nbr_new_bc_in_range
                            }
                        },
                        _ => {}
                    }
                }
            }
        }
        
        let mut new_body = self.blk_info.body.clone();
        
        patch_return_non_locals(&mut new_body);
        
        // this isn't handled at the same time as the ReturnNonLocal because it's fine for them to keep jumping to what they think is a ReturnLocal - what we really want is them to jump to the POP we insert right now
        new_body.insert(new_body.len() - 1, Bytecode::Pop);
        
        // dbg!(&self.blk_info.body);
        // dbg!(&new_body);

        Rc::from(Block {
            frame: self.frame.clone(),
            blk_info: Rc::new(BlockInfo {
                inline_cache: RefCell::new(vec![None; new_body.len()]),
                literals: self.blk_info.literals.clone(),
                body: new_body,
                nb_locals: self.blk_info.nb_locals,
                nb_params: self.blk_info.nb_params,
                #[cfg(feature = "frame-debug-info")]
                block_debug_info: self.blk_info.block_debug_info.clone()
            }),
        })
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(&format!("Block{}", self.nb_parameters() + 1))
            .finish()
    }
}
