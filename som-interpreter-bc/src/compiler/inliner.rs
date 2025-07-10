#![allow(unused)] // because inlining can be disabled

use crate::compiler::compile::{get_max_stack_size, InnerGenCtxt, MethodCodegen};
use crate::compiler::inliner::JumpType::{JumpOnFalse, JumpOnTrue};
use crate::compiler::inliner::OrAndChoice::{And, Or};
use crate::compiler::Literal;
use crate::vm_objects::block::Block;
use crate::vm_objects::method::{BasicMethodInfo, Method, MethodInfo};
use som_core::ast;
use som_core::bytecode::Bytecode;
use som_core::interner::Interner;
use som_gc::gc_interface::GCInterface;
use som_gc::gcref::Gc;

pub(crate) enum JumpType {
    JumpOnFalse,
    JumpOnTrue,
}

pub(crate) enum OrAndChoice {
    Or,
    And,
}

// TODO some of those should return Result types and throw errors instead, most likely.
pub(crate) trait PrimMessageInliner {
    /// Starts inlining a function if it's on the list of inlinable functions.
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines an expression. If this results in a PushBlock, calls `inline_last_push_block_bc(...)` to inline the block.
    fn inline_expression(&self, ctxt: &mut dyn InnerGenCtxt, expression: &ast::Expression, gc_interface: &mut GCInterface) -> Option<()>;
    /// Gets the last bytecode, assumes it to be a PushBlock, removes it and inlines the block - a set of operations for which there is a redundant need.
    fn inline_last_push_block_bc(&self, ctxt: &mut dyn InnerGenCtxt, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines a compiled block into the current scope.
    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &Method, gc_interface: &mut GCInterface) -> Option<()>;
    /// When inlining a block, adapt its potential children blocks to account for the inlining changes.
    fn adapt_block_after_outer_inlined(&self, block_body: Gc<Block>, adjust_scope_by: usize, interner: &Interner);
    /// Inlines `ifTrue:` and `ifFalse:`.
    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines `ifTrue:ifFalse:`.
    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines `whileTrue:` and `whileFalse:`.
    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines `and:` and `or:`.
    fn inline_or_and(&self, ctxt: &mut dyn InnerGenCtxt, or_and_choice: OrAndChoice, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines `to:do`.
    fn inline_to_do(&self, ctxt: &mut dyn InnerGenCtxt, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines `ifNil:` and `ifNotNil:`.
    fn inline_if_nil_or_if_not_nil(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()>;
    /// Inlines `ifNil:ifNotNil` and `ifNotNil:ifNil:`.
    fn inline_if_nil_if_not_nil(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()>;
}

impl PrimMessageInliner for ast::Message {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, gc_interface: &mut GCInterface) -> Option<()> {
        match self.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(ctxt, JumpOnFalse, gc_interface),
            "ifFalse:" => self.inline_if_true_or_if_false(ctxt, JumpOnTrue, gc_interface),
            "ifTrue:ifFalse:" => self.inline_if_true_if_false(ctxt, JumpOnFalse, gc_interface),
            "ifFalse:ifTrue:" => self.inline_if_true_if_false(ctxt, JumpOnTrue, gc_interface),
            "whileTrue:" => self.inline_while(ctxt, JumpOnFalse, gc_interface),
            "whileFalse:" => self.inline_while(ctxt, JumpOnTrue, gc_interface),
            "or:" | "||" => self.inline_or_and(ctxt, Or, gc_interface),
            "and:" | "&&" => self.inline_or_and(ctxt, And, gc_interface),
            "to:do:" => self.inline_to_do(ctxt, gc_interface),
            "ifNil:" => self.inline_if_nil_or_if_not_nil(ctxt, JumpOnFalse, gc_interface),
            "ifNotNil:" => self.inline_if_nil_or_if_not_nil(ctxt, JumpOnTrue, gc_interface),
            "ifNil:ifNotNil:" => self.inline_if_nil_if_not_nil(ctxt, JumpOnFalse, gc_interface),
            "ifNotNil:ifNil:" => self.inline_if_nil_if_not_nil(ctxt, JumpOnTrue, gc_interface),
            _ => None,
        }
    }

    fn inline_expression(&self, ctxt: &mut dyn InnerGenCtxt, expression: &ast::Expression, gc_interface: &mut GCInterface) -> Option<()> {
        expression.codegen(ctxt, gc_interface)?;
        match ctxt.get_instructions().last()? {
            Bytecode::PushBlock(_) => self.inline_last_push_block_bc(ctxt, gc_interface),
            _ => Some(()),
        }
    }

    fn inline_last_push_block_bc(&self, ctxt: &mut dyn InnerGenCtxt, gc_interface: &mut GCInterface) -> Option<()> {
        let block_idx = match ctxt.get_instructions().last()? {
            Bytecode::PushBlock(val) => *val,
            bc => panic!("inlining function expects last bytecode to be a PUSH_BLOCK, instead it was {}.", bc),
        };
        ctxt.pop_instr(); // removing the PUSH_BLOCK

        let cond_block_ref = match ctxt.get_literal(block_idx as usize)? {
            Literal::Block(val) => val.clone(),
            _ => return None,
        };
        ctxt.remove_literal(block_idx as usize);

        // because we just inlined that block, i don't want it counted towards the total program representation amount
        gc_interface.total_program_repr_size -= (size_of::<Block>() + 8) as u128;
        //gc_interface.total_program_repr_size -= 1;

        match self.inline_compiled_block(ctxt, &(cond_block_ref.blk_info), gc_interface) {
            None => panic!("Inlining a compiled block failed!"),
            _ => Some(()),
        }
    }

    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &Method, gc_interface: &mut GCInterface) -> Option<()> {
        let nbr_locals_pre_inlining = ctxt.get_nbr_locals() as u8;
        let block = block.get_env();
        let nbr_args_pre_inlining = block.nbr_params as u8;

        ctxt.set_nbr_locals(nbr_locals_pre_inlining as usize + block.nbr_locals + block.nbr_params);

        // all params in the block become local variables. not the prettiest way of going about it? works though
        let block = &{
            let mut new_blk = block.clone();
            new_blk.nbr_locals += new_blk.nbr_params;
            new_blk.nbr_params = 0;
            new_blk
        };

        // last is always ReturnLocal, so it gets ignored
        if let Some((_, body)) = block.body.split_last() {
            for block_bc in body {
                match block_bc {
                    Bytecode::PushLocal(idx) => ctxt.push_instr(Bytecode::PushLocal(nbr_locals_pre_inlining + nbr_args_pre_inlining + *idx)),
                    Bytecode::PushNonLocal(up_idx, idx) => match *up_idx - 1 {
                        0 => ctxt.push_instr(Bytecode::PushLocal(*idx)),
                        _ => ctxt.push_instr(Bytecode::PushNonLocal(*up_idx - 1, *idx)),
                    },
                    Bytecode::PopLocal(up_idx, idx) => match up_idx {
                        0 => ctxt.push_instr(Bytecode::PopLocal(*up_idx, nbr_locals_pre_inlining + nbr_args_pre_inlining + *idx)),
                        1.. => ctxt.push_instr(Bytecode::PopLocal(*up_idx - 1, *idx)),
                    },
                    Bytecode::PushArg(idx) => {
                        ctxt.push_instr(Bytecode::PushLocal(nbr_locals_pre_inlining + *idx - 1))
                        // -1 because of the self arg
                    }
                    Bytecode::PushNonLocalArg(up_idx, idx) => match *up_idx - 1 {
                        0 => match *idx {
                            0 => ctxt.push_instr(Bytecode::PushSelf),
                            _ => ctxt.push_instr(Bytecode::PushArg(*idx)),
                        },
                        _ => ctxt.push_instr(Bytecode::PushNonLocalArg(*up_idx - 1, *idx)),
                    },
                    Bytecode::PopArg(up_idx, idx) => ctxt.push_instr(Bytecode::PopArg(*up_idx - 1, *idx)),
                    Bytecode::Send1(lit_idx)
                    | Bytecode::Send2(lit_idx)
                    | Bytecode::Send3(lit_idx)
                    | Bytecode::SendN(lit_idx)
                    | Bytecode::SuperSend(lit_idx) => match block_bc {
                        Bytecode::Send1(_) => ctxt.push_instr(Bytecode::Send1(*lit_idx)),
                        Bytecode::Send2(_) => ctxt.push_instr(Bytecode::Send2(*lit_idx)),
                        Bytecode::Send3(_) => ctxt.push_instr(Bytecode::Send3(*lit_idx)),
                        Bytecode::SendN(_) => ctxt.push_instr(Bytecode::SendN(*lit_idx)),
                        Bytecode::SuperSend(_) => ctxt.push_instr(Bytecode::SuperSend(*lit_idx)),
                        _ => unreachable!(),
                    },
                    Bytecode::PushBlock(block_idx) => {
                        match block.literals.get(*block_idx as usize)? {
                            Literal::Block(inner_block) => {
                                self.adapt_block_after_outer_inlined(inner_block.clone(), 1, ctxt.get_interner());
                                let idx = ctxt.push_literal(Literal::Block(inner_block.clone()));
                                ctxt.push_instr(Bytecode::PushBlock(idx as u8));
                            }
                            _ => panic!("PushBlock not actually pushing a block somehow"),
                        };
                    }
                    Bytecode::PushGlobal(global_idx) => {
                        let lit = block.literals.get(*global_idx as usize)?;
                        let lit_idx = ctxt.push_literal(lit.clone());
                        ctxt.push_instr(Bytecode::PushGlobal(lit_idx as u8));
                    }
                    Bytecode::PushConstant(_) => {
                        let constant_idx = match block_bc {
                            Bytecode::PushConstant(idx) => *idx,
                            _ => unreachable!(),
                        };

                        let lit = block.literals.get(constant_idx as usize)?;
                        let lit_idx = ctxt.push_literal(lit.clone());
                        ctxt.push_instr(Bytecode::PushConstant(lit_idx as u8))
                    }
                    Bytecode::ReturnNonLocal(scope) => match scope - 1 {
                        0 => ctxt.push_instr(Bytecode::ReturnLocal),
                        new_scope => ctxt.push_instr(Bytecode::ReturnNonLocal(new_scope)),
                    },
                    Bytecode::ReturnLocal => {}
                    Bytecode::ReturnSelf => {
                        panic!("Inlining found a ReturnSelf in a block, which should be impossible.");
                    }
                    Bytecode::Jump(idx) => ctxt.push_instr(Bytecode::Jump(*idx)),
                    Bytecode::JumpBackward(idx) => ctxt.push_instr(Bytecode::JumpBackward(*idx)),
                    Bytecode::JumpOnTruePop(idx) => ctxt.push_instr(Bytecode::JumpOnTruePop(*idx)),
                    Bytecode::JumpOnFalsePop(idx) => ctxt.push_instr(Bytecode::JumpOnFalsePop(*idx)),
                    Bytecode::JumpOnTrueTopNil(idx) => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(*idx)),
                    Bytecode::JumpOnFalseTopNil(idx) => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(*idx)),
                    Bytecode::JumpIfGreater(idx) => ctxt.push_instr(Bytecode::JumpIfGreater(*idx)),
                    Bytecode::JumpOnNilTopTop(idx) => ctxt.push_instr(Bytecode::JumpOnNilTopTop(*idx)),
                    Bytecode::JumpOnNotNilTopTop(idx) => ctxt.push_instr(Bytecode::JumpOnNotNilTopTop(*idx)),
                    Bytecode::JumpOnNilPop(idx) => ctxt.push_instr(Bytecode::JumpOnNilPop(*idx)),
                    Bytecode::JumpOnNotNilPop(idx) => ctxt.push_instr(Bytecode::JumpOnNotNilPop(*idx)),
                    Bytecode::Dup
                    | Bytecode::Dup2
                    | Bytecode::Inc
                    | Bytecode::Dec
                    | Bytecode::Push0
                    | Bytecode::Push1
                    | Bytecode::PushNil
                    | Bytecode::PushSelf
                    | Bytecode::Pop
                    | Bytecode::PushField(_)
                    | Bytecode::PopField(_) => ctxt.push_instr(*block_bc),
                }
            }
        }

        Some(())
    }

    // TODO: kinda ugly orig_block isn't passed as a &mut Gc<Block> to show this function is all about mutating it.
    fn adapt_block_after_outer_inlined(&self, mut orig_block: Gc<Block>, adjust_scope_by: usize, interner: &Interner) {
        let new_body: Vec<Bytecode> = orig_block
            .blk_info
            .get_env()
            .body
            .iter()
            .map(|b| match b {
                Bytecode::PushNonLocal(up_idx, _)
                | Bytecode::PopLocal(up_idx, _)
                | Bytecode::PushNonLocalArg(up_idx, _)
                | Bytecode::PopArg(up_idx, _) => {
                    let new_up_idx = match *up_idx {
                        0 => 0, // local var/arg, not affected by inlining, stays the same
                        d if d > adjust_scope_by as u8 => *up_idx - 1,
                        _ => *up_idx,
                    };

                    // TODO ACTUALLY shouldn't the idx be adjusted depending on the amount of inlined variables in the block? make a test for that!
                    // and for the args case too!
                    // (present me): that's correct. see AST for correct (AFAIK) implem for inlining

                    match b {
                        Bytecode::PushNonLocal(_, idx) => match new_up_idx {
                            0 => Bytecode::PushLocal(*idx),
                            _ => Bytecode::PushNonLocal(new_up_idx, *idx),
                        },
                        Bytecode::PopLocal(_, idx) => Bytecode::PopLocal(new_up_idx, *idx),
                        Bytecode::PushNonLocalArg(_, idx) => match new_up_idx {
                            0 => match *idx {
                                0 => Bytecode::PushSelf,
                                _ => Bytecode::PushArg(*idx),
                            },
                            _ => Bytecode::PushNonLocalArg(new_up_idx, *idx),
                        },
                        Bytecode::PopArg(_, idx) => Bytecode::PopArg(new_up_idx, *idx),
                        _ => unreachable!(),
                    }
                }
                Bytecode::ReturnNonLocal(scope) => match scope - 1 {
                    0 => Bytecode::ReturnLocal,
                    new_scope => Bytecode::ReturnNonLocal(new_scope),
                },
                Bytecode::PushBlock(block_idx) => {
                    let inner_lit = orig_block
                        .blk_info
                        .get_env()
                        .literals
                        .get(*block_idx as usize)
                        .unwrap_or_else(|| panic!("PushBlock is associated with no literal whatsoever?"));
                    let inner_block = match inner_lit {
                        Literal::Block(inner_blk) => inner_blk.clone(),
                        _ => panic!("PushBlock is not actually pushing a block somehow"),
                    };

                    self.adapt_block_after_outer_inlined(inner_block, adjust_scope_by, interner);

                    Bytecode::PushBlock(*block_idx)
                }
                _ => *b,
            })
            .collect();

        let new_max_stack_size = get_max_stack_size(&new_body, interner);

        let blk_method = orig_block.blk_info.get_env_mut();
        blk_method.body = new_body;
        blk_method.max_stack_size = new_max_stack_size;
    }

    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()> {
        // TODO opt: we only inline when it's a block (see BooleanTest:testIfTrueWithValueBlock to see why), but we could easily only inline when it's any expression that's safe to be inlined. Most fall under that category
        if self.values.len() != 1 || !matches!(self.values.first()?, ast::Expression::Block(_)) {
            return None;
        }

        let jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(0)),
        }

        self.inline_expression(ctxt, self.values.first()?, gc_interface);

        ctxt.backpatch_jump_to_current(jump_idx);

        Some(())
    }

    fn inline_if_nil_or_if_not_nil(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()> {
        if self.values.len() != 1 || !matches!(self.values.first()?, ast::Expression::Block(_)) {
            return None;
        }

        let jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnNotNilTopTop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnNilTopTop(0)),
        }

        self.inline_expression(ctxt, self.values.first()?, gc_interface);

        ctxt.backpatch_jump_to_current(jump_idx);

        Some(())
    }

    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()> {
        if self.values.len() != 2
            || !matches!(self.values.first()?, ast::Expression::Block(_))
            || !matches!(self.values.get(1)?, ast::Expression::Block(_))
        {
            return None;
        }

        let start_jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
        }

        self.inline_expression(ctxt, self.values.first()?, gc_interface);

        let middle_jump_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        ctxt.backpatch_jump_to_current(start_jump_idx);

        self.inline_expression(ctxt, self.values.get(1)?, gc_interface);

        ctxt.backpatch_jump_to_current(middle_jump_idx);

        Some(())
    }

    fn inline_if_nil_if_not_nil(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()> {
        if self.values.len() != 2
            || !matches!(self.values.first()?, ast::Expression::Block(_))
            || !matches!(self.values.get(1)?, ast::Expression::Block(_))
        {
            return None;
        }

        let start_jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnNotNilPop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnNilPop(0)),
        }

        self.inline_expression(ctxt, self.values.first()?, gc_interface);

        let middle_jump_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        ctxt.backpatch_jump_to_current(start_jump_idx);

        self.inline_expression(ctxt, self.values.get(1)?, gc_interface);

        ctxt.backpatch_jump_to_current(middle_jump_idx);

        Some(())
    }

    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, jump_type: JumpType, gc_interface: &mut GCInterface) -> Option<()> {
        if self.values.len() != 1
            || !matches!(ctxt.get_instructions().last(), Some(Bytecode::PushBlock(_)))
            || !matches!(self.values.first()?, ast::Expression::Block(_))
        {
            // I guess it doesn't have to be a block, but really, it is in all our benchmarks
            return None;
        }

        let idx_pre_condition = ctxt.get_cur_instr_idx();

        // by the time we see it's a "whileTrue:" or a "whileFalse:", there's already been a PushBlock, since they're methods defined on Block
        self.inline_last_push_block_bc(ctxt, gc_interface);

        let cond_jump_idx = ctxt.get_cur_instr_idx();
        match jump_type {
            JumpOnFalse => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            JumpOnTrue => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
        }

        self.inline_expression(ctxt, self.values.first()?, gc_interface);

        ctxt.push_instr(Bytecode::Pop);

        ctxt.push_instr(Bytecode::JumpBackward((ctxt.get_cur_instr_idx() - idx_pre_condition + 1) as u16));
        ctxt.backpatch_jump_to_current(cond_jump_idx);

        ctxt.push_instr(Bytecode::PushNil);

        Some(())
    }

    fn inline_or_and(&self, ctxt: &mut dyn InnerGenCtxt, or_and_choice: OrAndChoice, gc_interface: &mut GCInterface) -> Option<()> {
        if self.values.len() != 1 || !matches!(self.values.first()?, ast::Expression::Block(_)) {
            return None;
        }

        let skip_cond_jump_idx = ctxt.get_cur_instr_idx();

        match or_and_choice {
            Or => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
            And => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
        }

        self.inline_expression(ctxt, self.values.first()?, gc_interface);

        let skip_return_true_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        ctxt.backpatch_jump_to_current(skip_cond_jump_idx);

        let name = match or_and_choice {
            Or => ctxt.intern_symbol("true"),
            And => ctxt.intern_symbol("false"),
        };
        let idx = ctxt.push_literal(Literal::Symbol(name));
        ctxt.push_instr(Bytecode::PushGlobal(idx as u8));

        ctxt.backpatch_jump_to_current(skip_return_true_idx);

        Some(())
    }

    fn inline_to_do(&self, ctxt: &mut dyn InnerGenCtxt, gc_interface: &mut GCInterface) -> Option<()> {
        if self.values.len() != 2 || !matches!(self.values.get(1)?, ast::Expression::Block(_)) {
            return None;
        }

        self.inline_expression(ctxt, self.values.first()?, gc_interface);

        let idx_loop_accumulator = ctxt.get_nbr_locals() as u8;

        ctxt.push_instr(Bytecode::Dup2);

        let jump_if_greater_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::JumpIfGreater(0));

        ctxt.push_instr(Bytecode::Dup);
        ctxt.push_instr(Bytecode::PopLocal(0, idx_loop_accumulator));

        self.inline_expression(ctxt, self.values.get(1)?, gc_interface); // inline the block

        ctxt.push_instr(Bytecode::Pop);
        ctxt.push_instr(Bytecode::Inc);
        ctxt.push_instr(Bytecode::JumpBackward((ctxt.get_cur_instr_idx() - jump_if_greater_idx) as u16));

        ctxt.backpatch_jump_to_current(jump_if_greater_idx);

        // println!("--- Inlined to:do:.");

        Some(())
    }
}
