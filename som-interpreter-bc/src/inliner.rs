use std::rc::Rc;
use som_core::ast;
use som_core::bytecode::Bytecode;
use crate::block::Block;
use crate::compiler::{compile_block, InnerGenCtxt, Literal};
use crate::compiler::MethodCodegen;

// TODO some of those should return Result types and throw errors instead, most likely.
pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_expr(&self, ctxt: &mut dyn InnerGenCtxt, block: &ast::Expression) -> Option<()>;
    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &Block) -> Option<()>;

    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()>;
}

impl PrimMessageInliner for ast::Expression {
    fn inline_if_possible(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        match message.signature.as_str() {
            "ifTrue:" | "ifFalse:" => self.inline_if_true_or_if_false(ctxt, message),
            "ifTrue:ifFalse:" | "ifFalse:ifTrue:" => self.inline_if_true_if_false(ctxt, message),
            "whileTrue:" | "whileFalse:" => self.inline_while(ctxt, message),
            // TODO: [or, and]
            _ => None
        }
    }

    fn inline_expr(&self, ctxt: &mut dyn InnerGenCtxt, block_expr: &ast::Expression) -> Option<()> {
        match block_expr {
            ast::Expression::Block(block) => {
                for block_local in &block.locals {
                    ctxt.push_local(String::from(block_local)); // breaks shadowing
                }

                // TODO need remove those POPs somehow.
                if let Some((last, rest)) = block.body.exprs.split_last() {
                    for expr in rest {
                        expr.codegen(ctxt);
                        ctxt.push_instr(Bytecode::Pop);
                    }
                    last.codegen(ctxt)?;
                }
                Some(())
            },
            expr => expr.codegen(ctxt)
        }
    }

    fn inline_compiled_block(&self, ctxt: &mut dyn InnerGenCtxt, block: &Block) -> Option<()> {
        for block_local in &block.locals {
            dbg!(block_local);
            todo!("actually pushing locals would be nice!")
            // ctxt.push_local(String::from(block_local));
        }

        // last is always ReturnLocal, so it gets ignored
        if let Some((_, body)) = block.body.split_last() {
            for block_bc in body {
                match block_bc {
                    Bytecode::PushLocal(up_idx, idx) => ctxt.push_instr(Bytecode::PushLocal(*up_idx - 1, *idx)),
                    Bytecode::PopLocal(up_idx, idx) => ctxt.push_instr(Bytecode::PopLocal(*up_idx - 1, *idx)),
                    Bytecode::PushArgument(up_idx, idx) => ctxt.push_instr(Bytecode::PushArgument(*up_idx - 1, *idx)),
                    Bytecode::PopArgument(up_idx, idx) => ctxt.push_instr(Bytecode::PopArgument(*up_idx - 1, *idx)),
                    Bytecode::Send1(lit_idx) | Bytecode::Send2(lit_idx) |
                    Bytecode::Send3(lit_idx) | Bytecode::SendN(lit_idx) => {
                        match block.literals.get(*lit_idx as usize)? {
                            Literal::Symbol(interned) => {
                                // does this push duplicate literals? I think it doesn't?
                                let idx = ctxt.push_literal(Literal::Symbol(*interned));
                                match block_bc {
                                    Bytecode::Send1(_) => ctxt.push_instr(Bytecode::Send1(idx as u8)),
                                    Bytecode::Send2(_) => ctxt.push_instr(Bytecode::Send2(idx as u8)),
                                    Bytecode::Send3(_) => ctxt.push_instr(Bytecode::Send3(idx as u8)),
                                    Bytecode::SendN(_) => ctxt.push_instr(Bytecode::SendN(idx as u8)),
                                    _ => panic!("Unreachable branch")
                                }
                            },
                            _ => panic!("Unexpected block literal type, not yet implemented")
                        }
                    },
                    Bytecode::PushBlock(block_idx) => {
                        match block.literals.get(*block_idx as usize)? {
                            Literal::Block(inner_block) => {
                                let new_block = compile_block(ctxt.as_gen_ctxt(), &inner_block.ast_body)?;
                                let idx = ctxt.push_literal(Literal::Block(Rc::new(new_block)));
                                ctxt.push_instr(Bytecode::PushBlock(idx as u8));
                            },
                            _ => panic!("PushBlock not actually pushing a block somehow")
                        };
                    },
                    Bytecode::PushConstant(constant_idx) => {
                        match block.literals.get(*constant_idx as usize)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                ctxt.push_instr(Bytecode::PushConstant(lit_idx as u8));
                            }
                        };
                    },
                    Bytecode::PushGlobal(global_idx) => {
                        match block.literals.get(*global_idx as usize)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                ctxt.push_instr(Bytecode::PushGlobal(lit_idx as u8));
                            }
                        };
                    },
                    Bytecode::PushConstant0 | Bytecode::PushConstant1 | Bytecode::PushConstant2 => {
                        let constant_idx: usize = match block_bc {
                            Bytecode::PushConstant0 => 0,
                            Bytecode::PushConstant1 => 1,
                            Bytecode::PushConstant2 => 2,
                            _ => panic!("Unreachable")
                        };

                        match block.literals.get(constant_idx)? {
                            lit => {
                                let lit_idx = ctxt.push_literal(lit.clone());
                                ctxt.push_instr(Bytecode::PushConstant(lit_idx as u8));
                            }
                        };
                    },
                    Bytecode::ReturnNonLocal => panic!("There shouldn't be a return here"),
                    _ => ctxt.push_instr(*block_bc)
                }
            }
        }

        Some(())
    }

    fn inline_if_true_or_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        let is_if_true = message.signature == "ifTrue:";

        if message.values.len() != 1 {
            return None;
        }

        let jump_idx = ctxt.get_cur_instr_idx();
        match is_if_true {
            true => ctxt.push_instr(Bytecode::JumpOnFalseTopNil(0)),
            false => ctxt.push_instr(Bytecode::JumpOnTrueTopNil(0))
        }

        self.inline_expr(ctxt, message.values.get(0)?);
        ctxt.backpatch_jump(jump_idx);

        return Some(());
    }

    fn inline_if_true_if_false(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        let is_if_true_if_false = message.signature == "ifTrue:ifFalse:";

        if message.values.len() != 2 {
            return None;
        }

        let start_jump_idx = ctxt.get_cur_instr_idx();
        match is_if_true_if_false {
            true => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            false => ctxt.push_instr(Bytecode::JumpOnTruePop(0)),
        }

        self.inline_expr(ctxt, message.values.get(0)?);

        let middle_jump_idx = ctxt.get_cur_instr_idx();
        ctxt.push_instr(Bytecode::Jump(0));

        ctxt.backpatch_jump(start_jump_idx);
        self.inline_expr(ctxt, message.values.get(1)?);
        ctxt.backpatch_jump(middle_jump_idx);

        return Some(());
    }

    fn inline_while(&self, ctxt: &mut dyn InnerGenCtxt, message: &ast::Message) -> Option<()> {
        let is_while_true = message.signature == "whileTrue:";

        let block_idx = match ctxt.get_instructions().last()? {
            Bytecode::PushBlock(val) => val,
            _ => return None
        };

        // todo pop the literal
        let cond_block_ref = match ctxt.get_literal(*block_idx as usize)? {
            Literal::Block(val) => val.clone(),
            _ => return None
        };

        if message.values.len() != 1 {
            return None;
        }

        ctxt.pop_instr(); // we remove the PUSH_BLOCK

        let idx_before_condition = ctxt.get_cur_instr_idx();

        self.inline_compiled_block(ctxt, cond_block_ref.as_ref());

        let cond_jump_idx = ctxt.get_cur_instr_idx();
        match is_while_true {
            true => ctxt.push_instr(Bytecode::JumpOnFalsePop(0)),
            false => ctxt.push_instr(Bytecode::JumpOnTruePop(0))
        }

        self.inline_expr(ctxt, message.values.get(0).unwrap());

        // we push a POP, unless the body of the loop is empty.
        match message.values.get(0).unwrap() {
            ast::Expression::Block(block)  => {
                if block.body.exprs.len() != 0 {
                    ctxt.push_instr(Bytecode::Pop);
                }
            },
            _ => {}
        };

        ctxt.push_instr(Bytecode::JumpBackward(ctxt.get_cur_instr_idx() - idx_before_condition));
        ctxt.backpatch_jump(cond_jump_idx);
        ctxt.push_instr(Bytecode::PushNil);

        return Some(());
    }
}