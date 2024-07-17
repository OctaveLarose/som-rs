use std::rc::Rc;
use som_core::ast;
use som_core::ast::{Block, Expression};

use crate::ast::{AstBlock, AstBody, AstExpression, InlinedNode};
use crate::compiler::AstMethodCompilerCtxt;
use crate::specialized::if_inlined_node::IfInlinedNode;

pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: &mut AstMethodCompilerCtxt) -> Option<InlinedNode>;
    fn inline_expression(&self, ctxt: &mut AstMethodCompilerCtxt, expression: &Expression) -> Option<AstBody>;
    fn inline_block(&self, ctxt: &mut AstMethodCompilerCtxt, expression: &Block) -> Option<AstBody>;
    fn adapt_block_after_outer_inlined(&self, ctxt: &mut AstMethodCompilerCtxt, blk: &Rc<Block>) -> Option<AstBlock>;
    fn inline_if_true_or_if_false(&self, ctxt: &mut AstMethodCompilerCtxt, jump_type: bool) -> Option<InlinedNode>;
}

impl PrimMessageInliner for ast::Message {
    fn inline_if_possible(&self, ctxt: &mut AstMethodCompilerCtxt) -> Option<InlinedNode> {
        match self.signature.as_str() {
            // "ifTrue:" => self.inline_if_true_or_if_false(ctxt, true),
            "ifFalse:" => self.inline_if_true_or_if_false(ctxt, false),
            _ => None,
        }
    }

    #[allow(dead_code, unreachable_code, unused_variables)]
    fn inline_expression(&self, ctxt: &mut AstMethodCompilerCtxt, expression: &Expression) -> Option<AstBody> {
        let nbr_of_locals_pre_inlining = ctxt.get_nbr_locals();
        let nbr_of_args_pre_inlining = ctxt.get_nbr_args();
        
        let expr = match expression {
            Expression::Block(blk) => {
                match ctxt.inlining_level {
                    0 => {
                        ctxt.inlining_level += 1;
                        let inlined_blk = self.inline_block(ctxt, blk);
                        ctxt.inlining_level -= 1;
                        inlined_blk
                    },
                    _ => {
                        let new_blk = self.adapt_block_after_outer_inlined(ctxt, blk)?;
                        Some(AstBody { exprs: vec![AstExpression::Block(Rc::new(new_blk))] })
                    }
                }
            }
            _ => {
                let expr = match expression {
                    Expression::LocalVarRead(idx) => AstExpression::LocalVarRead(idx + nbr_of_locals_pre_inlining),
                    Expression::LocalVarWrite(idx, expr) => {
                        AstExpression::LocalVarWrite(idx + nbr_of_locals_pre_inlining,
                                                     Box::new(ctxt.parse_expression(expr)))
                    },
                    Expression::NonLocalVarRead(scope, idx) => {
                        match *scope - 1 {
                            0 => AstExpression::LocalVarRead(*idx),
                            _ => AstExpression::NonLocalVarRead(*scope - 1, *idx)
                        }
                    },
                    Expression::NonLocalVarWrite(scope, idx, expr) => {
                        let ast_expr = Box::new(ctxt.parse_expression(expr));
                        match *scope - 1 {
                            0 => AstExpression::LocalVarWrite(*idx, ast_expr),
                            _ => AstExpression::NonLocalVarWrite(*scope - 1, *idx, ast_expr)
                        }
                    },
                    Expression::ArgRead(scope, idx) => {
                        match *scope {
                            0 => AstExpression::ArgRead(0, idx + nbr_of_args_pre_inlining),
                            _ => AstExpression::ArgRead(*scope - 1, *idx)
                        }
                    }
                    Expression::ArgWrite(scope, idx, expr) => {
                        let ast_expr = Box::new(ctxt.parse_expression(expr));
                        match *scope {
                            0 => AstExpression::ArgWrite(0, idx + nbr_of_args_pre_inlining, ast_expr),
                            _ => AstExpression::ArgWrite(*scope - 1, *idx, ast_expr)
                        }
                    }
                    Expression::GlobalRead(a) => AstExpression::GlobalRead(a.clone()),
                    Expression::FieldRead(idx) => AstExpression::FieldRead(idx - 1),
                    Expression::FieldWrite(idx, expr) => AstExpression::FieldWrite(idx - 1, Box::new(ctxt.parse_expression(expr))),
                    Expression::Message(msg) => ctxt.parse_message_maybe_inline(msg), // todo - can this recursive inlining cause issues? I assume it's fine?
                    Expression::SuperMessage(super_msg) => AstExpression::SuperMessage(Box::new(ctxt.parse_super_message(super_msg))),
                    Expression::BinaryOp(bin_op) => AstExpression::BinaryOp(Box::new(ctxt.parse_binary_op(bin_op))),
                    Expression::Exit(expr, scope) => AstExpression::Exit(Box::new(ctxt.parse_expression(expr)), scope - 1), // todo incorrect: the expression should get inlined also.
                    Expression::Literal(lit) => AstExpression::Literal(lit.clone()),
                    Expression::Block(_) => unreachable!()
                };
                
                Some(AstBody { exprs: vec![expr] })
            }
        };
        
        expr
    }

    fn inline_block(&self, ctxt: &mut AstMethodCompilerCtxt, blk: &Block) -> Option<AstBody> {
        ctxt.add_nbr_args(blk.nbr_params);
        ctxt.add_nbr_locals(blk.nbr_locals);
        
        let all_exprs_unflattened: Vec<AstBody> = blk.body.exprs.iter().filter_map(|e| self.inline_expression(ctxt, e)).collect();
        
        let mut all_exprs = vec![];
        for a in all_exprs_unflattened {
            all_exprs.extend(a.exprs)
        }
        
        Some(AstBody { exprs: all_exprs })
    }
    fn adapt_block_after_outer_inlined(&self, ctxt: &mut AstMethodCompilerCtxt, blk: &Rc<Block>) -> Option<AstBlock> {
        todo!()
    }

    fn inline_if_true_or_if_false(&self, ctxt: &mut AstMethodCompilerCtxt, expected_bool: bool) -> Option<InlinedNode> {
        let cond_instrs = self.inline_expression(ctxt, &self.receiver)?;
        let body_instrs = self.inline_expression(ctxt, self.values.first()?)?;

        let if_inlined_node = IfInlinedNode {
            expected_bool,
            cond_instrs,
            body_instrs,
        };

        Some(InlinedNode::IfInlined(if_inlined_node))
    }
}