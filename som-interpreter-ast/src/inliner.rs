use std::rc::Rc;

use som_core::ast;
use som_core::ast::{Block, Expression};

use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstSuperMessage, InlinedNode};
use crate::compiler::{AstMethodCompilerCtxt, AstScopeCtxt};
use crate::specialized::if_inlined_node::IfInlinedNode;

pub trait PrimMessageInliner {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
    fn get_inline_expression(&mut self, expression: &Expression) -> Option<AstExpression>;
    fn inline_block(&mut self, expression: &Block) -> Option<AstBody>;
    fn adapt_block_after_outer_inlined(&mut self, blk: &Rc<Block>) -> Option<AstBlock>;
    fn get_inline_expression_for_adapted_block(&mut self, expression: &Expression) -> Option<AstExpression>;
    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
}

impl PrimMessageInliner for AstMethodCompilerCtxt {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        match msg.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(msg, true),
            "ifFalse:" => self.inline_if_true_or_if_false(msg, false),
            _ => None,
        }
    }
    
    fn get_inline_expression(&mut self, expression: &Expression) -> Option<AstExpression> {
        let adjust_scope_by = self.scopes.iter().filter(|e| e.is_getting_inlined).count();

        let expr = match expression {
            Expression::Block(blk) => {
                let new_blk = self.adapt_block_after_outer_inlined(blk)?;
                AstExpression::Block(Rc::new(new_blk))
            }
            // Expression::LocalVarRead(idx) => AstExpression::LocalVarRead(idx + nbr_of_locals_pre_inlining),
            Expression::LocalVarRead(idx) => AstExpression::LocalVarRead(*idx),
            Expression::LocalVarWrite(idx, expr) => {
                // AstExpression::LocalVarWrite(idx + nbr_of_locals_pre_inlining, Box::new(self.get_inline_expression(expr)?))
                AstExpression::LocalVarWrite(*idx, Box::new(self.get_inline_expression(expr)?))
            }
            Expression::NonLocalVarRead(scope, idx) => {
                match *scope {
                    1 => AstExpression::LocalVarRead(*idx), // todo this guy shouldn't be the only one to be special-cased. the logic needs to be generalized
                    _ => {
                        match *scope - adjust_scope_by {
                            0 => AstExpression::LocalVarRead(*idx),
                            new_scope => AstExpression::NonLocalVarRead(new_scope, *idx),
                        }
                    }
                }
            }
            Expression::NonLocalVarWrite(scope, idx, expr) => {
                let ast_expr = Box::new(self.get_inline_expression(expr)?);
                match *scope - adjust_scope_by {
                    0 => AstExpression::LocalVarWrite(*idx, ast_expr),
                    _ => AstExpression::NonLocalVarWrite(*scope - adjust_scope_by, *idx, ast_expr)
                }
            }
            Expression::ArgRead(scope, idx) => {
                match *scope {
                    x if x < adjust_scope_by => AstExpression::ArgRead(*scope, *idx), // todo eeehhhhh... sure?
                    // 0 => AstExpression::ArgRead(0, *idx + nbr_of_args_pre_inlining),
                    _ => AstExpression::ArgRead(*scope - adjust_scope_by, *idx)
                }
            }
            Expression::ArgWrite(scope, idx, expr) => {
                let ast_expr = Box::new(self.get_inline_expression(expr)?);
                match *scope {
                    0 => unreachable!("we're writing to self or blockself?"),
                    _ => AstExpression::ArgWrite(*scope - adjust_scope_by, *idx, ast_expr)
                }
            }
            Expression::GlobalRead(a) => AstExpression::GlobalRead(a.clone()),
            Expression::FieldRead(idx) => AstExpression::FieldRead(*idx),
            Expression::FieldWrite(idx, expr) => AstExpression::FieldWrite(*idx, Box::new(self.get_inline_expression(expr)?)),
            Expression::Message(msg) => {
                if let Some(inlined_node) = self.inline_if_possible(msg) {
                    return Some(AstExpression::InlinedCall(Box::new(inlined_node)));
                }
                AstExpression::Message(Box::new(AstMessage {
                    receiver: self.get_inline_expression(&msg.receiver)?,
                    signature: msg.signature.clone(),
                    values: msg.values.iter().filter_map(|val| self.get_inline_expression(val)).collect(),
                }))
            }
            Expression::SuperMessage(super_msg) => {
                AstExpression::SuperMessage(Box::new(AstSuperMessage {
                    receiver_name: super_msg.receiver_name.clone(),
                    is_static_class_call: super_msg.is_static_class_call,
                    signature: super_msg.signature.clone(),
                    values: super_msg.values.iter().filter_map(|e| self.get_inline_expression(e)).collect(),
                }))
            }
            Expression::BinaryOp(bin_op) => {
                AstExpression::BinaryOp(Box::new(AstBinaryOp {
                    op: bin_op.op.clone(),
                    lhs: self.get_inline_expression(&bin_op.lhs)?,
                    rhs: self.get_inline_expression(&bin_op.rhs)?,
                }))
            }
            Expression::Exit(expr, scope) => {
                let inline_expr = self.get_inline_expression(expr)?;
                AstExpression::Exit(Box::new(inline_expr), scope - adjust_scope_by)
            }
            Expression::Literal(lit) => AstExpression::Literal(lit.clone()),
        };

        Some(expr)
    }

    fn inline_block(&mut self, blk: &Block) -> Option<AstBody> {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, true));

        let inlined_block = Some(AstBody { exprs: blk.body.exprs.iter().filter_map(|e| self.get_inline_expression(e)).collect() });

        self.scopes.pop();

        self.scopes.last_mut().unwrap().add_nbr_args(blk.nbr_params);
        self.scopes.last_mut().unwrap().add_nbr_locals(blk.nbr_locals);

        inlined_block
    }

    fn adapt_block_after_outer_inlined(&mut self, blk: &Rc<Block>) -> Option<AstBlock> {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, false));

        let exprs: Vec<AstExpression> = blk.body.exprs.iter()
            .filter_map(|og_expr| {
                self.get_inline_expression_for_adapted_block(og_expr)
            }).collect();

        let (nbr_params, nbr_locals) = {
            let outer_blk_scope = self.scopes.last().unwrap();
            (outer_blk_scope.get_nbr_args(), outer_blk_scope.get_nbr_locals())
        };

        let adapted_inner_block = Some(AstBlock {
            nbr_params,
            nbr_locals,
            body: AstBody { exprs },
        });

        self.scopes.pop();

        adapted_inner_block
    }

    fn get_inline_expression_for_adapted_block(&mut self, og_expr: &Expression) -> Option<AstExpression> {
        let new_expr = match og_expr {
            Expression::NonLocalVarRead(up_idx, _) | Expression::NonLocalVarWrite(up_idx, _, _)
            | Expression::ArgRead(up_idx, _) | Expression::ArgWrite(up_idx, _, _) | Expression::Exit(_, up_idx) => {
                let new_up_idx = match up_idx {
                    0 => 0,
                    _ => up_idx - self.scopes.iter().rev().take(*up_idx).filter(|e| e.is_getting_inlined).count() // minus the number of inlined scopes in between the current scope and the target var
                };

                // todo also the idx be adjusted depending on the amount of inlined variables in the block, IF the target block is an inline target

                match og_expr {
                    Expression::NonLocalVarRead(_, idx) => {
                        match new_up_idx {
                            0 => AstExpression::LocalVarRead(*idx),
                            _ => AstExpression::NonLocalVarRead(new_up_idx, *idx),
                        }
                    }
                    Expression::NonLocalVarWrite(_, idx, expr) => {
                        let new_expr = Box::new(self.get_inline_expression_for_adapted_block(expr)?);
                        match new_up_idx {
                            0 => AstExpression::LocalVarWrite(*idx, new_expr),
                            _ => AstExpression::NonLocalVarWrite(new_up_idx, *idx, new_expr),
                        }
                    }
                    Expression::ArgRead(_, idx) => { AstExpression::ArgRead(new_up_idx, *idx) }
                    Expression::ArgWrite(_, idx, expr) => {
                        let new_expr = Box::new(self.get_inline_expression_for_adapted_block(expr)?);
                        AstExpression::ArgWrite(new_up_idx, *idx, new_expr)
                    }
                    Expression::Exit(expr, _) => {
                        AstExpression::Exit(Box::new(self.get_inline_expression_for_adapted_block(expr)?), new_up_idx)
                    }
                    _ => unreachable!(),
                }
            }
            Expression::FieldWrite(idx, expr) => {
                AstExpression::FieldWrite(*idx, Box::new(self.get_inline_expression_for_adapted_block(expr)?))
            }
            Expression::LocalVarWrite(idx, expr) => {
                AstExpression::LocalVarWrite(*idx, Box::new(self.get_inline_expression_for_adapted_block(expr)?))
            }
            Expression::Block(blk) => {
                let new_block = self.adapt_block_after_outer_inlined(blk)?;
                AstExpression::Block(Rc::new(new_block))
            }
            Expression::Message(msg) => {
                if let Some(inlined_method) = self.inline_if_possible(msg) {
                    AstExpression::InlinedCall(Box::new(inlined_method))
                } else {
                    AstExpression::Message(Box::new(AstMessage {
                        receiver: self.get_inline_expression_for_adapted_block(&msg.receiver)?,
                        signature: msg.signature.clone(),
                        values: msg.values.iter().filter_map(|e| self.get_inline_expression_for_adapted_block(e)).collect(),
                    }))
                }
            }
            Expression::SuperMessage(super_msg) => {
                AstExpression::SuperMessage(Box::new(AstSuperMessage {
                    receiver_name: super_msg.receiver_name.clone(),
                    is_static_class_call: super_msg.is_static_class_call,
                    signature: super_msg.signature.clone(),
                    values: super_msg.values.iter().filter_map(|e| self.get_inline_expression_for_adapted_block(e)).collect(),
                }))
            }
            Expression::BinaryOp(bin_op) => {
                AstExpression::BinaryOp(Box::new(AstBinaryOp {
                    op: bin_op.op.clone(),
                    lhs: self.get_inline_expression_for_adapted_block(&bin_op.lhs)?,
                    rhs: self.get_inline_expression_for_adapted_block(&bin_op.rhs)?,
                }))
            }
            Expression::GlobalRead(_) |
            Expression::LocalVarRead(_) |
            Expression::FieldRead(_) |
            Expression::Literal(_) => self.parse_expression(og_expr)
        };

        Some(new_expr)
    }

    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None
        };

        let inlining_scope_adjust = self.scopes.iter().filter(|e| e.is_getting_inlined).count();

        let if_inlined_node = IfInlinedNode {
            expected_bool,
            cond_instrs: {
                // the condition of an if block gets parsed in the current context: it's not a block. so a regular call to parse_expression().
                // when doing recursive inlining though, it needs to be adapted based on the inlining that's happened. TODO doesn't that mean adapt_after_X instead of get_inline_expression?
                match inlining_scope_adjust {
                    0 => AstBody { exprs: vec![self.parse_expression(&msg.receiver)] },
                    _ => AstBody { exprs: vec![self.get_inline_expression(&msg.receiver)?] }
                }
            },
            body_instrs: self.inline_block(body_blk)?,
        };

        // dbg!(&msg);
        // println!("{}", &if_inlined_node.body_instrs);

        Some(InlinedNode::IfInlined(if_inlined_node))
    }
}