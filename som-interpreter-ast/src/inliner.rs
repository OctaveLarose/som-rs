use std::rc::Rc;

use som_core::ast;
use som_core::ast::{Block, Expression};

use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstSuperMessage, InlinedNode};
use crate::compiler::{AstMethodCompilerCtxt, AstScopeCtxt};
use crate::specialized::if_inlined_node::IfInlinedNode;
use crate::specialized::while_inlined_node::WhileInlinedNode;

pub trait PrimMessageInliner {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
    fn get_inline_expression(&mut self, expression: &Expression) -> Option<AstExpression>;
    fn inline_block(&mut self, expression: &Block) -> Option<AstBody>;
    fn adapt_block_after_outer_inlined(&mut self, blk: &Rc<Block>) -> Option<AstBlock>;
    fn adapt_expr_from_inlining(&mut self, expression: &Expression) -> Option<AstExpression>;
    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_while(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
}

impl PrimMessageInliner for AstMethodCompilerCtxt {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        match msg.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(msg, true),
            "ifFalse:" => self.inline_if_true_or_if_false(msg, false),
            // "whileTrue:" => self.inline_while(msg, true),
            // "whileFalse:" => self.inline_while(msg, false),
            _ => None,
        }
    }

    fn get_inline_expression(&mut self, expression: &Expression) -> Option<AstExpression> {
        let expr = match expression {
            Expression::Block(blk) => {
                let new_blk = self.adapt_block_after_outer_inlined(blk)?;
                AstExpression::Block(Rc::new(new_blk))
            }
            Expression::LocalVarRead(..) | Expression::LocalVarWrite(..) | Expression::NonLocalVarRead(..) | Expression::NonLocalVarWrite(..) => {
                // let nbr_locals_in_target_scope = self.scopes.iter().nth_back(1).unwrap().get_nbr_locals();
                let nbr_locals_in_target_scope = self.get_nbr_locals_in_scope_post_inlining(1);

                match expression {
                    Expression::LocalVarRead(idx) => AstExpression::LocalVarRead(idx + nbr_locals_in_target_scope),
                    Expression::LocalVarWrite(idx, expr) => {
                        AstExpression::LocalVarWrite(idx + nbr_locals_in_target_scope, Box::new(self.get_inline_expression(expr)?))
                    }
                    Expression::NonLocalVarRead(up_idx, ..) | Expression::NonLocalVarWrite(up_idx, ..) => {
                        let new_up_idx = match up_idx {
                            0 => 0,
                            1 => 0,
                            _ => up_idx - self.scopes.iter().rev().take(*up_idx).filter(|e| e.is_getting_inlined).count() // minus the number of inlined scopes in between the current scope and the target var
                        };

                        match new_up_idx {
                            0 => {
                                match expression {
                                    Expression::NonLocalVarRead(.., idx) => AstExpression::LocalVarRead(*idx),
                                    Expression::NonLocalVarWrite(_, idx, expr) => AstExpression::LocalVarWrite(*idx, Box::new(self.get_inline_expression(expr)?)),
                                    _ => unreachable!()
                                }
                            }
                            _ => {
                                match expression {
                                    Expression::NonLocalVarRead(.., idx) => AstExpression::NonLocalVarRead(new_up_idx, *idx),
                                    Expression::NonLocalVarWrite(_, idx, expr) => AstExpression::NonLocalVarWrite(new_up_idx, *idx, Box::new(self.get_inline_expression(expr)?)),
                                    _ => unreachable!()
                                }
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            Expression::ArgRead(up_idx, _) | Expression::ArgWrite(up_idx, _, _) => {
                match up_idx {
                    0 => {
                        let nbr_args_in_target_scope = self.scopes.iter().nth_back(1).unwrap().get_nbr_args();
                        // let nbr_args_in_target_scope = self.get_nbr_args_in_scope_post_inlining(1);
                        // todo for future me: the fact that this get_nbr_args_in_scope_post_inlining didn't work makes me think my implem for this and the local vars isn't 100% correct.
                        
                        match expression {
                            Expression::ArgRead(_, idx) => AstExpression::ArgRead(0, idx + nbr_args_in_target_scope),
                            Expression::ArgWrite(_, idx, expr) => AstExpression::ArgWrite(0, idx + nbr_args_in_target_scope, Box::new(self.get_inline_expression(expr)?)),
                            _ => unreachable!()
                        }
                    }
                    _ => {
                        let new_up_idx = match up_idx {
                            0 => 0,
                            1 => 0,
                            _ => up_idx - self.scopes.iter().rev().take(*up_idx).filter(|e| e.is_getting_inlined).count()
                        };

                        match expression {
                            Expression::ArgRead(_, idx) => AstExpression::ArgRead(new_up_idx, *idx),
                            Expression::ArgWrite(_, idx, expr) => AstExpression::ArgWrite(new_up_idx, *idx, Box::new(self.get_inline_expression(expr)?)),
                            _ => unreachable!()
                        }
                    }
                }
            }
            Expression::Exit(expr, scope) => {
                let inline_expr = self.get_inline_expression(expr)?;
                let adjust_scope_by = self.scopes.iter().rev().take(*scope).filter(|e| e.is_getting_inlined).count();
                AstExpression::Exit(Box::new(inline_expr), scope - adjust_scope_by)
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
                self.adapt_expr_from_inlining(og_expr)
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

    fn adapt_expr_from_inlining(&mut self, og_expr: &Expression) -> Option<AstExpression> {
        let new_expr = match og_expr {
            // Expression::NonLocalVarRead(up_idx, _) | Expression::NonLocalVarWrite(up_idx, _, _)
            // | Expression::ArgRead(up_idx, _) | Expression::ArgWrite(up_idx, _, _) => {
            //     let new_up_idx = match up_idx {
            //         0 => 0,
            //         _ => up_idx - self.scopes.iter().rev().take(*up_idx).filter(|e| e.is_getting_inlined).count() // minus the number of inlined scopes in between the current scope and the target var
            //     };
            // 
            //     match og_expr {
            //         Expression::NonLocalVarRead(_, idx) => {
            //             match new_up_idx {
            //                 0 => AstExpression::LocalVarRead(*idx),
            //                 _ => AstExpression::NonLocalVarRead(new_up_idx, *idx),
            //             }
            //         }
            //         Expression::NonLocalVarWrite(_, idx, expr) => {
            //             let new_expr = Box::new(self.adapt_expr_from_inlining(expr)?);
            //             match new_up_idx {
            //                 0 => AstExpression::LocalVarWrite(*idx, new_expr),
            //                 _ => AstExpression::NonLocalVarWrite(new_up_idx, *idx, new_expr),
            //             }
            //         }
            //         Expression::ArgRead(_, idx) => { AstExpression::ArgRead(new_up_idx, *idx) }
            //         Expression::ArgWrite(_, idx, expr) => {
            //             let new_expr = Box::new(self.adapt_expr_from_inlining(expr)?);
            //             AstExpression::ArgWrite(new_up_idx, *idx, new_expr)
            //         }
            //         _ => unreachable!(),
            //     }
            // }
            Expression::NonLocalVarRead(up_idx, idx) | Expression::NonLocalVarWrite(up_idx, idx, _) => {
                let (new_up_idx, new_idx) = self.adapt_var_coords_from_inlining(*up_idx, *idx);
                debug_assert_ne!(new_up_idx, 0);
                
                match og_expr {
                    Expression::NonLocalVarRead(..) => AstExpression::NonLocalVarRead(new_up_idx, new_idx),
                    Expression::NonLocalVarWrite(.., expr) => AstExpression::NonLocalVarWrite(new_up_idx, new_idx, Box::new(self.adapt_expr_from_inlining(expr)?)),
                    _ => unreachable!()
                }
            }
            Expression::ArgRead(up_idx, idx) | Expression::ArgWrite(up_idx, idx, _) => {
                let (new_up_idx, new_idx) = self.adapt_arg_coords_from_inlining(*up_idx, *idx);

                match og_expr {
                    Expression::ArgRead(..) => AstExpression::ArgRead(new_up_idx, new_idx),
                    Expression::ArgWrite(.., expr) => AstExpression::ArgWrite(new_up_idx, new_idx, Box::new(self.adapt_expr_from_inlining(expr)?)),
                    _ => unreachable!()
                }
            },
            Expression::Exit(expr, up_idx) => {
                let new_up_idx = up_idx - self.scopes.iter().rev().take(*up_idx).filter(|e| e.is_getting_inlined).count();
                AstExpression::Exit(Box::new(self.adapt_expr_from_inlining(expr)?), new_up_idx)
            }
            Expression::FieldWrite(idx, expr) => {
                AstExpression::FieldWrite(*idx, Box::new(self.adapt_expr_from_inlining(expr)?))
            }
            Expression::LocalVarWrite(idx, expr) => {
                AstExpression::LocalVarWrite(*idx, Box::new(self.adapt_expr_from_inlining(expr)?))
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
                        receiver: self.adapt_expr_from_inlining(&msg.receiver)?,
                        signature: msg.signature.clone(),
                        values: msg.values.iter().filter_map(|e| self.adapt_expr_from_inlining(e)).collect(),
                    }))
                }
            }
            Expression::SuperMessage(super_msg) => {
                AstExpression::SuperMessage(Box::new(AstSuperMessage {
                    receiver_name: super_msg.receiver_name.clone(),
                    is_static_class_call: super_msg.is_static_class_call,
                    signature: super_msg.signature.clone(),
                    values: super_msg.values.iter().filter_map(|e| self.adapt_expr_from_inlining(e)).collect(),
                }))
            }
            Expression::BinaryOp(bin_op) => {
                AstExpression::BinaryOp(Box::new(AstBinaryOp {
                    op: bin_op.op.clone(),
                    lhs: self.adapt_expr_from_inlining(&bin_op.lhs)?,
                    rhs: self.adapt_expr_from_inlining(&bin_op.rhs)?,
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

    fn inline_while(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        let (cond_blk, body_blk) = match (&msg.receiver, msg.values.first()) {
            (Expression::Block(cond_blk), Some(Expression::Block(body_blk))) => (cond_blk, body_blk),
            _ => return None
        };

        let while_inlined_node = WhileInlinedNode {
            expected_bool,
            cond_instrs: self.inline_block(cond_blk)?,
            body_instrs: self.inline_block(body_blk)?,
        };

        // dbg!(&msg);
        // println!("{}", &if_inlined_node.body_instrs);

        Some(InlinedNode::WhileInlined(while_inlined_node))
    }
}