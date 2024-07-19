use std::rc::Rc;

use som_core::ast;
use som_core::ast::{Block, Expression};

use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstSuperMessage, InlinedNode};
use crate::compiler::AstMethodCompilerCtxt;
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
        let nbr_of_locals_pre_inlining = self.get_nbr_locals();
        let nbr_of_args_pre_inlining = self.get_nbr_args();

        let expr = match expression {
            Expression::Block(blk) => {
                let new_blk = self.adapt_block_after_outer_inlined(blk)?;
                AstExpression::Block(Rc::new(new_blk))
            }
            Expression::LocalVarRead(idx) => AstExpression::LocalVarRead(idx + nbr_of_locals_pre_inlining),
            Expression::LocalVarWrite(idx, expr) => {
                AstExpression::LocalVarWrite(idx + nbr_of_locals_pre_inlining, Box::new(self.get_inline_expression(expr)?))
            }
            Expression::NonLocalVarRead(scope, idx) => {
                match *scope - self.inlining_level {
                    0 => AstExpression::LocalVarRead(*idx),
                    _ => AstExpression::NonLocalVarRead(*scope - self.inlining_level, *idx)
                }
            }
            Expression::NonLocalVarWrite(scope, idx, expr) => {
                let ast_expr = Box::new(self.get_inline_expression(expr)?);
                match *scope - self.inlining_level {
                    0 => AstExpression::LocalVarWrite(*idx, ast_expr),
                    _ => AstExpression::NonLocalVarWrite(*scope - self.inlining_level, *idx, ast_expr)
                }
            }
            Expression::ArgRead(scope, idx) => {
                match *scope {
                    0 => AstExpression::ArgRead(0, idx + nbr_of_args_pre_inlining),
                    _ => AstExpression::ArgRead(*scope - self.inlining_level, *idx)
                }
            }
            Expression::ArgWrite(scope, idx, expr) => {
                let ast_expr = Box::new(self.get_inline_expression(expr)?);
                match *scope {
                    0 => AstExpression::ArgWrite(0, idx + nbr_of_args_pre_inlining, ast_expr),
                    _ => AstExpression::ArgWrite(*scope - self.inlining_level, *idx, ast_expr)
                }
            }
            Expression::GlobalRead(a) => AstExpression::GlobalRead(a.clone()),
            Expression::FieldRead(idx) => AstExpression::FieldRead(*idx),
            Expression::FieldWrite(idx, expr) => AstExpression::FieldWrite(*idx, Box::new(self.get_inline_expression(expr)?)),
            Expression::Message(msg) => {
                self.inlining_level += 1;
                if let Some(inlined_node) = self.inline_if_possible(msg) {
                    self.inlining_level -= 1;
                    return Some(AstExpression::InlinedCall(Box::new(inlined_node)));
                }
                self.inlining_level -= 1;
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
                AstExpression::Exit(Box::new(inline_expr), scope - self.inlining_level)
            }
            Expression::Literal(lit) => AstExpression::Literal(lit.clone()),
        };

        Some(expr)
    }

    fn inline_block(&mut self, blk: &Block) -> Option<AstBody> {
        let inlined_block = Some(AstBody { exprs: blk.body.exprs.iter().filter_map(|e| self.get_inline_expression(e)).collect() });

        self.add_nbr_args(blk.nbr_params);
        self.add_nbr_locals(blk.nbr_locals);

        inlined_block
    }

    fn adapt_block_after_outer_inlined(&mut self, blk: &Rc<Block>) -> Option<AstBlock> {
        let mut blk_ctxt = AstMethodCompilerCtxt::init(blk.nbr_params, blk.nbr_locals, 1);

        let exprs: Vec<AstExpression> = blk.body.exprs.iter()
            .filter_map(|og_expr| {
                blk_ctxt.get_inline_expression_for_adapted_block(og_expr)
            }).collect();

        Some(AstBlock {
            nbr_params: blk.nbr_params,
            nbr_locals: blk.nbr_locals,
            body: AstBody { exprs },
        })
    }

    fn get_inline_expression_for_adapted_block(&mut self, og_expr: &Expression) -> Option<AstExpression> {
        let new_expr = match og_expr {
            Expression::NonLocalVarRead(up_idx, _) | Expression::NonLocalVarWrite(up_idx, _, _)
            | Expression::ArgRead(up_idx, _) | Expression::ArgWrite(up_idx, _, _) => {
                let new_up_idx = match *up_idx {
                    0 => 0, // local var/arg, not affected by inlining, stays the same
                    1 => 1, // non-local access to previous scope, which is getting inlined. no changes needed.
                    // d if d <= self.inlining_level => d,
                    _ => *up_idx - self.inlining_level, // access to a scope beyond that one - now we need to account for the impact of inlining.
                };

                // todo ACTUALLY shouldn't the idx be adjusted depending on the amount of inlined variables in the block? make a test for that! and for the args case too!

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
                    _ => unreachable!(),
                }
            }
            Expression::Exit(expr, scope) => {
                AstExpression::Exit(Box::new(self.get_inline_expression_for_adapted_block(expr)?), scope - self.inlining_level)
            }
            Expression::FieldWrite(idx, expr) => {
                AstExpression::FieldWrite(*idx, Box::new(self.get_inline_expression_for_adapted_block(expr)?))
            },
            Expression::LocalVarWrite(idx, expr) => {
                AstExpression::LocalVarWrite(*idx, Box::new(self.get_inline_expression_for_adapted_block(expr)?))
            },
            Expression::Block(blk) => {
                let new_block = self.adapt_block_after_outer_inlined(blk)?;
                AstExpression::Block(Rc::new(new_block))
            }
            Expression::Message(msg) => {
                // todo enable nested inlining
                // if let Some(inlined_method) = self.inline_if_possible(msg) {
                //     AstExpression::InlinedCall(Box::new(inlined_method))
                // } else {
                AstExpression::Message(Box::new(AstMessage {
                    receiver: self.get_inline_expression_for_adapted_block(&msg.receiver)?,
                    signature: msg.signature.clone(),
                    values: msg.values.iter().filter_map(|e| self.get_inline_expression_for_adapted_block(e)).collect(),
                }))
                // }
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
            Expression::Literal(_) => AstMethodCompilerCtxt::parse_expression(self, og_expr)
        };

        Some(new_expr)
    }

    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None
        };

        let if_inlined_node = IfInlinedNode {
            expected_bool,
            cond_instrs: {
                match self.inlining_level {
                    1 => AstBody { exprs: vec![self.parse_expression(&msg.receiver)] },
                    _ => {
                        // todo: this code works, but doesn't feel good. also, enabling inlining in nested blocks breaks. i reckon inlining level should be a function param instead! this might fix things, also.
                        self.inlining_level -= 1;
                        let ret = AstBody { exprs: vec![self.get_inline_expression(&msg.receiver)?] };
                        self.inlining_level += 1;
                        ret
                    }
                }
            },
            body_instrs: self.inline_block(body_blk)?,
        };

        // dbg!(&msg);
        // println!("{}", &if_inlined_node.body_instrs);

        Some(InlinedNode::IfInlined(if_inlined_node))
    }
}