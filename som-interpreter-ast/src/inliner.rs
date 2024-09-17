use std::cell::RefCell;
use std::rc::Rc;

use som_core::ast;
use som_core::ast::{Block, Expression};

use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstSuperMessage, InlinedNode};
use crate::compiler::{AstMethodCompilerCtxt, AstScopeCtxt};
use crate::specialized::inlined::and_inlined_node::AndInlinedNode;
use crate::specialized::inlined::if_inlined_node::IfInlinedNode;
use crate::specialized::inlined::if_true_if_false_inlined_node::IfTrueIfFalseInlinedNode;
use crate::specialized::inlined::or_inlined_node::OrInlinedNode;
use crate::specialized::inlined::while_inlined_node::WhileInlinedNode;

pub trait PrimMessageInliner {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
    fn parse_expr_with_inlining(&mut self, expression: &Expression) -> Option<AstExpression>;
    fn inline_block(&mut self, expression: &Block) -> Option<AstBody>;
    fn adapt_block_after_outer_inlined(&mut self, blk: &Block) -> Option<AstBlock>;
    fn adapt_var_or_args_coords_from_inlining(&self, up_idx: usize, idx: usize, vars_or_args_fetcher_func: fn(&AstScopeCtxt) -> usize) -> (usize, usize);
    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_if_true_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_while(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_or(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
    fn inline_and(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
}

impl PrimMessageInliner for AstMethodCompilerCtxt {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        match msg.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(msg, true),
            "ifFalse:" => self.inline_if_true_or_if_false(msg, false),
            "ifTrue:ifFalse:" => self.inline_if_true_if_false(msg, true),
            "ifFalse:ifTrue:" => self.inline_if_true_if_false(msg, false),
            "whileTrue:" => self.inline_while(msg, true),
            "whileFalse:" => self.inline_while(msg, false),
            "or:" | "||" => self.inline_or(msg),
            "and:" | "&&" => self.inline_and(msg),
            _ => None,
        }
    }

    /// Parses an expression while taking the possible effects of inlining into account.
    fn parse_expr_with_inlining(&mut self, expression: &Expression) -> Option<AstExpression> {
        let expr = match expression {
            Expression::Block(blk) => {
                let new_blk = self.adapt_block_after_outer_inlined(blk)?;
                AstExpression::Block(Rc::new(RefCell::new(new_blk)))
            }
            Expression::LocalVarRead(idx) | Expression::LocalVarWrite(idx, _) |
            Expression::NonLocalVarRead(_, idx) | Expression::NonLocalVarWrite(_, idx, _) => {
                let up_idx = match expression {
                    Expression::LocalVarRead(..) | Expression::LocalVarWrite(..) => 0,
                    Expression::NonLocalVarRead(up_idx, ..) | Expression::NonLocalVarWrite(up_idx, ..) => *up_idx,
                    _ => unreachable!()
                };

                let (new_up_idx, new_idx) = self.adapt_var_or_args_coords_from_inlining(up_idx, *idx, AstScopeCtxt::get_nbr_locals);

                match new_up_idx {
                    0 => {
                        match expression {
                            Expression::LocalVarRead(..) | Expression::NonLocalVarRead(..) => AstExpression::LocalVarRead(new_idx),
                            Expression::LocalVarWrite(.., expr) | Expression::NonLocalVarWrite(.., expr) => AstExpression::LocalVarWrite(new_idx, Box::new(self.parse_expr_with_inlining(expr)?)),
                            _ => unreachable!()
                        }
                    }
                    _ => {
                        match expression {
                            Expression::NonLocalVarRead(..) => AstExpression::NonLocalVarRead(new_up_idx, new_idx),
                            Expression::NonLocalVarWrite(.., expr) => AstExpression::NonLocalVarWrite(new_up_idx, new_idx, Box::new(self.parse_expr_with_inlining(expr)?)),
                            _ => unreachable!()
                        }
                    }
                }
            }
            Expression::ArgRead(up_idx, idx) | Expression::ArgWrite(up_idx, idx, _) => {
                let (new_up_idx, new_idx) = self.adapt_var_or_args_coords_from_inlining(*up_idx, *idx, AstScopeCtxt::get_nbr_locals);

                match expression {
                    Expression::ArgRead(..) => AstExpression::ArgRead(new_up_idx, new_idx),
                    Expression::ArgWrite(.., expr) => AstExpression::ArgWrite(new_up_idx, new_idx, Box::new(self.parse_expr_with_inlining(expr)?)),
                    _ => unreachable!()
                }
            }
            Expression::Exit(expr, scope) => {
                let inline_expr = self.parse_expr_with_inlining(expr)?;
                let adjust_scope_by = self.scopes.iter().rev().take(*scope).filter(|e| e.is_getting_inlined).count();
                let new_scope = scope - adjust_scope_by;
                match new_scope {
                    0 => AstExpression::LocalExit(Box::new(inline_expr)),
                    _ => AstExpression::NonLocalExit(Box::new(inline_expr), new_scope)
                }
            }
            Expression::GlobalRead(a) => AstExpression::GlobalRead(a.clone()),
            Expression::FieldRead(idx) => AstExpression::FieldRead(*idx),
            Expression::FieldWrite(idx, expr) => AstExpression::FieldWrite(*idx, Box::new(self.parse_expr_with_inlining(expr)?)),
            Expression::Message(msg) => {
                if let Some(inlined_node) = self.inline_if_possible(msg) {
                    return Some(AstExpression::InlinedCall(Box::new(inlined_node)));
                }
                AstExpression::Message(Box::new(AstMessage {
                    receiver: self.parse_expr_with_inlining(&msg.receiver)?,
                    signature: msg.signature.clone(),
                    values: msg.values.iter().filter_map(|val| self.parse_expr_with_inlining(val)).collect(),
                }))
            }
            Expression::SuperMessage(super_msg) => {
                AstExpression::SuperMessage(Box::new(AstSuperMessage {
                    receiver_name: super_msg.receiver_name.clone(),
                    is_static_class_call: super_msg.is_static_class_call,
                    signature: super_msg.signature.clone(),
                    values: super_msg.values.iter().filter_map(|e| self.parse_expr_with_inlining(e)).collect(),
                }))
            }
            Expression::BinaryOp(bin_op) => {
                AstExpression::BinaryOp(Box::new(AstBinaryOp {
                    op: bin_op.op.clone(),
                    lhs: self.parse_expr_with_inlining(&bin_op.lhs)?,
                    rhs: self.parse_expr_with_inlining(&bin_op.rhs)?,
                }))
            }
            Expression::Literal(lit) => AstExpression::Literal(lit.clone()),
        };

        Some(expr)
    }

    fn inline_block(&mut self, blk: &Block) -> Option<AstBody> {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, true));

        let inlined_block = Some(AstBody { exprs: blk.body.exprs.iter().filter_map(|e| self.parse_expr_with_inlining(e)).collect() });

        let (nbr_params_post_inlining, nbr_locals_post_inlining) = {
            let blk_scope = self.scopes.last().unwrap();
            (blk_scope.get_nbr_args(), blk_scope.get_nbr_locals())
        };

        self.scopes.pop();

        self.scopes.last_mut().unwrap().add_nbr_args(nbr_params_post_inlining);
        self.scopes.last_mut().unwrap().add_nbr_locals(nbr_locals_post_inlining);

        inlined_block
    }

    fn adapt_block_after_outer_inlined(&mut self, blk: &Block) -> Option<AstBlock> {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, false));

        let exprs: Vec<AstExpression> = blk.body.exprs.iter()
            .filter_map(|og_expr| {
                self.parse_expr_with_inlining(og_expr)
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

    // fn adapt_expr_from_inlining(&mut self, og_expr: &Expression) -> Option<AstExpression> {
    //     let new_expr = match og_expr {
    //         Expression::NonLocalVarRead(up_idx, idx) | Expression::NonLocalVarWrite(up_idx, idx, _) => {
    //             let (new_up_idx, new_idx) = self.adapt_var_or_args_coords_from_inlining(*up_idx, *idx, AstScopeCtxt::get_nbr_locals);
    //             debug_assert_ne!(new_up_idx, 0);
    // 
    //             match og_expr {
    //                 Expression::NonLocalVarRead(..) => AstExpression::NonLocalVarRead(new_up_idx, new_idx),
    //                 Expression::NonLocalVarWrite(.., expr) => AstExpression::NonLocalVarWrite(new_up_idx, new_idx, Box::new(self.adapt_expr_from_inlining(expr)?)),
    //                 _ => unreachable!()
    //             }
    //         }
    //         Expression::ArgRead(up_idx, idx) | Expression::ArgWrite(up_idx, idx, _) => {
    //             let (new_up_idx, new_idx) = self.adapt_var_or_args_coords_from_inlining(*up_idx, *idx, AstScopeCtxt::get_nbr_args);
    // 
    //             match og_expr {
    //                 Expression::ArgRead(..) => AstExpression::ArgRead(new_up_idx, new_idx),
    //                 Expression::ArgWrite(.., expr) => AstExpression::ArgWrite(new_up_idx, new_idx, Box::new(self.adapt_expr_from_inlining(expr)?)),
    //                 _ => unreachable!()
    //             }
    //         }
    //         Expression::Exit(expr, up_idx) => {
    //             let new_up_idx = up_idx - self.scopes.iter().rev().take(*up_idx).filter(|e| e.is_getting_inlined).count();
    //             AstExpression::Exit(Box::new(self.adapt_expr_from_inlining(expr)?), new_up_idx)
    //         }
    //         Expression::FieldWrite(idx, expr) => {
    //             AstExpression::FieldWrite(*idx, Box::new(self.adapt_expr_from_inlining(expr)?))
    //         }
    //         Expression::LocalVarWrite(idx, expr) => {
    //             AstExpression::LocalVarWrite(*idx, Box::new(self.adapt_expr_from_inlining(expr)?))
    //         }
    //         Expression::Block(blk) => {
    //             let new_block = self.adapt_block_after_outer_inlined(blk)?;
    //             AstExpression::Block(Rc::new(new_block))
    //         }
    //         Expression::Message(msg) => {
    //             if let Some(inlined_method) = self.inline_if_possible(msg) {
    //                 AstExpression::InlinedCall(Box::new(inlined_method))
    //             } else {
    //                 AstExpression::Message(Box::new(AstMessage {
    //                     receiver: self.adapt_expr_from_inlining(&msg.receiver)?,
    //                     signature: msg.signature.clone(),
    //                     values: msg.values.iter().filter_map(|e| self.adapt_expr_from_inlining(e)).collect(),
    //                 }))
    //             }
    //         }
    //         Expression::SuperMessage(super_msg) => {
    //             AstExpression::SuperMessage(Box::new(AstSuperMessage {
    //                 receiver_name: super_msg.receiver_name.clone(),
    //                 is_static_class_call: super_msg.is_static_class_call,
    //                 signature: super_msg.signature.clone(),
    //                 values: super_msg.values.iter().filter_map(|e| self.adapt_expr_from_inlining(e)).collect(),
    //             }))
    //         }
    //         Expression::BinaryOp(bin_op) => {
    //             AstExpression::BinaryOp(Box::new(AstBinaryOp {
    //                 op: bin_op.op.clone(),
    //                 lhs: self.adapt_expr_from_inlining(&bin_op.lhs)?,
    //                 rhs: self.adapt_expr_from_inlining(&bin_op.rhs)?,
    //             }))
    //         }
    //         Expression::GlobalRead(_) |
    //         Expression::LocalVarRead(_) |
    //         Expression::FieldRead(_) |
    //         Expression::Literal(_) => self.parse_expression(og_expr)
    //     };
    // 
    //     Some(new_expr)
    // }
    
    fn adapt_var_or_args_coords_from_inlining(&self, up_idx: usize, idx: usize, vars_or_args_fetcher_func: fn(&AstScopeCtxt) -> usize) -> (usize, usize) {
        // new up index is the target var scope minus the number of inlined scopes in between the current scope and the target var scope
        // if you do a NonLocalVarRead(3, 0), and there's 1 inlined scope before that (3) target, then now that target scope is only (2) scopes away.
        let new_up_idx = match up_idx {
            0 => 0, // branch not strictly necessary, but faster, and for a very common case.
            _ => up_idx - self.scopes.iter().rev().take(up_idx).filter(|e| e.is_getting_inlined).count()
        };
        
        // new index is more complicated, since some variables can have gotten inlined into other scopes.
        let new_idx = {
            let up_idx_of_scope_var_will_end_up_into = self.scopes.iter()
                .rev()
                .skip(up_idx)
                .take_while(|scope| scope.is_getting_inlined)
                .count() + up_idx; // if we're accessing a var into a scope that's getting inlined, then its idx may have changed due to the inlining.

            let nbr_vars_in_final_scope_to_offset_by = self.scopes.iter()
                .rev()
                .skip(up_idx + 1) // we go back right before the target scope...
                .take(up_idx_of_scope_var_will_end_up_into - up_idx) // ...and we get all the scopes that get inlined before the target scope...
                .map(vars_or_args_fetcher_func) // ...and we aggregate their args/vars. This number will be by how much to offset the variable index to account for inlining. 
                .sum::<usize>();

            // Visual aide to understand why and how vars need to be modified:
            // _
            // | |a b| 
            // |      // V -- THIS SCOPE GETS INLINED (prev scope vars become |a b c| )
            // |         _
            // |         | |c|
            // |         |    // V -- not inlined.
            // |         |       _
            // |         |      | |d|
            // |         |      |  VarRead(0, 0)... becomes: => VarRead(0, 0)
            // |         |      |  VarRead(1, 0) => VarRead(1, 2)
            // |         |      |  VarRead(2, 0) => VarRead(1, 0)
            // |         |      |  VarRead(2, 1) => VarRead(1, 1)
            // |         |    _
            // |         _
            // _
            
            nbr_vars_in_final_scope_to_offset_by + idx
        };

        (new_up_idx, new_idx)
    }

    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None
        };

        let if_inlined_node = IfInlinedNode {
            expected_bool,
            cond_expr: self.parse_expr_with_inlining(&msg.receiver)?,
            body_instrs: self.inline_block(body_blk)?,
        };

        Some(InlinedNode::IfInlined(if_inlined_node))
    }

    fn inline_if_true_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        let (body_blk_1, body_blk_2) = match (msg.values.first(), msg.values.get(1)) {
            (Some(Expression::Block(blk)), Some(Expression::Block(blk2))) => (blk, blk2),
            _ => return None
        };

        let if_true_if_false_inlined_node = IfTrueIfFalseInlinedNode {
            expected_bool,
            cond_expr: self.parse_expr_with_inlining(&msg.receiver)?,
            body_1_instrs: self.inline_block(body_blk_1)?,
            body_2_instrs: self.inline_block(body_blk_2)?,
        };

        Some(InlinedNode::IfTrueIfFalseInlined(if_true_if_false_inlined_node))
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

        Some(InlinedNode::WhileInlined(while_inlined_node))
    }

    fn inline_or(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        let snd_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None
        };

        let or_inlined_node = OrInlinedNode {
            first: self.parse_expr_with_inlining(&msg.receiver)?,
            second: self.inline_block(snd_blk)?,
        };

        Some(InlinedNode::OrInlined(or_inlined_node))
    }

    fn inline_and(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        let snd_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None
        };

        let and_inlined_node = AndInlinedNode {
            first: self.parse_expr_with_inlining(&msg.receiver)?,
            second: self.inline_block(snd_blk)?,
        };

        Some(InlinedNode::AndInlined(and_inlined_node))
    }
}