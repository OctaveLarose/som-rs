use crate::AstGenCtxt;
use som_core::ast::{self, Message};
use som_core::ast::{Block, Expression};

/// Helper enum for some variable-related logic when inlining.
pub enum VarType<'a> {
    Read,
    Write(&'a Expression),
}

#[allow(unused)] // if inlining is disabled, a lot of them go completely unused.
pub(crate) trait PrimMessageInliner {
    fn inline_if_possible(&mut self, msg: &ast::RegularMessage) -> Option<Message>;
    fn parse_expression_with_inlining(&mut self, expression: &Expression) -> Expression;
    fn inline_block(&mut self, expression: &Block) -> Vec<Expression>;
    fn adapt_block_after_outer_inlined(&mut self, blk: &Block) -> ast::Block;
    fn adapt_var_coords_from_inlining(&mut self, up_idx: usize, idx: usize) -> (u8, u8);
    fn adapt_arg_access_from_inlining(&mut self, input_expr: &Expression) -> Expression;
    fn inline_if_true_or_if_false(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message>;
    fn get_nbr_vars_in_scope(&self, _access_from_up_idx: usize) -> usize;
    fn var_from_coords(&mut self, up_idx: u8, idx: u8, var_type: VarType) -> Expression;
}

impl PrimMessageInliner for AstGenCtxt<'_> {
    fn inline_if_possible(&mut self, msg: &ast::RegularMessage) -> Option<Message> {
        match msg.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(msg, true),
            //"ifFalse:" => self.inline_if_true_or_if_false(msg, false),
            // more to come. how exciting
            _ => None,
        }
    }

    /// Parses an expression while taking the possible effects of inlining into account.
    fn parse_expression_with_inlining(&mut self, _expression: &Expression) -> Expression {
        todo!();
        //let expr = match expression {
        //Expression::Block(blk) => {
        //    let new_blk = self.adapt_block_after_outer_inlined(blk);
        //    Expression::Block(new_blk)
        //}
        //Expression::VarRead(idx, up_idx) | Expression::VarWrite(idx, up_idx, _) => {
        //    let (new_up_idx, new_idx) = self.adapt_var_coords_from_inlining(*up_idx, *idx);
        //
        //    let var_type = match expression {
        //        Expression::VarRead(..) => VarType::Read,
        //        Expression::VarWrite(_, _, expr) => VarType::Write(expr),
        //        _ => unreachable!(),
        //    };
        //
        //    self.var_from_coords(new_up_idx, new_idx, var_type)
        //}
        //expr @ Expression::ArgRead(..) | expr @ Expression::ArgWrite(..) => self.adapt_arg_access_from_inlining(expr),
        //Expression::Exit(expr, scope) => {
        //    let inline_expr = self.parse_expression_with_inlining(expr);
        //    //let adjust_scope_by = self.scopes.iter().rev().take(*scope).filter(|e| e.is_getting_inlined).count();
        //    let adjust_scope_by = {
        //        let mut cur: Option<AstGenCtxt<'_>> = Some(self.clone());
        //        let mut adjust_by = 0;
        //
        //        while cur.is_some() {
        //            adjust_by += 1;
        //            cur = cur.unwrap().borrow().outer_ctxt.clone();
        //        }
        //
        //        adjust_by
        //    };
        //    let new_scope = scope - adjust_scope_by;
        //    Expression::Exit(Box::new(inline_expr), (new_scope as u8).into())
        //}
        //global_read @ Expression::GlobalRead(_a) => global_read.clone(),
        //Expression::GlobalWrite(name, expr) => Expression::GlobalWrite(name.clone(), Box::new(self.parse_expression_with_inlining(expr))),
        ////Expression::Message(msg) => self.parse_message_with_inlining(msg),
        //Expression::Message(msg) => {
        //    let old_msg = match &**msg {
        //        Message::Regular(reg_msg) => reg_msg,
        //        _ => todo!("not handling inlining methods within inlined blocks"),
        //    };
        //
        //    let new_msg = RegularMessage {
        //        receiver: self.parse_expression_with_inlining(&old_msg.receiver),
        //        signature: old_msg.signature.clone(),
        //        values: old_msg.values.iter().map(|v| self.parse_expression_with_inlining(v)).collect(),
        //    };
        //
        //    Expression::Message(Box::new(Message::Regular(new_msg)))
        //}
        //lit_expr @ Expression::Literal(_) => lit_expr.clone(),
        //};

        //expr
    }

    fn inline_block(&mut self, blk: &Block) -> Vec<Expression> {
        //let blk_scope = AstGenCtxtData::new_ctxt_from(self.borrow_mut(), AstGenCtxtType::Block);
        //self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, true));

        dbg!("really, this should set the scope.");
        //let inlined_block = blk.body.exprs.iter().map(|e| self.parse_expression_with_inlining(e)).collect();
        blk.body.exprs.iter().map(|e| self.parse_expression_with_inlining(e)).collect()

        //let (nbr_locals_post_inlining, _blk_nbr_args) = {
        //let blk_scope = self.scopes.last().unwrap();
        //(blk_scope.get_nbr_locals(), blk_scope.get_nbr_args())
        //};

        //self.scopes.pop();

        //// self.scopes.last_mut().unwrap().add_nbr_locals(nbr_locals_post_inlining + blk_nbr_args);

        // TODO: how do we restore that behavior?
        //self.borrow_mut().add_locals(nbr_locals_post_inlining);

        //inlined_block
    }

    fn adapt_block_after_outer_inlined(&mut self, _blk: &Block) -> ast::Block {
        todo!("after outer is inlined, need to adapt scopes")
        //self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, false));
        //
        //let exprs: Vec<Expression> = blk.body.exprs.iter().map(|og_expr| self.parse_expression_with_inlining(og_expr)).collect();
        //
        //let (nbr_params, nbr_locals) = {
        //    let outer_blk_scope = self.scopes.last().unwrap();
        //    (outer_blk_scope.get_nbr_args() as u8, outer_blk_scope.get_nbr_locals() as u8)
        //};
        //
        //let adapted_inner_block = AstBlock {
        //    nbr_params,
        //    nbr_locals,
        //    body: AstBody { exprs },
        //};
        //
        //self.scopes.pop();
        //
        //adapted_inner_block
    }

    fn adapt_var_coords_from_inlining(&mut self, _up_idx: usize, _idx: usize) -> (u8, u8) {
        todo!("err, annoying to implement here honestly")
    }

    fn adapt_arg_access_from_inlining(&mut self, _input_expr: &Expression) -> Expression {
        todo!("same as var coords, a hassle that's simpler here")
    }

    /// Helper function: generates a local variable expression given coordinates. We get duplicated logic otherwise.
    fn var_from_coords(&mut self, _up_idx: u8, _idx: u8, _var_type: VarType) -> Expression {
        todo!()
        //match var_type {
        //VarType::Read => Expression::VarRead(up_idx as usize, idx as usize),
        //VarType::Write(expr) => Expression::VarWrite(up_idx as usize, idx as usize, Box::new(self.parse_expression_with_inlining(expr))),
        //}
    }

    /// Returns the number of arguments in a given scope, accounting for inlining.
    fn get_nbr_vars_in_scope(&self, _access_from_up_idx: usize) -> usize {
        todo!("we only used this in the to:do: inlining? ah, because it's the only one where we also inline args.")
        //let up_idx_scope_arg_inlined_into = self.scopes.iter().rev().take_while(|c| c.is_getting_inlined).count();
        //
        //let nbr_locals_in_target_scope =
        //    self.scopes.iter().rev().take(up_idx_scope_arg_inlined_into + 1).map(AstScopeCtxt::get_nbr_locals).sum::<usize>();
        //
        //let nbr_args_inlined_in_target_scope =
        //    self.scopes.iter().rev().take(up_idx_scope_arg_inlined_into).map(AstScopeCtxt::get_nbr_args).sum::<usize>();
        //
        //nbr_locals_in_target_scope + nbr_args_inlined_in_target_scope
    }

    fn inline_if_true_or_if_false(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        let if_inlined_msg = ast::IfInlinedMessage {
            expected_bool,
            cond_expr: self.parse_expression_with_inlining(&msg.receiver),
            body_instrs: self.inline_block(body_blk),
        };

        Some(Message::IfInlined(if_inlined_msg))
    }
}
