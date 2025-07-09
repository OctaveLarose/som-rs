use super::compile::{AstMethodCompilerCtxt, AstScopeCtxt};
use crate::ast::{AstBlock, AstBody, AstExpression, InlinedNode};
use crate::nodes::inlined::and_inlined_node::AndInlinedNode;
use crate::nodes::inlined::if_inlined_node::IfInlinedNode;
use crate::nodes::inlined::if_nil_if_not_nil_inlined_node::IfNilIfNotNilInlinedNode;
use crate::nodes::inlined::if_nil_inlined_node::IfNilInlinedNode;
use crate::nodes::inlined::if_true_if_false_inlined_node::IfTrueIfFalseInlinedNode;
use crate::nodes::inlined::or_inlined_node::OrInlinedNode;
use crate::nodes::inlined::to_do_inlined_node::ToDoInlinedNode;
use crate::nodes::inlined::while_inlined_node::WhileInlinedNode;
use som_core::ast;
use som_core::ast::{Block, Expression, Literal};
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};

/// Helper enum for some variable-related logic when inlining.
pub enum VarType<'a> {
    Read,
    Write(&'a Expression),
}

#[allow(unused)] // if inlining is disabled, a lot of them go completely unused.
pub(crate) trait PrimMessageInliner {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
    fn parse_expression_with_inlining(&mut self, expression: &Expression) -> AstExpression;
    fn inline_block(&mut self, expression: &Block) -> AstBody;
    fn adapt_block_after_outer_inlined(&mut self, blk: &Block) -> AstBlock;
    fn adapt_var_coords_from_inlining(&self, up_idx: usize, idx: usize) -> (u8, u8);
    fn adapt_arg_access_from_inlining(&mut self, input_expr: &Expression) -> AstExpression;
    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_if_true_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_if_nil_or_if_not_nil(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_if_nil_if_not_nil(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_while(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode>;
    fn inline_or(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
    fn inline_and(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
    fn inline_to_do(&mut self, msg: &ast::Message) -> Option<InlinedNode>;
}

impl PrimMessageInliner for AstMethodCompilerCtxt<'_> {
    fn inline_if_possible(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        // return None;
        match msg.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(msg, true),
            "ifFalse:" => self.inline_if_true_or_if_false(msg, false),
            "ifTrue:ifFalse:" => self.inline_if_true_if_false(msg, true),
            "ifFalse:ifTrue:" => self.inline_if_true_if_false(msg, false),
            "ifNil:" => self.inline_if_nil_or_if_not_nil(msg, true),
            "ifNotNil:" => self.inline_if_nil_or_if_not_nil(msg, false),
            "ifNil:ifNotNil:" => self.inline_if_nil_if_not_nil(msg, true),
            "ifNotNil:ifNil:" => self.inline_if_nil_if_not_nil(msg, false),
            "whileTrue:" => self.inline_while(msg, true),
            "whileFalse:" => self.inline_while(msg, false),
            "or:" | "||" => self.inline_or(msg),
            "and:" | "&&" => self.inline_and(msg),
            "to:do:" => self.inline_to_do(msg),
            _ => None,
        }
    }

    /// Parses an expression while taking the possible effects of inlining into account.
    fn parse_expression_with_inlining(&mut self, expression: &Expression) -> AstExpression {
        let expr = match expression {
            Expression::Block(blk) => {
                let new_blk = self.adapt_block_after_outer_inlined(blk);
                let new_blk_ptr = self.gc_interface.alloc(new_blk, AllocSiteMarker::Block); // could we just adapt the old block instead of allocating?
                AstExpression::Block(new_blk_ptr)
            }
            Expression::LocalVarRead(idx)
            | Expression::LocalVarWrite(idx, _)
            | Expression::NonLocalVarRead(_, idx)
            | Expression::NonLocalVarWrite(_, idx, _) => {
                let up_idx = match expression {
                    Expression::LocalVarRead(..) | Expression::LocalVarWrite(..) => 0,
                    Expression::NonLocalVarRead(up_idx, ..) | Expression::NonLocalVarWrite(up_idx, ..) => *up_idx,
                    _ => unreachable!(),
                };

                let (new_up_idx, new_idx) = self.adapt_var_coords_from_inlining(up_idx, *idx);

                let var_type = match expression {
                    Expression::LocalVarRead(..) | Expression::NonLocalVarRead(..) => VarType::Read,
                    Expression::LocalVarWrite(_, expr) | Expression::NonLocalVarWrite(_, _, expr) => VarType::Write(expr),
                    _ => unreachable!(),
                };

                self.var_from_coords(new_up_idx, new_idx, var_type)
            }
            expr @ Expression::ArgRead(..) | expr @ Expression::ArgWrite(..) => self.adapt_arg_access_from_inlining(expr),
            Expression::Exit(expr, scope) => {
                let inline_expr = self.parse_expression_with_inlining(expr);
                let adjust_scope_by = self.scopes.iter().rev().take(*scope).filter(|e| e.is_getting_inlined).count();
                let new_scope = scope - adjust_scope_by;
                match new_scope {
                    0 => AstExpression::LocalExit(self.gc_interface.alloc(inline_expr, AllocSiteMarker::AstExpression)),
                    _ => AstExpression::NonLocalExit(self.gc_interface.alloc(inline_expr, AllocSiteMarker::AstExpression), new_scope as u8),
                }
            }
            Expression::GlobalRead(a) => self.global_or_field_read_from_superclass(a.clone()),
            Expression::GlobalWrite(name, expr) => {
                if self.class.is_none() {
                    panic!(
                        "can't turn the GlobalWrite `{}` into a FieldWrite, and GlobalWrite shouldn't exist at runtime",
                        name
                    )
                }
                match self.class.clone().unwrap().get_field_offset_by_name(name) {
                    Some(offset) => {
                        let expr_with_inlining = self.parse_expression_with_inlining(expr);
                        AstExpression::FieldWrite(offset as u8, self.gc_interface.alloc(expr_with_inlining, AllocSiteMarker::AstExpression))
                    }
                    _ => panic!(
                        "can't turn the GlobalWrite `{}` into a FieldWrite, and GlobalWrite shouldn't exist at runtime",
                        name
                    ),
                }
            }
            Expression::Message(msg) => self.parse_message_with_inlining(msg),
            Expression::Literal(lit) => AstExpression::Literal(self.parse_literal(lit)),
        };

        expr
    }

    fn inline_block(&mut self, blk: &Block) -> AstBody {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, true));

        let inlined_block = AstBody {
            exprs: {
                let exprs: Vec<AstExpression> = blk.body.exprs.iter().map(|e| self.parse_expression_with_inlining(e)).collect();
                self.gc_interface.alloc_slice(exprs.as_slice(), AllocSiteMarker::SliceAstExpression)
            },
        };

        let (nbr_locals_post_inlining, _blk_nbr_args) = {
            let blk_scope = self.scopes.last().unwrap();
            (blk_scope.get_nbr_locals(), blk_scope.get_nbr_args())
        };

        self.scopes.pop();

        // self.scopes.last_mut().unwrap().add_nbr_locals(nbr_locals_post_inlining + blk_nbr_args);
        self.scopes.last_mut().unwrap().add_nbr_locals(nbr_locals_post_inlining);

        inlined_block
    }

    fn adapt_block_after_outer_inlined(&mut self, blk: &Block) -> AstBlock {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, false));

        let exprs: Vec<AstExpression> = blk.body.exprs.iter().map(|og_expr| self.parse_expression_with_inlining(og_expr)).collect();

        let (nbr_params, nbr_locals) = {
            let outer_blk_scope = self.scopes.last().unwrap();
            (outer_blk_scope.get_nbr_args() as u8, outer_blk_scope.get_nbr_locals() as u8)
        };

        let adapted_inner_block = AstBlock {
            nbr_params,
            nbr_locals,
            body: AstBody {
                exprs: self.gc_interface.alloc_slice(exprs.as_slice(), AllocSiteMarker::SliceAstExpression),
            },
        };

        self.scopes.pop();

        adapted_inner_block
    }

    fn adapt_var_coords_from_inlining(&self, up_idx: usize, idx: usize) -> (u8, u8) {
        // new up index is the target var scope minus the number of inlined scopes in between the current scope and the target var scope
        // if you do a NonLocalVarRead(3, 0), and there's 1 inlined scope before that (3) target, then now that target scope is only (2) scopes away.
        let new_up_idx = match up_idx {
            0 => 0, // branch not strictly necessary, but faster, and for a very common case.
            _ => up_idx - self.scopes.iter().rev().take(up_idx).filter(|e| e.is_getting_inlined).count(),
        };

        // new index is more complicated, since some variables can have gotten inlined into other scopes.
        let new_idx = {
            let nbr_scopes_between_up_idx_and_inline_target =
                self.scopes.iter().rev().skip(up_idx).take_while(|scope| scope.is_getting_inlined).count(); // if we're accessing a var into a scope that's getting inlined, then its idx may have changed due to the inlining.

            let nbr_vars_in_final_scope_to_offset_by = self
                .scopes
                .iter()
                .rev()
                .skip(up_idx + 1) // we go back right before the original target scope...
                .take(nbr_scopes_between_up_idx_and_inline_target) // ...and we get all the scopes that get inlined before the target scope...
                .map(AstScopeCtxt::get_nbr_locals) // ...and we aggregate their vars. This number will be by how much to offset the variable index to account for inlining.
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

            // additional complication: we rarely inline blocks that have arguments. It's most often 0, but at the moment it can be 1 in the case of `to:do:`.
            let nbr_inlined_args_turned_vars = self
                .scopes
                .iter()
                .rev()
                .skip(up_idx) // we go back TO the original target scope, so that we take its arguments into account if it gets inlined.
                .take(nbr_scopes_between_up_idx_and_inline_target) // we stop right before the scope we end up into (the arguments of that final target scope will not be inlined)
                .map(AstScopeCtxt::get_nbr_args) // ...and we aggregate the arguments instead of the locals
                .sum::<usize>();

            // _
            // | |a b|
            // |      // V -- THIS SCOPE GETS INLINED (prev scope vars become |a b i c| )
            // |         _
            // |         | Arg: |i|
            // |         | |c|
            // |         |       V -- not inlined.
            // |         |       _
            // |         |      | |d|
            // |         |      |  VarRead(0, 0)... becomes: => VarRead(0, 0)
            // |         |      |  VarRead(1, 0) => VarRead(1, 3)
            // |         |      |  VarRead(2, 0) => VarRead(1, 0)
            // |         |      |  VarRead(2, 1) => VarRead(1, 1)
            // |         |      |  ArgRead(1, 1) => VarRead(1, 2) <----------- NEW CASE
            // |         |    _
            // |         _
            // _

            nbr_vars_in_final_scope_to_offset_by + nbr_inlined_args_turned_vars + idx
        };

        (new_up_idx as u8, new_idx as u8)
    }

    fn adapt_arg_access_from_inlining(&mut self, input_expr: &Expression) -> AstExpression {
        let (up_idx, idx) = match input_expr {
            Expression::ArgRead(up_idx, idx) | Expression::ArgWrite(up_idx, idx, _) => (*up_idx, *idx),
            _ => unreachable!("adapt_arg_access_from_inlining called without an argument-related expression"),
        };

        // new up idx is the same logic as for variables.
        let new_up_idx = match up_idx {
            0 => 0,
            _ => (up_idx - self.scopes.iter().rev().take(up_idx).filter(|e| e.is_getting_inlined).count()) as u8,
        };

        // then in 99% of cases, the arg index is the exact same as in the original expression: if the argread/write isn't of an arg of a scope that's getting inlined, it's easy...
        let are_args_getting_inlined = !self.scopes.iter().nth_back(up_idx).unwrap().is_getting_inlined;
        match are_args_getting_inlined {
            true => match input_expr {
                Expression::ArgRead(..) => AstExpression::ArgRead(new_up_idx, idx as u8),
                Expression::ArgWrite(_, _, arg_write_expr) => {
                    let expr_with_inlining = self.parse_expression_with_inlining(arg_write_expr);
                    AstExpression::ArgWrite(
                        new_up_idx,
                        idx as u8,
                        self.gc_interface.alloc(expr_with_inlining, AllocSiteMarker::AstExpression),
                    )
                }
                _ => unreachable!(),
            },
            false => {
                // ...but if we DO inline a scope that has arguments, they become local variables!
                let nbr_scopes_between_up_idx_and_inline_target = self.scopes.iter().rev().skip(up_idx).take_while(|c| c.is_getting_inlined).count();

                let nbr_locals_in_target_scope = self
                    .scopes
                    .iter()
                    .rev()
                    .skip(up_idx + 1)
                    .take(nbr_scopes_between_up_idx_and_inline_target)
                    .map(AstScopeCtxt::get_nbr_locals)
                    .sum::<usize>();

                let nbr_args_inlined_in_target_scope = self
                    .scopes
                    .iter()
                    .rev()
                    .skip(up_idx + 1)
                    .take(nbr_scopes_between_up_idx_and_inline_target - 1)
                    .map(AstScopeCtxt::get_nbr_args)
                    .sum::<usize>();

                let arg_idx_as_local = nbr_locals_in_target_scope + nbr_args_inlined_in_target_scope + (idx - 1);
                // minus one because blockself has been removed. An ArgRead(1) is reading the first non blockself arg, and inlining wipes the block thus its blockself argument.

                let var_type = match input_expr {
                    Expression::ArgRead(..) => VarType::Read,
                    Expression::ArgWrite(_, _, expr) => VarType::Write(expr),
                    _ => unreachable!(),
                };

                self.var_from_coords(new_up_idx, arg_idx_as_local as u8, var_type)
            }
        }
    }

    fn inline_if_true_or_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        let if_inlined_node = IfInlinedNode {
            expected_bool,
            cond_expr: self.parse_expression_with_inlining(&msg.receiver),
            body_instrs: self.inline_block(body_blk),
        };

        Some(InlinedNode::IfInlined(if_inlined_node))
    }

    fn inline_if_true_if_false(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        // With a special case for the Fibonacci benchmark.
        // This code could easily be made more generalized/modular, have some blocks/expressions be considered "inlinable", but this special-casing is less dev time...
        let (body_blk_1, body_blk_2) = match (msg.values.first(), msg.values.get(1)) {
            (Some(Expression::Block(blk)), Some(Expression::Block(blk2))) => (blk, blk2),
            (Some(Expression::Literal(Literal::Integer(1))), Some(Expression::Block(blk))) => (
                &Block {
                    nbr_params: 0,
                    nbr_locals: 0,
                    body: som_core::ast::Body {
                        exprs: vec![Expression::Literal(Literal::Integer(1))],
                        full_stopped: false,
                    },
                    #[cfg(feature = "debug-info")]
                    dbg_info: som_core::ast::BlockDebugInfo {},
                },
                blk,
            ),
            _ => return None,
        };

        let if_true_if_false_inlined_node = IfTrueIfFalseInlinedNode {
            expected_bool,
            cond_expr: self.parse_expression_with_inlining(&msg.receiver),
            body_1_instrs: self.inline_block(body_blk_1),
            body_2_instrs: self.inline_block(body_blk_2),
        };

        Some(InlinedNode::IfTrueIfFalseInlined(if_true_if_false_inlined_node))
    }

    fn inline_while(&mut self, msg: &ast::Message, expected_bool: bool) -> Option<InlinedNode> {
        let (cond_blk, body_blk) = match (&msg.receiver, msg.values.first()) {
            (Expression::Block(cond_blk), Some(Expression::Block(body_blk))) => (cond_blk, body_blk),
            _ => return None,
        };

        let while_inlined_node = WhileInlinedNode {
            expected_bool,
            cond_instrs: self.inline_block(cond_blk),
            body_instrs: self.inline_block(body_blk),
        };

        Some(InlinedNode::WhileInlined(while_inlined_node))
    }

    fn inline_or(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        let snd_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        let or_inlined_node = OrInlinedNode {
            first: self.parse_expression_with_inlining(&msg.receiver),
            second: self.inline_block(snd_blk),
        };

        Some(InlinedNode::OrInlined(or_inlined_node))
    }

    fn inline_and(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        let snd_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        let and_inlined_node = AndInlinedNode {
            first: self.parse_expression_with_inlining(&msg.receiver),
            second: self.inline_block(snd_blk),
        };

        Some(InlinedNode::AndInlined(and_inlined_node))
    }

    fn inline_to_do(&mut self, msg: &ast::Message) -> Option<InlinedNode> {
        let (start_expr, end_expr, body_blk) = match (&msg.receiver, msg.values.first(), msg.values.get(1)) {
            (Expression::Block(_), _, _) | (_, Some(Expression::Block(_)), _) => {
                todo!("to:do: inlining: those cases should be handled (may be trivial)")
            }
            (a, Some(b), Some(Expression::Block(blk))) => (a, b, blk),
            _ => return None,
        };

        // eprintln!("Inlining to:do: in class {:?}", &self.class.unwrap().name);

        let accumulator_arg_idx = self.get_nbr_vars_in_scope(0); // and it's the first and only argument in a to:do: block, so no additional offset.

        let to_do_inlined_node = ToDoInlinedNode {
            start: self.parse_expression_with_inlining(start_expr),
            end: self.parse_expression_with_inlining(end_expr),
            body: self.inline_block(body_blk),
            accumulator_idx: accumulator_arg_idx,
        };

        // to be honest, should be handled by the code above somewhere, and not sure why that's necessary. still, good enough quickfix.
        // though TODO investigate root cause, it's probably simple
        self.scopes.last_mut()?.add_nbr_locals(1);

        // dbg!(&to_do_inlined_node); std::process::exit(1);

        Some(InlinedNode::ToDoInlined(to_do_inlined_node))
    }

    fn inline_if_nil_or_if_not_nil(&mut self, msg: &ast::Message, expects_nil: bool) -> Option<InlinedNode> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        let if_nil_inlined_node = IfNilInlinedNode {
            expects_nil,
            cond_expr: self.parse_expression_with_inlining(&msg.receiver),
            body_instrs: self.inline_block(body_blk),
        };

        Some(InlinedNode::IfNilInlined(if_nil_inlined_node))
    }

    fn inline_if_nil_if_not_nil(&mut self, msg: &ast::Message, expects_nil: bool) -> Option<InlinedNode> {
        let (body_blk_1, body_blk_2) = match (msg.values.first(), msg.values.get(1)) {
            (Some(Expression::Block(blk)), Some(Expression::Block(blk2))) => (blk, blk2),
            _ => return None,
        };

        let if_nil_if_not_nil_inlined_node = IfNilIfNotNilInlinedNode {
            expects_nil,
            cond_expr: self.parse_expression_with_inlining(&msg.receiver),
            body_1_instrs: self.inline_block(body_blk_1),
            body_2_instrs: self.inline_block(body_blk_2),
        };

        Some(InlinedNode::IfNilIfNotNilInlined(if_nil_if_not_nil_inlined_node))
    }
}

impl AstMethodCompilerCtxt<'_> {
    /// Helper function: generates a local variable expression given coordinates. We get duplicated logic otherwise.
    fn var_from_coords(&mut self, up_idx: u8, idx: u8, var_type: VarType) -> AstExpression {
        match (up_idx, var_type) {
            (0, VarType::Read) => AstExpression::LocalVarRead(idx),
            (0, VarType::Write(expr)) => {
                let expr_with_inlining = self.parse_expression_with_inlining(expr);
                let local_write_expr = AstExpression::LocalVarWrite(idx, self.gc_interface.alloc(expr_with_inlining, AllocSiteMarker::AstExpression));
                match self.maybe_make_inc_or_dec(&local_write_expr) {
                    Some(inc_or_dec) => inc_or_dec,
                    None => local_write_expr,
                }
            }
            (_, VarType::Read) => AstExpression::NonLocalVarRead(up_idx, idx),
            (_, VarType::Write(expr)) => {
                let expr_with_inlining = self.parse_expression_with_inlining(expr);
                AstExpression::NonLocalVarWrite(up_idx, idx, self.gc_interface.alloc(expr_with_inlining, AllocSiteMarker::AstExpression))
            }
        }
    }

    /// Returns the number of arguments in a given scope, accounting for inlining.
    /// TODO: this should also be used in the arg up_idx/idx resolver for inlining. And likely even in the local var up_idx/idx resolver, which is very similar also.
    /// It was designed for this purpose, hence the unused _access_from_up_idx argument: and the logic in `adapt_arg_access_from_inlining()` is highly similar.
    /// The reason why it's not actually unified is simple: A) it works as is, and B) it would take me a bit more time to work out the logic. And I never have time to do everything I need as is...
    /// So moving on to more pressing stuff. For future me though: I *think* the arg might need to be made into an isize and be "-1" to say "I want to access the previous scope", maybe.
    fn get_nbr_vars_in_scope(&self, _access_from_up_idx: usize) -> usize {
        let up_idx_scope_arg_inlined_into = self.scopes.iter().rev().take_while(|c| c.is_getting_inlined).count();

        let nbr_locals_in_target_scope =
            self.scopes.iter().rev().take(up_idx_scope_arg_inlined_into + 1).map(AstScopeCtxt::get_nbr_locals).sum::<usize>();

        let nbr_args_inlined_in_target_scope =
            self.scopes.iter().rev().take(up_idx_scope_arg_inlined_into).map(AstScopeCtxt::get_nbr_args).sum::<usize>();

        nbr_locals_in_target_scope + nbr_args_inlined_in_target_scope
    }
}
