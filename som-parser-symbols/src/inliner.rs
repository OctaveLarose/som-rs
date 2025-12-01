use crate::AstGenCtxt;
use som_core::ast::{self, Message};
use som_core::ast::{Expression, ToDoInlinedMsg};

#[allow(unused)] // if inlining is disabled, a lot of them go completely unused.
pub(crate) trait PrimMessageInliner {
    fn inline_if_possible(&mut self, msg: ast::RegularMessage) -> Message;
    fn inline_block_context(&mut self, blk: &mut ast::Block) -> bool;
    fn try_inline_if_true_or_if_false(&mut self, msg: ast::RegularMessage, expected_bool: bool) -> Message;
    fn try_inline_if_true_if_false(&mut self, msg: ast::RegularMessage, expected_bool: bool) -> Message;
    fn try_inline_if_nil_or_if_not_nil(&mut self, msg: ast::RegularMessage, expected_bool: bool) -> Message;
    fn try_inline_if_nil_if_not_nil(&mut self, msg: ast::RegularMessage, expected_bool: bool) -> Message;
    fn try_inline_while(&mut self, msg: ast::RegularMessage, expected_bool: bool) -> Message;
    fn try_inline_or(&mut self, msg: ast::RegularMessage) -> Message;
    fn try_inline_and(&mut self, msg: ast::RegularMessage) -> Message;
    fn try_inline_to_do(&mut self, msg: ast::RegularMessage) -> Message;
}

impl PrimMessageInliner for AstGenCtxt<'_> {
    fn inline_if_possible(&mut self, msg: ast::RegularMessage) -> Message {
        match msg.signature.as_str() {
            "ifTrue:" => self.try_inline_if_true_or_if_false(msg, true),
            "ifFalse:" => self.try_inline_if_true_or_if_false(msg, false),
            "ifTrue:ifFalse:" => self.try_inline_if_true_if_false(msg, true),
            "ifFalse:ifTrue:" => self.try_inline_if_true_if_false(msg, false),
            "ifNil:" => self.try_inline_if_nil_or_if_not_nil(msg, true),
            "ifNotNil:" => self.try_inline_if_nil_or_if_not_nil(msg, false),
            "ifNil:ifNotNil:" => self.try_inline_if_nil_if_not_nil(msg, true),
            "ifNotNil:ifNil:" => self.try_inline_if_nil_if_not_nil(msg, false),
            "whileTrue:" => self.try_inline_while(msg, true),
            "whileFalse:" => self.try_inline_while(msg, false),
            "or:" | "||" => self.try_inline_or(msg),
            "and:" | "&&" => self.try_inline_and(msg),
            "to:do:" => self.try_inline_to_do(msg),
            _ => Message::Regular(msg),
        }
    }

    // HACK: should not return bool! We should just handle shadowing, and never fail.
    fn inline_block_context(&mut self, blk: &mut ast::Block) -> bool {
        for blk_local in &blk.locals {
            if self.borrow().has_local(blk_local) {
                return false;
            }
        }

        for blk_arg in &blk.parameters {
            if self.borrow().has_local(blk_arg) {
                return false;
            }
        }

        self.borrow_mut().add_locals(&blk.locals);
        self.borrow_mut().add_locals(&blk.parameters);

        true
    }

    fn try_inline_if_true_or_if_false(&mut self, mut msg: ast::RegularMessage, expected_bool: bool) -> Message {
        let body_blk = match msg.values.first_mut() {
            Some(Expression::Block(blk)) => blk,
            _ => return Message::Regular(msg),
        };

        if !self.inline_block_context(body_blk) {
            return Message::Regular(msg);
        }

        let if_inlined_msg = ast::IfInlinedMsg {
            expected_bool,
            cond_expr: msg.receiver,
            body_instrs: std::mem::take(&mut body_blk.body.exprs),
        };

        Message::IfInlined(if_inlined_msg)
    }

    fn try_inline_if_nil_or_if_not_nil(&mut self, mut msg: ast::RegularMessage, expected_bool: bool) -> Message {
        let body_blk = match msg.values.first_mut() {
            Some(Expression::Block(blk)) => blk,
            _ => return Message::Regular(msg),
        };

        if !self.inline_block_context(body_blk) {
            return Message::Regular(msg);
        }

        let if_inlined_msg = ast::IfNilInlinedMsg {
            expects_nil: expected_bool,
            cond_expr: msg.receiver,
            body_instrs: std::mem::take(&mut body_blk.body.exprs),
        };

        Message::IfNilInlined(if_inlined_msg)
    }

    fn try_inline_if_true_if_false(&mut self, mut msg: ast::RegularMessage, expected_bool: bool) -> Message {
        // With a special case for the Fibonacci benchmark.
        // This code could easily be made more generalized/modular, have some blocks/expressions be considered "inlinable", but this special-casing is less dev time... TODO, generalize a bit.
        let (body_blk_1, body_blk_2) = {
            let (left, right) = msg.values.split_at_mut(1); // NOTE: I wanna use `get_disjoint_mut` instead but that's only for more recent Rust versions.

            match (&mut left[0], &mut right[0]) {
                (Expression::Block(blk), Expression::Block(blk2)) => (blk, blk2),
                (Expression::Literal(ast::Literal::Integer(1)), Expression::Block(blk)) => (
                    &mut ast::Block {
                        parameters: vec![],
                        locals: vec![],
                        body: som_core::ast::Body {
                            exprs: vec![Expression::Literal(ast::Literal::Integer(1))],
                            full_stopped: false,
                        },
                        nbr_params: 0,
                        nbr_locals: 0,
                    },
                    blk,
                ),

                _ => return Message::Regular(msg),
            }
        };

        for blk_local in &body_blk_2.locals {
            if self.borrow().has_local(blk_local) {
                return Message::Regular(msg);
            }
        }
        for blk_arg in &body_blk_2.parameters {
            if self.borrow().has_local(blk_arg) {
                return Message::Regular(msg);
            }
        }

        if !self.inline_block_context(body_blk_1) {
            return Message::Regular(msg);
        }

        if !self.inline_block_context(body_blk_2) {
            return Message::Regular(msg);
        }

        let if_true_if_false_inlined_node = ast::IfTrueIfFalseInlinedMsg {
            expected_bool,
            cond_expr: msg.receiver,
            body_1_instrs: std::mem::take(&mut body_blk_1.body.exprs),
            body_2_instrs: std::mem::take(&mut body_blk_2.body.exprs),
        };

        Message::IfTrueIfFalseInlined(if_true_if_false_inlined_node)
    }

    fn try_inline_if_nil_if_not_nil(&mut self, mut msg: ast::RegularMessage, expects_nil: bool) -> Message {
        let (body_blk_1, body_blk_2) = {
            let (left, right) = msg.values.split_at_mut(1);

            match (&mut left[0], &mut right[0]) {
                (Expression::Block(blk), Expression::Block(blk2)) => (blk, blk2),
                _ => return Message::Regular(msg),
            }
        };

        for blk_local in &body_blk_2.locals {
            if self.borrow().has_local(blk_local) {
                return Message::Regular(msg);
            }
        }
        for blk_arg in &body_blk_2.parameters {
            if self.borrow().has_local(blk_arg) {
                return Message::Regular(msg);
            }
        }

        if !self.inline_block_context(body_blk_1) {
            return Message::Regular(msg);
        }

        if !self.inline_block_context(body_blk_2) {
            return Message::Regular(msg);
        }

        let if_true_if_false_inlined_node = ast::IfNilIfNotNilInlinedMsg {
            expects_nil,
            cond_expr: msg.receiver,
            body_1_instrs: std::mem::take(&mut body_blk_1.body.exprs),
            body_2_instrs: std::mem::take(&mut body_blk_2.body.exprs),
        };

        Message::IfNilIfNotNilInlined(if_true_if_false_inlined_node)
    }

    fn try_inline_while(&mut self, mut msg: ast::RegularMessage, expected_bool: bool) -> Message {
        let (cond_blk, body_blk) = match (&mut msg.receiver, msg.values.first_mut()) {
            (Expression::Block(cond_blk), Some(Expression::Block(body_blk))) => (cond_blk, body_blk),
            _ => return Message::Regular(msg),
        };

        for blk_local in &body_blk.locals {
            if self.borrow().has_local(blk_local) {
                return Message::Regular(msg);
            }
        }
        for blk_arg in &body_blk.parameters {
            if self.borrow().has_local(blk_arg) {
                return Message::Regular(msg);
            }
        }

        if !self.inline_block_context(cond_blk) {
            return Message::Regular(msg);
        }

        if !self.inline_block_context(body_blk) {
            return Message::Regular(msg);
        }

        let while_inlined_node = ast::WhileInlinedMsg {
            expected_bool,
            cond_instrs: std::mem::take(&mut cond_blk.body.exprs),
            body_instrs: std::mem::take(&mut body_blk.body.exprs),
        };

        Message::WhileInlined(while_inlined_node)
    }

    fn try_inline_or(&mut self, mut msg: ast::RegularMessage) -> Message {
        let snd_blk = match msg.values.first_mut() {
            Some(Expression::Block(blk)) => blk,
            _ => return Message::Regular(msg),
        };

        if !self.inline_block_context(snd_blk) {
            return Message::Regular(msg);
        }

        let or_inlined_node = ast::OrInlinedMsg {
            first: msg.receiver,
            second: std::mem::take(&mut snd_blk.body.exprs),
        };

        Message::OrInlined(or_inlined_node)
    }

    fn try_inline_and(&mut self, mut msg: ast::RegularMessage) -> Message {
        let snd_blk = match msg.values.first_mut() {
            Some(Expression::Block(blk)) => blk,
            _ => return Message::Regular(msg),
        };

        if !self.inline_block_context(snd_blk) {
            return Message::Regular(msg);
        }

        let and_inlined_node = ast::AndInlinedMsg {
            first: msg.receiver,
            second: std::mem::take(&mut snd_blk.body.exprs),
        };

        Message::AndInlined(and_inlined_node)
    }

    fn try_inline_to_do(&mut self, mut msg: ast::RegularMessage) -> Message {
        let (start_expr, end_expr, body_blk) = {
            let (left, right) = msg.values.split_at_mut(1);

            match (&mut msg.receiver, (&mut left[0], &mut right[0])) {
                (Expression::Block(_), _) | (_, (Expression::Block(_), _)) => {
                    todo!("to:do: inlining: those cases should be handled (may be trivial)")
                }
                (a, (b, Expression::Block(blk))) => (a, b, blk),
                _ => return Message::Regular(msg),
            }
        };

        let accumulator_name = body_blk.parameters.first().unwrap_or_else(|| panic!("inlining to:do:, but found no accumulator argument?")).clone();

        if !self.inline_block_context(body_blk) {
            return Message::Regular(msg);
        }

        let to_do_inlined_node = ToDoInlinedMsg {
            start_expr: start_expr.clone(),
            end_expr: end_expr.clone(),
            body_instrs: std::mem::take(&mut body_blk.body.exprs),
            accumulator_name,
        };

        Message::ToDoInlined(to_do_inlined_node)
    }
}
