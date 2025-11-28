use crate::AstGenCtxt;
use som_core::ast::Expression;
use som_core::ast::{self, Message};

#[allow(unused)] // if inlining is disabled, a lot of them go completely unused.
pub(crate) trait PrimMessageInliner {
    fn inline_if_possible(&mut self, msg: &ast::RegularMessage) -> Option<Message>;
    fn inline_if_true_or_if_false(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message>;
    fn inline_if_true_if_false(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message>;
    fn inline_if_nil_or_if_not_nil(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message>;
    fn inline_if_nil_if_not_nil(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message>;
    fn inline_while(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message>;
    fn inline_or(&mut self, msg: &ast::RegularMessage) -> Option<Message>;
    fn inline_and(&mut self, msg: &ast::RegularMessage) -> Option<Message>;
}

impl PrimMessageInliner for AstGenCtxt<'_> {
    fn inline_if_possible(&mut self, msg: &ast::RegularMessage) -> Option<Message> {
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
            //"to:do:" => self.inline_to_do(msg),
            _ => None,
        }
    }

    fn inline_if_true_or_if_false(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        if body_blk.nbr_locals != 0 || body_blk.nbr_params != 0 {
            // TODO: not handled yet
            return None;
        }

        //dbg!(&msg);

        let if_inlined_msg = ast::IfInlinedMessage {
            expected_bool,
            cond_expr: msg.receiver.clone(), // TODO: change structure to avoid clone (we should consume the message itself)
            body_instrs: body_blk.body.exprs.clone(),
        };

        Some(Message::IfInlined(if_inlined_msg))
    }

    fn inline_if_nil_or_if_not_nil(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        if body_blk.nbr_locals != 0 || body_blk.nbr_params != 0 {
            // TODO: not handled yet
            return None;
        }

        let if_inlined_msg = ast::IfNilInlinedMessage {
            expects_nil: expected_bool,
            cond_expr: msg.receiver.clone(),
            body_instrs: body_blk.body.exprs.clone(),
        };

        Some(Message::IfNilInlined(if_inlined_msg))
    }

    fn inline_if_true_if_false(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message> {
        // With a special case for the Fibonacci benchmark.
        // This code could easily be made more generalized/modular, have some blocks/expressions be considered "inlinable", but this special-casing is less dev time... TODO, generalize a bit.
        let (body_blk_1, body_blk_2) = match (msg.values.first(), msg.values.get(1)) {
            (Some(Expression::Block(blk)), Some(Expression::Block(blk2))) => (blk, blk2),
            (Some(Expression::Literal(ast::Literal::Integer(1))), Some(Expression::Block(blk))) => (
                &ast::Block {
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
            _ => return None,
        };

        if body_blk_1.nbr_locals != 0 || body_blk_1.nbr_params != 0 || body_blk_2.nbr_locals != 0 || body_blk_2.nbr_params != 0 {
            // TODO: not handled yet
            return None;
        }

        let if_true_if_false_inlined_node = ast::IfTrueIfFalseInlinedMessage {
            expected_bool,
            cond_expr: msg.receiver.clone(),
            body_1_instrs: body_blk_1.body.exprs.clone(),
            body_2_instrs: body_blk_2.body.exprs.clone(),
        };

        Some(Message::IfTrueIfFalseInlined(if_true_if_false_inlined_node))
    }

    fn inline_if_nil_if_not_nil(&mut self, msg: &ast::RegularMessage, expects_nil: bool) -> Option<Message> {
        let (body_blk_1, body_blk_2) = match (msg.values.first(), msg.values.get(1)) {
            (Some(Expression::Block(blk)), Some(Expression::Block(blk2))) => (blk, blk2),
            _ => return None,
        };

        if body_blk_1.nbr_locals != 0 || body_blk_1.nbr_params != 0 || body_blk_2.nbr_locals != 0 || body_blk_2.nbr_params != 0 {
            // TODO: not handled yet
            return None;
        }

        let if_true_if_false_inlined_node = ast::IfNilIfNotNilInlinedMessage {
            expects_nil,
            cond_expr: msg.receiver.clone(),
            body_1_instrs: body_blk_1.body.exprs.clone(),
            body_2_instrs: body_blk_2.body.exprs.clone(),
        };

        Some(Message::IfNilIfNotNilInlined(if_true_if_false_inlined_node))
    }

    fn inline_while(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message> {
        let (cond_blk, body_blk) = match (&msg.receiver, msg.values.first()) {
            (Expression::Block(cond_blk), Some(Expression::Block(body_blk))) => (cond_blk, body_blk),
            _ => return None,
        };

        if cond_blk.nbr_locals != 0 || cond_blk.nbr_params != 0 || body_blk.nbr_locals != 0 || body_blk.nbr_params != 0 {
            // TODO: not handled yet
            return None;
        }

        let while_inlined_node = ast::WhileInlinedMessage {
            expected_bool,
            cond_instrs: cond_blk.body.exprs.clone(),
            body_instrs: body_blk.body.exprs.clone(),
        };

        Some(Message::WhileInlined(while_inlined_node))
    }

    fn inline_or(&mut self, msg: &ast::RegularMessage) -> Option<Message> {
        let snd_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        if snd_blk.nbr_locals != 0 || snd_blk.nbr_params != 0 {
            // TODO: not handled yet
            return None;
        }

        let or_inlined_node = ast::OrInlinedMessage {
            first: msg.receiver.clone(),
            second: snd_blk.body.exprs.clone(),
        };

        Some(Message::OrInlined(or_inlined_node))
    }

    fn inline_and(&mut self, msg: &ast::RegularMessage) -> Option<Message> {
        let snd_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        if snd_blk.nbr_locals != 0 || snd_blk.nbr_params != 0 {
            // TODO: not handled yet
            return None;
        }
        
        let and_inlined_node = ast::AndInlinedMessage {
            first: msg.receiver.clone(),
            second: snd_blk.body.exprs.clone(),
        };

        Some(Message::AndInlined(and_inlined_node))
    }
}
