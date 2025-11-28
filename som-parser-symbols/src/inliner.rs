use crate::AstGenCtxt;
use som_core::ast::{self, Message};
use som_core::ast::{Expression, ToDoInlinedMsg};

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
    fn inline_to_do(&mut self, msg: &ast::RegularMessage) -> Option<Message>;
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
            "to:do:" => self.inline_to_do(msg),
            _ => None,
        }
    }

    fn inline_if_true_or_if_false(&mut self, msg: &ast::RegularMessage, expected_bool: bool) -> Option<Message> {
        let body_blk = match msg.values.first() {
            Some(Expression::Block(blk)) => blk,
            _ => return None,
        };

        // if body_blk.nbr_locals != 0 || body_blk.nbr_params != 0 {
        // dbg!(&self.borrow().local_names);
        // if body_blk.locals.first().unwrap() == "tmp" {
        //     dbg!("bp");
        // }
        self.borrow_mut().add_locals(&body_blk.locals);
        self.borrow_mut().add_locals(&body_blk.parameters);
        // dbg!(&self.borrow().local_names);
        // }

        //dbg!(&msg);

        let if_inlined_msg = ast::IfInlinedMsg {
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

        self.borrow_mut().add_locals(&body_blk.locals);
        self.borrow_mut().add_locals(&body_blk.parameters);

        let if_inlined_msg = ast::IfNilInlinedMsg {
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

        self.borrow_mut().add_locals(&body_blk_1.locals);
        self.borrow_mut().add_locals(&body_blk_1.parameters);
        self.borrow_mut().add_locals(&body_blk_2.locals);
        self.borrow_mut().add_locals(&body_blk_2.parameters);

        let if_true_if_false_inlined_node = ast::IfTrueIfFalseInlinedMsg {
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

        self.borrow_mut().add_locals(&body_blk_1.locals);
        self.borrow_mut().add_locals(&body_blk_1.parameters);
        self.borrow_mut().add_locals(&body_blk_2.locals);
        self.borrow_mut().add_locals(&body_blk_2.parameters);

        let if_true_if_false_inlined_node = ast::IfNilIfNotNilInlinedMsg {
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

        // if cond_blk.nbr_locals != 0 || cond_blk.nbr_params != 0 || body_blk.nbr_locals != 0 || body_blk.nbr_params != 0 {
        self.borrow_mut().add_locals(&cond_blk.locals);
        self.borrow_mut().add_locals(&cond_blk.parameters);
        self.borrow_mut().add_locals(&body_blk.locals);
        self.borrow_mut().add_locals(&body_blk.parameters);
        // }

        let while_inlined_node = ast::WhileInlinedMsg {
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

        self.borrow_mut().add_locals(&snd_blk.locals);
        self.borrow_mut().add_locals(&snd_blk.parameters);

        let or_inlined_node = ast::OrInlinedMsg {
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

        self.borrow_mut().add_locals(&snd_blk.locals);
        self.borrow_mut().add_locals(&snd_blk.parameters);

        let and_inlined_node = ast::AndInlinedMsg {
            first: msg.receiver.clone(),
            second: snd_blk.body.exprs.clone(),
        };

        Some(Message::AndInlined(and_inlined_node))
    }

    fn inline_to_do(&mut self, msg: &ast::RegularMessage) -> Option<Message> {
        let (start_expr, end_expr, body_blk) = match (&msg.receiver, msg.values.first(), msg.values.get(1)) {
            (Expression::Block(_), _, _) | (_, Some(Expression::Block(_)), _) => {
                todo!("to:do: inlining: those cases should be handled (may be trivial)")
            }
            (a, Some(b), Some(Expression::Block(blk))) => (a, b, blk),
            _ => return None,
        };

        let accumulator_name = body_blk.parameters.first()?.clone();

        self.borrow_mut().add_locals(&body_blk.locals);
        self.borrow_mut().add_locals(&body_blk.parameters);

        let to_do_inlined_node = ToDoInlinedMsg {
            start_expr: start_expr.clone(),
            end_expr: end_expr.clone(),
            body_instrs: body_blk.body.exprs.clone(),
            accumulator_name,
        };

        Some(Message::ToDoInlined(to_do_inlined_node))
    }
}
