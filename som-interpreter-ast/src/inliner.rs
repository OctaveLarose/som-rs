use som_core::ast;
use som_core::ast::Expression;

use crate::ast::{AstBody, InlinedNode};
use crate::specialized::if_inlined_node::IfInlinedNode;

pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: usize) -> Option<InlinedNode>;

    fn inline_expression(&self, ctxt: usize, expression: &Expression) -> Option<()>;

    fn inline_if_true_or_if_false(
        &self,
        ctxt: usize, // todo we need some sort of AST parser gen context, now
        jump_type: bool,
    ) -> Option<InlinedNode>;
}

impl PrimMessageInliner for ast::Message {
    fn inline_if_possible(&self, ctxt: usize) -> Option<InlinedNode> {
        match self.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(ctxt, true),
            "ifFalse:" => self.inline_if_true_or_if_false(ctxt, false),
            _ => None,
        }
    }

    #[allow(dead_code, unreachable_code, unused_variables)]
    fn inline_expression(&self, ctxt: usize, expression: &Expression) -> Option<()> {
        todo!()
    }

    #[allow(dead_code, unreachable_code, unused_variables)]
    fn inline_if_true_or_if_false(&self, ctxt: usize, expected_bool: bool) -> Option<InlinedNode> {
        dbg!(&self.receiver);
        dbg!(&self.values);

        let cond_blk = self.inline_expression(ctxt, &self.receiver);
        let body_blk = self.inline_expression(ctxt, self.values.get(0)?);

        let if_inlined_node = IfInlinedNode {
            expected_bool,
            cond: AstBody { exprs: todo!() },
            body: AstBody { exprs: todo!() },
        };

        Some(InlinedNode::IfInlined(if_inlined_node))
    }
}