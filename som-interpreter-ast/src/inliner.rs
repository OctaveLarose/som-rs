use som_core::ast;
use crate::ast::AstBody;
use crate::specialized::if_inlined_node::IfInlinedNode;

pub trait PrimMessageInliner {
    fn inline_if_possible(&self, ctxt: usize) -> Option<()>;

    fn inline_if_true_or_if_false(
        &self,
        ctxt: usize, // todo we need some sort of AST parser gen context, now
        jump_type: bool,
    ) -> Option<()>;
}

impl PrimMessageInliner for ast::Message {
     fn inline_if_possible(&self, ctxt: usize) -> Option<()> {
        match self.signature.as_str() {
            "ifTrue:" => self.inline_if_true_or_if_false(ctxt, true),
            "ifFalse:" => self.inline_if_true_or_if_false(ctxt, false),
            _ => None,
        }
    }
     fn inline_if_true_or_if_false(&self, ctxt: usize, expected_bool: bool) -> Option<()> {
         let if_inlined_node = IfInlinedNode {
             expected_bool,
             cond: AstBody { exprs: todo!() },
             body: AstBody { exprs: todo!() },
         };
         Some(())
    }
    
}