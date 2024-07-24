use std::rc::Rc;

use som_core::ast;
use som_core::ast::{Expression, MethodBody};

use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstMethodBody, AstMethodDef, AstSuperMessage};
use crate::inliner::PrimMessageInliner;

pub struct AstMethodCompilerCtxt {
    pub scopes: Vec<AstScopeCtxt>,
}

impl AstMethodCompilerCtxt {
    pub(crate) fn adapt_var_coords_from_inlining(&self, up_idx: usize, idx: usize) -> (usize, usize) {
        let nbr_inlined_scopes_in_between = self.scopes.iter().rev().take(up_idx).filter(|e| e.is_getting_inlined).count();
        
        // new up index is the target var scope minus the number of inlined scopes in between the current scope and the target var scope
        // if you do a NonLocalVarRead(3, 0), and there's 1 inlined scope before that (3) target, then now that target scope is only (2) scopes away.
        let new_up_idx = match up_idx {
            0 => 0,
            _ => up_idx - nbr_inlined_scopes_in_between
        };
        
        // new index is more complicated, since some variables can have gotten inlined into other scopes.
        let new_idx = {
            let lol = self.get_nbr_locals_in_scope_post_inlining(up_idx); // todo incorrect. see below
            idx + (lol - 1)
        };
        
        // _
        // | |a b| 
        // |       V SCOPE GETS INLINED (prev scope vars become |a b c| )
        // |       _
        // |       | |c|
        // |       |     _
        // |       |    | |d|
        // |       |    |  VarRead(0, 0)... becomes: => VarRead(0, 0)
        // |       |    |  VarRead(1, 0) => VarRead(1, 2)
        // |       |    |  VarRead(2, 0) => VarRead(1, 1)
        // |       |    |  VarRead(2, 1) => VarRead(1, 0)
        // |       |    _
        // |       _
        // _
        
        (new_up_idx, new_idx)
    }
    
    pub(crate) fn adapt_arg_coords_from_inlining(&self, up_idx: usize, idx: usize) -> (usize, usize) {
        todo!("same code as adapt var coords, but calling get_nbr_args().")
    }

    // todo: not quuuite true to its name.
    pub(crate) fn get_nbr_locals_in_scope_post_inlining(&self, idx: usize) -> usize {
        let mut scope_iter = self.scopes.iter();
        let mut inline_scope_target = scope_iter.nth_back(idx);
        let mut nbr_locals_in_target_scope = inline_scope_target.unwrap().get_nbr_locals();

        while inline_scope_target.unwrap().is_getting_inlined {
            inline_scope_target = scope_iter.next_back();
            nbr_locals_in_target_scope += inline_scope_target.unwrap().get_nbr_locals();
        }

        nbr_locals_in_target_scope
    }

    // todo use
    // pub(crate) fn get_nbr_args_in_scope_post_inlining(&self, idx: usize) -> usize {
    //     let mut scope_iter = self.scopes.iter();
    //     let mut inline_scope_target = scope_iter.nth_back(idx);
    //     let mut nbr_locals_in_target_scope = inline_scope_target.unwrap().get_nbr_args();
    // 
    //     while inline_scope_target.unwrap().is_getting_inlined {
    //         inline_scope_target = scope_iter.next_back();
    //         nbr_locals_in_target_scope += inline_scope_target.unwrap().get_nbr_args();
    //     }
    // 
    //     nbr_locals_in_target_scope
    // }
}

#[derive(Debug, Default)]
pub struct AstScopeCtxt {
    nbr_args: usize,
    nbr_locals: usize,
    pub is_getting_inlined: bool,
}

impl AstScopeCtxt {
    pub fn init(nbr_args: usize,
                nbr_locals: usize,
                is_getting_inlined: bool) -> Self {
        Self {
            nbr_args,
            nbr_locals,
            is_getting_inlined,
        }
    }

    pub fn get_nbr_locals(&self) -> usize {
        self.nbr_locals
    }

    pub fn add_nbr_locals(&mut self, nbr_to_add: usize) {
        self.nbr_locals += nbr_to_add;
    }
    pub fn get_nbr_args(&self) -> usize {
        self.nbr_args
    }

    pub fn add_nbr_args(&mut self, nbr_to_add: usize) {
        self.nbr_args += nbr_to_add;
    }
}

impl AstMethodCompilerCtxt {
    pub fn parse_method_def(method_def: &ast::MethodDef) -> AstMethodDef {
        AstMethodDef {
            signature: method_def.signature.clone(),
            body: {
                match &method_def.body {
                    MethodBody::Primitive => { AstMethodBody::Primitive }
                    MethodBody::Body { locals_nbr, body, .. } => {
                        let args_nbr = method_def.signature.chars().filter(|e| *e == ':').count(); // not sure if needed
                        let mut ctxt = AstMethodCompilerCtxt { scopes: vec![AstScopeCtxt::init(args_nbr, *locals_nbr, false)] };

                        AstMethodBody::Body {
                            body: ctxt.parse_body(body),
                            locals_nbr: ctxt.scopes.last().unwrap().get_nbr_locals(),
                        }
                    }
                }
            },
        }
    }

    pub fn parse_expression(&mut self, expr: &Expression) -> AstExpression {
        match expr.clone() {
            Expression::GlobalRead(global_name) => AstExpression::GlobalRead(global_name.clone()),
            Expression::LocalVarRead(idx) => AstExpression::LocalVarRead(idx),
            Expression::NonLocalVarRead(scope, idx) => AstExpression::NonLocalVarRead(scope, idx),
            Expression::ArgRead(scope, idx) => AstExpression::ArgRead(scope, idx),
            Expression::FieldRead(idx) => AstExpression::FieldRead(idx),
            Expression::LocalVarWrite(a, b) => AstExpression::LocalVarWrite(a, Box::new(self.parse_expression(b.as_ref()))),
            Expression::NonLocalVarWrite(a, b, c) => AstExpression::NonLocalVarWrite(a, b, Box::new(self.parse_expression(c.as_ref()))),
            Expression::ArgWrite(a, b, c) => AstExpression::ArgWrite(a, b, Box::new(self.parse_expression(c.as_ref()))),
            Expression::FieldWrite(a, b) => AstExpression::FieldWrite(a, Box::new(self.parse_expression(b.as_ref()))),
            Expression::Message(msg) => self.parse_message_maybe_inline(msg.as_ref()),
            Expression::SuperMessage(a) => AstExpression::SuperMessage(Box::new(self.parse_super_message(a.as_ref()))),
            Expression::BinaryOp(a) => AstExpression::BinaryOp(Box::new(self.parse_binary_op(a.as_ref()))),
            Expression::Exit(a, b) => AstExpression::Exit(Box::new(self.parse_expression(a.as_ref())), b),
            Expression::Literal(a) => AstExpression::Literal(a),
            Expression::Block(a) => AstExpression::Block(Rc::new(self.parse_block(&a)))
        }
    }

    pub fn parse_body(&mut self, body: &ast::Body) -> AstBody {
        AstBody {
            exprs: body.exprs.iter().map(|expr| self.parse_expression(expr)).collect()
        }
    }

    pub fn parse_block(&mut self, blk: &ast::Block) -> AstBlock {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, false));

        let body = self.parse_body(&blk.body);
        let bl = self.scopes.last().unwrap();
        let output_blk = AstBlock {
            nbr_params: bl.get_nbr_args(),
            nbr_locals: bl.get_nbr_locals(),
            body,
        };

        self.scopes.pop();
        output_blk
    }

    pub fn parse_binary_op(&mut self, binary_op: &ast::BinaryOp) -> AstBinaryOp {
        AstBinaryOp {
            op: binary_op.op.clone(),
            lhs: self.parse_expression(&binary_op.lhs),
            rhs: self.parse_expression(&binary_op.rhs),
        }
    }

    pub fn parse_message_maybe_inline(&mut self, msg: &ast::Message) -> AstExpression {
        let maybe_inlined = self.inline_if_possible(msg);
        if let Some(inlined_node) = maybe_inlined {
            return AstExpression::InlinedCall(Box::new(inlined_node));
        }

        AstExpression::Message(Box::new(
            AstMessage {
                receiver: self.parse_expression(&msg.receiver),
                signature: msg.signature.clone(),
                values: msg.values.iter().map(|e| self.parse_expression(e)).collect(),
            })
        )
    }

    pub fn parse_super_message(&mut self, super_msg: &ast::SuperMessage) -> AstSuperMessage {
        AstSuperMessage {
            receiver_name: super_msg.receiver_name.clone(),
            is_static_class_call: super_msg.is_static_class_call,
            signature: super_msg.signature.clone(),
            values: super_msg.values.iter().map(|e| self.parse_expression(e)).collect(),
        }
    }
}