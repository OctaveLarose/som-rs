use std::rc::Rc;

use som_core::ast;
use som_core::ast::{Expression, MethodBody};

use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstMethodBody, AstMethodDef, AstSuperMessage};
use crate::inliner::PrimMessageInliner;

#[derive(Debug, Default)]
pub struct AstMethodCompilerCtxt {
    nbr_args: usize,
    nbr_locals: usize,
    inlining_scope_adjust: usize
}

impl AstMethodCompilerCtxt {
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
    pub fn get_inlining_scope_adjust(&self) -> usize { self.inlining_scope_adjust } 
    pub fn incr_inlining_level(&mut self) { self.inlining_scope_adjust += 1; } 
    pub fn decr_inlining_level(&mut self) { self.inlining_scope_adjust -= 1; } 
}

impl AstMethodCompilerCtxt {
    pub fn init(nbr_args: usize, nbr_locals: usize, inlining_scope_adjust: usize) -> Self {
        Self {
            nbr_args,
            nbr_locals,
            inlining_scope_adjust
        }
    }
    
    pub fn parse_method_def(method_def: &ast::MethodDef) -> AstMethodDef {
        AstMethodDef {
            signature: method_def.signature.clone(),
            body: {
                match &method_def.body {
                    MethodBody::Primitive => { AstMethodBody::Primitive }
                    MethodBody::Body { locals_nbr, body, .. } => {
                        let args_nbr = method_def.signature.chars().filter(|e| *e == ':').count(); // not sure if needed
                        let mut compiler = AstMethodCompilerCtxt::init(args_nbr, *locals_nbr, 1);
                        
                        AstMethodBody::Body {
                            body: compiler.parse_body(body),
                            locals_nbr: compiler.get_nbr_locals(),
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
        let mut block_compiler = Self::init(blk.nbr_params, blk.nbr_locals, 1);
        let body = block_compiler.parse_body(&blk.body);
        AstBlock {
            nbr_params: block_compiler.get_nbr_args(),
            nbr_locals: block_compiler.get_nbr_locals(),
            body,
        }
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