use std::rc::Rc;
use som_core::ast;
use som_core::ast::{Expression, MethodBody};
use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstMethodBody, AstMethodDef, AstSuperMessage};
use crate::inliner::PrimMessageInliner;

#[derive(Debug, Default)]
pub struct AstMethodCompiler {}

impl AstMethodCompiler {
    pub fn parse_method_def(method_def: &ast::MethodDef) -> AstMethodDef {
        let compiler = AstMethodCompiler::default();
        AstMethodDef {
            signature: method_def.signature.clone(),
            body: {
                match &method_def.body {
                    MethodBody::Primitive => {AstMethodBody::Primitive}
                    MethodBody::Body { locals_nbr, body, ..} => {
                        AstMethodBody::Body { locals_nbr: *locals_nbr, body: compiler.parse_body(body) }
                    }
                }
            },
        }
    }

    pub fn parse_expression(&self, expr: &Expression) -> AstExpression {
        match expr.clone() {
            Expression::GlobalRead(a) => AstExpression::GlobalRead(a),
            Expression::LocalVarRead(a) => AstExpression::LocalVarRead(a),
            Expression::NonLocalVarRead(a, b) => AstExpression::NonLocalVarRead(a, b),
            Expression::ArgRead(a, b) => AstExpression::ArgRead(a, b),
            Expression::FieldRead(a) => AstExpression::FieldRead(a),
            Expression::LocalVarWrite(a, b) => AstExpression::LocalVarWrite(a, Box::new(self.parse_expression(b.as_ref()))),
            Expression::NonLocalVarWrite(a, b, c) => AstExpression::NonLocalVarWrite(a, b, Box::new(self.parse_expression(c.as_ref()))),
            Expression::ArgWrite(a, b, c) => AstExpression::ArgWrite(a, b, Box::new(self.parse_expression(c.as_ref()))),
            Expression::FieldWrite(a, b) => AstExpression::FieldWrite(a, Box::new(self.parse_expression(b.as_ref()))),
            Expression::Message(msg) => {
                let maybe_inlined = msg.inline_if_possible(42);
                match maybe_inlined {
                    None => AstExpression::Message(Box::new(self.parse_message(msg.as_ref()))),
                    Some(v) => AstExpression::InlinedCall(Box::new(v))
                }
            }
            Expression::SuperMessage(a) => AstExpression::SuperMessage(Box::new(self.parse_super_message(a.as_ref()))),
            Expression::BinaryOp(a) => AstExpression::BinaryOp(Box::new(self.parse_binary_op(a.as_ref()))),
            Expression::Exit(a, b) => AstExpression::Exit(Box::new(self.parse_expression(a.as_ref())), b),
            Expression::Literal(a) => AstExpression::Literal(a),
            Expression::Block(a) => AstExpression::Block(Rc::new(self.parse_block(&a)))
        }
    }

    pub fn parse_body(&self, body: &ast::Body) -> AstBody {
        AstBody {
            exprs: body.exprs.iter().map(|expr| self.parse_expression(expr)).collect()
        }
    }

    pub fn parse_block(&self, blk: &ast::Block) -> AstBlock {
        AstBlock {
            nbr_params: blk.nbr_params,
            nbr_locals: blk.nbr_locals,
            body: self.parse_body(&blk.body),
        }
    }

    pub fn parse_binary_op(&self, binary_op: &ast::BinaryOp) -> AstBinaryOp {
        AstBinaryOp {
            op: binary_op.op.clone(),
            lhs: self.parse_expression(&binary_op.lhs),
            rhs: self.parse_expression(&binary_op.rhs),
        }
    }

    pub fn parse_message(&self, msg: &ast::Message) -> AstMessage {
        AstMessage {
            receiver: self.parse_expression(&msg.receiver),
            signature: msg.signature.clone(),
            values: msg.values.iter().map(|e| self.parse_expression(e)).collect(),
        }
    }

    pub fn parse_super_message(&self, super_msg: &ast::SuperMessage) -> AstSuperMessage {
        AstSuperMessage {
            receiver_name: super_msg.receiver_name.clone(),
            is_static_class_call: super_msg.is_static_class_call,
            signature: super_msg.signature.clone(),
            values: super_msg.values.iter().map(|e| self.parse_expression(e)).collect(),
        }
    }
}