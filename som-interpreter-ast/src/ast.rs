use std::rc::Rc;
use som_core::ast;
use som_core::ast::{Expression, MethodBody};
use crate::inliner::PrimMessageInliner;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprRef(u32);

// #[derive(Debug, Clone, PartialEq)]
// pub struct ExprPool(Vec<AstExpression>);

#[derive(Debug, Clone, PartialEq)]
pub struct AstBody {
    pub exprs: Vec<AstExpression>,
}

impl AstBody {
    pub fn from_parser_ast(method_body: &ast::Body) -> AstBody {
        AstBody {
            exprs: method_body.exprs.iter().map(AstExpression::from_parser_ast).collect()
        }
    }
}

// identical but using refs as
#[derive(Debug, Clone, PartialEq)]
pub enum AstExpression {
    GlobalRead(String),
    LocalVarRead(usize),
    NonLocalVarRead(usize, usize),
    ArgRead(usize, usize),
    FieldRead(usize),
    LocalVarWrite(usize, Box<AstExpression>),
    NonLocalVarWrite(usize, usize, Box<AstExpression>),
    ArgWrite(usize, usize, Box<AstExpression>),
    FieldWrite(usize, Box<AstExpression>),
    Message(Box<AstMessage>),
    SuperMessage(Box<AstSuperMessage>),
    BinaryOp(Box<AstBinaryOp>),
    Exit(Box<AstExpression>, usize),
    Literal(som_core::ast::Literal),
    Block(Rc<AstBlock>),
}

impl AstExpression {
    pub fn from_parser_ast(method_body: &ast::Expression) -> AstExpression {
        match method_body.clone() {
            Expression::GlobalRead(a) => AstExpression::GlobalRead(a),
            Expression::LocalVarRead(a) => AstExpression::LocalVarRead(a),
            Expression::NonLocalVarRead(a, b) => AstExpression::NonLocalVarRead(a, b),
            Expression::ArgRead(a, b) => AstExpression::ArgRead(a, b),
            Expression::FieldRead(a) => AstExpression::FieldRead(a),
            Expression::LocalVarWrite(a, b) => AstExpression::LocalVarWrite(a, Box::new(AstExpression::from_parser_ast(b.as_ref()))),
            Expression::NonLocalVarWrite(a, b, c) => AstExpression::NonLocalVarWrite(a, b, Box::new(AstExpression::from_parser_ast(c.as_ref()))),
            Expression::ArgWrite(a, b, c) => AstExpression::ArgWrite(a, b, Box::new(AstExpression::from_parser_ast(c.as_ref()))),
            Expression::FieldWrite(a, b) => AstExpression::FieldWrite(a, Box::new(AstExpression::from_parser_ast(b.as_ref()))),
            Expression::Message(a) => AstExpression::Message(Box::new(AstMessage::from_parser_ast(a.as_ref()))),
            Expression::SuperMessage(a) => AstExpression::SuperMessage(Box::new(AstSuperMessage::from_parser_ast(a.as_ref()))),
            Expression::BinaryOp(a) => AstExpression::BinaryOp(Box::new(AstBinaryOp::from_parser_ast(a.as_ref()))),
            Expression::Exit(a, b) => AstExpression::Exit(Box::new(AstExpression::from_parser_ast(a.as_ref())), b),
            Expression::Literal(a) => AstExpression::Literal(a),
            Expression::Block(a) => AstExpression::Block(Rc::new(AstBlock::from_parser_ast(&a)))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstTerm {
    pub body: AstBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstBlock {
    pub nbr_params: usize,
    pub nbr_locals: usize,
    pub body: AstBody
}

impl AstBlock {
    pub fn from_parser_ast(method_body: &ast::Block) -> AstBlock {
        AstBlock {
            nbr_params: method_body.nbr_params,
            nbr_locals: method_body.nbr_locals,
            body: AstBody::from_parser_ast(&method_body.body),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct AstBinaryOp {
    /// Represents the operator symbol.
    pub op: String,
    /// Represents the left-hand side.
    pub lhs: AstExpression,
    /// Represents the right-hand side.
    pub rhs: AstExpression,
}

impl AstBinaryOp {
    pub fn from_parser_ast(method_body: &ast::BinaryOp) -> AstBinaryOp {
        AstBinaryOp {
            op: method_body.op.clone(),
            lhs: AstExpression::from_parser_ast(&method_body.lhs),
            rhs: AstExpression::from_parser_ast(&method_body.rhs),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstMessage {
    pub receiver: AstExpression,
    pub signature: String,
    pub values: Vec<AstExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstSuperMessage {
    pub receiver_name: String,
    pub is_static_class_call: bool,
    pub signature: String,
    pub values: Vec<AstExpression>,
}


impl AstMessage {
    pub fn from_parser_ast(method_body: &ast::Message) -> AstMessage {
        if method_body.inline_if_possible(42).is_some() {
            todo!()
        }
        
        AstMessage {
            receiver: AstExpression::from_parser_ast(&method_body.receiver),
            signature: method_body.signature.clone(),
            values: method_body.values.iter().map(|e| AstExpression::from_parser_ast(e)).collect(),
        }
    }
}

impl AstSuperMessage {
    pub fn from_parser_ast(super_msg_ast: &ast::SuperMessage) -> AstSuperMessage {
        AstSuperMessage {
            receiver_name: super_msg_ast.receiver_name.clone(),
            is_static_class_call: super_msg_ast.is_static_class_call,
            signature: super_msg_ast.signature.clone(),
            values: super_msg_ast.values.iter().map(|e| AstExpression::from_parser_ast(e)).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstMethodBody {
    Primitive,
    Body {
        locals_nbr: usize,
        body: AstBody,
    },
}

impl AstMethodBody {
    pub fn from_parser_ast(method_body: &ast::MethodBody) -> AstMethodBody {
        match method_body {
            MethodBody::Primitive => {AstMethodBody::Primitive}
            MethodBody::Body { locals_nbr, body, ..} => {
                AstMethodBody::Body { locals_nbr: *locals_nbr, body: AstBody::from_parser_ast(body) }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstMethodDef {
    /// The method's signature (eg. `println`, `at:put:` or `==`).
    pub signature: String,
    /// The method's body.
    pub body: AstMethodBody,
}

impl AstMethodDef {
    pub fn from_parser_ast(method_def: &ast::MethodDef) -> AstMethodDef {
        AstMethodDef {
            signature: method_def.signature.clone(),
            body: AstMethodBody::from_parser_ast(&method_def.body),
        }
    }
}