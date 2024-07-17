use std::rc::Rc;
use crate::specialized::if_inlined_node::IfInlinedNode;


#[derive(Debug, Clone, PartialEq)]
pub enum InlinedNode {
    IfInlined(IfInlinedNode)
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstBody {
    pub exprs: Vec<AstExpression>,
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
    /// Call to an inlined method node (no dispatching like a message would)
    InlinedCall(Box<InlinedNode>)
    // todo we might want a SEQUENCENODE of some kind. instead of relying on AstBody at all, actually.
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

#[derive(Debug, Clone, PartialEq)]
pub struct AstBinaryOp {
    /// Represents the operator symbol.
    pub op: String,
    /// Represents the left-hand side.
    pub lhs: AstExpression,
    /// Represents the right-hand side.
    pub rhs: AstExpression,
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

#[derive(Debug, Clone, PartialEq)]
pub enum AstMethodBody {
    Primitive,
    Body {
        locals_nbr: usize,
        body: AstBody,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstMethodDef {
    /// The method's signature (eg. `println`, `at:put:` or `==`).
    pub signature: String,
    /// The method's body.
    pub body: AstMethodBody,
}