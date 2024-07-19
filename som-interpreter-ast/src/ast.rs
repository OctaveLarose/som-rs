use std::fmt::{Display, Formatter};
use std::rc::Rc;
use indenter::indented;
use crate::specialized::if_inlined_node::IfInlinedNode;
use std::fmt::Write;

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

// ----------------

impl Display for AstMethodDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Method {}", &self.signature))?;
        f.write_str(self.body.to_string().as_str())
    }
}

impl Display for AstMethodBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstMethodBody::Primitive => {f.write_str("(primitive)")}
            AstMethodBody::Body { locals_nbr, body } => {
                f.write_fmt(format_args!("locals_nbr: {}\n", locals_nbr))?;
                f.write_str(body.to_string().as_str())
            }
        }
    }
}

impl Display for AstBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "AstBody:")?;
        for expr in &self.exprs {
            write!(indented(f), "{}", expr)?;
        }
        Ok(())
    }
}

impl Display for AstBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "AstBlock({} params, {} locals):", self.nbr_params, self.nbr_locals)?;
        for expr in &self.body.exprs {
            write!(indented(f), "{}", expr)?;
        }
        Ok(())
    }
}

// probably not using the indenter lib as one should? though it works. I've given it as little effort as possible.
impl Display for AstExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstExpression::GlobalRead(name) => writeln!(f, "GlobalRead({})", name),
            AstExpression::LocalVarRead(index) => writeln!(f, "LocalVarRead({})", index),
            AstExpression::NonLocalVarRead(level, index) => writeln!(f, "NonLocalVarRead({}, {})", level, index),
            AstExpression::ArgRead(level, index) => writeln!(f, "ArgRead({}, {})", level, index),
            AstExpression::FieldRead(index) => writeln!(f, "FieldRead({})", index),
            AstExpression::LocalVarWrite(index, expr) => {
                writeln!(f, "LocalVarWrite({}):", index)?;
                write!(indented(f), "{}", expr)
            }
            AstExpression::NonLocalVarWrite(level, index, expr) => {
                writeln!(f, "NonLocalVarWrite({}, {}):", level, index)?;
                write!(indented(f), "{}", expr)
            }
            AstExpression::ArgWrite(level, index, expr) => {
                writeln!(f, "ArgWrite({}, {}):", level, index)?;
                write!(indented(f), "{}", expr)
            }
            AstExpression::FieldWrite(index, expr) => {
                writeln!(f, "FieldWrite({}):", index)?;
                write!(indented(f), "{}", expr)
            }
            AstExpression::Message(msg) => {
                writeln!(f, "Message \"{}\":", msg.signature)?;
                writeln!(indented(f), "Receiver:")?;
                write!(indented(&mut indented(f)), "{}", msg.receiver)?;
                writeln!(indented(f), "Values: {}", if msg.values.is_empty() { "(none)" } else { "" })?;
                for value in &msg.values {
                    write!(indented(&mut indented(f)), "{}", value)?;
                }
                Ok(())
            }
            AstExpression::SuperMessage(msg) => {
                writeln!(f, "SuperMessage \"{}\":", msg.signature)?;
                writeln!(indented(f), "Receiver: {} (is static: {})", msg.receiver_name, msg.is_static_class_call)?;
                writeln!(indented(f), "Values: {}", if msg.values.is_empty() { "(none)" } else { "" })?;
                for value in &msg.values {
                    write!(indented(&mut indented(f)), "{}", value)?;
                }
                Ok(())
            }
            AstExpression::BinaryOp(op) => {
                writeln!(f, "BinaryOp({})", op.op)?;
                writeln!(indented(f), "LHS:")?;
                write!(indented(&mut indented(f)), "{}", op.lhs)?;
                writeln!(indented(f), "RHS:")?;
                write!(indented(&mut indented(f)), "{}", op.rhs)
            }
            AstExpression::Exit(expr, index) => {
                writeln!(f, "Exit({})", index)?;
                writeln!(indented(f), "{}", expr)
            }
            AstExpression::Literal(literal) => writeln!(f, "Literal({:?})", literal),
            AstExpression::Block(block) => {
                writeln!(f, "Block:")?;
                writeln!(indented(f), "{}", block)
            }
            AstExpression::InlinedCall(inlined_node) => match inlined_node.as_ref() {
                InlinedNode::IfInlined(node) => writeln!(f, "IfInlinedNode: {:?}", node),
            },
        }
    }
}
