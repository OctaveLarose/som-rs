use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use indenter::indented;
use std::fmt::Write;
use crate::class::Class;
use crate::method::Method;
use crate::SOMRef;
use crate::specialized::inlined::and_inlined_node::AndInlinedNode;
use crate::specialized::inlined::if_inlined_node::IfInlinedNode;
use crate::specialized::inlined::if_true_if_false_inlined_node::IfTrueIfFalseInlinedNode;
use crate::specialized::inlined::or_inlined_node::OrInlinedNode;
use crate::specialized::inlined::while_inlined_node::WhileInlinedNode;

#[derive(Debug, Clone, PartialEq)]
pub enum InlinedNode {
    IfInlined(IfInlinedNode),
    IfTrueIfFalseInlined(IfTrueIfFalseInlinedNode),
    WhileInlined(WhileInlinedNode),
    OrInlined(OrInlinedNode),
    AndInlined(AndInlinedNode)
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
    Message(Box<AstMessageDispatch>),
    SuperMessage(Box<AstSuperMessage>),
    BinaryOp(Box<AstBinaryOpDispatch>),
    LocalExit(Box<AstExpression>),
    NonLocalExit(Box<AstExpression>, usize),
    Literal(som_core::ast::Literal),
    Block(Rc<RefCell<AstBlock>>), // Rc here, while it's not an Rc in the parser/som-core AST since BC doesn't need that same Rc.
    /// Call to an inlined method node (no dispatching like a message would)
    InlinedCall(Box<InlinedNode>),
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

#[derive(Clone)]
pub struct AstBinaryOpDispatch {
    /// Represents the operator symbol.
    pub op: String,
    /// Represents the left-hand side.
    pub lhs: AstExpression,
    /// Represents the right-hand side.
    pub rhs: AstExpression,
    /// Inline cache
    pub inline_cache: Option<CacheEntry>,
}

impl Debug for AstBinaryOpDispatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AstBinaryOpDispatch")
            .field("op", &self.op)
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .field("has cache:", &self.inline_cache.is_some())
            .finish()
    }
}

impl PartialEq for AstBinaryOpDispatch {
    fn eq(&self, other: &Self) -> bool {
        self.lhs == other.lhs && self.rhs == other.rhs && self.op == other.op
    }
}

type CacheEntry = (*const Class, SOMRef<Method>);
#[derive(Clone)]
pub struct AstMessageDispatch {
    pub receiver: AstExpression,
    pub signature: String,
    pub values: Vec<AstExpression>,
    pub inline_cache: Option<CacheEntry>,
    // pub inline_cache: Box<[Option<CacheEntry>; INLINE_CACHE_SIZE]>,
}

impl Debug for AstMessageDispatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AstMessageDispatch")
            .field("receiver", &self.receiver)
            .field("signature", &self.signature)
            .field("values", &self.values)
            .field("has cache:", &self.inline_cache.is_some())
            .finish()
    }
}

impl PartialEq for AstMessageDispatch {
    fn eq(&self, other: &Self) -> bool {
        self.receiver == other.receiver && self.signature == other.signature && self.values == other.values
    }
}

#[derive(Debug, Clone)]
pub struct AstSuperMessage {
    pub super_class: SOMRef<Class>,
    pub signature: String,
    pub values: Vec<AstExpression>,
    // NB: no inline cache. I don't think it's super worth it since super calls are uncommon. Easy to implement though
}

impl PartialEq for AstSuperMessage {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.super_class.as_ptr(), other.super_class.as_ptr()) && self.signature == other.signature && self.values == other.values
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstMethodDef {
    /// The method's signature (eg. `println`, `at:put:` or `==`).
    pub signature: String,
    /// The method's body.
    pub body: AstBody,
    /// Number of local variables
    pub locals_nbr: usize,
}

// ----------------

impl Display for AstMethodDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Method {} ({} locals):", &self.signature, self.locals_nbr))?;
        f.write_str(self.body.to_string().as_str())
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
                writeln!(indented(f), "Receiver: {}", msg.super_class.borrow().name)?;
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
            AstExpression::LocalExit(expr) => {
                writeln!(f, "LocalExit")?;
                writeln!(indented(f), "{}", expr)
            }
            AstExpression::NonLocalExit(expr, index) => {
                writeln!(f, "NonLocalExit({})", index)?;
                writeln!(indented(f), "{}", expr)
            }
            AstExpression::Literal(literal) => writeln!(f, "Literal({:?})", literal),
            AstExpression::Block(block) => {
                writeln!(f, "Block:")?;
                writeln!(indented(f), "{}", block.borrow())
            }
            AstExpression::InlinedCall(inlined_node) => match inlined_node.as_ref() {
                InlinedNode::IfInlined(node) => writeln!(f, "{}", node),
                InlinedNode::IfTrueIfFalseInlined(node) => writeln!(f, "{}", node),
                InlinedNode::WhileInlined(node) => writeln!(f, "{}", node),
                InlinedNode::OrInlined(node) => writeln!(f, "{}", node),
                InlinedNode::AndInlined(node) => writeln!(f, "{}", node),
            },
        }
    }
}
