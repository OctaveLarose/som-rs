use crate::nodes::global_read::GlobalNode;
use crate::nodes::inlined::and_inlined_node::AndInlinedNode;
use crate::nodes::inlined::if_inlined_node::IfInlinedNode;
use crate::nodes::inlined::if_nil_if_not_nil_inlined_node::IfNilIfNotNilInlinedNode;
use crate::nodes::inlined::if_nil_inlined_node::IfNilInlinedNode;
use crate::nodes::inlined::if_true_if_false_inlined_node::IfTrueIfFalseInlinedNode;
use crate::nodes::inlined::or_inlined_node::OrInlinedNode;
use crate::nodes::inlined::to_do_inlined_node::ToDoInlinedNode;
use crate::nodes::inlined::while_inlined_node::WhileInlinedNode;
use crate::vm_objects::class::Class;
use crate::vm_objects::method::Method;
use indenter::indented;
use num_bigint::BigInt;
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;
use som_value::interned::Interned;
use std::fmt::Write;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum InlinedNode {
    IfInlined(IfInlinedNode),
    IfTrueIfFalseInlined(IfTrueIfFalseInlinedNode),
    IfNilInlined(IfNilInlinedNode),
    IfNilIfNotNilInlined(IfNilIfNotNilInlinedNode),
    WhileInlined(WhileInlinedNode),
    OrInlined(OrInlinedNode),
    AndInlined(AndInlinedNode),
    ToDoInlined(ToDoInlinedNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstBody {
    pub exprs: Vec<AstExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstExpression {
    GlobalRead(Box<GlobalNode>),
    LocalVarRead(u8),
    NonLocalVarRead(u8, u8),
    ArgRead(u8, u8),
    FieldRead(u8),
    IncLocal(u8),
    DecLocal(u8),
    LocalVarWrite(u8, Gc<AstExpression>),
    NonLocalVarWrite(u8, u8, Gc<AstExpression>),
    ArgWrite(u8, u8, Gc<AstExpression>),
    FieldWrite(u8, Gc<AstExpression>),
    UnaryDispatch(Box<AstUnaryDispatch>),
    BinaryDispatch(Box<AstBinaryDispatch>),
    TernaryDispatch(Box<AstTernaryDispatch>),
    NAryDispatch(Box<AstNAryDispatch>),
    SuperMessage(Box<AstSuperMessage>),
    LocalExit(Gc<AstExpression>),
    NonLocalExit(Gc<AstExpression>, u8),
    Literal(AstLiteral),
    Block(Gc<AstBlock>),
    /// Call to an inlined method node (no dispatching like a message would)
    InlinedCall(Box<InlinedNode>),
    // TODO: we might want a SEQUENCENODE of some kind. instead of relying on AstBody at all, actually.
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstLiteral {
    /// Represents a symbol literal (eg. `#foo`).
    Symbol(Interned),
    /// Represents a string literal (eg. `'hello'`).
    String(Gc<String>),
    /// Represents a decimal number literal (eg. `3.14`).
    Double(f64),
    /// Represents a integer number literal (eg. `42`).
    Integer(i32),
    /// Represents a big integer (bigger than a 64-bit signed integer can represent).
    BigInteger(Gc<BigInt>),
    /// Represents an array literal (eg. `$(1 2 3)`)
    Array(GcSlice<AstLiteral>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstTerm {
    pub body: AstBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstBlock {
    pub nbr_params: u8,
    pub nbr_locals: u8,
    pub body: AstBody,
}

pub type CacheEntry = (Gc<Class>, Gc<Method>);

#[derive(Debug, Clone, PartialEq)]
pub struct AstDispatchNode {
    pub signature: Interned,
    pub receiver: AstExpression,
    pub inline_cache: Option<CacheEntry>,
}

// TODO: not positive it's better to have them all own a dispatch node, as opposed to making one "Dispatch" enum encapsulating them all. checking would be nice.
#[derive(Debug, Clone, PartialEq)]
pub struct AstUnaryDispatch {
    pub dispatch_node: AstDispatchNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstBinaryDispatch {
    pub dispatch_node: AstDispatchNode,
    pub arg: AstExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstTernaryDispatch {
    pub dispatch_node: AstDispatchNode,
    pub arg1: AstExpression,
    pub arg2: AstExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstNAryDispatch {
    pub dispatch_node: AstDispatchNode,
    pub values: Vec<AstExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstSuperMessage {
    pub super_class: Gc<Class>,
    pub signature: Interned,
    pub values: Vec<AstExpression>,
    // NB: no inline cache. I don't think it's super worth it since super calls are uncommon. Easy to implement though
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstMethodDef {
    /// The method's signature (eg. `println`, `at:put:` or `==`).
    pub signature: String,
    /// The method's body.
    pub body: AstBody,
    /// Number of local variables
    pub locals_nbr: u8,
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
            AstExpression::GlobalRead(global_node) => writeln!(f, "GlobalRead({:?})", global_node.global),
            AstExpression::LocalVarRead(index) => writeln!(f, "LocalVarRead({})", index),
            AstExpression::NonLocalVarRead(level, index) => {
                writeln!(f, "NonLocalVarRead({}, {})", level, index)
            }
            AstExpression::ArgRead(level, index) => writeln!(f, "ArgRead({}, {})", level, index),
            AstExpression::FieldRead(index) => writeln!(f, "FieldRead({})", index),
            AstExpression::LocalVarWrite(index, expr) => {
                writeln!(f, "LocalVarWrite({}):", index)?;
                write!(indented(f), "{}", **expr)
            }
            AstExpression::NonLocalVarWrite(level, index, expr) => {
                writeln!(f, "NonLocalVarWrite({}, {}):", level, index)?;
                write!(indented(f), "{}", **expr)
            }
            AstExpression::ArgWrite(level, index, expr) => {
                writeln!(f, "ArgWrite({}, {}):", level, index)?;
                write!(indented(f), "{}", **expr)
            }
            AstExpression::FieldWrite(index, expr) => {
                writeln!(f, "FieldWrite({}):", index)?;
                write!(indented(f), "{}", **expr)
            }
            AstExpression::UnaryDispatch(op) => {
                writeln!(f, "UnaryDispatch \"{:?}\":", op.dispatch_node.signature)?;
                writeln!(indented(f), "Receiver:")?;
                write!(indented(&mut indented(f)), "{}", op.dispatch_node.receiver)
            }
            AstExpression::BinaryDispatch(op) => {
                writeln!(f, "BinaryDispatch \"{:?}\":", op.dispatch_node.signature)?;
                writeln!(indented(f), "Receiver:")?;
                write!(indented(&mut indented(f)), "{}", op.dispatch_node.receiver)?;
                writeln!(indented(f), "arg:")?;
                write!(indented(&mut indented(f)), "{}", op.arg)
            }
            AstExpression::TernaryDispatch(op) => {
                writeln!(f, "TernaryDispatch \"{:?}\":", op.dispatch_node.signature)?;
                writeln!(indented(f), "Receiver:")?;
                write!(indented(&mut indented(f)), "{}", op.dispatch_node.receiver)?;
                writeln!(indented(f), "arg1:")?;
                write!(indented(&mut indented(f)), "{}", op.arg1)?;
                writeln!(indented(f), "arg2:")?;
                write!(indented(&mut indented(f)), "{}", op.arg2)
            }
            AstExpression::NAryDispatch(msg) => {
                writeln!(f, "N-AryDispatch \"{:?}\":", msg.dispatch_node.signature)?;
                writeln!(indented(f), "Receiver:")?;
                write!(indented(&mut indented(f)), "{}", msg.dispatch_node.receiver)?;
                writeln!(indented(f), "Values: {}", if msg.values.is_empty() { "(none)" } else { "" })?;
                for value in &msg.values {
                    write!(indented(&mut indented(f)), "{}", value)?;
                }
                Ok(())
            }
            AstExpression::SuperMessage(msg) => {
                writeln!(f, "SuperMessage \"{:?}\":", msg.signature)?;
                writeln!(indented(f), "Receiver: {}", msg.super_class.name)?;
                writeln!(indented(f), "Values: {}", if msg.values.is_empty() { "(none)" } else { "" })?;
                for value in &msg.values {
                    write!(indented(&mut indented(f)), "{}", value)?;
                }
                Ok(())
            }
            AstExpression::LocalExit(expr) => {
                writeln!(f, "LocalExit")?;
                writeln!(indented(f), "{}", **expr)
            }
            AstExpression::NonLocalExit(expr, index) => {
                writeln!(f, "NonLocalExit({})", index)?;
                writeln!(indented(f), "{}", **expr)
            }
            AstExpression::Literal(literal) => writeln!(f, "Literal({:?})", literal),
            AstExpression::Block(block) => {
                writeln!(f, "Block:")?;
                writeln!(indented(f), "{}", **block)
            }
            AstExpression::InlinedCall(inlined_node) => match inlined_node.as_ref() {
                InlinedNode::IfInlined(node) => writeln!(f, "{}", node),
                InlinedNode::IfTrueIfFalseInlined(node) => writeln!(f, "{}", node),
                InlinedNode::IfNilInlined(node) => writeln!(f, "{}", node),
                InlinedNode::WhileInlined(node) => writeln!(f, "{}", node),
                InlinedNode::OrInlined(node) => writeln!(f, "{}", node),
                InlinedNode::AndInlined(node) => writeln!(f, "{}", node),
                InlinedNode::ToDoInlined(node) => writeln!(f, "{}", node),
                InlinedNode::IfNilIfNotNilInlined(node) => writeln!(f, "{}", node),
            },
            AstExpression::IncLocal(idx) => writeln!(f, "Inc({})", idx),
            AstExpression::DecLocal(idx) => writeln!(f, "Dec({})", idx),
        }
    }
}
