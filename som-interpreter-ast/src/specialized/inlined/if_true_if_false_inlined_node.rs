use std::fmt::{Display, Formatter};
use std::fmt::Write;
use indenter::indented;
use crate::ast::{AstBody, AstExpression};
use crate::evaluate::Evaluate;
use crate::invokable::Return;
use crate::universe::UniverseAST;
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct IfTrueIfFalseInlinedNode {
    pub expected_bool: bool,
    pub cond_expr: AstExpression,
    pub body_1_instrs: AstBody,
    pub body_2_instrs: AstBody
}

impl Display for IfTrueIfFalseInlinedNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "IfTrueIfFalseInlinedNode (expected bool: {}):", self.expected_bool)?;
        writeln!(indented(f), "condition block:")?;
        write!(indented(&mut indented(f)), "{}", self.cond_expr)?;
        writeln!(indented(f), "body block 1:")?;
        write!(indented(&mut indented(f)), "{}", self.body_1_instrs)?;
        writeln!(indented(f), "body block 2:")?;
        write!(indented(&mut indented(f)), "{}", self.body_2_instrs)
    }
}

impl Evaluate for IfTrueIfFalseInlinedNode {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let cond_result = propagate!(self.cond_expr.evaluate(universe));
        if cond_result == Value::Boolean(self.expected_bool) {
            self.body_1_instrs.evaluate(universe)
        } else {
            self.body_2_instrs.evaluate(universe)
        } 
    }
}