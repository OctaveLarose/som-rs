use std::fmt::{Display, Formatter};
use std::fmt::Write;
use indenter::indented;
use crate::ast::{AstBody, AstExpression};
use crate::evaluate::Evaluate;
use crate::invokable::Return;
use crate::universe::UniverseAST;
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct OrInlinedNode {
    pub first: AstExpression,
    pub second: AstBody
}

impl Display for OrInlinedNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "OrInlinedNode:")?;
        writeln!(indented(f), "first block:")?;
        write!(indented(&mut indented(f)), "{}", self.first)?;
        writeln!(indented(f), "second block:")?;
        write!(indented(&mut indented(f)), "{}", self.second)
    }
}

impl Evaluate for OrInlinedNode {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let first_result = propagate!(self.first.evaluate(universe));
        match first_result {
            Value::Boolean(true) => Return::Local(first_result),
            Value::Boolean(false) => self.second.evaluate(universe),
            _ => panic!("or:'s first part didn't evaluate to a boolean?")
        }
    }
}