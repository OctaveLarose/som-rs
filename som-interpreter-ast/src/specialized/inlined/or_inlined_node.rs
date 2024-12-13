use crate::ast::{AstBody, AstExpression};
use crate::evaluate::Evaluate;
use crate::invokable::Return;
use crate::universe::Universe;
use crate::value::Value;
use indenter::indented;
use std::fmt::Write;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct OrInlinedNode {
    pub first: AstExpression,
    pub second: AstBody,
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
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let first_result = propagate!(self.first.evaluate(universe, stack_args));
        debug_assert!(first_result.is_boolean());
        if first_result.is_boolean_true() {
            Return::Local(first_result)
        } else {
            self.second.evaluate(universe, stack_args)
        }
    }
}
