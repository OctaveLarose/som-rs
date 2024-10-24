use std::fmt::{Display, Formatter};
use std::fmt::Write;
use indenter::indented;
use crate::ast::{AstBody, AstExpression};
use crate::evaluate::Evaluate;
use crate::invokable::Return;
use crate::universe::Universe;

#[derive(Debug, Clone, PartialEq)]
pub struct AndInlinedNode {
    pub first: AstExpression,
    pub second: AstBody
}

impl Display for AndInlinedNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "AndInlinedNode:")?;
        writeln!(indented(f), "first block:")?;
        write!(indented(&mut indented(f)), "{}", self.first)?;
        writeln!(indented(f), "second block:")?;
        write!(indented(&mut indented(f)), "{}", self.second)
    }
}

impl Evaluate for AndInlinedNode {
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
        let first_result = propagate!(self.first.evaluate(universe));
        debug_assert!(first_result.is_boolean());
        if first_result.is_boolean_false() {
            Return::Local(first_result)
        } else {
            match self.second.evaluate(universe) {
                Return::Local(a) if a.is_boolean() => Return::Local(a),
                invalid => panic!("and:'s second part didn't evaluate to a returnlocal + boolean, but {:?}?", invalid)
            }
        }
    }
}