use std::fmt::{Display, Formatter};
use std::fmt::Write;
use indenter::indented;
use crate::ast::AstBody;
use crate::evaluate::Evaluate;
use crate::invokable::Return;
use crate::universe::UniverseAST;
use crate::value::Value;
use crate::value::Value::Nil;

#[derive(Debug, Clone, PartialEq)]
pub struct IfInlinedNode {
    pub expected_bool: bool,
    pub cond_instrs: AstBody,
    pub body_instrs: AstBody
}

impl Display for IfInlinedNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "IfInlinedNode (expected bool: {}):", self.expected_bool)?;
        writeln!(indented(f), "condition block:")?;
        write!(indented(&mut indented(f)), "{}", self.cond_instrs)?;
        writeln!(indented(f), "body block:")?;
        write!(indented(&mut indented(f)), "{}", self.body_instrs)
    }
}

impl Evaluate for IfInlinedNode {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        let cond_result = propagate!(self.cond_instrs.evaluate(universe));
        if cond_result == Value::Boolean(self.expected_bool) {
            self.body_instrs.evaluate(universe)
        } else {
            Return::Local(Nil)
        } 
    }
}