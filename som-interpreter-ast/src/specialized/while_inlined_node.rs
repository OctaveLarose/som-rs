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
pub struct WhileInlinedNode {
    pub expected_bool: bool,
    pub cond_instrs: AstBody,
    pub body_instrs: AstBody,
}

impl Display for WhileInlinedNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "WhileInlinedNode (expected bool: {}):", self.expected_bool)?;
        writeln!(indented(f), "condition block:")?;
        write!(indented(&mut indented(f)), "{}", self.cond_instrs)?;
        writeln!(indented(f), "body block:")?;
        write!(indented(&mut indented(f)), "{}", self.body_instrs)
    }
}

impl Evaluate for WhileInlinedNode {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        loop {
            let cond_result = propagate!(self.cond_instrs.evaluate(universe));
            match cond_result {
                Value::Boolean(b) => {
                    if b != self.expected_bool {
                        break;
                    } else {
                        propagate!(self.body_instrs.evaluate(universe));                        
                    }
                },
                val => panic!("whileinlined condition did not evaluate to boolean but {:?}", val)
            };
        }
        Return::Local(Nil)
    }
}