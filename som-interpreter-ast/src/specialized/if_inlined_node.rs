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