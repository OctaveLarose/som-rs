use crate::ast::AstBody;
use crate::evaluate::Evaluate;
use crate::invokable::Return;
use crate::universe::UniverseAST;
use crate::value::Value;
use crate::value::Value::Nil;

#[derive(Clone)]
pub struct IfInlinedNode {
    pub(crate) expected_bool: bool,
    pub(crate) cond: AstBody,
    pub(crate) body: AstBody
}

impl Evaluate for IfInlinedNode {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        let cond_result = propagate!(self.cond.evaluate(universe));
        if cond_result == Value::Boolean(self.expected_bool) {
            self.body.evaluate(universe)
        } else {
            Return::Local(Nil)
        } 
    }
}