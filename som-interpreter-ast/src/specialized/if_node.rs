use std::rc::Rc;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;
use crate::value::Value::Nil;

#[derive(Clone)]
pub struct IfNode {
    pub(crate) expected_bool: bool
}

impl Invoke for IfNode {
    fn invoke(&self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let cond_block_val = unsafe { args.get_unchecked(0) };
        let body_block_arg = unsafe { args.get_unchecked(1) };

        let (bool_val, body_block) = match (cond_block_val, body_block_arg) {
            (Value::Boolean(b), Value::Block(c)) => (*b, Rc::clone(&c)),
            (a, b) => panic!("if[True|False] was not given a bool and a block as arguments, but {:?} and {:?}", a, b)
        };
        
        if bool_val != self.expected_bool {
            Return::Local(Nil)
        } else {
            universe.with_frame(
                Value::Block(Rc::clone(&body_block)),
                body_block.block.nbr_locals,
                0,
                |universe| body_block.invoke(universe, vec![]),
            )
        }
    }
}