use std::rc::Rc;
use crate::invokable::{Invoke, Return};
use crate::universe::Universe;
use crate::value::Value;
use crate::value::Value::Nil;

#[derive(Clone)]
pub struct IfNode {
    pub(crate) expected_bool: bool
}

impl Invoke for IfNode {
    fn invoke(&mut self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let cond_block_val = args.get(0).unwrap();
        let body_block_arg = args.get(1).unwrap();

        let (bool_val, body_block) = match (cond_block_val, body_block_arg) {
            (Value::Boolean(b), Value::Block(c)) => (*b, Rc::clone(&c)),
            (a, b) => panic!("if[True|False] was not given a bool and a block as arguments, but {:?} and {:?}", a, b)
        };

        let nbr_locals = body_block.borrow().block.nbr_locals;

        if bool_val != self.expected_bool {
            Return::Local(Nil)
        } else {
            universe.with_frame(
                Value::Block(Rc::clone(&body_block)),
                nbr_locals,
                0,
                |universe| body_block.borrow_mut().invoke(universe, vec![]),
            )
        }
    }
}