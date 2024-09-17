use std::rc::Rc;
use crate::evaluate::Evaluate;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;

#[derive(Clone)]
pub struct ToByDoNode {}

impl Invoke for ToByDoNode {
    fn invoke(&mut self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let start_int_val = args.first().unwrap();
        let step_int_val = args.get(1).unwrap();
        let end_int_val = args.get(2).unwrap();
        let body_block_val = args.get(3).unwrap();

        let (start_int, end_int, step_int, body_block) = match (start_int_val, step_int_val, end_int_val, body_block_val) {
            (Value::Integer(a), Value::Integer(b), Value::Integer(c), Value::Block(d)) => (*a, *b, *c, d.clone()),
            (a, b, c, d) => panic!("to:by:do: was not given three ints and a block as arguments, but {:?} and {:?} and {:?} and {:?}", a, b, c, d)
        };

        let mut i = start_int;

        while i <= end_int {
            propagate!(universe.with_frame(
                body_block.borrow().block.borrow().nbr_locals,
                vec![Value::Block(Rc::clone(&body_block)), Value::Integer(i)],
                |universe| body_block.borrow_mut().evaluate(universe),
            ));
            i += step_int;
        }

        Return::Local(Value::Integer(start_int))
    }
}