use std::rc::Rc;
use crate::evaluate::Evaluate;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;

#[derive(Clone)]
pub struct ToDoNode {}

impl Invoke for ToDoNode {
    fn invoke(&self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let start_int_val = args.first().unwrap();
        let end_int_val = args.get(1).unwrap();
        let body_block_val = args.get(2).unwrap();

        let (start_int, end_int, body_block) = match (start_int_val, end_int_val, body_block_val) {
            (Value::Integer(a), Value::Integer(b), Value::Block(c)) => (*a, *b, c.clone()),
            (a, b, c) => panic!("to:do: was not given two ints and a block as arguments, but {:?} and {:?} and {:?}", a, b, c)
        };
        
        // dbg!(&body_block.block.body);
        for i in start_int..=end_int {
            // dbg!(&i);
            universe.with_frame(
                body_block.block.nbr_locals,
                vec![Value::Block(Rc::clone(&body_block)), Value::Integer(i)],
                |universe| body_block.evaluate(universe),
            );
        }

        // dbg!("done");
        Return::Local(Value::Integer(start_int))
    }
}