use std::rc::Rc;
use crate::invokable::{Invoke, Return};
use crate::universe::Universe;
use crate::value::Value;

#[derive(Clone)]
pub struct IfTrueIfFalseNode {}

impl Invoke for IfTrueIfFalseNode {
    fn invoke(&mut self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let cond_block_val = args.get(0).unwrap();
        let body_block_arg = args.get(1).unwrap();
        let body_block_arg2 = args.get(2).unwrap();

        let bool_val = match cond_block_val {
            Value::Boolean(a) => *a,
            _x => panic!()
        };

        // let (bool_val, body_block, body_block2) = match (cond_block_val, body_block_arg, body_block_arg2) {
        //     (Value::Boolean(a), Value::Block(b), Value::Block(c)) => (*a, b.clone(), c.clone()),
        //     (a, b, c) => panic!("ifTrue:ifFalse: was not given a bool and two blocks as arguments, but {:?} and {:?} and {:?}", a, b, c)
        // };

        if bool_val {
            // body_block.invoke(universe, vec![])
            // TODO: this kinda sucks, right? It's to make the case work where you don't provide a block.
            // Shouldn't this be changed in IfNode too
            match body_block_arg {
                Value::Block(b) => {
                    universe.with_frame(
                        Value::Block(Rc::clone(&b)),
                        b.borrow().block.nbr_locals,
                        0,
                        |universe| b.borrow_mut().invoke(universe, vec![]),
                    )
                },
                a => Return::Local(a.clone()),
            }
        } else {
            // body_block2.invoke(universe, vec![])
            match body_block_arg2 {
                Value::Block(b) => {
                    universe.with_frame(
                        Value::Block(Rc::clone(&b)),
                        b.borrow().block.nbr_locals,
                        0,
                        |universe| b.borrow_mut().invoke(universe, vec![]),
                    )
                },
                a => Return::Local(a.clone()),
            }
        }
    }
}