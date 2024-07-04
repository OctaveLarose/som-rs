use std::rc::Rc;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;

#[derive(Clone)]
pub struct IfTrueIfFalseNode {}

impl Invoke for IfTrueIfFalseNode {
    fn invoke(&self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let (cond_block_val, block_1_arg, block_2_arg) = unsafe {
            (args.get_unchecked(0), args.get_unchecked(1), args.get_unchecked(2))
        };
        
        let bool_val = match cond_block_val {
            Value::Boolean(a) => *a,
            _ => panic!("ifTrue:ifFalse: condition did not evaluate to boolean")
        };

        // let (bool_val, body_block, body_block2) = match (cond_block_val, body_block_arg, body_block_arg2) {
        //     (Value::Boolean(a), Value::Block(b), Value::Block(c)) => (*a, b.clone(), c.clone()),
        //     (a, b, c) => panic!("ifTrue:ifFalse: was not given a bool and two blocks as arguments, but {:?} and {:?} and {:?}", a, b, c)
        // };

        let block_to_evaluate = if bool_val { block_1_arg } else { block_2_arg };
        
        match block_to_evaluate {
            Value::Block(b) => {
                universe.with_frame(
                    Value::Block(Rc::clone(&b)),
                    b.block.nbr_locals,
                    0,
                    |universe| b.invoke(universe, vec![]),
                )
            },
            a => Return::Local(a.clone()),
        }
    }
}