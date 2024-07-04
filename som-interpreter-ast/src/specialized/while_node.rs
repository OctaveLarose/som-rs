use std::rc::Rc;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;
use crate::value::Value::Nil;

#[derive(Clone)]
pub struct WhileNode {
    pub(crate) expected_bool: bool
}

impl Invoke for WhileNode {
    fn invoke(&self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let cond_block_val = unsafe { args.get_unchecked(0) };
        let body_block_arg = unsafe { args.get_unchecked(1) };

        let (cond_block, body_block) = match (cond_block_val, body_block_arg) {
            (Value::Block(b), Value::Block(c)) => (b.clone(), c.clone()),
            _ => panic!("while[True|False] was not given two blocks as arguments")
        };

        loop {
            let cond_block_return = universe.with_frame(
                cond_block.block.nbr_locals,
                vec![Value::Block(Rc::clone(&cond_block))],
                |universe| cond_block.invoke(universe, vec![]),
            );

            let bool_val = match cond_block_return {
                Return::Local(Value::Boolean(b)) => b,
                v => panic!("Invalid, condition block should return a boolean: instead was {:?}.", v)
            };

            if bool_val != self.expected_bool {
                break Return::Local(Nil)
            } else {
                let ret_val = universe.with_frame(
                    body_block.block.nbr_locals,
                    vec![Value::Block(Rc::clone(&body_block))],
                    |universe| body_block.invoke(universe, vec![]),
                );

                match ret_val {
                    Return::Restart | Return::Local(_) => {},
                    ret @ Return::NonLocal(_, _) => break ret,
                    Return::Exception(e) => panic!("Exception thrown: {}", e),
                }
            }
        }
    }
}