use crate::evaluate::Evaluate;
use crate::invokable::{Invoke, Return};
use crate::universe::Universe;
use crate::value::Value;
#[derive(Debug, Clone, PartialEq)]
pub struct WhileNode {
    pub(crate) expected_bool: bool,
}

impl Invoke for WhileNode {
    fn invoke(&mut self, universe: &mut Universe, nbr_args: usize) -> Return {
        let args = universe.stack_n_last_elems(nbr_args);
        let cond_block_val = unsafe { args.get_unchecked(0) };
        let body_block_arg = unsafe { args.get_unchecked(1) };

        let (mut cond_block, mut body_block) = match (cond_block_val.as_block(), body_block_arg.as_block()) {
            (Some(b), Some(c)) => (b, c),
            _ => panic!("while[True|False] was not given two blocks as arguments"),
        };

        let nbr_locals = cond_block.block.nbr_locals;

        loop {
            universe.stack_args.push(Value::Block(cond_block));
            let cond_block_return = universe.with_frame(nbr_locals, 1, |universe| cond_block.evaluate(universe));

            let bool_val = match cond_block_return {
                Return::Local(b_val) => match b_val.as_boolean() {
                    Some(b) => b,
                    None => panic!("Invalid, condition block should return a boolean: instead was {:?}.", b_val),
                },
                _ => panic!("condition block returned a nonlocal (is that valid?) or exception"),
            };

            if bool_val != self.expected_bool {
                return Return::Local(Value::NIL);
            } else {
                let nbr_locals = body_block.block.nbr_locals;

                universe.stack_args.push(Value::Block(body_block));
                propagate!(universe.with_frame(nbr_locals, 1, |universe| body_block.evaluate(universe),));
            }
        }
    }
}
