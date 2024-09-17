use std::cell::RefCell;
use std::rc::Rc;
use crate::block::Block;
use crate::evaluate::Evaluate;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;

#[derive(Clone)]
pub struct ToDoNode {}

impl Invoke for ToDoNode {
    fn invoke(&mut self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let start_int_val = args.first().unwrap();
        let end_int_val = args.get(1).unwrap();
        let body_block_val = args.get(2).unwrap();

        match (start_int_val, end_int_val, body_block_val) {
            (Value::Integer(a), Value::Integer(b), Value::Block(c)) => Self::do_int_loop(*a, *b, Rc::clone(c), universe),
            (Value::Double(a), Value::Double(b), Value::Block(c)) => Self::do_double_loop(*a, *b, Rc::clone(c), universe),
            (a, b, c) => panic!("to:do: was not given two ints and a block as arguments, but {:?} and {:?} and {:?}", a, b, c) // TODO for this and all other to:do: nodes, it should instead execute the normal method instead.
        }
    }
}

impl ToDoNode {
    fn do_int_loop(start_int: i64, end_int: i64, body_block: Rc<RefCell<Block>>, universe: &mut UniverseAST) -> Return {
        let nbr_locals = body_block.borrow().block.borrow().nbr_locals;
        for i in start_int..=end_int {
            propagate!(universe.with_frame(
                nbr_locals,
                vec![Value::Block(Rc::clone(&body_block)), Value::Integer(i)],
                |universe| body_block.borrow_mut().evaluate(universe),
            ));
        }
        Return::Local(Value::Integer(start_int))
    }

    fn do_double_loop(start_double: f64, end_double: f64, body_block: Rc<RefCell<Block>>, universe: &mut UniverseAST) -> Return {
        let mut i = start_double;
        while i <= end_double {
            propagate!(universe.with_frame(
                body_block.borrow().block.borrow().nbr_locals,
                vec![Value::Block(Rc::clone(&body_block)), Value::Double(i)],
                |universe| body_block.borrow_mut().evaluate(universe),
            ));
            i += 1.0
        }
        Return::Local(Value::Double(start_double))
    }
}