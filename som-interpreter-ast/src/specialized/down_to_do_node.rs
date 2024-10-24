use som_core::gc::GCRef;
use crate::block::Block;
use crate::evaluate::Evaluate;
use crate::invokable::{Invoke, Return};
use crate::universe::Universe;
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct DownToDoNode {}

impl Invoke for DownToDoNode {
    
    fn invoke(&mut self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let start_int_val = args.first().unwrap();
        let end_int_val = args.get(1).unwrap();
        let body_block_val = args.get(2).unwrap();
        
        if let (Some(a), Some(b), Some(c)) = (start_int_val.as_integer(), end_int_val.as_integer(), body_block_val.as_block()) {
            Self::do_int_loop(a, b, c, universe)
        } else if let (Some(a), Some(b), Some(c)) = (start_int_val.as_double(), end_int_val.as_double(), body_block_val.as_block()) {
            Self::do_double_loop(a, b, c, universe)
        } else {
            panic!("downTo:do: was not given two numbers and a block as arguments, but {:?} and {:?} and {:?}", start_int_val, end_int_val, body_block_val)
        }
    }
}

impl DownToDoNode {
    fn do_int_loop(start_int: i32, end_int: i32, mut body_block: GCRef<Block>, universe: &mut Universe) -> Return {
        let nbr_locals = body_block.borrow().block.borrow().nbr_locals;
        let mut i = start_int;
        while i >= end_int {
            propagate!(universe.with_frame(
                nbr_locals,
                vec![Value::Block(body_block), Value::Integer(i)],
                |universe| body_block.evaluate(universe),
            ));
            i -= 1;
        }
        Return::Local(Value::Integer(start_int))
    }

    fn do_double_loop(start_double: f64, end_double: f64, mut body_block: GCRef<Block>, universe: &mut Universe) -> Return {
        let nbr_locals = body_block.borrow().block.borrow().nbr_locals;
        let mut i = start_double;
        while i >= end_double {
            propagate!(universe.with_frame(
                nbr_locals,
                vec![Value::Block(body_block), Value::Double(i)],
                |universe| body_block.evaluate(universe),
            ));
            i -= 1.0;
        }
        Return::Local(Value::Double(start_double))
    }
}