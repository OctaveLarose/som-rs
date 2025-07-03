use indenter::indented;
use std::fmt::Write;

use crate::ast::{AstBody, AstExpression};
use crate::evaluate::Evaluate;
use crate::invokable::Return;
use crate::universe::{GlobalValueStack, Universe};
use crate::value::Value;
use crate::vm_objects::frame::FrameAccess;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct ToDoInlinedNode {
    pub start: AstExpression,
    pub end: AstExpression,
    pub body: AstBody,
    pub accumulator_idx: usize,
}

impl Display for ToDoInlinedNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "TodoInlinedNode:",)?;
        writeln!(indented(f), "start:")?;
        write!(indented(&mut indented(f)), "{}", self.start)?;
        writeln!(indented(f), "end:")?;
        write!(indented(&mut indented(f)), "{}", self.end)?;
        writeln!(indented(f), "body block:")?;
        write!(indented(&mut indented(f)), "{}", self.body)?;
        writeln!(indented(f), "acc. idx: {}", self.accumulator_idx)
    }
}

impl Evaluate for ToDoInlinedNode {
    fn evaluate(&mut self, universe: &mut Universe, value_stack: &mut GlobalValueStack) -> Return {
        let start = propagate!(self.start.evaluate(universe, value_stack));
        let end = propagate!(self.end.evaluate(universe, value_stack));

        if let (Some(start_int), Some(end_int)) = (start.as_integer(), end.as_integer()) {
            self.int_loop(universe, value_stack, start_int, end_int)
        } else if let (Some(start_double), Some(end_double)) = (start.as_double(), end.as_double()) {
            self.float_loop(universe, value_stack, start_double, end_double)
        } else {
            unimplemented!("to:do: case that isn't int nor float")
        }
    }
}

impl ToDoInlinedNode {
    fn int_loop(&mut self, universe: &mut Universe, value_stack: &mut GlobalValueStack, start: i32, end: i32) -> Return {
        for i in start..=end {
            universe.current_frame.assign_local(self.accumulator_idx as u8, Value::Integer(i));
            propagate!(self.body.evaluate(universe, value_stack));
        }

        Return::Local(Value::Integer(start))
    }

    fn float_loop(&mut self, universe: &mut Universe, value_stack: &mut GlobalValueStack, start: f64, end: f64) -> Return {
        let mut i = start;

        while i <= end {
            universe.current_frame.assign_local(self.accumulator_idx as u8, Value::Double(i));
            propagate!(self.body.evaluate(universe, value_stack));
            i += 1.0
        }

        Return::Local(Value::Double(start))
    }
}
