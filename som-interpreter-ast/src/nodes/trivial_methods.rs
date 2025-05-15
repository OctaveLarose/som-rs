use som_gc::gcref::Gc;

use crate::ast::AstLiteral;
use crate::evaluate::Evaluate;
use crate::invokable::{Invoke, Return};
use crate::universe::{GlobalValueStack, Universe};
use crate::value::Value;

use super::global_read::GlobalNode;

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialLiteralMethod {
    pub(crate) literal: AstLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialGlobalMethod {
    pub(crate) global_name: Gc<GlobalNode>,
}

impl Evaluate for TrivialGlobalMethod {
    fn evaluate(&mut self, universe: &mut Universe, value_stack: &mut GlobalValueStack) -> Return {
        self.global_name.evaluate(universe, value_stack)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialGetterMethod {
    pub(crate) field_idx: u8,
}

impl Invoke for TrivialGetterMethod {
    fn invoke(&mut self, _universe: &mut Universe, value_stack: &mut GlobalValueStack, nbr_args: usize) -> Return {
        debug_assert_eq!(nbr_args, 1);
        let arg = value_stack.pop();

        if let Some(cls) = arg.as_class() {
            Return::Local(cls.class().lookup_field(self.field_idx))
        } else if let Some(instance) = arg.as_instance() {
            Return::Local(*instance.lookup_field(self.field_idx))
        } else {
            panic!("trivial getter not called on a class/instance?")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialSetterMethod {
    pub(crate) field_idx: u8,
}

impl Invoke for TrivialSetterMethod {
    fn invoke(&mut self, _universe: &mut Universe, value_stack: &mut GlobalValueStack, nbr_args: usize) -> Return {
        debug_assert_eq!(nbr_args, 2);
        let val = value_stack.pop();
        let rcvr = value_stack.pop();

        if let Some(cls) = rcvr.as_class() {
            cls.class().assign_field(self.field_idx, val);
            Return::Local(Value::Class(cls))
        } else if let Some(mut instance) = rcvr.as_instance() {
            instance.assign_field(self.field_idx, val);
            Return::Local(Value::Instance(instance))
        } else {
            panic!("trivial getter not called on a class/instance?")
        }
    }
}
