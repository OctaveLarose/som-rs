use crate::evaluate::Evaluate;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;
use som_core::ast::Literal;
use std::rc::Rc;

#[derive(Clone)]
pub struct TrivialLiteralMethod {
    pub(crate) literal: Literal,
}

#[derive(Clone)]
pub struct TrivialGlobalMethod {
    pub(crate) global_name: String,
}

impl Evaluate for TrivialGlobalMethod {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let name = self.global_name.as_str();
        // todo logic duplicated with globalread - need to avoid that
        universe.lookup_global(name)
            .map(Return::Local)
            .or_else(|| {
                let frame = universe.current_frame();
                let self_value = frame.borrow().get_self();
                universe.unknown_global(self_value, name)
            })
            .unwrap_or_else(|| Return::Exception(format!("global variable '{}' not found", name)))
    }
}

#[derive(Clone)]
pub struct TrivialGetterMethod {
    pub(crate) field_idx: usize,
}

impl Invoke for TrivialGetterMethod {
    fn invoke(&mut self, _: &mut UniverseAST, args: Vec<Value>) -> Return {
        match args.first().unwrap() {
            Value::Class(cls) => Return::Local(cls.borrow().class().borrow().lookup_field(self.field_idx)),
            Value::Instance(instance) => Return::Local(instance.borrow().lookup_local(self.field_idx)),
            _ => panic!("trivial getter not called on a class/instance?")
        }
    }
}

#[derive(Clone)]
pub struct TrivialSetterMethod {
    pub(crate) field_idx: usize,
}

impl Invoke for TrivialSetterMethod {
    fn invoke(&mut self, _: &mut UniverseAST, args: Vec<Value>) -> Return {
        let val = args.get(1).unwrap();
        match args.first().unwrap() {
            Value::Class(cls) => {
                cls.borrow().class().borrow_mut().assign_field(self.field_idx, val.clone());
                Return::Local(Value::Class(Rc::clone(cls)))
            }
            Value::Instance(instance) => {
                instance.borrow_mut().assign_local(self.field_idx, val.clone());
                Return::Local(Value::Instance(Rc::clone(instance)))
            }
            _ => panic!("trivial getter not called on a class/instance?")
        }
    }
}