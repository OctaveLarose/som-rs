
use crate::evaluate::Evaluate;
use crate::frame::Frame;
use crate::method::{Method, MethodKind, MethodKindSpecialized};
use crate::universe::UniverseAST;
use crate::value::Value;
use crate::SOMRef;

/// Represents the kinds of possible returns from an invocation.
#[derive(Debug)]
pub enum Return {
    /// A local return, the value is for the immediate caller.
    Local(Value),
    /// A non-local return, the value is for the parent of the referenced stack frame.
    NonLocal(Value, SOMRef<Frame>),
    /// An exception, expected to bubble all the way up.
    Exception(String),
    /// A request to restart execution from the top of the closest body.
    Restart,
}

/// The trait for invoking methods and primitives.
pub trait Invoke {
    /// Invoke within the given universe and with the given arguments.
    fn invoke(&mut self, universe: &mut UniverseAST, args: Vec<Value>) -> Return;
}

impl Invoke for Method {
    fn invoke(&mut self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        // println!("--- Invoking \"{:1}\" ({:2})", &self.signature, &self.holder.upgrade().unwrap().borrow().name);
        // println!("--- ...with args: {:?}", &args);

        match &mut self.kind {
            MethodKind::Defined(method) => {
                universe.with_frame(
                    method.locals_nbr,
                    args,
                    |universe| method.evaluate(universe),
                )
            }
            MethodKind::Primitive(func) => func(universe, args),
            MethodKind::Specialized(specialized_kind) => {
                match specialized_kind {
                    MethodKindSpecialized::While(while_node) => { while_node.invoke(universe, args) }
                    MethodKindSpecialized::If(if_node) => { if_node.invoke(universe, args) }
                    MethodKindSpecialized::IfTrueIfFalse(if_true_if_false_node) => { if_true_if_false_node.invoke(universe, args) },
                    MethodKindSpecialized::ToDo(to_do_node) => { to_do_node.invoke(universe, args) },
                    MethodKindSpecialized::ToByDo(to_by_do_node) => { to_by_do_node.invoke(universe, args) },
                    MethodKindSpecialized::DownToDo(down_to_do_node) => { down_to_do_node.invoke(universe, args) },
                }
            },
            // since those two trivial methods don't need args, i guess it could be faster to handle them before args are even instantiated... probably not that useful though
            MethodKind::TrivialLiteral(trivial_literal) => { trivial_literal.literal.evaluate(universe) },
            MethodKind::TrivialGlobal(trivial_global) => { trivial_global.evaluate(universe) },
            MethodKind::TrivialGetter(trivial_getter) => { trivial_getter.invoke(universe, args) },
            MethodKind::TrivialSetter(trivial_setter) => { trivial_setter.invoke(universe, args) },
            MethodKind::NotImplemented(name) => { Return::Exception(format!("unimplemented primitive: {}", name)) }
        }
    }
}
