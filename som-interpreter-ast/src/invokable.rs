
use crate::evaluate::Evaluate;
use crate::frame::Frame;
use crate::method::{Method, MethodKind};
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
    fn invoke(&self, universe: &mut UniverseAST, args: Vec<Value>) -> Return;
}

impl Invoke for Method {
    fn invoke(&self, universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        // println!("--- Invoking \"{:1}\" ({:2})", &self.signature, &self.holder.upgrade().unwrap().borrow().name);
        // println!("--- ...with args: {:?}", &args);

        match self.kind() {
            MethodKind::Defined(method) => {
                universe.with_frame(
                    method.locals_nbr,
                    args,
                    |universe| method.evaluate(universe),
                )
            }
            MethodKind::Primitive(func) => func(universe, args),
            MethodKind::While(while_node) => { while_node.invoke(universe, args) }
            MethodKind::If(if_node) => { if_node.invoke(universe, args) }
            MethodKind::IfTrueIfFalse(if_true_if_false_node) => { if_true_if_false_node.invoke(universe, args) },
            MethodKind::ToDo(to_do_node) => { to_do_node.invoke(universe, args) },
            MethodKind::ToByDo(to_by_do_node) => { to_by_do_node.invoke(universe, args) },
            MethodKind::DownToDo(down_to_do_node) => { down_to_do_node.invoke(universe, args) },
            MethodKind::TrivialLiteral(trivial_literal) => { trivial_literal.literal.evaluate(universe) },
            MethodKind::TrivialGlobal(trivial_global) => { trivial_global.evaluate(universe) },
            MethodKind::NotImplemented(name) => { Return::Exception(format!("unimplemented primitive: {}", name)) }
        }
    }
}
