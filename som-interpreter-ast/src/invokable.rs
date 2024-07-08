use som_core::ast::MethodBody;

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
                let nbr_locals = match &method.body {
                    MethodBody::Body { locals_nbr, .. } => *locals_nbr,
                    MethodBody::Primitive => unreachable!()
                };

                universe.with_frame(
                    nbr_locals,
                    args,
                    |universe| method.evaluate(universe),
                )
            }
            MethodKind::Primitive(func) => func(universe, args),
            MethodKind::WhileInlined(while_node) => { while_node.invoke(universe, args) }
            MethodKind::IfInlined(if_node) => { if_node.invoke(universe, args) }
            MethodKind::IfTrueIfFalseInlined(if_true_if_false_node) => { if_true_if_false_node.invoke(universe, args) },
            MethodKind::ToDoInlined(to_do_node) => { to_do_node.invoke(universe, args) },
            MethodKind::ToByDoInlined(to_by_do_node) => { to_by_do_node.invoke(universe, args) },
            MethodKind::DownToDoInlined(down_to_do_node) => { down_to_do_node.invoke(universe, args) },
            MethodKind::NotImplemented(name) => { Return::Exception(format!("unimplemented primitive: {}", name)) }
        }
    }
}
