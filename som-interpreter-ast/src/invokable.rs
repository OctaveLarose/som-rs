use crate::evaluate::Evaluate;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::method::{Method, MethodKind, MethodKindSpecialized};
use som_gc::gcref::Gc;

/// Represents the kinds of possible returns from an invocation.
#[derive(Debug)]
pub enum Return {
    /// A local return, the value is for the immediate caller.
    Local(Value),
    /// A non-local return, the value is for the parent of the referenced stack frame.
    /// Not well named: as opposed to in our other interpreters, here NonLocal means "any return that exits the scope", so it can be a regular, "local" return (by going back one frame).
    NonLocal(Value, Gc<Frame>),
    #[cfg(feature = "inlining-disabled")]
    /// A request to restart execution from the top of the closest body.
    Restart,
}

/// The trait for invoking methods and primitives.
pub trait Invoke {
    /// Invoke within the given universe and with the given arguments.
    fn invoke(&mut self, universe: &mut Universe, nbr_nbr: usize) -> Return;
}

impl Invoke for Method {
    fn invoke(&mut self, universe: &mut Universe, nbr_args: usize) -> Return {
        // println!("--- ...with args: {:?}", &args);

        match &mut self.kind {
            MethodKind::Defined(method) => {
                let args = universe.stack_n_last_elems(nbr_args);

                // println!("--- Invoking \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
                universe.with_frame(method.locals_nbr, args, |universe| method.evaluate(universe))
            }
            MethodKind::Primitive(func) => {
                let args = universe.stack_n_last_elems(nbr_args);

                // println!("--- Invoking prim \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
                func(universe, args)
            }
            MethodKind::Specialized(specialized_kind) => {
                // println!("--- Invoking specialized method \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
                match specialized_kind {
                    MethodKindSpecialized::While(while_node) => while_node.invoke(universe, nbr_args),
                    MethodKindSpecialized::If(if_node) => if_node.invoke(universe, nbr_args),
                    MethodKindSpecialized::IfTrueIfFalse(if_true_if_false_node) => if_true_if_false_node.invoke(universe, nbr_args),
                    MethodKindSpecialized::ToDo(to_do_node) => to_do_node.invoke(universe, nbr_args),
                    MethodKindSpecialized::ToByDo(to_by_do_node) => to_by_do_node.invoke(universe, nbr_args),
                    MethodKindSpecialized::DownToDo(down_to_do_node) => down_to_do_node.invoke(universe, nbr_args),
                }
            }
            // since those two trivial methods don't need args, i guess it could be faster to handle them before args are even instantiated...
            MethodKind::TrivialLiteral(trivial_literal) => {
                let _ = universe.stack_n_last_elems(nbr_args);
                trivial_literal.literal.evaluate(universe)
            }
            MethodKind::TrivialGlobal(trivial_global) => {
                let _ = universe.stack_n_last_elems(nbr_args);
                trivial_global.evaluate(universe)
            }
            MethodKind::TrivialGetter(trivial_getter) => trivial_getter.invoke(universe, nbr_args),
            MethodKind::TrivialSetter(trivial_setter) => trivial_setter.invoke(universe, nbr_args),
        }
    }
}
