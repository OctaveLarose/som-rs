use crate::evaluate::Evaluate;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::method::{Method, MethodKind, MethodKindSpecialized};
use som_gc::debug_assert_valid_semispace_ptr;
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
    fn invoke(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>, nbr_args: usize) -> Return;
}

impl Invoke for Gc<Method> {
    fn invoke(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>, nbr_args: usize) -> Return {
        // println!("--- ...with args: {:?}", &args);

        debug_assert_valid_semispace_ptr!(self);

        match &mut self.kind {
            MethodKind::Defined(method) => {
                // println!("--- Invoking \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
                universe.eval_with_frame(stack_args, method.locals_nbr, nbr_args, method)
            }
            MethodKind::Primitive(func) => {
                // println!("--- Invoking prim \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
                func(universe, stack_args, nbr_args)
            }
            MethodKind::Specialized(specialized_kind) => {
                // println!("--- Invoking specialized method \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
                match specialized_kind {
                    MethodKindSpecialized::ToByDo(to_by_do_node) => to_by_do_node.invoke(universe, stack_args, nbr_args),
                    MethodKindSpecialized::DownToDo(down_to_do_node) => down_to_do_node.invoke(universe, stack_args, nbr_args),
                }
            }
            // since those two trivial methods don't need args, i guess it could be faster to handle them before args are even instantiated...
            MethodKind::TrivialLiteral(trivial_literal) => {
                let _ = Universe::stack_n_last_elems(stack_args, nbr_args);
                trivial_literal.literal.evaluate(universe, stack_args)
            }
            MethodKind::TrivialGlobal(trivial_global) => {
                let _ = Universe::stack_n_last_elems(stack_args, nbr_args);
                trivial_global.evaluate(universe, stack_args)
            }
            MethodKind::TrivialGetter(trivial_getter) => trivial_getter.invoke(universe, stack_args, nbr_args),
            MethodKind::TrivialSetter(trivial_setter) => trivial_setter.invoke(universe, stack_args, nbr_args),
        }
    }
}
