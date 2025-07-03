use crate::evaluate::Evaluate;
use crate::universe::{GlobalValueStack, Universe};
use crate::value::Value;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::method::{Method, MethodKind};
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
    fn invoke(&mut self, universe: &mut Universe, value_stack: &mut GlobalValueStack, nbr_args: usize) -> Return;
}

impl Invoke for Gc<Method> {
    fn invoke(&mut self, universe: &mut Universe, value_stack: &mut GlobalValueStack, nbr_args: usize) -> Return {
        debug_assert_valid_semispace_ptr!(self);

        //match &self.kind {
        //   MethodKind::Defined(_method) => {
        //       println!("--- Invoking \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
        //   }
        //   MethodKind::Primitive(_func) => {
        //       println!("--- Invoking prim \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
        //   }
        //   // MethodKind::TrivialGetter(_g) => {
        //   //     println!("--- Invoking trivial getter \"{:1}\" ({:2})", &self.signature, &self.holder.class().name);
        //   // }
        //   _ => {
        //       println!("--- Invoking some trivial method (TODO)")
        //   }
        //}

        match &mut self.kind {
            MethodKind::Defined(method) => universe.eval_with_frame(value_stack, method.locals_nbr, nbr_args, method),
            MethodKind::Primitive(func) => func(universe, value_stack, nbr_args),
            // since those two trivial methods don't need args, i guess it could be faster to handle them before args are even instantiated...
            MethodKind::TrivialLiteral(trivial_literal) => {
                value_stack.remove_n_last(nbr_args);
                trivial_literal.literal.evaluate(universe, value_stack)
            }
            MethodKind::TrivialGlobal(trivial_global) => {
                value_stack.remove_n_last(nbr_args);
                trivial_global.evaluate(universe, value_stack)
            }
            MethodKind::TrivialGetter(trivial_getter) => trivial_getter.invoke(universe, value_stack, nbr_args),
            MethodKind::TrivialSetter(trivial_setter) => trivial_setter.invoke(universe, value_stack, nbr_args),
        }
    }
}
