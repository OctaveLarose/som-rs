use som_core::ast;
use som_core::ast::MethodBody;

use crate::block::Block;
use crate::evaluate::Evaluate;
use crate::frame::Frame;
use crate::method::{Method, MethodKind};
use crate::universe::Universe;
use crate::value::Value;

/// Represents the kinds of possible returns from an invocation.
#[derive(Debug)]
pub enum Return {
    /// A local return, the value is for the immediate caller.
    Local(Value),
    /// A non-local return, the value is for the parent of the referenced stack frame.
    NonLocal(Value, *mut Frame),
    /// An exception, expected to bubble all the way up.
    Exception(String),
    /// A request to restart execution from the top of the closest body.
    Restart,
}

/// The trait for invoking methods and primitives.
pub trait Invoke {
    /// Invoke within the given universe and with the given arguments.
    fn invoke(&mut self, universe: &mut Universe, args: Vec<Value>) -> Return;
}

impl Invoke for Method {
    fn invoke(&mut self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let output = match &mut self.kind {
            MethodKind::Defined(method) => {
                let nbr_params = args.len();

                let (self_value, params) = {
                    let mut iter = args.into_iter();
                    let receiver = match iter.next() {
                        Some(receiver) => receiver,
                        None => {
                            return Return::Exception("missing receiver for invocation".to_string());
                        }
                    };
                    (receiver, iter.collect::<Vec<_>>())
                };
                // let holder = match self.holder.upgrade() {
                //     Some(holder) => holder,
                //     None => {
                //         return Return::Exception(
                //             "cannot invoke this method because its holder has been collected"
                //                 .to_string(),
                //         );
                //     }
                // };
                let nbr_locals = match &method.body {
                    MethodBody::Body { locals_nbr, .. } => *locals_nbr,
                    MethodBody::Primitive => unreachable!()
                };

                universe.with_frame(
                    self_value,
                    nbr_locals,
                    nbr_params,
                    |universe| method.invoke(universe, params),
                )
            }
            MethodKind::Primitive(func) => func(universe, args),
            MethodKind::WhileInlined(while_node) => { while_node.invoke(universe, args) }
            MethodKind::IfInlined(if_node) => { if_node.invoke(universe, args) }
            MethodKind::IfTrueIfFalseInlined(if_true_if_false_node) => { if_true_if_false_node.invoke(universe, args) },
            MethodKind::NotImplemented(name) => {
                Return::Exception(format!("unimplemented primitive: {}", name))
            }
        };
        // println!("...exiting {:}.", self.signature);
        match output {
            // Return::Exception(msg) => Return::Exception(format!(
            //     "from {}>>#{}\n{}",
            //     self.holder().borrow().name(),
            //     self.signature(),
            //     msg,
            // )),
            output => output,
        }
    }
}

impl Invoke for ast::GenericMethodDef {
    fn invoke(&mut self, universe: &mut Universe, args: Vec<Value>) -> Return {
        let current_frame = universe.current_frame().clone();
        unsafe {
            match &self.kind {
                ast::MethodKind::Unary => {}
                ast::MethodKind::Positional { .. } => (*current_frame).params.extend(args),
                ast::MethodKind::Operator { .. } => {
                    let rhs_value = match args.into_iter().next() {
                        Some(value) => value,
                        None => {
                            // This should never happen in theory (the parser would have caught the missing rhs).
                            return Return::Exception(format!(
                                "no right-hand side for operator call ?"
                            ));
                        }
                    };
                    (*current_frame).params.push(rhs_value);
                }
            }
            match &mut self.body {
                ast::MethodBody::Body { body, .. } => {
                    // (*current_frame)
                    //     .bindings
                    //     .extend(locals.iter().cloned().zip(std::iter::repeat(Value::Nil)));
                    loop {
                        match body.evaluate(universe) {
                            Return::NonLocal(value, frame) => {
                                if std::ptr::eq(current_frame, frame) {
                                    break Return::Local(value);
                                } else {
                                    break Return::NonLocal(value, frame);
                                }
                            }
                            Return::Local(_) => break Return::Local((*current_frame).get_self()),
                            Return::Exception(msg) => break Return::Exception(msg),
                            Return::Restart => continue,
                        }
                    }
                }
                ast::MethodBody::Primitive => Return::Exception(format!(
                    "unimplemented primitive: {}>>#{}",
                    (*current_frame)
                        .get_self()
                        .class(universe)
                        .borrow()
                        .name(),
                    self.signature,
                )),
            }
        }
    }
}

impl Invoke for Block {
    fn invoke(&mut self, universe: &mut Universe, args: Vec<Value>) -> Return {
        unsafe {
            let current_frame = universe.current_frame();
            (*current_frame).params.extend(args);
            self.block.body.evaluate(universe)
        }
    }
}
