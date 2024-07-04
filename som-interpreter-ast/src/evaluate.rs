use std::cell::RefCell;
use std::rc::Rc;

use som_core::ast;

use crate::block::Block;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;

macro_rules! propagate {
    ($expr:expr) => {
        match $expr {
            Return::Local(value) => value,
            ret => return ret,
        }
    };
}

/// The trait for evaluating AST nodes.
pub trait Evaluate {
    /// Evaluate the node within a given universe.
    fn evaluate(&self, universe: &mut UniverseAST) -> Return;
}

impl Evaluate for ast::Expression {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        match self {
            Self::LocalVarWrite(idx, expr) => {
                // TODO: this doesn't call the fastest path for evaluate, still has to dispatch the right expr even though it's always a var write. potential minor speedup there
                let value = propagate!(expr.evaluate(universe));
                universe.assign_local(*idx, &value);
                Return::Local(value)
            },
            Self::NonLocalVarWrite(scope, idx, expr) => {
                let value = propagate!(expr.evaluate(universe));
                universe.assign_non_local(*idx, *scope, &value);
                Return::Local(value)
            },
            Self::FieldWrite(idx, expr) => {
                let value = propagate!(expr.evaluate(universe));
                universe.assign_field(*idx, &value);
                Return::Local(value)
            },
            Self::ArgWrite(scope, idx, expr) => {
                let value = propagate!(expr.evaluate(universe));
                universe.assign_arg(*idx, *scope, &value);
                Return::Local(value)
            },
            Self::BinaryOp(bin_op) => bin_op.evaluate(universe),
            Self::Block(blk) => blk.evaluate(universe),
            Self::Exit(expr, _scope) => {
                let value = propagate!(expr.evaluate(universe));
                let frame = universe.current_method_frame();
                let has_not_escaped = universe
                    .frames
                    .iter()
                    .rev()
                    .any(|live_frame| Rc::ptr_eq(&live_frame, &frame));
                if has_not_escaped {
                    Return::NonLocal(value, frame)
                } else {
                    // Block has escaped its method frame.
                    let instance = frame.borrow().get_self();
                    let frame = universe.current_frame();
                    let block = match frame.borrow().params.get(0) {
                        Some(Value::Block(b)) => b.clone(),
                        _ => {
                            // Should never happen, because `universe.current_frame()` would
                            // have been equal to `universe.current_method_frame()`.
                            return Return::Exception(format!(
                                "A method frame has escaped itself ??"
                            ));
                        }
                    };
                    universe.escaped_block(instance, block).unwrap_or_else(|| {
                        // TODO: should we call `doesNotUnderstand:` here ?
                        Return::Exception(
                            "A block has escaped and `escapedBlock:` is not defined on receiver"
                                .to_string(),
                        )
                    })
                }
            }
            Self::Literal(literal) => literal.evaluate(universe),
            Self::LocalVarRead(idx) => {
                Return::Local(universe.lookup_local(*idx))
            },
            Self::NonLocalVarRead(scope, idx) => {
                Return::Local(universe.lookup_non_local(*idx, *scope))
            },
            Self::FieldRead(idx) => {
                Return::Local(universe.lookup_field(*idx))
            },
            Self::ArgRead(scope, idx) => {
                Return::Local(universe.lookup_arg(*idx, *scope))
            },
            Self::GlobalRead(name) => universe.lookup_global(name)
                .map(Return::Local)
                .or_else(|| {
                    let frame = universe.current_frame();
                    let self_value = frame.borrow().get_self();
                    universe.unknown_global(self_value, name.as_str())
                })
                .unwrap_or_else(|| Return::Exception(format!("global variable '{}' not found", name))),
            Self::Message(msg) => msg.evaluate(universe),
        }
    }
}

impl Evaluate for ast::BinaryOp {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        let (lhs, invokable) = match self.lhs.as_ref() {
            ast::Expression::GlobalRead(ident) if ident == "super" => {
                let frame = universe.current_frame();
                let lhs = frame.borrow().get_self();
                let holder = frame.borrow().get_method_holder();
                let super_class = match holder.borrow().super_class() {
                    Some(class) => class,
                    None => {
                        return Return::Exception(
                            "`super` used without any superclass available".to_string(),
                        )
                    }
                };
                let invokable = super_class.borrow().lookup_method(&self.op);
                (lhs, invokable)
            }
            lhs => {
                let lhs = propagate!(lhs.evaluate(universe));
                let invokable = lhs.lookup_method(universe, &self.op);
                (lhs, invokable)
            }
        };

        let rhs = propagate!(self.rhs.evaluate(universe));

        // println!(
        //     "invoking {}>>#{}",
        //     lhs.class(universe).borrow().name(),
        //     self.signature
        // );

        if let Some(invokable) = invokable {
            invokable.invoke(universe, vec![lhs, rhs])
        } else {
            universe
                .does_not_understand(lhs.clone(), &self.op, vec![rhs])
                .unwrap_or_else(|| {
                    Return::Exception(format!(
                        "could not find method '{}>>#{}'",
                        lhs.class(universe).borrow().name(),
                        self.op
                    ))
                    // Return::Local(Value::Nil)
                })
        }
    }
}

impl Evaluate for ast::Literal {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        match self {
            Self::Array(array) => {
                let mut output = Vec::with_capacity(array.len());
                for literal in array {
                    let value = propagate!(literal.evaluate(universe));
                    output.push(value);
                }
                Return::Local(Value::Array(Rc::new(RefCell::new(output))))
            }
            Self::Integer(int) => Return::Local(Value::Integer(*int)),
            Self::BigInteger(int) => match int.parse() {
                Ok(value) => Return::Local(Value::BigInteger(value)),
                Err(err) => Return::Exception(err.to_string()),
            },
            Self::Double(double) => Return::Local(Value::Double(*double)),
            Self::Symbol(sym) => Return::Local(Value::Symbol(universe.intern_symbol(sym))),
            Self::String(string) => Return::Local(Value::String(Rc::new(string.clone()))),
        }
    }
}

impl Evaluate for ast::Term {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        self.body.evaluate(universe)
    }
}

impl Evaluate for ast::Block {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        let frame = universe.current_frame();
        // TODO: avoid cloning the whole block's AST.
        Return::Local(Value::Block(Rc::new(Block {
            block: self.clone(),
            frame: frame.clone(),
        })))
    }
}

impl Evaluate for ast::Message {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        let (receiver, invokable) = match self.receiver.as_ref() {
            // ast::Expression::Reference(ident) if ident == "self" => {
            //     let frame = universe.current_frame();
            //     let receiver = frame.borrow().get_self();
            //     let holder = frame.borrow().get_method_holder();
            //     let invokable = holder.borrow().lookup_method(&self.signature);
            //     (receiver, invokable)
            // }
            ast::Expression::GlobalRead(ident) if ident == "super" => {
                let frame = universe.current_frame();
                let receiver = frame.borrow().get_self();
                let holder = frame.borrow().get_method_holder();
                let super_class = match holder.borrow().super_class() {
                    Some(class) => class,
                    None => {
                        return Return::Exception(
                            "`super` used without any superclass available".to_string(),
                        )
                    }
                };
                let invokable = super_class.borrow().lookup_method(&self.signature);
                (receiver, invokable)
            }
            expr => {
                let receiver = propagate!(expr.evaluate(universe));
                let invokable = receiver.lookup_method(universe, &self.signature);
                (receiver, invokable)
            }
        };
        let args = {
            let mut output = Vec::with_capacity(self.values.len() + 1);
            output.push(receiver.clone());
            for expr in &self.values {
                let value = propagate!(expr.evaluate(universe));
                output.push(value);
            }
            output
        };

        // println!(
        //     "invoking {}>>#{} with ({:?})",
        //     receiver.class(universe).borrow().name(),
        //     self.signature,
        //     self.values,
        // );

        let value = match invokable {
            Some(invokable) => invokable.invoke(universe, args),
            None => {
                let mut args = args;
                args.remove(0);
                universe
                    .does_not_understand(receiver.clone(), &self.signature, args)
                    .unwrap_or_else(|| {
                        Return::Exception(format!(
                            "could not find method '{}>>#{}'",
                            receiver.class(universe).borrow().name(),
                            self.signature
                        ))
                        // Return::Local(Value::Nil)
                    })
            }
        };

        value
    }
}

impl Evaluate for ast::Body {
    fn evaluate(&self, universe: &mut UniverseAST) -> Return {
        let mut last_value = Value::Nil;
        for expr in &self.exprs {
            last_value = propagate!(expr.evaluate(universe));
        }
        Return::Local(last_value)
    }
}
