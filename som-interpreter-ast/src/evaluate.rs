use std::cell::RefCell;
use std::rc::Rc;

use som_core::ast;

use crate::block::Block;
use crate::invokable::{Invoke, Return};
use crate::universe::Universe;
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
    fn evaluate(&mut self, universe: &mut Universe) -> Return;
}

impl Evaluate for ast::Expression {
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
        unsafe {
            match self {
                Self::LocalVarWrite(idx, expr) => {
                    // TODO: this doesn't call the fastest path for evaluate, still has to dispatch the right expr even though it's always a var write. potential minor speedup there
                    let value = propagate!(expr.evaluate(universe));
                    universe.assign_local(*idx, &value)
                        .map(|_| Return::Local(value))
                        .unwrap_or_else(||
                            Return::Exception(format!("local var write: idx '{}' not found", idx)))
                }
                Self::NonLocalVarWrite(scope, idx, expr) => {
                    let value = propagate!(expr.evaluate(universe));
                    universe.assign_non_local(*idx, *scope, &value)
                        .map(|_| Return::Local(value))
                        .unwrap_or_else(||
                            Return::Exception(format!("non local var write: idx '{}' not found", idx)))
                }
                Self::FieldWrite(idx, expr) => {
                    let value = propagate!(expr.evaluate(universe));
                    universe.assign_field(*idx, &value)
                        .map(|_| Return::Local(value))
                        .unwrap_or_else(||
                            Return::Exception(format!("field write: idx '{}' not found", idx)))
                }
                Self::ArgWrite(scope, idx, expr) => {
                    let value = propagate!(expr.evaluate(universe));
                    universe.assign_arg(*idx, *scope, &value)
                        .map(|_| Return::Local(value))
                        .unwrap_or_else(||
                            Return::Exception(format!("arg write: idx '{}', scope '{}' not found", idx, scope)))
                }
                Self::GlobalWrite(name, expr) => {
                    let value = propagate!(expr.evaluate(universe));
                    universe.assign_global(&name, &value)
                        .map(|_| Return::Local(value))
                        .unwrap_or_else(|| {
                            Return::Exception(format!("global variable '{}' not found to assign to", name))
                        })
                }
                Self::BinaryOp(bin_op) => bin_op.evaluate(universe),
                Self::Block(blk) => blk.evaluate(universe),
                Self::Exit(expr, _scope) => {
                    let value = propagate!(expr.evaluate(universe));
                    let frame = universe.current_method_frame();
                    let has_not_escaped = universe
                        .frames
                        .iter()
                        .rev()
                        .any(|live_frame| std::ptr::eq(live_frame.as_ptr(), frame));
                    if has_not_escaped {
                        Return::NonLocal(value, frame)
                    } else {
                        // Block has escaped its method frame.
                        let instance = (*frame).get_self();
                        let frame = universe.current_frame();
                        let block = match (*frame).params.get(0) {
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
                    universe.lookup_local(*idx)
                        .map(|v| Return::Local(v.clone()))
                        .unwrap_or_else(||
                            Return::Exception(format!("local var read: idx '{}' not found", idx)))
                }
                Self::NonLocalVarRead(scope, idx) => {
                    universe.lookup_non_local(*idx, *scope)
                        .map(Return::Local)
                        .unwrap_or_else(|| {
                            Return::Exception(format!("non local var read: idx '{}' not found", idx))
                        })
                }
                Self::FieldRead(idx) => {
                    universe.lookup_field(*idx)
                        .map(Return::Local)
                        .unwrap_or_else(|| {
                            Return::Exception(format!("field read: idx '{}' not found", idx))
                        })
                }
                Self::ArgRead(scope, idx) => {
                    universe.lookup_arg(*idx, *scope)
                        .map(Return::Local)
                        .unwrap_or_else(|| {
                            Return::Exception(format!("arg read: idx '{}', scope '{}' not found", idx, scope))
                        })
                }
                Self::GlobalRead(name) => universe.lookup_global(&name)
                    .map(Return::Local)
                    .or_else(|| {
                        let frame = universe.current_frame();
                        let self_value = (*frame).get_self();
                        universe.unknown_global(self_value, name.as_str())
                    })
                    .unwrap_or_else(|| Return::Exception(format!("global variable '{}' not found", name))),
                Self::Message(msg) => msg.evaluate(universe),
            }
        }
    }
}

impl Evaluate for ast::BinaryOp {
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
        let (lhs, invokable) = match self.lhs.as_mut() {
            ast::Expression::GlobalRead(ident) if ident == "super" => unsafe {
                let frame = universe.current_frame();
                let lhs = (*frame).get_self();
                let holder = (*frame).get_method_holder();
                let super_class = match holder.borrow().super_class() {
                    Some(class) => class,
                    None => {
                        return Return::Exception(
                            "`super` used without any superclass available".to_string(),
                        );
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
            unsafe { (*invokable).invoke(universe, vec![lhs, rhs]) }
        } else {
            unsafe {
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
}

impl Evaluate for ast::Literal {
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
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
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
        self.body.evaluate(universe)
    }
}

impl Evaluate for ast::Block {
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
        let frame = universe.current_frame();
        // TODO: avoid cloning the whole block's AST.
        Return::Local(Value::Block(Rc::new(RefCell::new(Block {
            block: self.clone(),
            frame,
        }))))
    }
}

impl Evaluate for ast::MessageCall {
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
        // print!("Invoking (generic) {:?}", &generic_message.signature);
        // dbg!("generic messagin");
        let (receiver, invokable) = match self.message.receiver.as_mut() {
            ast::Expression::GlobalRead(ident) if ident == "super" => unsafe {
                let frame = universe.current_frame();
                let receiver = (*frame).get_self();
                let holder = (*frame).get_method_holder();
                let super_class = match holder.borrow().super_class() {
                    Some(class) => class,
                    None => {
                        return Return::Exception(
                            "`super` used without any superclass available".to_string(),
                        );
                    }
                };
                let invokable = super_class.borrow().lookup_method(&self.message.signature);
                (receiver, invokable)
            }
            expr => {
                let receiver = propagate!(expr.evaluate(universe));
                let rcvr_name = receiver.class(universe).borrow().name.clone();

                let invokable = match self.is_generic_call {
                    true => {
                        match self.cache_lookup(rcvr_name) {
                            Some(method) => Some(method as *mut crate::method::Method),
                            None => receiver.lookup_method(universe, &self.message.signature)
                        }
                    },
                    false => {
                        receiver.lookup_method(universe, &self.message.signature)
                    }
                };

                (receiver, invokable)
            }
        };
        
        // println!(" on {:?}", receiver);
        let args = {
            let mut output = Vec::with_capacity(self.message.values.len() + 1);
            output.push(receiver.clone());
            for expr in &mut self.message.values {
                let value = propagate!(expr.evaluate(universe));
                output.push(value);
            }
            output
        };

        match invokable {
            Some(invokable) => {
                let ret = unsafe { (*invokable).invoke(universe, args) };
                let rcvr_name = receiver.class(universe).borrow().name.clone();
                self.cache_add_maybe_turn_megamorphic((rcvr_name, invokable as usize));

                ret
            }
            None => unsafe {
                let mut args = args;
                args.remove(0);
                universe
                    .does_not_understand(receiver.clone(), &self.message.signature, args)
                    .unwrap_or_else(|| {
                        Return::Exception(format!(
                            "could not find method '{}>>#{}'",
                            receiver.class(universe).borrow().name(),
                            self.message.signature
                        ))
                        // Return::Local(Value::Nil)
                    })
            }
        }
    }
}

impl Evaluate for ast::Body {
    fn evaluate(&mut self, universe: &mut Universe) -> Return {
        let mut last_value = Value::Nil;
        for expr in &mut self.exprs {
            last_value = propagate!(expr.evaluate(universe));
        }
        Return::Local(last_value)
    }
}
