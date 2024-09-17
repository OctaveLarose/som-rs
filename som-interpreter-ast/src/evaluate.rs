use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{AstBinaryOp, AstBlock, AstBody, AstExpression, AstMessage, AstMethodDef, AstSuperMessage, AstTerm, InlinedNode};
use som_core::ast;

use crate::block::Block;
use crate::invokable::{Invoke, Return};
use crate::universe::UniverseAST;
use crate::value::Value;

/// The trait for evaluating AST nodes.
pub trait Evaluate {
    /// Evaluate the node within a given universe.
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return;
}

impl Evaluate for AstExpression {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        match self {
            Self::LocalVarWrite(idx, expr) => {
                // TODO: this doesn't call the fastest path for evaluate, still has to dispatch the right expr even though it's always a var write. potential minor speedup there
                let value = propagate!(expr.evaluate(universe));
                universe.assign_local(*idx, &value);
                Return::Local(value)
            }
            Self::NonLocalVarWrite(scope, idx, expr) => {
                let value = propagate!(expr.evaluate(universe));
                universe.assign_non_local(*idx, *scope, &value);
                Return::Local(value)
            }
            Self::FieldWrite(idx, expr) => {
                let value = propagate!(expr.evaluate(universe));
                universe.assign_field(*idx, &value);
                Return::Local(value)
            }
            Self::ArgWrite(scope, idx, expr) => {
                let value = propagate!(expr.evaluate(universe));
                universe.assign_arg(*idx, *scope, &value);
                Return::Local(value)
            }
            Self::BinaryOp(bin_op) => bin_op.evaluate(universe),
            Self::Block(blk) => blk.evaluate(universe),
            Self::LocalExit(expr) => {
                let value = propagate!(expr.evaluate(universe));
                Return::NonLocal(value, universe.current_frame().clone())  // not well named - Return::NonLocal means "exits the scope", so it can be a regular, local return. 
            }
            Self::NonLocalExit(expr, scope) => {
                debug_assert_ne!(*scope, 0);

                let value = propagate!(expr.evaluate(universe));
                let method_frame = universe.current_frame().borrow().nth_frame_back(*scope);
                let has_not_escaped = universe
                    .frames
                    .iter()
                    .rev()
                    .any(|live_frame| Rc::ptr_eq(live_frame, &method_frame));

                if has_not_escaped {
                    // the BC interp has to pop all the escaped frames here, we don't (because we chain return nonlocals, exception-style?).
                    Return::NonLocal(value, method_frame)
                } else {
                    // Block has escaped its method frame.
                    let instance = method_frame.borrow().get_self();
                    let frame = universe.current_frame();
                    let block = match frame.borrow().params.first() {
                        Some(Value::Block(b)) => b.clone(),
                        _ => {
                            // Should never happen, because `universe.current_frame()` would
                            // have been equal to `universe.current_method_frame()`.
                            return Return::Exception("A method frame has escaped itself ??".to_string());
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
            }
            Self::NonLocalVarRead(scope, idx) => {
                Return::Local(universe.lookup_non_local(*idx, *scope))
            }
            Self::FieldRead(idx) => {
                Return::Local(universe.lookup_field(*idx))
            }
            Self::ArgRead(scope, idx) => {
                Return::Local(universe.lookup_arg(*idx, *scope))
            }
            Self::GlobalRead(name) =>
                match name.as_str() {
                    "super" => Return::Local(universe.current_frame().borrow().get_self()),
                    _ => universe.lookup_global(name.as_str())
                        .map(Return::Local)
                        .or_else(|| {
                            let frame = universe.current_frame();
                            let self_value = frame.borrow().get_self();
                            universe.unknown_global(self_value, name.as_str())
                        })
                        .unwrap_or_else(|| Return::Exception(format!("global variable '{}' not found", name)))
                },
            Self::Message(msg) => msg.evaluate(universe),
            Self::SuperMessage(msg) => msg.evaluate(universe),
            Self::InlinedCall(inlined_node) => {
                match inlined_node.as_mut() {
                    InlinedNode::IfInlined(if_inlined) => if_inlined.evaluate(universe),
                    InlinedNode::IfTrueIfFalseInlined(if_true_if_false_inlined) => if_true_if_false_inlined.evaluate(universe),
                    InlinedNode::WhileInlined(while_inlined) => while_inlined.evaluate(universe),
                    InlinedNode::OrInlined(or_inlined) => or_inlined.evaluate(universe),
                    InlinedNode::AndInlined(and_inlined) => and_inlined.evaluate(universe)
                }
            }
        }
    }
}

impl Evaluate for AstBinaryOp {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let (lhs, invokable) = match &mut self.lhs {
            AstExpression::GlobalRead(ident) if ident == "super" => {
                let frame = universe.current_frame();
                let lhs = frame.borrow().get_self();
                let holder = lhs.class(universe);
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
            invokable.borrow_mut().invoke(universe, vec![lhs, rhs])
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
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
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

impl Evaluate for AstTerm {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        self.body.evaluate(universe)
    }
}

impl Evaluate for Rc<RefCell<AstBlock>> {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        Return::Local(Value::Block(Rc::new(RefCell::new(Block {
            block: Rc::clone(self),
            frame: Rc::clone(universe.current_frame()),
        }))))
    }
}

impl Evaluate for AstMessage {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let receiver = propagate!(self.receiver.evaluate(universe));
        let invokable = receiver.lookup_method(universe, &self.signature);
        let args = {
            let mut output = Vec::with_capacity(self.values.len() + 1);
            output.push(receiver.clone());
            for expr in &mut self.values {
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

        // println!("invoking {}>>#{}", receiver.class(universe).borrow().name(), self.signature);

        match invokable {
            Some(invokable) => invokable.borrow_mut().invoke(universe, args),
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
        }
    }
}

impl Evaluate for AstSuperMessage {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let super_class = match universe.lookup_global(&self.receiver_name) {
            Some(Value::Class(cls)) => {
                match self.is_static_class_call {
                    true => cls.borrow().class(),
                    false => Rc::clone(&cls),
                }
            }
            Some(_) => return Return::Exception(format!("superclass name \"{}\" is not associated with a super class?", &self.receiver_name)),
            None => return Return::Exception(format!("superclass \"{}\" does not exist?", &self.receiver_name))
        };

        let invokable = super_class.borrow().lookup_method(&self.signature);
        let receiver = universe.current_frame().borrow().get_self();
        let args = {
            let mut output = Vec::with_capacity(self.values.len() + 1);
            output.push(receiver.clone());
            for expr in &mut self.values {
                let value = propagate!(expr.evaluate(universe));
                output.push(value);
            }
            output
        };

        let value = match invokable {
            Some(invokable) => invokable.borrow_mut().invoke(universe, args),
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


impl Evaluate for AstBody {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let mut last_value = Value::Nil;
        for expr in &mut self.exprs {
            last_value = propagate!(expr.evaluate(universe));
        }
        Return::Local(last_value)
    }
}

impl Evaluate for AstMethodDef {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        let current_frame = universe.current_frame().clone();

        loop {
            match self.body.evaluate(universe) {
                Return::NonLocal(value, frame) => {
                    if Rc::ptr_eq(&current_frame, &frame) {
                        break Return::Local(value);
                    } else {
                        break Return::NonLocal(value, frame);
                    }
                }
                Return::Local(_) => break Return::Local(current_frame.borrow().get_self()),
                Return::Exception(msg) => break Return::Exception(msg),
                Return::Restart => continue,
            }
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&mut self, universe: &mut UniverseAST) -> Return {
        self.block.borrow_mut().body.evaluate(universe)
    }
}