use crate::ast::{
    AstBinaryDispatch, AstBlock, AstBody, AstDispatchNode, AstExpression, AstLiteral, AstMethodDef, AstNAryDispatch, AstSuperMessage, AstTerm,
    AstTernaryDispatch, AstUnaryDispatch, InlinedNode,
};
use crate::gc::VecValue;
use crate::invokable::{Invoke, Return};
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::frame::{Frame, FrameAccess};
use som_gc::debug_assert_valid_semispace_ptr;
use som_gc::gcref::Gc;

/// The trait for evaluating AST nodes.
pub trait Evaluate {
    /// Evaluate the node within a given universe.
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return;
}

impl Evaluate for AstExpression {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        match self {
            Self::LocalVarWrite(idx, expr) => {
                let value = propagate!(expr.evaluate(universe, stack_args));
                universe.current_frame.assign_local(*idx, value);
                Return::Local(value)
            }
            Self::NonLocalVarWrite(scope, idx, expr) => {
                let value = propagate!(expr.evaluate(universe, stack_args));
                Frame::nth_frame_back(&universe.current_frame, *scope).assign_local(*idx, value);
                Return::Local(value)
            }
            Self::FieldWrite(idx, expr) => {
                let value = propagate!(expr.evaluate(universe, stack_args));
                universe.current_frame.assign_field(*idx, &value);
                Return::Local(value)
            }
            Self::ArgWrite(scope, idx, expr) => {
                let value = propagate!(expr.evaluate(universe, stack_args));
                Frame::nth_frame_back(&universe.current_frame, *scope).assign_arg(*idx, value);
                Return::Local(value)
            }
            Self::Block(blk) => blk.evaluate(universe, stack_args),
            Self::LocalExit(expr) => {
                let value = propagate!(expr.evaluate(universe, stack_args));
                Return::NonLocal(value, universe.current_frame)
            }
            Self::NonLocalExit(expr, scope) => {
                debug_assert_ne!(*scope, 0);

                let value = propagate!(expr.evaluate(universe, stack_args));
                let method_frame = Frame::nth_frame_back(&universe.current_frame, *scope);

                let has_not_escaped = {
                    let mut current_frame = universe.current_frame;

                    loop {
                        if current_frame == method_frame {
                            break true;
                        } else if current_frame.is_empty() {
                            break false;
                        } else {
                            current_frame = current_frame.prev_frame;
                        }
                    }
                };

                if has_not_escaped {
                    // the BC interp has to pop all the escaped frames here, we don't (because we chain return nonlocals, exception-style?).
                    Return::NonLocal(value, method_frame)
                } else {
                    // Block has escaped its method frame.
                    let instance = method_frame.get_self();
                    let frame = universe.current_frame;
                    let block = match frame.lookup_argument(0).as_block() {
                        Some(blk) => blk,
                        _ => {
                            // Should never happen, because `universe.current_frame()` would
                            // have been equal to `universe.current_method_frame()`.
                            panic!("A method frame has escaped itself ??");
                        }
                    };
                    universe
                        .escaped_block(stack_args, instance, block)
                        .unwrap_or_else(|| panic!("A block has escaped and `escapedBlock:` is not defined on receiver"))
                }
            }
            Self::Literal(literal) => literal.evaluate(universe, stack_args),
            Self::LocalVarRead(idx) => {
                let local = universe.current_frame.lookup_local(*idx);
                Return::Local(*local)
            }
            Self::NonLocalVarRead(scope, idx) => {
                let non_local = *Frame::nth_frame_back(&universe.current_frame, *scope).lookup_local(*idx);
                Return::Local(non_local)
            }
            Self::FieldRead(idx) => {
                let field = universe.current_frame.lookup_field(*idx);
                Return::Local(field)
            }
            Self::ArgRead(scope, idx) => {
                let arg = *Frame::nth_frame_back(&universe.current_frame, *scope).lookup_argument(*idx);
                Return::Local(arg)
            }
            Self::GlobalRead(name) => universe
                .lookup_global(*name)
                .map(Return::Local)
                .or_else(|| {
                    let frame = universe.current_frame;
                    let self_value = frame.get_self();
                    universe.unknown_global(stack_args, self_value, *name)
                })
                .unwrap_or_else(|| panic!("global not found and unknown_global call failed somehow?")),
            Self::UnaryDispatch(un_op) => un_op.evaluate(universe, stack_args),
            Self::BinaryDispatch(bin_op) => bin_op.evaluate(universe, stack_args),
            Self::TernaryDispatch(ter_op) => ter_op.evaluate(universe, stack_args),
            Self::NAryDispatch(msg) => msg.evaluate(universe, stack_args),
            Self::SuperMessage(msg) => msg.evaluate(universe, stack_args),
            Self::InlinedCall(inlined_node) => match inlined_node.as_mut() {
                InlinedNode::IfInlined(if_inlined) => if_inlined.evaluate(universe, stack_args),
                InlinedNode::IfTrueIfFalseInlined(if_true_if_false_inlined) => if_true_if_false_inlined.evaluate(universe, stack_args),
                InlinedNode::WhileInlined(while_inlined) => while_inlined.evaluate(universe, stack_args),
                InlinedNode::OrInlined(or_inlined) => or_inlined.evaluate(universe, stack_args),
                InlinedNode::AndInlined(and_inlined) => and_inlined.evaluate(universe, stack_args),
                InlinedNode::ToDoInlined(to_do_inlined) => to_do_inlined.evaluate(universe, stack_args),
            },
        }
    }
}

impl Evaluate for AstLiteral {
    fn evaluate(&mut self, universe: &mut Universe, _stack_args: &mut Vec<Value>) -> Return {
        match self {
            Self::Array(array) => {
                // todo: couldn't we precompute those astliterals, really?
                let mut output = Vec::with_capacity(array.0.len());
                for literal in &mut array.0 {
                    let value = propagate!(literal.evaluate(universe, _stack_args));
                    output.push(value);
                }
                Return::Local(Value::Array(universe.gc_interface.alloc(VecValue(output))))
            }
            Self::Integer(int) => Return::Local(Value::Integer(*int)),
            Self::BigInteger(bigint) => Return::Local(Value::BigInteger(*bigint)),
            Self::Double(double) => Return::Local(Value::Double(*double)),
            Self::Symbol(sym) => Return::Local(Value::Symbol(*sym)),
            Self::String(string) => Return::Local(Value::String(*string)),
        }
    }
}

impl Evaluate for AstTerm {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        self.body.evaluate(universe, stack_args)
    }
}

impl Evaluate for Gc<AstBlock> {
    fn evaluate(&mut self, universe: &mut Universe, _stack_args: &mut Vec<Value>) -> Return {
        debug_assert_valid_semispace_ptr!(self);
        let mut block_ptr = universe.gc_interface.request_memory_for_type(size_of::<Block>());
        *block_ptr = Block {
            block: *self,
            frame: universe.current_frame,
        };
        Return::Local(Value::Block(block_ptr))
    }
}

impl AstDispatchNode {
    #[inline(always)]
    fn lookup_and_dispatch(&mut self, nbr_args: usize, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let receiver = *stack_args.iter().nth_back(nbr_args - 1).unwrap();

        let invokable = match &self.inline_cache {
            Some((cached_rcvr_ptr, mut method)) => {
                debug_assert_valid_semispace_ptr!(method);

                if *cached_rcvr_ptr == receiver.class(universe) {
                    // dbg!("cache hit");
                    return method.invoke(universe, stack_args, nbr_args);
                } else {
                    // dbg!("cache miss");
                    receiver.lookup_method(universe, self.signature)
                }
            }
            None => receiver.lookup_method(universe, self.signature),
        };
        // let invokable = receiver.lookup_method(universe, &self.signature);

        match invokable {
            Some(mut invokable) => {
                let receiver_class_ref = receiver.class(universe);
                let invoke_ret = invokable.invoke(universe, stack_args, nbr_args);
                debug_assert_valid_semispace_ptr!(receiver_class_ref);
                debug_assert_valid_semispace_ptr!(invokable);
                self.inline_cache = Some((receiver_class_ref, invokable));
                invoke_ret
            }
            None => {
                let mut args = Universe::stack_n_last_elems(stack_args, nbr_args);
                let receiver = args.remove(0);
                universe.does_not_understand(stack_args, receiver, self.signature, args).unwrap_or_else(|| {
                    panic!(
                        "could not find method '{}>>#{}'",
                        receiver.class(universe).name(),
                        universe.lookup_symbol(self.signature)
                    )
                })
            }
        }
    }
}

impl Evaluate for AstUnaryDispatch {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let receiver = propagate!(self.dispatch_node.receiver.evaluate(universe, stack_args));
        stack_args.push(receiver);
        self.dispatch_node.lookup_and_dispatch(1, universe, stack_args)
    }
}

impl Evaluate for AstBinaryDispatch {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let receiver = propagate!(self.dispatch_node.receiver.evaluate(universe, stack_args));
        stack_args.push(receiver);

        let arg = propagate!(self.arg.evaluate(universe, stack_args));
        stack_args.push(arg);

        self.dispatch_node.lookup_and_dispatch(2, universe, stack_args)
    }
}

impl Evaluate for AstTernaryDispatch {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let receiver = propagate!(self.dispatch_node.receiver.evaluate(universe, stack_args));

        stack_args.push(receiver);

        let arg1 = propagate!(self.arg1.evaluate(universe, stack_args));
        stack_args.push(arg1);

        let arg2 = propagate!(self.arg2.evaluate(universe, stack_args));
        stack_args.push(arg2);

        self.dispatch_node.lookup_and_dispatch(3, universe, stack_args)
    }
}

impl Evaluate for AstNAryDispatch {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let receiver = propagate!(self.dispatch_node.receiver.evaluate(universe, stack_args));

        stack_args.push(receiver);

        for expr in &mut self.values {
            let value = propagate!(expr.evaluate(universe, stack_args));
            stack_args.push(value);
        }

        debug_assert!(
            self.values.len() > 2,
            "should be a specialized unary/binary/ternary node, not a generic N-ary node"
        );

        self.dispatch_node.lookup_and_dispatch(self.values.len() + 1, universe, stack_args)
    }
}

impl Evaluate for AstSuperMessage {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let invokable = self.super_class.lookup_method(self.signature);
        let receiver = universe.current_frame.get_self();
        stack_args.push(receiver);

        for expr in &mut self.values {
            let value = propagate!(expr.evaluate(universe, stack_args));
            stack_args.push(value);
        }

        match invokable {
            Some(mut invokable) => invokable.invoke(universe, stack_args, self.values.len() + 1),
            None => {
                let mut args = Universe::stack_n_last_elems(stack_args, self.values.len() + 1);
                let receiver = args.remove(0);
                universe.does_not_understand(stack_args, receiver, self.signature, args).unwrap_or_else(|| {
                    panic!(
                        "could not find method '{}>>#{}'",
                        receiver.class(universe).name(),
                        universe.lookup_symbol(self.signature)
                    )
                    // Return::Local(Value::Nil)
                })
            }
        }
    }
}

impl Evaluate for AstBody {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        let mut last_value = Value::NIL;
        for expr in &mut self.exprs {
            last_value = propagate!(expr.evaluate(universe, stack_args));
        }
        Return::Local(last_value)
    }
}

impl Evaluate for AstMethodDef {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        // HACK: we want to hold on to a reference to the current frame at that point in time.
        // We can't copy/clone the pointer, because we want to have a reference to that pointer in case moving GC moves it.
        // And we can't hold onto an immutable ref to universe while passing a mutable ref universe to `self.body.evaluate` lower down. Hence this hack.
        // Not sure how to better solve that one, to be honest.
        let current_frame = unsafe { &*(&universe.current_frame as *const Gc<Frame>) };

        #[cfg(not(feature = "inlining-disabled"))]
        match self.body.evaluate(universe, stack_args) {
            Return::NonLocal(value, frame) => {
                debug_assert_valid_semispace_ptr!(frame);
                debug_assert_valid_semispace_ptr!(current_frame);
                if *current_frame == frame {
                    Return::Local(value)
                } else {
                    Return::NonLocal(value, frame)
                }
            }
            Return::Local(_) => Return::Local(current_frame.get_self()),
        }

        #[cfg(feature = "inlining-disabled")]
        loop {
            match self.body.evaluate(universe, stack_args) {
                Return::NonLocal(value, frame) => {
                    if *current_frame == frame {
                        break Return::Local(value);
                    } else {
                        break Return::NonLocal(value, frame);
                    }
                }
                Return::Local(_) => break Return::Local(current_frame.get_self()),
                Return::Restart => continue,
            }
        }
    }
}

impl Evaluate for Gc<Block> {
    fn evaluate(&mut self, universe: &mut Universe, stack_args: &mut Vec<Value>) -> Return {
        debug_assert_valid_semispace_ptr!(self.block);
        debug_assert_valid_semispace_ptr!(self);

        self.block.body.evaluate(universe, stack_args)
    }
}
