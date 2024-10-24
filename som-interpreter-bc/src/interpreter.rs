use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::frame::Frame;
use crate::gc::gc_interface::{GCInterface, GCRef};
use crate::instance::InstanceAccess;
use crate::method::{Method, MethodKind};
use crate::universe::Universe;
use crate::value::Value;
use anyhow::Context;
use som_core::bytecode::Bytecode;
use som_core::interner::Interned;
use std::time::Instant;

macro_rules! send {
    ($interp:expr, $universe:expr, $frame:expr, $lit_idx:expr, $nb_params:expr) => {{
        let Literal::Symbol(symbol) = $frame.to_obj().lookup_constant($lit_idx as usize) else { unreachable!() };
        let nb_params = match $nb_params {
            Some(v) => v,
            None => {
                let signature = $universe.lookup_symbol(symbol);
                nb_params(signature)
            }
        };
        let method = {
            // dbg!($universe.lookup_symbol(symbol));
            let receiver = $interp.current_frame.to_obj().stack_nth_back(nb_params);
            let receiver_class = receiver.class($universe);
            resolve_method($frame, &receiver_class, symbol, $interp.bytecode_idx)
        };
        do_send($interp, $universe, method, symbol, nb_params as usize);
    }};
}

macro_rules! super_send {
    ($interp:expr, $universe:expr, $frame:expr, $lit_idx:expr, $nb_params:expr) => {{
        let Literal::Symbol(symbol) = $frame.to_obj().lookup_constant($lit_idx as usize) else { unreachable!() };
        let nb_params = match $nb_params {
            Some(v) => v,
            None => {
                let signature = $universe.lookup_symbol(symbol);
                nb_params(signature)
            }
        };
        let method = {
            // let method_with_holder = $frame.borrow().get_holding_method();
            let holder = $frame.to_obj().get_method_holder();
            // dbg!(&holder);
            let super_class = holder.borrow().super_class().unwrap();
            // dbg!(&super_class);
            resolve_method($frame, &super_class, symbol, $interp.bytecode_idx)
        };
        do_send($interp, $universe, method, symbol, nb_params as usize);
    }};
}

pub struct Interpreter {
    /// The time record of the interpreter's creation.
    pub start_time: Instant,
    /// The current bytecode index.
    pub bytecode_idx: usize,
    /// The current frame.
    pub current_frame: GCRef<Frame>,
    /// Pointer to the frame's bytecodes, to not have to read them from the frame directly
    pub current_bytecodes: *const Vec<Bytecode>,
}

impl Interpreter {
    pub fn new(base_frame: GCRef<Frame>) -> Self {
        Self {
            start_time: Instant::now(),
            bytecode_idx: 0,
            current_frame: base_frame,
            current_bytecodes: base_frame.to_obj().bytecodes,
        }
    }

    /// Creates and allocates a new frame corresponding to a method.
    /// nbr_args is the number of arguments, including the self value, which it takes from the previous frame.
    pub fn push_method_frame(&mut self, method: GCRef<Method>, nbr_args: usize, mutator: &mut GCInterface) -> GCRef<Frame> {
        let args = self.current_frame.to_obj().stack_n_last_elements(nbr_args);

        let frame_ptr = Frame::alloc_from_method(method, args, self.current_frame, mutator);

        self.bytecode_idx = 0;
        self.current_bytecodes = frame_ptr.to_obj().bytecodes;
        self.current_frame = frame_ptr;
        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method, with arguments provided.
    /// Used in primitives and
    pub fn push_method_frame_with_args(&mut self, method: GCRef<Method>, args: &[Value], mutator: &mut GCInterface) -> GCRef<Frame> {
        let frame_ptr = Frame::alloc_from_method(method, args, self.current_frame, mutator);

        self.bytecode_idx = 0;
        self.current_bytecodes = frame_ptr.to_obj().bytecodes;
        self.current_frame = frame_ptr;

        frame_ptr
    }

    /// Creates and allocates a new frame corresponding to a method.
    /// Always passes arguments directly since we don't take them as a slice off the previous frame, like we do for methods.
    /// ...which would likely be faster, actually. TODO.
    pub fn push_block_frame_with_args(&mut self, block: GCRef<Block>, args: &[Value], mutator: &mut GCInterface) -> GCRef<Frame> {
        let current_method = self.current_frame.borrow().current_method;
        let frame_ptr = Frame::alloc_from_block(block, args, current_method, self.current_frame, mutator);
        self.bytecode_idx = 0;
        self.current_bytecodes = frame_ptr.to_obj().bytecodes;
        self.current_frame = frame_ptr;
        frame_ptr
    }

    pub fn pop_frame(&mut self) {
        GCInterface::safepoint_maybe_pause_for_gc();
        
        let new_current_frame = self.current_frame.to_obj().prev_frame;
        self.current_frame = new_current_frame;
        match new_current_frame.is_empty() {
            true => {}
            false => {
                self.bytecode_idx = new_current_frame.to_obj().bytecode_idx;
                self.current_bytecodes = new_current_frame.to_obj().bytecodes;
            }
        }
    }

    pub fn pop_n_frames(&mut self, n: u8) {
        GCInterface::safepoint_maybe_pause_for_gc();

        let new_current_frame = Frame::nth_frame_back_through_frame_list(&self.current_frame, n + 1);
        self.current_frame = new_current_frame;
        match new_current_frame.is_empty() {
            true => {}
            false => {
                self.bytecode_idx = new_current_frame.to_obj().bytecode_idx;
                self.current_bytecodes = new_current_frame.to_obj().bytecodes;
            }
        }
    }

    pub fn run(&mut self, universe: &mut Universe) -> Option<Value> {
        loop {
            // Actually safe, there's always a reference to the current bytecodes. Need unsafe because we want to store a ref for quick access in perf-critical code
            let bytecode = *(unsafe { (*self.current_bytecodes).get_unchecked(self.bytecode_idx) });

            // dbg!(&self.current_frame.to_obj().stack);
            // dbg!(&bytecode);

            self.bytecode_idx += 1;

            match bytecode {
                Bytecode::Dup2 => {
                    let second_to_last = self.current_frame.to_obj().stack_nth_back(1).clone();
                    self.current_frame.to_obj().stack_push(second_to_last)
                }
                Bytecode::JumpIfGreater(offset) => {
                    let top = self.current_frame.to_obj().stack_last();
                    let top2 = self.current_frame.to_obj().stack_nth_back(1);

                    let is_greater = {
                        if let (Some(a), Some(b)) = (top.as_integer(), top2.as_integer()) {
                            a > b
                        } else if let (Some(a), Some(b)) = (top.as_double(), top2.as_double()) {
                            a > b
                        } else {
                            panic!("we don't handle this case.")
                        }
                    };

                    if is_greater {
                        self.current_frame.to_obj().stack_pop();
                        self.current_frame.to_obj().stack_pop();
                        self.bytecode_idx += offset as usize - 1;
                    }
                },
                Bytecode::Halt => {
                    return Some(Value::NIL);
                }
                Bytecode::Dup => {
                    let value = self.current_frame.to_obj().stack_last().clone();
                    self.current_frame.to_obj().stack_push(value);
                }
                Bytecode::Inc => {
                    let last = self.current_frame.to_obj().stack_last_mut();
                    if let Some(int) = last.as_integer() {
                        *last = Value::new_integer(int + 1);
                    } else if let Some(double) = last.as_double() {
                        *last = Value::new_double(double + 1.0);
                    } else if let Some(big_int) = last.as_big_integer() {
                        *big_int.to_obj() += 1;
                    } else {
                        panic!("Invalid type in Inc")
                    }
                }
                Bytecode::Dec => {
                    let last = self.current_frame.to_obj().stack_last_mut();
                    if let Some(int) = last.as_integer() {
                        *last = Value::new_integer(int - 1); // TODO: see Bytecode::Inc
                    } else if let Some(double) = last.as_double() {
                        *last = Value::new_double(double - 1.0);
                    } else if let Some(big_int) = last.as_big_integer() {
                        *big_int.to_obj() -= 1;
                    } else {
                        panic!("Invalid type in DEC")
                    }
                }
                Bytecode::PushLocal(idx) => {
                    let value = self.current_frame.to_obj().lookup_local(idx as usize).clone();
                    self.current_frame.to_obj().stack_push(value);
                }
                Bytecode::PushNonLocal(up_idx, idx) => {
                    debug_assert_ne!(up_idx, 0);
                    let from = Frame::nth_frame_back(&self.current_frame, up_idx);
                    let value = from.to_obj().lookup_local(idx as usize).clone();
                    self.current_frame.to_obj().stack_push(value);
                }
                Bytecode::PushArg(idx) => {
                    debug_assert_ne!(idx, 0); // that's a ReturnSelf case.
                    let value = self.current_frame.to_obj().lookup_argument(idx as usize);
                    self.current_frame.to_obj().stack_push(value.clone());
                }
                Bytecode::PushNonLocalArg(up_idx, idx) => {
                    debug_assert_ne!(up_idx, 0);
                    debug_assert_ne!((up_idx, idx), (0, 0)); // that's a ReturnSelf case.
                    let from = Frame::nth_frame_back(&self.current_frame, up_idx);
                    let value = from.to_obj().lookup_argument(idx as usize);
                    self.current_frame.to_obj().stack_push(value.clone());
                }
                Bytecode::PushField(idx) => {
                    let self_val = self.current_frame.to_obj().get_self();
                    let val = {
                        if let Some(instance) = self_val.as_instance() {
                            instance.lookup_local(idx as usize)
                        } else if let Some(cls) = self_val.as_class() {
                            cls.to_obj().class().to_obj().lookup_local(idx as usize)
                        } else {
                            panic!("trying to read a field from a {:?}?", &self_val)
                        }
                    };
                    self.current_frame.to_obj().stack_push(val);
                }
                Bytecode::PushBlock(idx) => {
                    let literal = self.current_frame.to_obj().lookup_constant(idx as usize);
                    let block = match literal {
                        Literal::Block(blk) => GCRef::<Block>::alloc(blk.to_obj().clone(), &mut universe.gc_interface),
                        _ => panic!("PushBlock expected a block, but got another invalid literal"),
                    };
                    block.to_obj().frame.replace(self.current_frame);
                    self.current_frame.to_obj().stack_push(Value::Block(block));
                }
                Bytecode::PushConstant(idx) => {
                    let literal = self.current_frame.to_obj().lookup_constant(idx as usize);
                    let value = convert_literal(&self.current_frame, literal, &mut universe.gc_interface);
                    self.current_frame.to_obj().stack_push(value);
                }
                Bytecode::PushConstant0 => {
                    let literal = self.current_frame.to_obj().lookup_constant(0);
                    let value = convert_literal(&self.current_frame, literal, &mut universe.gc_interface);
                    self.current_frame.to_obj().stack_push(value);
                }
                Bytecode::PushConstant1 => {
                    let literal = self.current_frame.to_obj().lookup_constant(1);
                    let value = convert_literal(&self.current_frame, literal, &mut universe.gc_interface);
                    self.current_frame.to_obj().stack_push(value);
                }
                Bytecode::PushConstant2 => {
                    let literal = self.current_frame.to_obj().lookup_constant(2);
                    let value = convert_literal(&self.current_frame, literal, &mut universe.gc_interface);
                    self.current_frame.to_obj().stack_push(value);
                }
                Bytecode::PushGlobal(idx) => {
                    let literal = self.current_frame.to_obj().lookup_constant(idx as usize);
                    let symbol = match literal {
                        Literal::Symbol(sym) => sym,
                        _ => panic!("Global is not a symbol."),
                    };
                    if let Some(value) = universe.lookup_global(symbol) {
                        self.current_frame.to_obj().stack_push(value);
                    } else {
                        let self_value = self.current_frame.to_obj().get_self();
                        universe.unknown_global(self, self_value, symbol)?;
                    }
                }
                Bytecode::Push0 => {
                    self.current_frame.to_obj().stack_push(Value::INTEGER_ZERO);
                }
                Bytecode::Push1 => {
                    self.current_frame.to_obj().stack_push(Value::INTEGER_ONE);
                }
                Bytecode::PushNil => {
                    self.current_frame.to_obj().stack_push(Value::NIL);
                }
                Bytecode::PushSelf => {
                    self.current_frame.to_obj().stack_push(self.current_frame.to_obj().lookup_argument(0).clone());
                }
                Bytecode::Pop => {
                    self.current_frame.to_obj().stack_pop();
                }
                Bytecode::PopLocal(up_idx, idx) => {
                    let value = self.current_frame.to_obj().stack_pop();
                    let from = Frame::nth_frame_back(&self.current_frame, up_idx);
                    from.to_obj().assign_local(idx as usize, value);
                }
                Bytecode::PopArg(up_idx, idx) => {
                    let value = self.current_frame.to_obj().stack_pop();
                    let from = Frame::nth_frame_back(&self.current_frame, up_idx);
                    from.to_obj().assign_arg(idx as usize, value);
                }
                Bytecode::PopField(idx) => {
                    let value = self.current_frame.to_obj().stack_pop();
                    let self_val = self.current_frame.to_obj().get_self();
                    if let Some(mut instance) = self_val.as_instance() {
                        instance.assign_local(idx as usize, value)
                    } else if let Some(cls) = self_val.as_class() {
                        cls.to_obj().class().to_obj().assign_local(idx as usize, value)
                    } else {
                        panic!("trying to assign a field to a {:?}?", &self_val)
                    }
                }
                Bytecode::Send1(idx) => {
                    send! {self, universe, &self.current_frame, idx, Some(0)} // Send1 => receiver + 0 args, so we pass Some(0)
                }
                Bytecode::Send2(idx) => {
                    send! {self, universe, &self.current_frame, idx, Some(1)}
                }
                Bytecode::Send3(idx) => {
                    send! {self, universe, &self.current_frame, idx, Some(2)}
                }
                Bytecode::SendN(idx) => {
                    send! {self, universe, &self.current_frame, idx, None}
                }
                Bytecode::SuperSend1(idx) => {
                    super_send! {self, universe, &self.current_frame, idx, Some(0)}
                }
                Bytecode::SuperSend2(idx) => {
                    super_send! {self, universe, &self.current_frame, idx, Some(1)}
                }
                Bytecode::SuperSend3(idx) => {
                    super_send! {self, universe, &self.current_frame, idx, Some(2)}
                }
                Bytecode::SuperSendN(idx) => {
                    super_send! {self, universe, &self.current_frame, idx, None}
                }
                Bytecode::ReturnSelf => {
                    let self_val = self.current_frame.to_obj().lookup_argument(0).clone();
                    self.pop_frame();
                    // if self.current_frame.is_empty() {
                    //     return Some(self.stack.pop().unwrap_or(Value::NIL));
                    // }
                    self.current_frame.to_obj().stack_push(self_val);
                }
                Bytecode::ReturnLocal => {
                    let val = self.current_frame.to_obj().stack_pop();
                    self.pop_frame();
                    if self.current_frame.is_empty() {
                        return Some(val);
                    }
                    self.current_frame.to_obj().stack_push(val);
                }
                Bytecode::ReturnNonLocal(up_idx) => {
                    let method_frame = Frame::nth_frame_back(&self.current_frame, up_idx);
                    // let escaped_frames = self
                    //     .frames
                    //     .iter()
                    //     .rev()
                    //     .position(|live_frame| *live_frame == method_frame);

                    let escaped_frames = {
                        let mut current_frame = self.current_frame;
                        let mut count = 0;

                        loop {
                            if current_frame == method_frame {
                                break Some(count);
                            } else if current_frame.is_empty() {
                                break None
                            } else {
                                current_frame = current_frame.to_obj().prev_frame;
                                count += 1;
                            }
                        }
                    };

                    if let Some(count) = escaped_frames {
                        let val = self.current_frame.to_obj().stack_pop();

                        self.pop_n_frames(count + 1);
                        // if self.current_frame.is_empty() {
                        //      return Some(self.stack.pop().unwrap_or(Value::NIL));
                        // }
                        self.current_frame.to_obj().stack_push(val);

                    } else {
                        // NB: I did some changes there with the blockself bits and i'm not positive it works the same as before, but it should.

                        // Block has escaped its method frame.
                        let instance = self.current_frame.to_obj().get_self();
                        let block = match self.current_frame.to_obj().lookup_argument(0).as_block() {
                            Some(block) => block,
                            _ => {
                                // Should never happen, because `universe.current_frame()` would
                                // have been equal to `universe.current_method_frame()`.
                                panic!("A method frame has escaped itself ??");
                            }
                        };

                        // we store the current bytecode idx to be able to correctly restore the bytecode state when we pop frames
                        self.current_frame.to_obj().bytecode_idx = self.bytecode_idx;

                        universe.escaped_block(self, instance, block).expect(
                            "A block has escaped and `escapedBlock:` is not defined on receiver",
                        );
                    }
                }
                Bytecode::Jump(offset) => {
                    self.bytecode_idx += offset as usize - 1; // minus one because it gets incremented by one already every loop
                }
                Bytecode::JumpBackward(offset) => {
                    self.bytecode_idx -= offset as usize + 1;
                }
                Bytecode::JumpOnTrueTopNil(offset) => {
                    let condition_result = self.current_frame.to_obj().stack_last();

                    if condition_result.is_boolean_true() {
                        self.bytecode_idx += offset as usize - 1;
                        *self.current_frame.to_obj().stack_last_mut() = Value::NIL;
                    } else if condition_result.is_boolean_false() {
                        self.current_frame.to_obj().stack_pop();
                    } else {
                        panic!("JumpOnTrueTopNil condition did not evaluate to boolean (was {:?})", condition_result)
                    }
                }
                Bytecode::JumpOnFalseTopNil(offset) => {
                    let condition_result = self.current_frame.to_obj().stack_last();

                    if condition_result.is_boolean_true() {
                        self.current_frame.to_obj().stack_pop();
                    } else if condition_result.is_boolean_false(){
                        self.bytecode_idx += offset as usize - 1;
                        *self.current_frame.to_obj().stack_last_mut() = Value::NIL;
                    } else {
                        panic!("JumpOnFalseTopNil condition did not evaluate to boolean (was {:?})", condition_result)
                    }
                }
                Bytecode::JumpOnTruePop(offset) => {
                    let condition_result = self.current_frame.to_obj().stack_pop();

                    if condition_result.is_boolean_true() {
                        self.bytecode_idx += offset as usize - 1;
                    } else if condition_result.is_boolean_false() {
                        // pass
                    }
                    else {
                        panic!("JumpOnTruePop condition did not evaluate to boolean (was {:?})", condition_result)
                    }
                }
                Bytecode::JumpOnFalsePop(offset) => {
                    let condition_result = self.current_frame.to_obj().stack_pop();

                    if condition_result.is_boolean_false() {
                        self.bytecode_idx += offset as usize - 1;
                    } else if condition_result.is_boolean_true() {
                        // pass
                    }
                    else {
                        panic!("JumpOnFalsePop condition did not evaluate to boolean (was {:?})", condition_result)
                    }
                }
            }
        }

        pub fn do_send(
            interpreter: &mut Interpreter,
            universe: &mut Universe,
            method: Option<GCRef<Method>>,
            symbol: Interned,
            nb_params: usize,
        ) {
            // we store the current bytecode idx to be able to correctly restore the bytecode state when we pop frames
            interpreter.current_frame.to_obj().bytecode_idx = interpreter.bytecode_idx;

            GCInterface::safepoint_maybe_pause_for_gc();
            
            let Some(method) = method else {
                let args = interpreter.current_frame.to_obj().stack_n_last_elements(nb_params);
                let self_value = interpreter.current_frame.clone().to_obj().stack_pop();

                // could be avoided by passing args slice directly...
                // ...but A) DNU is a very rare path and B) i guess we allocate a new args arr in the DNU call anyway
                let args = args.iter().map(|v| v.clone()).collect();

                universe.does_not_understand(interpreter, self_value, symbol, args)
                    .expect(
                        "A message cannot be handled and `doesNotUnderstand:arguments:` is not defined on receiver"
                    );

                return;
            };

            match method.to_obj().kind() {
                MethodKind::Defined(_) => {
                    // let name = &method.to_obj().holder.borrow().name.clone();
                    // eprintln!("Invoking {:?} (in {:?})", &method.to_obj().signature, &name);
                    // if method.to_obj().signature == "initializeWith:selector:arguments:" {
                    //     dbg!("wow");
                    // }
                    // let filter_list = ["Integer", "Vector", "True", "Pair"];
                    // let filter_list = [];

                    // if !filter_list.contains(&name.as_str()) {
                    // if !SYSTEM_CLASS_NAMES.contains(&name.as_str()) {
                    // }

                    interpreter.push_method_frame(method, nb_params + 1, &mut universe.gc_interface);
                }
                MethodKind::Primitive(func) => {
                    // eprintln!("Invoking prim {:?} (in {:?})", &method.to_obj().signature, &method.to_obj().holder.borrow().name);
                    func(interpreter, universe).with_context(|| anyhow::anyhow!("error calling primitive `{}`", universe.lookup_symbol(symbol))).unwrap();
                }
                MethodKind::NotImplemented(err) => {
                    let self_value = interpreter.current_frame.to_obj().stack_nth_back(nb_params);
                    println!(
                        "{}>>#{}",
                        self_value.class(&universe).to_obj().name(),
                        method.to_obj().signature(),
                    );
                    panic!("Primitive `#{}` not implemented", err)
                }
            }
        }

        fn resolve_method(
            frame: &GCRef<Frame>,
            class: &GCRef<Class>,
            signature: Interned,
            bytecode_idx: usize,
        ) -> Option<GCRef<Method>> {
            let mut inline_cache = unsafe {
                (*frame.to_obj().inline_cache).borrow_mut()
            };

            // SAFETY: this access is actually safe because the bytecode compiler
            // makes sure the cache has as many entries as there are bytecode instructions,
            // therefore we can avoid doing any redundant bounds checks here.
            let maybe_found = unsafe { inline_cache.get_unchecked_mut(bytecode_idx) };

            match maybe_found {
                Some((receiver, method)) if *receiver == class.ptr.to_ptr() => {
                    Some(*method)
                }
                place @ None => {
                    let found = class.to_obj().lookup_method(signature);
                    *place = found
                        .clone()
                        .map(|method| (class.ptr.to_ptr() as *const _, method));
                    found
                }
                _ => class.to_obj().lookup_method(signature),
            }
        }

        fn convert_literal(frame: &GCRef<Frame>, literal: Literal, gc_interface: &mut GCInterface) -> Value {
            let value = match literal {
                Literal::Symbol(sym) => Value::Symbol(sym),
                Literal::String(val) => Value::String(val),
                Literal::Double(val) => Value::Double(val),
                Literal::Integer(val) => Value::Integer(val),
                Literal::BigInteger(val) => {
                    Value::BigInteger(val)
                }
                Literal::Array(val) => {
                    let arr = val
                        .to_obj()
                        .into_iter()
                        .map(|idx| {
                            let lit = frame.to_obj().lookup_constant(*idx as usize);
                            convert_literal(frame, lit, gc_interface)
                        })
                        .collect::<Vec<_>>();
                    Value::Array(GCRef::<Vec<Value>>::alloc(arr, gc_interface))
                }
                Literal::Block(val) => Value::Block(val),
            };
            value
        }

        fn nb_params(signature: &str) -> usize {
            match signature.chars().nth(0) {
                Some(ch) if !ch.is_alphabetic() => 1,
                _ => signature.chars().filter(|ch| *ch == ':').count(),
            }
        }
    }
}
