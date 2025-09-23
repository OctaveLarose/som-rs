//!
//! This is the bytecode compiler for the Simple Object Machine.
//!

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use som_core::interner::Interner;
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;
use som_value::interned::Interned;
use std::cell::Cell;
use std::str::FromStr;

#[cfg(not(feature = "inlining-disabled"))]
use crate::compiler::inliner::PrimMessageInliner;
use crate::compiler::Literal;
use crate::primitives;
use crate::primitives::UNIMPLEM_PRIMITIVE;
use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::method::{BasicMethodInfo, Method, MethodInfo};
use crate::vm_objects::trivial_methods::{TrivialGetterMethod, TrivialGlobalMethod, TrivialLiteralMethod, TrivialSetterMethod};
use som_core::ast;
#[cfg(feature = "frame-debug-info")]
use som_core::ast::BlockDebugInfo;
use som_core::ast::{Expression, MethodBody};
use som_core::bytecode::Bytecode;
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};

pub(crate) trait GenCtxt {
    fn intern_symbol(&mut self, name: &str) -> Interned;
    fn get_scope(&self) -> usize;
    fn find_field(&mut self, name: &str) -> Option<usize>;
    fn get_interner(&self) -> &Interner;
}

pub(crate) trait InnerGenCtxt: GenCtxt {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt;
    fn push_instr(&mut self, instr: Bytecode);
    fn pop_instr(&mut self);
    fn get_instructions(&self) -> &Vec<Bytecode>;
    fn get_nbr_locals(&self) -> usize;
    fn set_nbr_locals(&mut self, nbr_locals: usize);
    fn get_literal(&self, idx: usize) -> Option<&Literal>;
    fn push_literal(&mut self, literal: Literal) -> usize;
    fn remove_literal(&mut self, idx: usize) -> Option<Literal>;
    fn get_cur_instr_idx(&self) -> usize;
    fn patch_jump(&mut self, idx_to_backpatch: usize, new_val: u16);
    fn backpatch_jump_to_current(&mut self, idx_to_backpatch: usize);
    fn remove_dup_popx_pop_sequences(&mut self);
}

/// Calculates the maximum stack size possible. For each frame, this allows us to allocate a stack of precisely the maximum possible size it needs.
/// TODO opt: it's possible our estimate is overly conservative. Reducing the max stack size reduces time spent allocating, and could maybe be a worthwhile optimization.
pub(crate) fn get_max_stack_size(body: &[Bytecode], interner: &Interner) -> u8 {
    let mut abstract_stack_size = 0;
    let mut max_stack_size_observed: u8 = 0;

    for bc in body {
        match bc {
            Bytecode::Dup
            | Bytecode::Dup2
            | Bytecode::PushLocal(..)
            | Bytecode::PushNonLocal(..)
            | Bytecode::PushArg(..)
            | Bytecode::PushNonLocalArg(..)
            | Bytecode::PushField(..)
            | Bytecode::PushBlock(..)
            | Bytecode::PushConstant(..)
            | Bytecode::PushGlobal(..)
            | Bytecode::Push0
            | Bytecode::Push1
            | Bytecode::PushNil
            | Bytecode::PushSelf => {
                abstract_stack_size += 1;
                if abstract_stack_size > max_stack_size_observed {
                    max_stack_size_observed = abstract_stack_size
                }
            }
            Bytecode::Pop
            | Bytecode::PopLocal(..)
            | Bytecode::PopArg(..)
            | Bytecode::PopField(..)
            | Bytecode::JumpOnTruePop(..)
            | Bytecode::JumpOnFalsePop(..) => abstract_stack_size -= 1,
            Bytecode::Send1(_) => {}
            Bytecode::Send2(_) => abstract_stack_size -= 1, // number of arguments (they all get popped) + 1 for the result
            Bytecode::Send3(_) => abstract_stack_size -= 2,
            Bytecode::SendN(symbol) | Bytecode::SuperSend(symbol) => {
                let nb_params = {
                    let uninterned = interner.lookup(*symbol);
                    match uninterned.chars().next() {
                        Some(ch) if !ch.is_alphabetic() => 1,
                        _ => uninterned.chars().filter(|ch| *ch == ':').count() as u8,
                    }
                };

                if nb_params > 0 {
                    abstract_stack_size -= nb_params - 1
                }
            }
            Bytecode::Inc => {}
            Bytecode::Dec => {}
            Bytecode::ReturnSelf => {}
            Bytecode::ReturnLocal => {}
            Bytecode::ReturnNonLocal(_) => {}
            Bytecode::Jump(_) => {}
            Bytecode::JumpBackward(_) => {}
            Bytecode::JumpOnTrueTopNil(_) => {}
            Bytecode::JumpOnFalseTopNil(_) => {}
            Bytecode::JumpOnNilTopTop(_) => {}
            Bytecode::JumpOnNotNilTopTop(_) => {}
            Bytecode::JumpOnNilPop(_) => {}
            Bytecode::JumpOnNotNilPop(_) => {}
            Bytecode::JumpIfGreater(_) => {}
        }
    }

    // Need to add an extra slot for the hack invoke case
    max_stack_size_observed + 1
}

struct BlockGenCtxt<'a> {
    pub outer: &'a mut dyn GenCtxt,
    pub args_nbr: usize,
    pub locals_nbr: usize,
    pub literals: IndexSet<Literal>,
    pub body: Option<Vec<Bytecode>>,
    #[cfg(feature = "frame-debug-info")]
    pub debug_info: BlockDebugInfo,
}

impl GenCtxt for BlockGenCtxt<'_> {
    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.outer.intern_symbol(name)
    }

    fn get_scope(&self) -> usize {
        self.outer.get_scope() + 1
    }

    fn find_field(&mut self, name: &str) -> Option<usize> {
        self.outer.find_field(name)
    }

    fn get_interner(&self) -> &Interner {
        self.outer.get_interner()
    }
}

impl InnerGenCtxt for BlockGenCtxt<'_> {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt {
        self
    }

    fn push_instr(&mut self, instr: Bytecode) {
        let body = self.body.get_or_insert_with(Vec::new);
        body.push(instr);
    }

    fn pop_instr(&mut self) {
        self.body.as_mut().unwrap().pop();
    }

    fn get_instructions(&self) -> &Vec<Bytecode> {
        self.body.as_ref().unwrap()
    }

    fn get_literal(&self, idx: usize) -> Option<&Literal> {
        self.literals.get_index(idx)
    }

    fn push_literal(&mut self, literal: Literal) -> usize {
        let (idx, _) = self.literals.insert_full(literal);
        idx
    }

    fn remove_literal(&mut self, idx: usize) -> Option<Literal> {
        self.literals.shift_remove_index(idx)
    }

    fn get_cur_instr_idx(&self) -> usize {
        self.body.as_ref().unwrap().iter().len()
    }

    fn backpatch_jump_to_current(&mut self, idx_to_backpatch: usize) {
        let jump_offset = self.get_cur_instr_idx() - idx_to_backpatch;
        self.patch_jump(idx_to_backpatch, jump_offset as u16)
    }

    fn patch_jump(&mut self, idx_to_patch: usize, new_val: u16) {
        match self.body.as_mut().unwrap().get_mut(idx_to_patch).unwrap() {
            Bytecode::Jump(jump_idx)
            | Bytecode::JumpBackward(jump_idx)
            | Bytecode::JumpOnTrueTopNil(jump_idx)
            | Bytecode::JumpOnFalseTopNil(jump_idx)
            | Bytecode::JumpOnTruePop(jump_idx)
            | Bytecode::JumpOnFalsePop(jump_idx)
            | Bytecode::JumpOnNilTopTop(jump_idx)
            | Bytecode::JumpOnNotNilTopTop(jump_idx)
            | Bytecode::JumpOnNilPop(jump_idx)
            | Bytecode::JumpOnNotNilPop(jump_idx)
            | Bytecode::JumpIfGreater(jump_idx) => *jump_idx = new_val,
            _ => panic!("Attempting to patch a bytecode non jump"),
        };
    }

    fn get_nbr_locals(&self) -> usize {
        self.locals_nbr
    }

    fn set_nbr_locals(&mut self, nbr_locals: usize) {
        self.locals_nbr = nbr_locals;
    }

    fn remove_dup_popx_pop_sequences(&mut self) {
        if self.body.is_none() || self.body.as_ref().unwrap().len() < 3 {
            return;
        }

        let body = self.body.as_mut().unwrap();

        let mut indices_to_remove: Vec<usize> = vec![];

        for (idx, bytecode_win) in body.windows(3).enumerate() {
            if matches!(bytecode_win[0], Bytecode::Dup)
                && matches!(bytecode_win[1], Bytecode::PopField(..) | Bytecode::PopLocal(..) | Bytecode::PopArg(..))
                && matches!(bytecode_win[2], Bytecode::Pop)
            {
                let are_bc_jump_targets = body.iter().enumerate().any(|(maybe_jump_idx, bc)| match bc {
                    Bytecode::Jump(jump_offset)
                    | Bytecode::JumpOnTrueTopNil(jump_offset)
                    | Bytecode::JumpOnFalseTopNil(jump_offset)
                    | Bytecode::JumpOnTruePop(jump_offset)
                    | Bytecode::JumpOnFalsePop(jump_offset)
                    | Bytecode::JumpIfGreater(jump_offset)
                    | Bytecode::JumpOnNilPop(jump_offset)
                    | Bytecode::JumpOnNotNilPop(jump_offset)
                    | Bytecode::JumpOnNilTopTop(jump_offset)
                    | Bytecode::JumpOnNotNilTopTop(jump_offset) => {
                        let bc_target_idx = maybe_jump_idx + *jump_offset as usize;
                        bc_target_idx == idx || bc_target_idx == idx + 2
                    }
                    _ => false,
                });

                if are_bc_jump_targets {
                    continue;
                }

                indices_to_remove.push(idx);
                indices_to_remove.push(idx + 2);
            }
        }

        if indices_to_remove.is_empty() {
            return;
        }

        let mut jumps_to_patch: Vec<(usize, u16)> = vec![];
        for (cur_idx, bc) in body.iter().enumerate() {
            match bc {
                Bytecode::Jump(jump_offset)
                | Bytecode::JumpOnTrueTopNil(jump_offset)
                | Bytecode::JumpOnFalseTopNil(jump_offset)
                | Bytecode::JumpOnTruePop(jump_offset)
                | Bytecode::JumpOnFalsePop(jump_offset)
                | Bytecode::JumpIfGreater(jump_offset) => {
                    let jump_offset = *jump_offset as usize;

                    if indices_to_remove.contains(&(cur_idx + jump_offset)) {
                        panic!("should be unreachable");
                        // let jump_target_in_removes_idx = indices_to_remove
                        //     .iter()
                        //     .position(|&v| v == cur_idx + jump_offset)
                        //     .unwrap();
                        // indices_to_remove.remove(jump_target_in_removes_idx);
                        // // indices_to_remove.remove(jump_target_in_removes_idx - 1);
                        // let to_remove = (jump_target_in_removes_idx,
                        //                  match jump_target_in_removes_idx % 2 {
                        //                      0 => jump_target_in_removes_idx + 1,
                        //                      1 => jump_target_in_removes_idx - 1,
                        //                      _ => unreachable!()
                        //                  });
                        //
                        // indices_to_remove.retain(|v| *v != to_remove.0 && *v != to_remove.1);
                        // continue;
                    }

                    let nbr_to_adjust = indices_to_remove.iter().filter(|&&idx| cur_idx < idx && idx <= cur_idx + jump_offset).count();
                    jumps_to_patch.push((cur_idx, (jump_offset - nbr_to_adjust) as u16));
                }
                Bytecode::JumpBackward(jump_offset) => {
                    let jump_offset = *jump_offset as usize;
                    let nbr_to_adjust = indices_to_remove.iter().filter(|&&idx| cur_idx > idx && idx > cur_idx - jump_offset).count();
                    jumps_to_patch.push((cur_idx, (jump_offset - nbr_to_adjust) as u16));
                    // It's impossible for a JumpBackward to be generated to point to a duplicated dup/pop/pox sequence, as it stands, and as far as I know.
                }
                _ => {}
            }
        }

        for (jump_idx, jump_val) in jumps_to_patch {
            self.patch_jump(jump_idx, jump_val);
        }

        let mut index = 0;
        self.body.as_mut().unwrap().retain(|_| {
            let is_kept = !indices_to_remove.contains(&index);
            index += 1;
            is_kept
        });
    }
}

struct MethodGenCtxt<'a> {
    pub signature: String,
    pub inner: BlockGenCtxt<'a>,
}

impl MethodGenCtxt<'_> {}

impl GenCtxt for MethodGenCtxt<'_> {
    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.inner.intern_symbol(name)
    }

    fn get_scope(&self) -> usize {
        0
    }

    fn find_field(&mut self, name: &str) -> Option<usize> {
        self.inner.find_field(name)
    }

    fn get_interner(&self) -> &Interner {
        self.inner.get_interner()
    }
}

impl InnerGenCtxt for MethodGenCtxt<'_> {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt {
        self
    }

    fn push_instr(&mut self, instr: Bytecode) {
        self.inner.push_instr(instr)
    }

    fn pop_instr(&mut self) {
        self.inner.pop_instr();
    }

    fn get_instructions(&self) -> &Vec<Bytecode> {
        self.inner.get_instructions()
    }

    fn push_literal(&mut self, literal: Literal) -> usize {
        self.inner.push_literal(literal)
    }

    fn get_literal(&self, idx: usize) -> Option<&Literal> {
        self.inner.get_literal(idx)
    }

    fn remove_literal(&mut self, idx: usize) -> Option<Literal> {
        self.inner.remove_literal(idx)
    }

    fn get_cur_instr_idx(&self) -> usize {
        self.inner.get_cur_instr_idx()
    }

    fn patch_jump(&mut self, idx_to_backpatch: usize, new_val: u16) {
        self.inner.patch_jump(idx_to_backpatch, new_val)
    }

    fn backpatch_jump_to_current(&mut self, idx_to_backpatch: usize) {
        self.inner.backpatch_jump_to_current(idx_to_backpatch);
    }

    fn remove_dup_popx_pop_sequences(&mut self) {
        self.inner.remove_dup_popx_pop_sequences();
    }

    fn get_nbr_locals(&self) -> usize {
        self.inner.get_nbr_locals()
    }

    fn set_nbr_locals(&mut self, nbr_locals: usize) {
        self.inner.set_nbr_locals(nbr_locals)
    }
}

pub(crate) trait MethodCodegen {
    fn codegen(&self, ctxt: &mut dyn InnerGenCtxt, mutator: &mut GCInterface) -> Option<()>;
}

impl MethodCodegen for ast::Body {
    fn codegen(&self, ctxt: &mut dyn InnerGenCtxt, mutator: &mut GCInterface) -> Option<()> {
        for expr in &self.exprs {
            expr.codegen(ctxt, mutator)?;
        }
        Some(())
    }
}

impl MethodCodegen for ast::Expression {
    fn codegen(&self, ctxt: &mut dyn InnerGenCtxt, mutator: &mut GCInterface) -> Option<()> {
        match self {
            ast::Expression::LocalVarRead(idx) => {
                ctxt.push_instr(Bytecode::PushLocal(*idx as u8));
                Some(())
            }
            ast::Expression::NonLocalVarRead(up_idx, idx) => {
                ctxt.push_instr(Bytecode::PushNonLocal(*up_idx as u8, *idx as u8));
                Some(())
            }
            ast::Expression::ArgRead(up_idx, idx) => {
                match (up_idx, idx) {
                    (0, 0) => ctxt.push_instr(Bytecode::PushSelf),
                    (0, _) => ctxt.push_instr(Bytecode::PushArg(*idx as u8)),
                    _ => ctxt.push_instr(Bytecode::PushNonLocalArg(*up_idx as u8, *idx as u8)),
                };
                Some(())
            }
            ast::Expression::GlobalRead(name) => {
                match ctxt.find_field(name) {
                    Some(idx) => ctxt.push_instr(Bytecode::PushField(idx as u8)),
                    None => match name.as_str() {
                        "nil" => ctxt.push_instr(Bytecode::PushNil),
                        "super" => match ctxt.get_scope() {
                            0 => ctxt.push_instr(Bytecode::PushSelf),
                            scope => ctxt.push_instr(Bytecode::PushNonLocalArg(scope as u8, 0)),
                        },
                        _ => {
                            let name = ctxt.intern_symbol(name);
                            let idx = ctxt.push_literal(Literal::Symbol(name));
                            ctxt.push_instr(Bytecode::PushGlobal(idx as u8));
                        }
                    },
                }

                Some(())
            }
            ast::Expression::LocalVarWrite(_, expr) | ast::Expression::NonLocalVarWrite(_, _, expr) => {
                expr.codegen(ctxt, mutator)?;
                ctxt.push_instr(Bytecode::Dup);
                match self {
                    ast::Expression::LocalVarWrite(idx, _) => ctxt.push_instr(Bytecode::PopLocal(0, *idx as u8)),
                    ast::Expression::NonLocalVarWrite(up_idx, idx, _) => ctxt.push_instr(Bytecode::PopLocal(*up_idx as u8, *idx as u8)),
                    _ => unreachable!(),
                }
                Some(())
            }
            ast::Expression::GlobalWrite(name, expr) => match ctxt.find_field(name) {
                Some(idx) => {
                    expr.codegen(ctxt, mutator)?;
                    ctxt.push_instr(Bytecode::Dup);
                    ctxt.push_instr(Bytecode::PopField(idx as u8));
                    Some(())
                }
                None => panic!("couldn't resolve a globalwrite (`{}`) to a field write", name),
            },
            ast::Expression::ArgWrite(up_idx, idx, expr) => {
                expr.codegen(ctxt, mutator)?;
                ctxt.push_instr(Bytecode::Dup);
                ctxt.push_instr(Bytecode::PopArg(*up_idx as u8, *idx as u8));
                Some(())
            }
            ast::Expression::Message(message) => {
                let is_super_call = matches!(&message.receiver, _super if _super == &Expression::GlobalRead(String::from("super")));

                message.receiver.codegen(ctxt, mutator)?;

                #[cfg(not(feature = "inlining-disabled"))]
                if message.inline_if_possible(ctxt, mutator).is_some() {
                    return Some(());
                }

                if (message.signature == "+" || message.signature == "-")
                    && !is_super_call
                    && message.values.len() == 1
                    && message.values.first()? == &Expression::Literal(ast::Literal::Integer(1))
                {
                    match message.signature.as_str() {
                        "+" => ctxt.push_instr(Bytecode::Inc),
                        "-" => ctxt.push_instr(Bytecode::Dec),
                        _ => unreachable!(),
                    };
                    return Some(());
                }

                message.values.iter().try_for_each(|value| value.codegen(ctxt, mutator))?;

                let nb_params = match message.signature.chars().nth(0) {
                    Some(ch) if !ch.is_alphabetic() => 1,
                    _ => message.signature.chars().filter(|ch| *ch == ':').count(),
                };

                let sym = ctxt.intern_symbol(message.signature.as_str());

                match is_super_call {
                    false => match nb_params {
                        0 => ctxt.push_instr(Bytecode::Send1(sym)),
                        1 => ctxt.push_instr(Bytecode::Send2(sym)),
                        2 => ctxt.push_instr(Bytecode::Send3(sym)),
                        _ => ctxt.push_instr(Bytecode::SendN(sym)),
                    },
                    true => ctxt.push_instr(Bytecode::SuperSend(sym)),
                }

                Some(())
            }
            ast::Expression::Exit(expr, scope) => {
                match scope {
                    0 => match expr.as_ref() {
                        Expression::ArgRead(0, 0) => ctxt.push_instr(Bytecode::ReturnSelf),
                        _ => {
                            expr.codegen(ctxt, mutator)?;
                            ctxt.push_instr(Bytecode::ReturnLocal)
                        }
                    },
                    _ => {
                        expr.codegen(ctxt, mutator)?;
                        ctxt.push_instr(Bytecode::ReturnNonLocal(*scope as u8));
                    }
                };

                Some(())
            }
            ast::Expression::Literal(literal) => {
                fn convert_literal(ctxt: &mut dyn InnerGenCtxt, literal: &ast::Literal, gc_interface: &mut GCInterface) -> Literal {
                    match literal {
                        ast::Literal::Symbol(val) => Literal::Symbol(ctxt.intern_symbol(val.as_str())),
                        ast::Literal::String(val) => {
                            // TODO: this whole bit is to avoid redundant literals. previous logic broke with strings being put on the GC heap. is it indicative of a deeper issue with redundant strings?
                            // it feels a bit bandaid-ey, since I'm not sure where the bug came from exactly.
                            // it feels like tests should still pass without all this logic, but they don't (see specialized BC PushConstant one), and I'm not *positive* that's normal?
                            // also NB: this code was a mild speeddown! could be removed, and the PushConst test deactivated/fixed another way, probably. keeping it for now.
                            let mut i = 0;
                            loop {
                                let lit = ctxt.get_literal(i);
                                match lit {
                                    None => break Literal::String(gc_interface.alloc(val.clone(), AllocSiteMarker::StringLiteral)), // reached end of literals and no duplicate, we alloc
                                    Some(str_lit @ Literal::String(str_ptr)) if **str_ptr == *val => break str_lit.clone(),
                                    _ => {}
                                }
                                i += 1;
                            }
                        }
                        ast::Literal::Double(val) => Literal::Double(*val),
                        ast::Literal::Integer(val) => Literal::Integer(*val),
                        ast::Literal::BigInteger(big_int_str) => {
                            // this is to handle a weird corner case where "-2147483648" is considered to be a bigint by the lexer and then parser, when it's in fact just barely in i32 range
                            match big_int_str.parse::<i32>() {
                                Ok(x) => Literal::Integer(x),
                                _ => Literal::BigInteger(gc_interface.alloc(BigInt::from_str(big_int_str).unwrap(), AllocSiteMarker::BigInt)),
                            }
                        }
                        ast::Literal::Array(val) => {
                            let literals: GcSlice<Literal> = {
                                let literals_vec: Vec<Literal> = val.iter().map(|val| convert_literal(ctxt, val, gc_interface)).collect();
                                gc_interface.alloc_slice(literals_vec.as_slice(), AllocSiteMarker::VecBCLiteral)
                            };
                            Literal::Array(literals)
                        }
                    }
                }

                let literal = convert_literal(ctxt, literal, mutator);

                match literal {
                    Literal::Integer(0) => ctxt.push_instr(Bytecode::Push0),
                    Literal::Integer(1) => ctxt.push_instr(Bytecode::Push1),
                    _ => {
                        let idx = ctxt.push_literal(literal);
                        ctxt.push_instr(Bytecode::PushConstant(idx as u8))
                    }
                }

                Some(())
            }
            ast::Expression::Block(val) => {
                let block = compile_block(ctxt.as_gen_ctxt(), val, mutator)?;
                let block = Literal::Block(mutator.alloc(block, AllocSiteMarker::Block));
                let idx = ctxt.push_literal(block);
                ctxt.push_instr(Bytecode::PushBlock(idx as u8));
                Some(())
            }
        }
    }
}

struct ClassGenCtxt<'a> {
    pub name: String,
    pub fields: IndexSet<Interned>,
    pub methods: IndexMap<Interned, Gc<Method>>,
    pub interner: &'a mut Interner,
}

impl GenCtxt for ClassGenCtxt<'_> {
    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.interner.intern(name)
    }

    fn get_scope(&self) -> usize {
        unreachable!("Asking for scope in a class generation context?")
    }

    fn find_field(&mut self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f_int| self.interner.lookup(*f_int) == name)
    }

    fn get_interner(&self) -> &Interner {
        self.interner
    }
}

fn compile_method(outer: &mut dyn GenCtxt, defn: &ast::MethodDef, gc_interface: &mut GCInterface) -> Option<Method> {
    /// Only add a ReturnSelf at the end of a method if needed: i.e. there's no existing return, and if there is, that it can't be jumped over.
    fn should_add_return_self(ctxt: &mut MethodGenCtxt, body: &ast::Body) -> bool {
        if body.exprs.is_empty() {
            return true;
        }

        // going back two BC to skip the POP added after each expr.codegen(...).
        match ctxt.get_instructions().iter().nth_back(1) {
            // if the last BC is a return, we check whether it can be skipped over. if so, we add a ReturnSelf
            Some(Bytecode::ReturnLocal) | Some(Bytecode::ReturnNonLocal(_)) | Some(Bytecode::ReturnSelf) => {
                let idx_of_pop_before_potential_return_self = ctxt.get_instructions().len() - 1;

                ctxt.get_instructions().iter().enumerate().any(|(bc_idx, bc)| match bc {
                    Bytecode::Jump(jump_idx)
                    | Bytecode::JumpOnTrueTopNil(jump_idx)
                    | Bytecode::JumpOnFalseTopNil(jump_idx)
                    | Bytecode::JumpOnTruePop(jump_idx)
                    | Bytecode::JumpOnFalsePop(jump_idx)
                    | Bytecode::JumpIfGreater(jump_idx) => bc_idx + *jump_idx as usize >= idx_of_pop_before_potential_return_self,
                    _ => false,
                })
            }
            _ => true,
        }
    }

    fn make_trivial_method_if_possible(body: &Vec<Bytecode>, literals: &[Literal], signature: &str, nbr_params: usize) -> Option<Method> {
        match (body.as_slice(), nbr_params) {
            ([Bytecode::PushGlobal(x), Bytecode::ReturnLocal], 0) => match literals.get(*x as usize)? {
                Literal::Symbol(interned) => Some(Method::TrivialGlobal(
                    TrivialGlobalMethod {
                        global_name: *interned,
                        cached_entry: Cell::new(None),
                    },
                    BasicMethodInfo::new(String::from(signature), Gc::default()),
                )),
                _ => None,
            },
            ([Bytecode::PushField(x), Bytecode::ReturnLocal], 0) => Some(Method::TrivialGetter(
                TrivialGetterMethod { field_idx: *x },
                BasicMethodInfo::new(String::from(signature), Gc::default()),
            )),
            ([Bytecode::PushArg(1), Bytecode::PopField(x), Bytecode::ReturnSelf], 1) => Some(Method::TrivialSetter(
                TrivialSetterMethod { field_idx: *x },
                BasicMethodInfo::new(String::from(signature), Gc::default()),
            )),
            ([literal_bc, Bytecode::ReturnLocal], 0) => {
                let maybe_literal = match literal_bc {
                    Bytecode::PushConstant(x) => literals.get(*x as usize),
                    Bytecode::Push0 => Some(&Literal::Integer(0)),
                    Bytecode::Push1 => Some(&Literal::Integer(1)),
                    // this case breaks, which i'm not sure makes sense. it's pretty much unused in our benchmarks anyway + AST doesn't have an equivalent optim like that, so it's OK.
                    // Bytecode::PushBlock(x) => literals.get(*x as usize),
                    _ => None,
                };

                maybe_literal.map(|lit| {
                    Method::TrivialLiteral(
                        TrivialLiteralMethod { literal: lit.clone() },
                        BasicMethodInfo::new(String::from(signature), Gc::default()),
                    )
                })
            }
            _ => None,
        }
    }

    let mut ctxt = MethodGenCtxt {
        signature: defn.signature.clone(),
        inner: BlockGenCtxt {
            outer,
            // args: {
            //     let mut args = IndexSet::new();
            //     args.insert(String::from("self"));
            //     args
            // },
            // locals: match &defn.body {
            //     ast::MethodBody::Primitive => IndexSet::new(),
            //     ast::MethodBody::Body { locals, .. } => locals.iter().cloned().collect(),
            // },
            literals: IndexSet::new(),
            body: None,
            locals_nbr: {
                match &defn.body {
                    MethodBody::Primitive => 0,
                    MethodBody::Body { locals_nbr, .. } => *locals_nbr,
                }
            },
            args_nbr: {
                match defn.signature.chars().next().unwrap() {
                    '~' | '&' | '|' | '*' | '/' | '\\' | '+' | '=' | '>' | '<' | ',' | '@' | '%' | '-' => 2,
                    _ => defn.signature.chars().filter(|c| *c == ':').count(),
                }
            },
            #[cfg(feature = "frame-debug-info")]
            debug_info: {
                match &defn.body {
                    MethodBody::Primitive => BlockDebugInfo {
                        parameters: vec![],
                        locals: vec![],
                    },
                    MethodBody::Body { debug_info, .. } => debug_info.clone(),
                }
            },
        },
    };

    // match &defn.kind {
    //     ast::MethodKind::Unary => {}
    //     ast::MethodKind::Positional { parameters } => {
    //         for param in parameters {
    //             ctxt.push_arg(param.clone());
    //         }
    //     }
    //     ast::MethodKind::Operator { rhs } => {
    //         ctxt.push_arg(rhs.clone());
    //     }
    // }

    match &defn.body {
        ast::MethodBody::Primitive => {}
        ast::MethodBody::Body { body, .. } => {
            for expr in &body.exprs {
                expr.codegen(&mut ctxt, gc_interface)?;
                ctxt.push_instr(Bytecode::Pop);
            }

            if should_add_return_self(&mut ctxt, body) {
                ctxt.push_instr(Bytecode::ReturnSelf);
            } else {
                ctxt.pop_instr(); // we can otherwise remove the then-redundant final POP (since it's after an unavoidable "Return" type bytecode.
            };

            ctxt.remove_dup_popx_pop_sequences();
        }
    }

    let method = {
        match &defn.body {
            ast::MethodBody::Primitive => Method::Primitive(&*UNIMPLEM_PRIMITIVE, BasicMethodInfo::new(String::from(""), Gc::default())),
            ast::MethodBody::Body { .. } => {
                // let locals = std::mem::take(&mut ctxt.inner.locals);
                let nbr_locals = ctxt.inner.locals_nbr as u8;
                let body = ctxt.inner.body.clone().unwrap_or_default();
                let literals: Vec<Literal> = ctxt.inner.literals.clone().into_iter().collect();
                let signature = ctxt.signature.clone();
                let max_stack_size = get_max_stack_size(&body, ctxt.get_interner());
                let nbr_params = {
                    match ctxt.signature.chars().next() {
                        Some(ch) if !ch.is_alphabetic() => 1,
                        _ => ctxt.signature.chars().filter(|ch| *ch == ':').count() as u8,
                    }
                };

                if let Some(trivial_method) = make_trivial_method_if_possible(&body, &literals, &signature, nbr_params as usize) {
                    trivial_method
                } else {
                    let inline_cache = vec![None; body.len()];
                    #[cfg(feature = "frame-debug-info")]
                    let dbg_info = ctxt.inner.debug_info;

                    Method::Defined(MethodInfo {
                        base_method_info: BasicMethodInfo::new(signature, Gc::default()),
                        body,
                        nbr_locals,
                        nbr_params,
                        literals,
                        inline_cache,
                        max_stack_size,
                        #[cfg(feature = "frame-debug-info")]
                        block_debug_info: dbg_info,
                    })
                }
            }
        }
    };

    // println!("(method) compiled '{}' !", defn.signature);

    Some(method)
}

fn compile_block(outer: &mut dyn GenCtxt, defn: &ast::Block, gc_interface: &mut GCInterface) -> Option<Block> {
    // println!("(system) compiling block ...");

    let mut ctxt = BlockGenCtxt {
        outer,
        args_nbr: defn.nbr_params,
        locals_nbr: defn.nbr_locals,
        // dbg_info: defn.dbg_info,
        literals: IndexSet::new(),
        body: None,
        #[cfg(feature = "frame-debug-info")]
        debug_info: defn.dbg_info.clone(),
    };

    let splitted = defn.body.exprs.split_last();
    if let Some((last, rest)) = splitted {
        for expr in rest {
            expr.codegen(&mut ctxt, gc_interface)?;
            ctxt.push_instr(Bytecode::Pop);
        }
        last.codegen(&mut ctxt, gc_interface)?;
        ctxt.push_instr(Bytecode::ReturnLocal);
    }
    ctxt.remove_dup_popx_pop_sequences();

    if ctxt.body.is_none() {
        ctxt.push_instr(Bytecode::PushNil);
        ctxt.push_instr(Bytecode::ReturnLocal);
    }

    let frame = None;
    // let locals = {
    // let locals = std::mem::take(&mut ctxt.locals);
    // locals
    //     .into_iter()
    //     .map(|name| ctxt.intern_symbol(&name))
    //     .collect()
    // };
    let literals: Vec<Literal> = ctxt.literals.clone().into_iter().collect();
    let signature = String::from("--block--");
    let body = ctxt.body.clone().unwrap_or_default();
    let nbr_locals = ctxt.locals_nbr as u8;
    let nbr_params = ctxt.args_nbr as u8;
    let inline_cache = vec![None; body.len()];
    let max_stack_size = get_max_stack_size(&body, ctxt.get_interner());

    let block = Block {
        frame,
        blk_info: gc_interface.alloc(
            Method::Defined(MethodInfo {
                base_method_info: BasicMethodInfo::new(signature, Gc::default()),
                nbr_locals,
                literals,
                body,
                nbr_params,
                inline_cache,
                max_stack_size,
                #[cfg(feature = "frame-debug-info")]
                block_debug_info: ctxt.debug_info,
            }),
            AllocSiteMarker::Method,
        ),
    };

    // println!("(system) compiled block !");

    Some(block)
}

pub fn compile_class(
    interner: &mut Interner,
    defn: &ast::ClassDef,
    super_class: Option<&Gc<Class>>,
    gc_interface: &mut GCInterface,
) -> Option<Gc<Class>> {
    let mut locals = IndexSet::new();

    fn collect_static_locals(class: &Gc<Class>, locals: &mut IndexSet<Interned>) {
        if let Some(class) = class.super_class() {
            collect_static_locals(&class, locals);
        }
        locals.extend(&class.field_names);
    }

    if let Some(super_class) = super_class {
        collect_static_locals(&super_class.class(), &mut locals);
    }

    locals.extend(defn.static_locals.iter().map(|name| interner.intern(name.as_str())));

    let mut static_class_ctxt = ClassGenCtxt {
        name: format!("{} class", defn.name),
        fields: locals,
        methods: IndexMap::new(),
        interner,
    };

    let static_class = Class {
        name: static_class_ctxt.name.clone(),
        class: Gc::default(),
        super_class: None,
        fields: vec![],
        field_names: vec![],
        methods: IndexMap::new(),
        is_static: true,
    };

    let static_class_gc_ptr = gc_interface.alloc(static_class, AllocSiteMarker::Class);

    for method in &defn.static_methods {
        let signature = static_class_ctxt.interner.intern(method.signature.as_str());
        let mut method = compile_method(&mut static_class_ctxt, method, gc_interface)?;
        method.set_holder(&static_class_gc_ptr);

        // bit of a hack, bytecode should be on the heap really
        if let Method::Defined(method_info) = &method {
            gc_interface.total_program_repr_size += (method_info.body.len() * size_of::<Bytecode>()) as u128;
        }

        static_class_ctxt.methods.insert(signature, gc_interface.alloc(method, AllocSiteMarker::Method));
    }

    if let Some(primitives) = primitives::get_class_primitives(&defn.name) {
        for &(signature, primitive, _warning) in primitives {
            //let symbol = static_class_ctxt.interner.intern(signature);
            //if warning && !static_class_ctxt.methods.contains_key(&symbol) {
            //    eprintln!("Warning: Primitive '{}' is not in class definition for class '{}'", signature, defn.name);
            //}

            let method = Method::Primitive(primitive, BasicMethodInfo::new(String::from(signature), static_class_gc_ptr.clone()));

            let signature = static_class_ctxt.interner.intern(signature);
            static_class_ctxt.methods.insert(signature, gc_interface.alloc(method, AllocSiteMarker::Method));
        }
    }

    let mut static_class_mut = static_class_gc_ptr.clone(); // todo couldn't we have done that before
    static_class_mut.fields = vec![Value::NIL; static_class_ctxt.fields.len()];
    static_class_mut.field_names = static_class_ctxt.fields.into_iter().collect();
    static_class_mut.methods = static_class_ctxt.methods;

    // not adding field names, since they're debugging information, and interned anyway.
    gc_interface.total_program_repr_size += (static_class_mut.fields.len() * size_of::<Value>()) as u128;

    let mut locals = IndexSet::new();

    fn collect_instance_locals(class: &Gc<Class>, locals: &mut IndexSet<Interned>) {
        if let Some(class) = class.super_class() {
            collect_instance_locals(&class, locals);
        }
        locals.extend(&class.field_names);
    }

    if let Some(super_class) = super_class {
        collect_instance_locals(super_class, &mut locals);
    }

    locals.extend(defn.instance_locals.iter().map(|name| interner.intern(name.as_str())));

    let mut instance_class_ctxt = ClassGenCtxt {
        name: defn.name.clone(),
        fields: locals,
        methods: IndexMap::new(),
        interner,
    };

    let instance_class = Class {
        name: instance_class_ctxt.name.clone(),
        class: static_class_gc_ptr,
        super_class: None,
        fields: vec![],
        field_names: vec![],
        methods: IndexMap::new(),
        is_static: false,
    };

    let instance_class_gc_ptr = gc_interface.alloc(instance_class, AllocSiteMarker::Class);

    for method in &defn.instance_methods {
        let signature = instance_class_ctxt.interner.intern(method.signature.as_str());
        let mut method = compile_method(&mut instance_class_ctxt, method, gc_interface)?;
        method.set_holder(&instance_class_gc_ptr);

        // bit of a hack, bytecode should be on the heap really
        if let Method::Defined(method_info) = &method {
            gc_interface.total_program_repr_size += (method_info.body.len() * size_of::<Bytecode>()) as u128;
        }

        instance_class_ctxt.methods.insert(signature, gc_interface.alloc(method, AllocSiteMarker::Method));
    }

    if let Some(primitives) = primitives::get_instance_primitives(&defn.name) {
        for &(signature, primitive, _warning) in primitives {
            //let symbol = instance_class_ctxt.interner.intern(signature);
            //if warning && !instance_class_ctxt.methods.contains_key(&symbol) {
            //    eprintln!("Warning: Primitive '{}' is not in class definition for class '{}'", signature, defn.name);
            //}

            let method = Method::Primitive(primitive, BasicMethodInfo::new(String::from(signature), instance_class_gc_ptr.clone()));
            let signature = instance_class_ctxt.interner.intern(signature);
            instance_class_ctxt.methods.insert(signature, gc_interface.alloc(method, AllocSiteMarker::Method));
        }
    }

    let mut instance_class_mut = instance_class_gc_ptr.clone();
    // instance_class_mut.fields = instance_class_ctxt.fields.into_iter().map(|name| (name, Value::NIL)).collect();
    instance_class_mut.fields = vec![Value::NIL; instance_class_ctxt.fields.len()];
    instance_class_mut.field_names = instance_class_ctxt.fields.into_iter().collect();
    instance_class_mut.methods = instance_class_ctxt.methods;

    // not adding field names, since they're debugging information, and interned anyway.
    gc_interface.total_program_repr_size += (instance_class_mut.fields.len() * size_of::<Value>()) as u128;

    Some(instance_class_gc_ptr)
}
