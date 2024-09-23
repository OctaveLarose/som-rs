//!
//! This is the bytecode compiler for the Simple Object Machine.
//!
use std::cell::RefCell;
use std::hash::{Hash, Hasher};

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;

use crate::block::{Block, BlockInfo};
use crate::class::Class;
#[cfg(not(feature = "inlining-disabled"))]
use crate::inliner::PrimMessageInliner;
use crate::interner::{Interned, Interner};
use crate::method::{Method, MethodEnv, MethodKind};
use crate::primitives;
use crate::value::Value;
use som_core::ast;
#[cfg(feature = "frame-debug-info")]
use som_core::ast::BlockDebugInfo;
use som_core::ast::{Expression, MethodBody};
use som_core::bytecode::Bytecode;
use som_core::gc::{GCInterface, GCRef};

#[derive(Debug, Clone)]
pub enum Literal {
    Symbol(Interned),
    String(GCRef<String>),
    Double(f64),
    Integer(i32),
    BigInteger(BigInt),
    Array(Vec<u8>),
    Block(GCRef<Block>),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::Symbol(val1), Literal::Symbol(val2)) => val1.eq(val2),
            (Literal::String(val1), Literal::String(val2)) => val1.eq(val2),
            (Literal::Double(val1), Literal::Double(val2)) => val1.eq(val2),
            (Literal::Integer(val1), Literal::Integer(val2)) => val1.eq(val2),
            (Literal::BigInteger(val1), Literal::BigInteger(val2)) => val1.eq(val2),
            (Literal::Array(val1), Literal::Array(val2)) => val1.eq(val2),
            (Literal::Block(val1), Literal::Block(val2)) => val1 == val2,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::Symbol(val) => {
                state.write(b"sym#");
                val.hash(state);
            }
            Literal::String(val) => {
                state.write(b"string#");
                val.to_obj().hash(state);
            }
            Literal::Double(val) => {
                state.write(b"dbl#");
                val.to_bits().hash(state);
            }
            Literal::Integer(val) => {
                state.write(b"int#");
                val.hash(state);
            }
            Literal::BigInteger(val) => {
                state.write(b"bigint#");
                val.hash(state);
            }
            Literal::Array(val) => {
                state.write(b"array#");
                val.hash(state);
            }
            Literal::Block(val) => {
                state.write(b"blk");
                val.to_obj().hash(state);
            }
        }
    }
}

pub trait GenCtxt {
    fn intern_symbol(&mut self, name: &str) -> Interned;
    fn lookup_symbol(&self, id: Interned) -> &str;
    fn get_scope(&self) -> usize; // todo: i think this might be better in innergenctxt
    fn class_name(&self) -> &str;
}

pub trait InnerGenCtxt: GenCtxt {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt;
    fn push_instr(&mut self, instr: Bytecode);
    fn pop_instr(&mut self);
    fn get_instructions(&self) -> &Vec<Bytecode>;
    fn get_nbr_locals(&self) -> usize;
    fn set_nbr_locals(&mut self, nbr_locals: usize);
    fn get_nbr_args(&self) -> usize;
    fn set_nbr_args(&mut self, nbr_args: usize);
    fn get_literal(&self, idx: usize) -> Option<&Literal>;
    fn push_literal(&mut self, literal: Literal) -> usize;
    fn remove_literal(&mut self, idx: usize) -> Option<Literal>;
    fn get_cur_instr_idx(&self) -> usize;
    fn patch_jump(&mut self, idx_to_backpatch: usize, new_val: usize);
    fn backpatch_jump_to_current(&mut self, idx_to_backpatch: usize);
    fn remove_dup_popx_pop_sequences(&mut self);
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

    fn lookup_symbol(&self, id: Interned) -> &str {
        self.outer.lookup_symbol(id)
    }

    fn get_scope(&self) -> usize {
        self.outer.get_scope() + 1
    }
    
    fn class_name(&self) -> &str {
        self.outer.class_name()
    }
}

impl InnerGenCtxt for BlockGenCtxt<'_> {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt {
        self
    }

    fn push_instr(&mut self, instr: Bytecode) {
        let body = self.body.get_or_insert_with(|| vec![]);
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
        return self.body.as_ref().unwrap().iter().len();
    }

    fn backpatch_jump_to_current(&mut self, idx_to_backpatch: usize) {
        let jump_offset = self.get_cur_instr_idx() - idx_to_backpatch;
        self.patch_jump(idx_to_backpatch, jump_offset)
    }

    fn patch_jump(&mut self, idx_to_patch: usize, new_val: usize) {
        match self.body.as_mut().unwrap().get_mut(idx_to_patch).unwrap() {
            Bytecode::Jump(jump_idx) | Bytecode::JumpBackward(jump_idx)
            | Bytecode::JumpOnTrueTopNil(jump_idx) | Bytecode::JumpOnFalseTopNil(jump_idx)
            | Bytecode::JumpOnTruePop(jump_idx) | Bytecode::JumpOnFalsePop(jump_idx) => {
                *jump_idx = new_val
            }
            _ => panic!("Attempting to patch a bytecode non jump"),
        };
    }

    fn get_nbr_locals(&self) -> usize {
        self.locals_nbr
    }

    fn set_nbr_locals(&mut self, nbr_locals: usize) {
        self.locals_nbr = nbr_locals;
    }

    fn get_nbr_args(&self) -> usize {
        self.args_nbr
    }

    fn set_nbr_args(&mut self, nbr_args: usize) {
        self.args_nbr = nbr_args
    }

    fn remove_dup_popx_pop_sequences(&mut self) {
        if self.body.is_none() || self.body.as_ref().unwrap().len() < 3 {
            return;
        }

        let body = self.body.as_mut().unwrap();

        let mut indices_to_remove: Vec<usize> = vec![];

        for (idx, bytecode_win) in body.windows(3).enumerate() {
            if matches!(bytecode_win[0], Bytecode::Dup)
                && matches!(
                    bytecode_win[1],
                    Bytecode::PopField(..) | Bytecode::PopLocal(..) | Bytecode::PopArg(..)
                )
                && matches!(bytecode_win[2], Bytecode::Pop)
            {
                let are_bc_jump_targets =
                    body
                        .iter()
                        .enumerate()
                        .any(|(maybe_jump_idx, bc)| match bc {
                            Bytecode::Jump(jump_offset)
                            | Bytecode::JumpOnTrueTopNil(jump_offset)
                            | Bytecode::JumpOnFalseTopNil(jump_offset)
                            | Bytecode::JumpOnTruePop(jump_offset)
                            | Bytecode::JumpOnFalsePop(jump_offset) => {
                                let bc_target_idx = maybe_jump_idx + *jump_offset;
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

        let mut jumps_to_patch = vec![];
        for (cur_idx, bc) in body.iter().enumerate() {
            match bc {
                Bytecode::Jump(jump_offset)
                | Bytecode::JumpOnTrueTopNil(jump_offset)
                | Bytecode::JumpOnFalseTopNil(jump_offset)
                | Bytecode::JumpOnTruePop(jump_offset)
                | Bytecode::JumpOnFalsePop(jump_offset) => {
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

                    let nbr_to_adjust = indices_to_remove
                        .iter()
                        .filter(|&&idx| cur_idx < idx && idx <= cur_idx + jump_offset)
                        .count();
                    jumps_to_patch.push((cur_idx, jump_offset - nbr_to_adjust));
                }
                Bytecode::JumpBackward(jump_offset) => {
                    let nbr_to_adjust = indices_to_remove
                        .iter()
                        .filter(|&&idx| cur_idx > idx && idx > cur_idx - jump_offset)
                        .count();
                    jumps_to_patch.push((cur_idx, jump_offset - nbr_to_adjust));
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

    fn lookup_symbol(&self, id: Interned) -> &str {
        self.inner.lookup_symbol(id)
    }

    fn get_scope(&self) -> usize {
        0
    }
    
    fn class_name(&self) -> &str {
        self.inner.class_name()
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
        return self.inner.get_cur_instr_idx();
    }

    fn patch_jump(&mut self, idx_to_backpatch: usize, new_val: usize) {
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
    
    fn get_nbr_args(&self) -> usize {
        self.inner.get_nbr_args()
    }

    fn set_nbr_args(&mut self, nbr_args: usize) {
        self.inner.set_nbr_args(nbr_args)
    }
}

pub trait MethodCodegen {
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
            ast::Expression::FieldRead(idx) => {
                ctxt.push_instr(Bytecode::PushField(*idx as u8));
                Some(())
            }
            ast::Expression::ArgRead(up_idx, idx) => {
                match (up_idx, idx) {
                    (0, 0) => ctxt.push_instr(Bytecode::PushSelf),
                    (0, _) => ctxt.push_instr(Bytecode::PushArg(*idx as u8)),
                    _ => ctxt.push_instr(Bytecode::PushNonLocalArg(*up_idx as u8, *idx as u8))
                };
                Some(())
            }
            ast::Expression::GlobalRead(name) => {
                match name.as_str() {
                    "nil" => ctxt.push_instr(Bytecode::PushNil),
                    "super" => {
                        match ctxt.get_scope() {
                            0 => ctxt.push_instr(Bytecode::PushSelf),
                            scope => ctxt.push_instr(Bytecode::PushNonLocalArg(scope as u8, 0))
                        }
                    },
                    _ => {
                        let name = ctxt.intern_symbol(name);
                        let idx = ctxt.push_literal(Literal::Symbol(name));
                        ctxt.push_instr(Bytecode::PushGlobal(idx as u8));
                    }
                }
                Some(())
            }
            ast::Expression::LocalVarWrite(_, expr) | ast::Expression::NonLocalVarWrite(_, _, expr) => {
                expr.codegen(ctxt, mutator)?;
                ctxt.push_instr(Bytecode::Dup);
                match self {
                    ast::Expression::LocalVarWrite(idx, _) => ctxt.push_instr(Bytecode::PopLocal(0, *idx as u8)),
                    ast::Expression::NonLocalVarWrite(up_idx, idx, _) => ctxt.push_instr(Bytecode::PopLocal(*up_idx as u8, *idx as u8)),
                    _ => unreachable!()
                }
                Some(())
            }
            ast::Expression::GlobalWrite(_name, _expr) => {
                todo!("handle global write: it's an unresolved field write")
            }
            ast::Expression::FieldWrite(idx, expr) => {
                expr.codegen(ctxt, mutator)?;
                ctxt.push_instr(Bytecode::Dup);
                ctxt.push_instr(Bytecode::PopField(*idx as u8));
                Some(())
            }
            ast::Expression::ArgWrite(up_idx, idx, expr) => {
                expr.codegen(ctxt, mutator)?;
                ctxt.push_instr(Bytecode::Dup);
                ctxt.push_instr(Bytecode::PopArg(*up_idx as u8, *idx as u8));
                Some(())
            }
            ast::Expression::Message(message) => {
                let is_super_call = match &message.receiver {
                    _super if _super == &Expression::GlobalRead(String::from("super")) => true,
                    _ => false
                };
                
                message.receiver.codegen(ctxt, mutator)?;

                #[cfg(not(feature = "inlining-disabled"))]
                if message.inline_if_possible(ctxt, mutator).is_some() {
                    return Some(());
                }

                if (message.signature == "+" || message.signature == "-") && !is_super_call && message.values.len() == 1 && message.values.get(0)? == &Expression::Literal(ast::Literal::Integer(1)) {
                    match message.signature.as_str() {
                        "+" => ctxt.push_instr(Bytecode::Inc), // also i was considering handling the "+ X" arbitrary case, maybe.,
                        "-" => ctxt.push_instr(Bytecode::Dec),
                        _ => unreachable!()
                    };
                    return Some(())
                }
                
                message
                    .values
                    .iter()
                    .try_for_each(|value| value.codegen(ctxt, mutator))?;

                let nb_params = match message.signature.chars().nth(0) {
                    Some(ch) if !ch.is_alphabetic() => 1,
                    _ => message.signature.chars().filter(|ch| *ch == ':').count(),
                };

                let sym = ctxt.intern_symbol(message.signature.as_str());
                let idx = ctxt.push_literal(Literal::Symbol(sym));
                
                match is_super_call {
                    false => {
                        match nb_params {
                            0 => ctxt.push_instr(Bytecode::Send1(idx as u8)),
                            1 => ctxt.push_instr(Bytecode::Send2(idx as u8)),
                            2 => ctxt.push_instr(Bytecode::Send3(idx as u8)),
                            _ => ctxt.push_instr(Bytecode::SendN(idx as u8)),
                        }
                    },
                    true => {
                        match nb_params {
                            0 => ctxt.push_instr(Bytecode::SuperSend1(idx as u8)),
                            1 => ctxt.push_instr(Bytecode::SuperSend2(idx as u8)),
                            2 => ctxt.push_instr(Bytecode::SuperSend3(idx as u8)),
                            _ => ctxt.push_instr(Bytecode::SuperSendN(idx as u8)),
                        }
                    }
                }
                
                Some(())
            }
            ast::Expression::Exit(expr, scope) => {
                match scope {
                    0 => {
                        match expr.as_ref() {
                            Expression::ArgRead(0, 0) => ctxt.push_instr(Bytecode::ReturnSelf),
                            _ => {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr(Bytecode::ReturnLocal)
                            }
                        }
                    }
                    _ => {
                        expr.codegen(ctxt, mutator)?;
                        ctxt.push_instr(Bytecode::ReturnNonLocal(*scope as u8));
                    }
                };

                Some(())
            }
            ast::Expression::Literal(literal) => {
                fn convert_literal(ctxt: &mut dyn InnerGenCtxt, literal: &ast::Literal, mutator: &mut GCInterface) -> Literal {
                    match literal {
                        ast::Literal::Symbol(val) => {
                            Literal::Symbol(ctxt.intern_symbol(val.as_str()))
                        }
                        ast::Literal::String(val) => {
                            // TODO: this whole bit is to avoid redundant literals. previous logic broke with strings being put on the GC heap. is it indicative of a deeper issue with redundant strings?
                            // it feels a bit bandaid-ey, since I'm not sure where the bug came from exactly.
                            // it feels like tests should still pass without all this logic, but they don't (see specialized BC PushConstant one), and I'm not *positive* that's normal?
                            // also NB: this code was a mild speeddown! could be removed, and the PushConst test deactivated/fixed another way, probably. keeping it for now. 
                            let mut i = 0;
                            loop {
                                let lit = ctxt.get_literal(i);
                                match lit {
                                    None => break Literal::String(GCRef::<String>::alloc(val.clone(), mutator)), // reached end of literals and no duplicate, we alloc
                                    Some(str_lit @ Literal::String(str_ptr)) if str_ptr.to_obj() == val => break str_lit.clone(),
                                    _ => {}
                                }
                                i += 1;
                            }
                        },
                        ast::Literal::Double(val) => Literal::Double(*val),
                        ast::Literal::Integer(val) => Literal::Integer(*val),
                        ast::Literal::BigInteger(val) => Literal::BigInteger(val.parse().unwrap()),
                        ast::Literal::Array(val) => {
                            let literals = val
                                .iter()
                                .map(|val| {
                                    let literal = convert_literal(ctxt, val, mutator);
                                    ctxt.push_literal(literal) as u8
                                })
                                .collect();
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
                        match idx {
                            0 => ctxt.push_instr(Bytecode::PushConstant0),
                            1 => ctxt.push_instr(Bytecode::PushConstant1),
                            2 => ctxt.push_instr(Bytecode::PushConstant2),
                            _ => ctxt.push_instr(Bytecode::PushConstant(idx as u8)),
                        }
                    }
                }

                Some(())
            }
            ast::Expression::Block(val) => {
                let block = compile_block(ctxt.as_gen_ctxt(), val, mutator)?;
                let block = Literal::Block(GCRef::<Block>::alloc(block, mutator));
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
    pub methods: IndexMap<Interned, GCRef<Method>>,
    pub interner: &'a mut Interner,
}

impl GenCtxt for ClassGenCtxt<'_> {
    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.interner.intern(name)
    }

    fn lookup_symbol(&self, id: Interned) -> &str {
        self.interner.lookup(id)
    }

    fn get_scope(&self) -> usize {
        unreachable!("Asking for scope in a class generation context?")
    }
    
    fn class_name(&self) -> &str {
        self.name.as_str()
    }
}

fn compile_method(outer: &mut dyn GenCtxt, defn: &ast::MethodDef, mutator: &mut GCInterface) -> Option<Method> {
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

                ctxt.get_instructions().iter().enumerate().any(|(bc_idx, bc)| {
                    match bc {
                        Bytecode::Jump(jump_idx)
                        | Bytecode::JumpOnTrueTopNil(jump_idx)
                        | Bytecode::JumpOnFalseTopNil(jump_idx)
                        | Bytecode::JumpOnTruePop(jump_idx)
                        | Bytecode::JumpOnFalsePop(jump_idx)
                        => {
                            bc_idx + jump_idx >= idx_of_pop_before_potential_return_self
                        }
                        _ => false
                    }
                })
            }
            _ => true
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
                    MethodBody::Body { locals_nbr, .. } => *locals_nbr
                }
            },
            args_nbr: {
                match defn.signature.chars().next().unwrap() {
                    '~' | '&' | '|' | '*' | '/' | '\\' | '+' | '=' | '>' | '<' | ',' | '@' | '%' | '-' => 2,
                    _ => defn.signature.chars().filter(|c| *c == ':').count()
                }
            },
            #[cfg(feature = "frame-debug-info")]
            debug_info: {
                match &defn.body {
                    MethodBody::Primitive => { BlockDebugInfo{ parameters: vec![], locals: vec![] } }
                    MethodBody::Body { debug_info, .. } => debug_info.clone()
            }},
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
                expr.codegen(&mut ctxt, mutator)?;
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

    let method = Method {
        kind: match &defn.body {
            ast::MethodBody::Primitive => MethodKind::NotImplemented(defn.signature.clone()),
            ast::MethodBody::Body { .. } => {
                // let locals = std::mem::take(&mut ctxt.inner.locals);
                let nbr_locals = ctxt.inner.locals_nbr;
                let body = ctxt.inner.body.unwrap_or_default();
                let literals = ctxt.inner.literals.into_iter().collect();
                let inline_cache = RefCell::new(vec![None; body.len()]);
                #[cfg(feature = "frame-debug-info")]
                let dbg_info = ctxt.inner.debug_info;

                MethodKind::Defined(MethodEnv {
                    body,
                    nbr_locals,
                    literals,
                    inline_cache,
                    #[cfg(feature = "frame-debug-info")]
                    block_debug_info: dbg_info
                })
            }
        },
        holder: GCRef::default(),
        signature: ctxt.signature,
    };

    // println!("(method) compiled '{}' !", defn.signature);

    Some(method)
}

fn compile_block(outer: &mut dyn GenCtxt, defn: &ast::Block, mutator: &mut GCInterface) -> Option<Block> {
    // println!("(system) compiling block ...");

    let mut ctxt = BlockGenCtxt {
        outer,
        args_nbr: defn.nbr_params,
        locals_nbr: defn.nbr_locals,
        // dbg_info: defn.dbg_info,
        literals: IndexSet::new(),
        body: None,
        #[cfg(feature = "frame-debug-info")]
        debug_info: defn.dbg_info.clone()
    };

    let splitted = defn.body.exprs.split_last();
    if let Some((last, rest)) = splitted {
        for expr in rest {
            expr.codegen(&mut ctxt, mutator)?;
            ctxt.push_instr(Bytecode::Pop);
        }
        last.codegen(&mut ctxt, mutator)?;
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
    let literals = ctxt.literals.into_iter().collect();
    let body = ctxt.body.unwrap_or_default();
    let nb_locals = ctxt.locals_nbr;
    let nb_params = ctxt.args_nbr;
    let inline_cache = RefCell::new(vec![None; body.len()]);

    let block = Block {
        frame,
        blk_info: GCRef::<BlockInfo>::alloc(BlockInfo {
            // locals,
            nb_locals,
            literals,
            body,
            nb_params,
            inline_cache,
            #[cfg(feature = "frame-debug-info")]
            block_debug_info: ctxt.debug_info
        }, mutator),
    };

    // println!("(system) compiled block !");

    Some(block)
}

pub fn compile_class(
    interner: &mut Interner,
    defn: &ast::ClassDef,
    super_class: Option<&GCRef<Class>>,
    mutator: &mut GCInterface,
) -> Option<GCRef<Class>> {
    let mut locals = IndexSet::new();

    fn collect_static_locals(
        interner: &mut Interner,
        class: &GCRef<Class>,
        locals: &mut IndexSet<Interned>,
    ) {
        if let Some(class) = class.to_obj().super_class() {
            collect_static_locals(interner, &class, locals);
        }
        locals.extend(class.to_obj().locals.keys().copied());
    }

    if let Some(super_class) = super_class {
        collect_static_locals(interner, &super_class.to_obj().class(), &mut locals);
    }

    locals.extend(
        defn.static_locals
            .iter()
            .map(|name| interner.intern(name.as_str())),
    );

    let mut static_class_ctxt = ClassGenCtxt {
        name: format!("{} class", defn.name),
        fields: locals,
        methods: IndexMap::new(),
        interner,
    };

    let static_class = Class {
        name: static_class_ctxt.name.clone(),
        class: GCRef::default(),
        super_class: None,
        locals: IndexMap::new(),
        methods: IndexMap::new(),
        is_static: true,
    };

    let static_class_gc_ptr = GCRef::<Class>::alloc(static_class, mutator);

    for method in &defn.static_methods {
        let signature = static_class_ctxt.interner.intern(method.signature.as_str());
        let mut method = compile_method(&mut static_class_ctxt, method, mutator)?;
        method.holder = static_class_gc_ptr;
        static_class_ctxt.methods.insert(signature, GCRef::<Method>::alloc(method, mutator));
    }

    if let Some(primitives) = primitives::get_class_primitives(&defn.name) {
        for &(signature, primitive, warning) in primitives {
            let symbol = static_class_ctxt.interner.intern(signature);
            if warning && !static_class_ctxt.methods.contains_key(&symbol) {
                eprintln!(
                    "Warning: Primitive '{}' is not in class definition for class '{}'",
                    signature, defn.name
                );
            }

            let method = Method {
                signature: signature.to_string(),
                kind: MethodKind::Primitive(primitive),
                holder: static_class_gc_ptr,
            };
            let signature = static_class_ctxt.interner.intern(signature);
            static_class_ctxt.methods.insert(signature, GCRef::<Method>::alloc(method, mutator));
        }
    }

    let static_class_mut = static_class_gc_ptr.to_obj(); // todo couldn't we have done that before
    static_class_mut.locals = static_class_ctxt
        .fields
        .into_iter()
        .map(|name| (name, Value::NIL))
        .collect();
    static_class_mut.methods = static_class_ctxt.methods;
    // drop(static_class_mut);

    // for method in static_class.borrow().methods.values() {
    //     println!("{}", method);
    // }

    let mut locals = IndexSet::new();

    fn collect_instance_locals(
        interner: &mut Interner,
        class: &GCRef<Class>,
        locals: &mut IndexSet<Interned>,
    ) {
        if let Some(class) = class.to_obj().super_class() {
            collect_instance_locals(interner, &class, locals);
        }
        locals.extend(class.to_obj().locals.keys());
    }

    if let Some(super_class) = super_class {
        collect_instance_locals(interner, super_class, &mut locals);
    }

    locals.extend(
        defn.instance_locals
            .iter()
            .map(|name| interner.intern(name.as_str())),
    );

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
        locals: IndexMap::new(),
        methods: IndexMap::new(),
        is_static: false,
    };

    let instance_class_gc_ptr = GCRef::<Class>::alloc(instance_class, mutator);

    for method in &defn.instance_methods {
        let signature = instance_class_ctxt
            .interner
            .intern(method.signature.as_str());
        let mut method = compile_method(&mut instance_class_ctxt, method, mutator)?;
        method.holder = instance_class_gc_ptr;
        instance_class_ctxt
            .methods
            .insert(signature, GCRef::<Method>::alloc(method, mutator));
    }

    if let Some(primitives) = primitives::get_instance_primitives(&defn.name) {
        for &(signature, primitive, warning) in primitives {
            let symbol = instance_class_ctxt.interner.intern(signature);
            if warning && !instance_class_ctxt.methods.contains_key(&symbol) {
                eprintln!(
                    "Warning: Primitive '{}' is not in class definition for class '{}'",
                    signature, defn.name
                );
            }

            let method = Method {
                signature: signature.to_string(),
                kind: MethodKind::Primitive(primitive),
                holder: instance_class_gc_ptr,
            };
            let signature = instance_class_ctxt.interner.intern(signature);
            instance_class_ctxt
                .methods
                .insert(signature, GCRef::<Method>::alloc(method, mutator));
        }
    }

    let instance_class_mut = instance_class_gc_ptr.to_obj();
    instance_class_mut.locals = instance_class_ctxt
        .fields
        .into_iter()
        .map(|name| (name, Value::NIL))
        .collect();
    instance_class_mut.methods = instance_class_ctxt.methods;
    // drop(instance_class_mut);

    // for method in instance_class.borrow().methods.values() {
    //     println!("{}", method);
    // }

    // println!("compiled '{}' !", defn.name);

    Some(instance_class_gc_ptr)
}
