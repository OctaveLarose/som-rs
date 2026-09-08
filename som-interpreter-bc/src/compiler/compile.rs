//!
//! This is the bytecode compiler for the Simple Object Machine.
//!

use crate::compiler::Literal;
use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use som_core::interner::Interner;
use som_gc::gcref::Gc;
use som_gc::gcslice::GcSlice;
use som_value::interned::Interned;
use std::cell::Cell;
use std::str::FromStr;

use crate::gc::VecLiteral;
use crate::primitives;
use crate::primitives::UNIMPLEM_PRIMITIVE;
use crate::value::Value;
use crate::vm_objects::class::Class;
use crate::vm_objects::method::{BasicMethodInfo, Method, MethodInfo};
use crate::vm_objects::trivial_methods::{TrivialGetterMethod, TrivialGlobalMethod, TrivialLiteralMethod, TrivialSetterMethod};
#[cfg(feature = "frame-debug-info")]
use som_core::ast::BlockDebugInfo;
use som_core::ast::{self};
use som_core::ast::{Expression, MethodBody};
use som_core::bytecode::{read_u16, split_u16, BcEntry, Bytecode, BytecodeIter, BC_SIZE_1_ARG, BC_SIZE_2_ARG, BC_SIZE_NO_ARGS, BC_SIZE_U16_ARG};
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};

#[derive(Debug)]
pub(crate) enum FoundVar {
    Local(u8, u8),
    Argument(u8, u8),
    Field(u8),
}

pub(crate) trait GenCtxt {
    fn find_var(&mut self, name: &str) -> Option<FoundVar>;
    fn intern_symbol(&mut self, name: &str) -> Interned;
    fn get_scope(&self) -> usize;
    fn get_interner(&self) -> &Interner;
}

#[allow(unused)] // Some of them were needed when we did BC-level inlining for a while. They're unused now. Maybe remove.
pub(crate) trait InnerGenCtxt: GenCtxt {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt;
    fn push_instr_no_arg(&mut self, instr: Bytecode);
    fn push_instr_1_arg(&mut self, instr: Bytecode, a: u8);
    fn push_instr_2_args(&mut self, instr: Bytecode, a: u8, b: u8);
    fn push_instr_u16_arg(&mut self, instr: Bytecode, arg: u16);
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

struct BlockGenCtxt<'a> {
    pub outer: &'a mut dyn GenCtxt,
    pub args_nbr: usize,
    pub locals_nbr: usize,
    pub args: IndexSet<String>,
    pub locals: IndexSet<String>,
    pub literals: IndexSet<Literal>,
    pub body: Option<Vec<Bytecode>>,
}

impl GenCtxt for BlockGenCtxt<'_> {
    fn find_var(&mut self, name: &str) -> Option<FoundVar> {
        let name = match name {
            "super" => "self",
            name => name,
        };
        (self.locals.get_index_of(name))
            .map(|idx| FoundVar::Local(0, idx as u8))
            .or_else(|| (self.args.get_index_of(name)).map(|idx| FoundVar::Argument(0, idx as u8)))
            .or_else(|| {
                self.outer.find_var(name).map(|found| match found {
                    FoundVar::Local(up_idx, idx) => FoundVar::Local(up_idx + 1, idx),
                    FoundVar::Argument(up_idx, idx) => FoundVar::Argument(up_idx + 1, idx),
                    FoundVar::Field(idx) => FoundVar::Field(idx),
                })
            })
    }

    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.outer.intern_symbol(name)
    }

    fn get_scope(&self) -> usize {
        self.outer.get_scope() + 1
    }

    fn get_interner(&self) -> &Interner {
        self.outer.get_interner()
    }
}

impl InnerGenCtxt for BlockGenCtxt<'_> {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt {
        self
    }

    fn push_instr_no_arg(&mut self, instr: Bytecode) {
        let body = self.body.get_or_insert_with(Vec::new);
        body.push(instr);
    }

    fn push_instr_1_arg(&mut self, instr: Bytecode, a: u8) {
        let body = self.body.get_or_insert_with(Vec::new);
        body.push(instr);
        unsafe {
            body.push(std::mem::transmute::<u8, Bytecode>(a));
        }
    }

    fn push_instr_2_args(&mut self, instr: Bytecode, a: u8, b: u8) {
        let body = self.body.get_or_insert_with(Vec::new);
        body.push(instr);
        unsafe {
            body.push(std::mem::transmute::<u8, Bytecode>(a));
            body.push(std::mem::transmute::<u8, Bytecode>(b));
        }
    }

    fn push_instr_u16_arg(&mut self, instr: Bytecode, arg: u16) {
        let body = self.body.get_or_insert_with(Vec::new);
        body.push(instr);
        let (a, b) = split_u16(arg);
        unsafe {
            body.push(std::mem::transmute::<u8, Bytecode>(a));
            body.push(std::mem::transmute::<u8, Bytecode>(b));
        }
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
        match self.body.as_ref() {
            Some(body) => body.len(),
            None => 0,
        }
    }

    fn backpatch_jump_to_current(&mut self, idx_to_backpatch: usize) {
        let jump_offset = self.get_cur_instr_idx() - idx_to_backpatch;
        self.patch_jump(idx_to_backpatch, jump_offset as u16)
    }

    fn patch_jump(&mut self, idx_to_patch: usize, new_val: u16) {
        match self.body.as_ref().unwrap().get(idx_to_patch).unwrap() {
            Bytecode::Jump
            | Bytecode::JumpBackward
            | Bytecode::JumpOnTrueTopNil
            | Bytecode::JumpOnFalseTopNil
            | Bytecode::JumpOnTruePop
            | Bytecode::JumpOnFalsePop
            | Bytecode::JumpOnNilTopTop
            | Bytecode::JumpOnNotNilTopTop
            | Bytecode::JumpOnNilPop
            | Bytecode::JumpOnNotNilPop
            | Bytecode::JumpIfGreater => {}
            _ => panic!("Attempting to patch a bytecode non jump"),
        };
        if let Some(body) = &mut self.body {
            let (high_byte, low_byte) = split_u16(new_val);
            body[idx_to_patch + 1] = unsafe { std::mem::transmute::<u8, Bytecode>(high_byte) };
            body[idx_to_patch + 2] = unsafe { std::mem::transmute::<u8, Bytecode>(low_byte) };
            debug_assert_eq!(read_u16(body, idx_to_patch + 1), new_val);
        }
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

        let body = self.body.as_ref().unwrap();

        let mut entries: Vec<(usize, BcEntry)> = Vec::new();
        let mut cur_idx = 0;
        for entry in BytecodeIter::init(body, 0) {
            let size = match entry {
                BcEntry::NoArg(_) => BC_SIZE_NO_ARGS as usize,
                BcEntry::OneArg(_, _) => BC_SIZE_1_ARG as usize,
                BcEntry::TwoArgs(_, _, _) => BC_SIZE_2_ARG as usize,
                BcEntry::U16Arg(_, _) => BC_SIZE_U16_ARG as usize,
            };
            entries.push((cur_idx, entry));
            cur_idx += size;
        }

        let mut indices_to_remove: Vec<usize> = vec![];

        for window in entries.windows(3) {
            let (dup_idx, dup_entry) = &window[0];
            let (_, popx_entry) = &window[1];
            let (pop_idx, pop_entry) = &window[2];

            let is_dup = matches!(dup_entry, BcEntry::NoArg(Bytecode::Dup));
            let is_popx = matches!(
                popx_entry,
                BcEntry::OneArg(Bytecode::PopField, _) | BcEntry::TwoArgs(Bytecode::PopLocal, _, _) | BcEntry::TwoArgs(Bytecode::PopArg, _, _)
            );
            let is_pop = matches!(pop_entry, BcEntry::NoArg(Bytecode::Pop));

            if !(is_dup && is_popx && is_pop) {
                continue;
            }

            let are_bc_jump_targets = entries.iter().any(|(jump_idx, entry)| match entry {
                BcEntry::U16Arg(
                    Bytecode::Jump
                    | Bytecode::JumpOnTrueTopNil
                    | Bytecode::JumpOnFalseTopNil
                    | Bytecode::JumpOnTruePop
                    | Bytecode::JumpOnFalsePop
                    | Bytecode::JumpIfGreater
                    | Bytecode::JumpOnNilPop
                    | Bytecode::JumpOnNotNilPop
                    | Bytecode::JumpOnNilTopTop
                    | Bytecode::JumpOnNotNilTopTop,
                    jump_offset,
                ) => {
                    let bc_target_idx = jump_idx + *jump_offset as usize;
                    bc_target_idx == *dup_idx || bc_target_idx == *pop_idx
                }
                _ => false,
            });

            if are_bc_jump_targets {
                continue;
            }

            indices_to_remove.push(*dup_idx);
            indices_to_remove.push(*pop_idx);
        }

        if indices_to_remove.is_empty() {
            return;
        }

        let mut jumps_to_patch: Vec<(usize, u16)> = vec![];
        for (cur_idx, entry) in &entries {
            match entry {
                BcEntry::U16Arg(
                    Bytecode::Jump
                    | Bytecode::JumpOnTrueTopNil
                    | Bytecode::JumpOnFalseTopNil
                    | Bytecode::JumpOnTruePop
                    | Bytecode::JumpOnFalsePop
                    | Bytecode::JumpIfGreater
                    | Bytecode::JumpOnNilPop
                    | Bytecode::JumpOnNotNilPop
                    | Bytecode::JumpOnNilTopTop
                    | Bytecode::JumpOnNotNilTopTop,
                    jump_offset,
                ) => {
                    if indices_to_remove.contains(&(*cur_idx + *jump_offset as usize)) {
                        panic!("should be unreachable");
                    }

                    let nbr_to_adjust = indices_to_remove.iter().filter(|&&idx| *cur_idx < idx && idx <= *cur_idx + *jump_offset as usize).count();
                    jumps_to_patch.push((*cur_idx, (*jump_offset as usize - nbr_to_adjust) as u16));
                }
                BcEntry::U16Arg(Bytecode::JumpBackward, jump_offset) => {
                    let nbr_to_adjust = indices_to_remove.iter().filter(|&&idx| *cur_idx > idx && idx > *cur_idx - *jump_offset as usize).count();
                    jumps_to_patch.push((*cur_idx, (*jump_offset as usize - nbr_to_adjust) as u16));
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
    fn find_var(&mut self, name: &str) -> Option<FoundVar> {
        self.inner.find_var(name)
    }

    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.inner.intern_symbol(name)
    }

    fn get_scope(&self) -> usize {
        0
    }

    fn get_interner(&self) -> &Interner {
        self.inner.get_interner()
    }
}

impl InnerGenCtxt for MethodGenCtxt<'_> {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt {
        self
    }

    fn push_instr_no_arg(&mut self, instr: Bytecode) {
        self.inner.push_instr_no_arg(instr)
    }

    fn push_instr_1_arg(&mut self, instr: Bytecode, a: u8) {
        self.inner.push_instr_1_arg(instr, a)
    }

    fn push_instr_2_args(&mut self, instr: Bytecode, a: u8, b: u8) {
        self.inner.push_instr_2_args(instr, a, b)
    }

    fn push_instr_u16_arg(&mut self, instr: Bytecode, arg: u16) {
        self.inner.push_instr_u16_arg(instr, arg)
    }

    fn pop_instr(&mut self) {
        self.inner.pop_instr();
    }

    fn get_instructions(&self) -> &Vec<Bytecode> {
        self.inner.get_instructions()
    }

    fn get_nbr_locals(&self) -> usize {
        self.inner.get_nbr_locals()
    }

    fn set_nbr_locals(&mut self, nbr_locals: usize) {
        self.inner.set_nbr_locals(nbr_locals)
    }

    fn get_literal(&self, idx: usize) -> Option<&Literal> {
        self.inner.get_literal(idx)
    }

    fn push_literal(&mut self, literal: Literal) -> usize {
        self.inner.push_literal(literal)
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
            ast::Expression::Read(name) => {
                match ctxt.find_var(name.as_str()) {
                    Some(FoundVar::Local(up_idx, idx)) => match up_idx {
                        0 => ctxt.push_instr_1_arg(Bytecode::PushLocal, idx),
                        _ => ctxt.push_instr_2_args(Bytecode::PushNonLocal, up_idx, idx),
                    },
                    Some(FoundVar::Argument(up_idx, idx)) => match (up_idx, idx) {
                        (0, 0) => ctxt.push_instr_no_arg(Bytecode::PushSelf),
                        (0, _) => ctxt.push_instr_1_arg(Bytecode::PushArg, idx),
                        _ => ctxt.push_instr_2_args(Bytecode::PushNonLocalArg, up_idx, idx),
                    },
                    Some(FoundVar::Field(idx)) => ctxt.push_instr_1_arg(Bytecode::PushField, idx),
                    None => match name.as_str() {
                        "nil" => ctxt.push_instr_no_arg(Bytecode::PushNil),
                        "super" => match ctxt.get_scope() {
                            0 => ctxt.push_instr_no_arg(Bytecode::PushSelf),
                            scope => ctxt.push_instr_2_args(Bytecode::PushNonLocalArg, scope as u8, 0),
                        },
                        _ => {
                            let name = ctxt.intern_symbol(name);
                            let idx = ctxt.push_literal(Literal::Symbol(name));
                            ctxt.push_instr_1_arg(Bytecode::PushGlobal, idx as u8);
                        }
                    },
                }
                Some(())
            }
            ast::Expression::Write(name, expr) => {
                expr.codegen(ctxt, mutator)?;
                ctxt.push_instr_no_arg(Bytecode::Dup);
                match ctxt.find_var(name.as_str())? {
                    FoundVar::Local(up_idx, idx) => match up_idx {
                        0 => ctxt.push_instr_2_args(Bytecode::PopLocal, 0, idx),
                        _ => ctxt.push_instr_2_args(Bytecode::PopLocal, up_idx, idx),
                    },
                    FoundVar::Argument(up_idx, idx) => ctxt.push_instr_2_args(Bytecode::PopArg, up_idx, idx),
                    FoundVar::Field(idx) => ctxt.push_instr_1_arg(Bytecode::PopField, idx),
                }
                Some(())
            }
            ast::Expression::Message(message) => {
                let message = {
                    match &**message {
                        ast::Message::Regular(reg_msg) => reg_msg,
                        ast::Message::IfInlined(if_inlined) => {
                            if_inlined.cond_expr.codegen(ctxt, mutator)?;
                            let jump_idx = ctxt.get_cur_instr_idx();

                            match if_inlined.expected_bool {
                                true => ctxt.push_instr_2_args(Bytecode::JumpOnFalseTopNil, 0, 0),
                                false => ctxt.push_instr_2_args(Bytecode::JumpOnTrueTopNil, 0, 0),
                            }

                            for expr in &if_inlined.body_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }
                            ctxt.pop_instr();

                            ctxt.backpatch_jump_to_current(jump_idx);
                            return Some(());
                        }
                        ast::Message::IfNilInlined(if_nil_inlined) => {
                            if_nil_inlined.cond_expr.codegen(ctxt, mutator)?;
                            let jump_idx = ctxt.get_cur_instr_idx();

                            match if_nil_inlined.expects_nil {
                                true => ctxt.push_instr_2_args(Bytecode::JumpOnNotNilTopTop, 0, 0),
                                false => ctxt.push_instr_2_args(Bytecode::JumpOnNilTopTop, 0, 0),
                            }

                            for expr in &if_nil_inlined.body_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }
                            ctxt.pop_instr();

                            ctxt.backpatch_jump_to_current(jump_idx);
                            return Some(());
                        }
                        ast::Message::IfTrueIfFalseInlined(if_true_if_false) => {
                            if_true_if_false.cond_expr.codegen(ctxt, mutator)?;

                            let start_jump_idx = ctxt.get_cur_instr_idx();
                            match if_true_if_false.expected_bool {
                                true => ctxt.push_instr_2_args(Bytecode::JumpOnFalsePop, 0, 0),
                                false => ctxt.push_instr_2_args(Bytecode::JumpOnTruePop, 0, 0),
                            }

                            for expr in &if_true_if_false.body_1_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }
                            ctxt.pop_instr();

                            let middle_jump_idx = ctxt.get_cur_instr_idx();
                            ctxt.push_instr_2_args(Bytecode::Jump, 0, 0);

                            ctxt.backpatch_jump_to_current(start_jump_idx);

                            for expr in &if_true_if_false.body_2_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }
                            ctxt.pop_instr();

                            ctxt.backpatch_jump_to_current(middle_jump_idx);
                            return Some(());
                        }
                        ast::Message::IfNilIfNotNilInlined(if_nil_if_not_nil) => {
                            if_nil_if_not_nil.cond_expr.codegen(ctxt, mutator)?;

                            let start_jump_idx = ctxt.get_cur_instr_idx();
                            match if_nil_if_not_nil.expects_nil {
                                true => ctxt.push_instr_2_args(Bytecode::JumpOnNotNilPop, 0, 0),
                                false => ctxt.push_instr_2_args(Bytecode::JumpOnNilPop, 0, 0),
                            }

                            for expr in &if_nil_if_not_nil.body_1_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }
                            ctxt.pop_instr();

                            let middle_jump_idx = ctxt.get_cur_instr_idx();
                            ctxt.push_instr_2_args(Bytecode::Jump, 0, 0);

                            ctxt.backpatch_jump_to_current(start_jump_idx);

                            for expr in &if_nil_if_not_nil.body_2_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }
                            ctxt.pop_instr();

                            ctxt.backpatch_jump_to_current(middle_jump_idx);
                            return Some(());
                        }
                        ast::Message::AndOrInlined(and_inlined) => {
                            and_inlined.first.codegen(ctxt, mutator)?;
                            let skip_cond_jump_idx = ctxt.get_cur_instr_idx();

                            match and_inlined.is_and {
                                true => ctxt.push_instr_2_args(Bytecode::JumpOnFalsePop, 0, 0),
                                false => ctxt.push_instr_2_args(Bytecode::JumpOnTruePop, 0, 0),
                            }

                            for expr in &and_inlined.second {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }
                            ctxt.pop_instr();

                            let skip_return_true_idx = ctxt.get_cur_instr_idx();
                            ctxt.push_instr_2_args(Bytecode::Jump, 0, 0);

                            ctxt.backpatch_jump_to_current(skip_cond_jump_idx);

                            let literal_idx = {
                                match and_inlined.is_and {
                                    true => ctxt.get_interner().reverse_lookup("false").unwrap_or_else(|| ctxt.intern_symbol("false")),
                                    false => ctxt.get_interner().reverse_lookup("true").unwrap_or_else(|| ctxt.intern_symbol("true")),
                                }
                            };
                            let idx = ctxt.push_literal(Literal::Symbol(literal_idx));
                            ctxt.push_instr_1_arg(Bytecode::PushGlobal, idx as u8);

                            ctxt.backpatch_jump_to_current(skip_return_true_idx);
                            return Some(());
                        }
                        ast::Message::WhileInlined(while_inlined) => {
                            let idx_pre_condition = ctxt.get_cur_instr_idx();

                            let splitted = while_inlined.cond_instrs.split_last();
                            if let Some((last, rest)) = splitted {
                                for expr in rest {
                                    expr.codegen(ctxt, mutator)?;
                                    ctxt.push_instr_no_arg(Bytecode::Pop);
                                }
                                last.codegen(ctxt, mutator)?;
                            }

                            let cond_jump_idx = ctxt.get_cur_instr_idx();
                            match while_inlined.expected_bool {
                                true => ctxt.push_instr_2_args(Bytecode::JumpOnFalsePop, 0, 0),
                                false => ctxt.push_instr_2_args(Bytecode::JumpOnTruePop, 0, 0),
                            }

                            for expr in &while_inlined.body_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }

                            let jump_offset = (ctxt.get_cur_instr_idx() - idx_pre_condition) as u16;
                            ctxt.push_instr_u16_arg(Bytecode::JumpBackward, jump_offset);
                            ctxt.backpatch_jump_to_current(cond_jump_idx);

                            ctxt.push_instr_no_arg(Bytecode::PushNil);

                            return Some(());
                        }
                        ast::Message::ToDoInlined(to_do_inlined) => {
                            to_do_inlined.start_expr.codegen(ctxt, mutator)?;
                            to_do_inlined.end_expr.codegen(ctxt, mutator)?;

                            let idx_loop_accumulator = match ctxt.find_var(&to_do_inlined.accumulator_name) {
                                Some(FoundVar::Local(0, a)) => a,
                                invalid => panic!("to do inlining couldn't find a valid index for its accumulator: got {:?}", invalid),
                            };

                            ctxt.push_instr_no_arg(Bytecode::Dup2);

                            let jump_if_greater_idx = ctxt.get_cur_instr_idx();
                            ctxt.push_instr_2_args(Bytecode::JumpIfGreater, 0, 0);

                            ctxt.push_instr_no_arg(Bytecode::Dup);
                            ctxt.push_instr_2_args(Bytecode::PopLocal, 0, idx_loop_accumulator);

                            for expr in &to_do_inlined.body_instrs {
                                expr.codegen(ctxt, mutator)?;
                                ctxt.push_instr_no_arg(Bytecode::Pop);
                            }

                            ctxt.push_instr_no_arg(Bytecode::Inc);
                            let jump_offset = (ctxt.get_cur_instr_idx() - jump_if_greater_idx) as u16;
                            ctxt.push_instr_u16_arg(Bytecode::JumpBackward, jump_offset);

                            ctxt.backpatch_jump_to_current(jump_if_greater_idx);

                            return Some(());
                        }
                    }
                };

                let is_super_call = matches!(&message.receiver, _super if _super == &Expression::Read(String::from("super")));

                message.receiver.codegen(ctxt, mutator)?;

                if (message.signature == "+" || message.signature == "-")
                    && !is_super_call
                    && message.values.len() == 1
                    && message.values.first()? == &Expression::Literal(ast::Literal::Integer(1))
                {
                    match message.signature.as_str() {
                        "+" => ctxt.push_instr_no_arg(Bytecode::Inc),
                        "-" => ctxt.push_instr_no_arg(Bytecode::Dec),
                        _ => unreachable!(),
                    };
                    return Some(());
                }

                message.values.iter().try_for_each(|value| value.codegen(ctxt, mutator))?;

                let nbr_args = match message.signature.chars().nth(0) {
                    Some(ch) if !ch.is_alphabetic() => 1,
                    _ => message.signature.chars().filter(|ch| *ch == ':').count(),
                };

                let sym = ctxt.intern_symbol(message.signature.as_str()).0;

                match is_super_call {
                    false => match nbr_args {
                        0 => ctxt.push_instr_u16_arg(Bytecode::Send1, sym),
                        1 => ctxt.push_instr_u16_arg(Bytecode::Send2, sym),
                        2 => ctxt.push_instr_u16_arg(Bytecode::Send3, sym),
                        _ => ctxt.push_instr_u16_arg(Bytecode::SendN, sym),
                    },
                    true => ctxt.push_instr_u16_arg(Bytecode::SuperSend, sym),
                }

                Some(())
            }
            ast::Expression::Exit(expr) => {
                let scope = ctxt.get_scope();

                match scope {
                    0 => match expr.as_ref() {
                        Expression::Read(s) if s == "self" => ctxt.push_instr_no_arg(Bytecode::ReturnSelf),
                        _ => {
                            expr.codegen(ctxt, mutator)?;
                            ctxt.push_instr_no_arg(Bytecode::ReturnLocal)
                        }
                    },
                    _ => {
                        expr.codegen(ctxt, mutator)?;
                        ctxt.push_instr_1_arg(Bytecode::ReturnNonLocal, scope as u8);
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
                            Literal::Array(VecLiteral(literals))
                        }
                    }
                }

                let literal = convert_literal(ctxt, literal, mutator);

                match literal {
                    Literal::Integer(0) => ctxt.push_instr_no_arg(Bytecode::Push0),
                    Literal::Integer(1) => ctxt.push_instr_no_arg(Bytecode::Push1),
                    _ => {
                        let idx = ctxt.push_literal(literal);
                        ctxt.push_instr_1_arg(Bytecode::PushConstant, idx as u8)
                    }
                }

                Some(())
            }
            ast::Expression::Block(val) => {
                let block_method = compile_block_method(ctxt.as_gen_ctxt(), val, mutator)?;
                let block = Literal::Block(block_method);
                let idx = ctxt.push_literal(block);
                ctxt.push_instr_1_arg(Bytecode::PushBlock, idx as u8);
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
    fn find_var(&mut self, name: &str) -> Option<FoundVar> {
        let sym = self.interner.intern(name);
        self.fields.get_index_of(&sym).map(|idx| FoundVar::Field(idx as u8))
    }

    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.interner.intern(name)
    }

    fn get_scope(&self) -> usize {
        unreachable!("Asking for scope in a class generation context?")
    }

    fn get_interner(&self) -> &Interner {
        self.interner
    }
}

fn compile_method(outer: &mut dyn GenCtxt, defn: &ast::MethodDef, gc_interface: &mut GCInterface) -> Option<Method> {
    fn make_trivial_method_if_possible(
        body: &Vec<Bytecode>,
        literals: &[Literal],
        signature: &str,
        nbr_args: usize,
        interner: &Interner,
    ) -> Option<Method> {
        match (body.as_slice(), nbr_args) {
            ([Bytecode::PushGlobal, x, Bytecode::ReturnLocal], 1) => match literals.get(*x as usize)? {
                Literal::Symbol(interned) => Some(Method::TrivialGlobal(
                    TrivialGlobalMethod {
                        global_name: *interned,
                        cached_entry: Cell::new(None),
                    },
                    BasicMethodInfo::new(String::from(signature), Gc::default()),
                )),
                _ => None,
            },
            ([Bytecode::PushField, x, Bytecode::ReturnLocal], 1) => Some(Method::TrivialGetter(
                TrivialGetterMethod { field_idx: *x as u8 },
                BasicMethodInfo::new(String::from(signature), Gc::default()),
            )),
            ([Bytecode::PushArg, expect_one, Bytecode::PopField, x, Bytecode::ReturnSelf], 2) => {
                if *expect_one as u8 != 1 {
                    return None;
                }
                Some(Method::TrivialSetter(
                    TrivialSetterMethod { field_idx: *x as u8 },
                    BasicMethodInfo::new(String::from(signature), Gc::default()),
                ))
            }
            ([Bytecode::PushConstant, const_idx, Bytecode::ReturnLocal], 1) => {
                let lit = literals.get(*const_idx as usize).cloned().unwrap();
                Some(Method::TrivialLiteral(
                    TrivialLiteralMethod { literal: lit.clone() },
                    BasicMethodInfo::new(String::from(signature), Gc::default()),
                ))
            }
            ([Bytecode::Push0, Bytecode::ReturnLocal], 1) => Some(Method::TrivialLiteral(
                TrivialLiteralMethod {
                    literal: Literal::Integer(0),
                },
                BasicMethodInfo::new(String::from(signature), Gc::default()),
            )),
            ([Bytecode::Push1, Bytecode::ReturnLocal], 1) => Some(Method::TrivialLiteral(
                TrivialLiteralMethod {
                    literal: Literal::Integer(1),
                },
                BasicMethodInfo::new(String::from(signature), Gc::default()),
            )),
            ([Bytecode::PushNil, Bytecode::ReturnLocal], 1) => {
                let nil_interned = interner.reverse_lookup("nil").unwrap_or_else(|| panic!("how did we not make nil a global yet?"));
                Some(Method::TrivialLiteral(
                    TrivialLiteralMethod {
                        literal: Literal::Symbol(nil_interned),
                    },
                    BasicMethodInfo::new(String::from(signature), Gc::default()),
                ))
            }
            _ => None,
        }
    }

    let mut ctxt = MethodGenCtxt {
        signature: defn.signature.clone(),
        inner: BlockGenCtxt {
            outer,
            literals: IndexSet::new(),
            body: None,
            args: {
                let mut args = IndexSet::new();
                args.insert(String::from("self"));
                for arg in &defn.args {
                    args.insert(arg.to_string());
                }
                args
            },
            locals: match &defn.body {
                ast::MethodBody::Primitive => IndexSet::new(),
                ast::MethodBody::Body { locals, .. } => locals.iter().cloned().collect(),
            },
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
        },
    };

    match &defn.body {
        ast::MethodBody::Primitive => {}
        ast::MethodBody::Body { body, .. } => {
            if let Some((last, exprs)) = body.exprs.split_last() {
                for expr in exprs {
                    expr.codegen(&mut ctxt, gc_interface)?;
                    ctxt.push_instr_no_arg(Bytecode::Pop);
                }
                last.codegen(&mut ctxt, gc_interface)?;
                if !matches!(last, Expression::Exit(_)) {
                    ctxt.push_instr_no_arg(Bytecode::Pop);
                    ctxt.push_instr_no_arg(Bytecode::ReturnSelf);
                }
            } else {
                // empty method body means we just return self
                ctxt.push_instr_no_arg(Bytecode::ReturnSelf);
            }

            ctxt.remove_dup_popx_pop_sequences();
        }
    }

    let method = {
        match &defn.body {
            ast::MethodBody::Primitive => Method::Primitive(&*UNIMPLEM_PRIMITIVE, BasicMethodInfo::new(String::from(""), Gc::default())),
            ast::MethodBody::Body { .. } => {
                let nbr_locals = ctxt.inner.locals_nbr as u8;
                let body = ctxt.inner.body.clone().unwrap_or_default();
                let literals: Vec<Literal> = ctxt.inner.literals.clone().into_iter().collect();
                let signature = ctxt.signature.clone();
                let nbr_args = {
                    match ctxt.signature.chars().next() {
                        Some(ch) if !ch.is_alphabetic() => 2,
                        _ => ctxt.signature.chars().filter(|ch| *ch == ':').count() as u8 + 1, // + 1 for self
                    }
                };

                if let Some(trivial_method) = make_trivial_method_if_possible(&body, &literals, &signature, nbr_args as usize, ctxt.get_interner()) {
                    trivial_method
                } else {
                    let inline_cache = vec![None; body.len()];
                    #[cfg(feature = "frame-debug-info")]
                    let dbg_info = ctxt.inner.debug_info;

                    let method_info = MethodInfo {
                        basic_method_info: BasicMethodInfo::new(signature, Gc::default()),
                        body,
                        nbr_locals,
                        nbr_args,
                        literals,
                        inline_cache,
                        #[cfg(feature = "frame-debug-info")]
                        block_debug_info: dbg_info,
                    };

                    Method::Defined(gc_interface.alloc(method_info, AllocSiteMarker::MethodInfo))
                }
            }
        }
    };

    // println!("(method) compiled '{}' !", defn.signature);

    Some(method)
}

fn compile_block_method(outer: &mut dyn GenCtxt, defn: &ast::Block, gc_interface: &mut GCInterface) -> Option<Gc<MethodInfo>> {
    // println!("(system) compiling block ...");

    let mut ctxt = BlockGenCtxt {
        outer,
        args_nbr: defn.args.len(),
        locals_nbr: defn.locals.len(),
        args: {
            let mut args = IndexSet::new();
            args.insert(String::from("#blockSelf"));
            for arg in &defn.args {
                args.insert(arg.to_string());
            }
            args
        },
        locals: defn.locals.iter().cloned().collect(),
        literals: IndexSet::new(),
        body: None,
        #[cfg(feature = "frame-debug-info")]
        debug_info: defn.dbg_info.clone(),
    };

    let splitted = defn.body.exprs.split_last();
    if let Some((last, rest)) = splitted {
        for expr in rest {
            expr.codegen(&mut ctxt, gc_interface)?;
            ctxt.push_instr_no_arg(Bytecode::Pop);
        }
        last.codegen(&mut ctxt, gc_interface)?;
        ctxt.push_instr_no_arg(Bytecode::ReturnLocal);
    }
    ctxt.remove_dup_popx_pop_sequences();

    if ctxt.body.is_none() {
        ctxt.push_instr_no_arg(Bytecode::PushNil);
        ctxt.push_instr_no_arg(Bytecode::ReturnLocal);
    }

    let literals: Vec<Literal> = ctxt.literals.clone().into_iter().collect();
    // FEAT: could probably make `signature` into an enum or a Cow<'static, str> to store either an owner string, or just a static str "--block--".
    // That'd save a bit of memory and I *think* there'd be no runtime cost.
    let signature = String::from("--block--");
    let body = ctxt.body.clone().unwrap_or_default();
    let nbr_locals = ctxt.locals_nbr as u8;
    let nbr_args = ctxt.args_nbr as u8 + 1; // + 1 for self
    let inline_cache = vec![None; body.len()];

    let method_info = MethodInfo {
        basic_method_info: BasicMethodInfo::new(signature, Gc::default()),
        nbr_locals,
        literals,
        body,
        nbr_args,
        inline_cache,
        #[cfg(feature = "frame-debug-info")]
        block_debug_info: ctxt.debug_info,
    };

    let method_info = gc_interface.alloc(method_info, AllocSiteMarker::MethodInfo);

    // println!("(system) compiled block !");

    Some(method_info)
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
        #[cfg(feature = "track-allocations")]
        {
            if let Method::Defined(method_info) = &method {
                gc_interface.total_program_repr_size += (method_info.body.len() * size_of::<Bytecode>()) as u128;
            }
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
    #[cfg(feature = "track-allocations")]
    {
        gc_interface.total_program_repr_size += (static_class_mut.fields.len() * size_of::<Value>()) as u128;
    }

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
        #[cfg(feature = "track-allocations")]
        {
            if let Method::Defined(method_info) = &method {
                gc_interface.total_program_repr_size += (method_info.body.len() * size_of::<Bytecode>()) as u128;
            }
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
    #[cfg(feature = "track-allocations")]
    {
        gc_interface.total_program_repr_size += (instance_class_mut.fields.len() * size_of::<Value>()) as u128;
    }

    Some(instance_class_gc_ptr)
}
