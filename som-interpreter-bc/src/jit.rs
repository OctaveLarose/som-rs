// # Safety
//
// There is no safety. This is a JIT.
#![allow(clippy::missing_safety_doc)]

use crate::value::Value;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::method::MethodInfo;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use num_bigint::BigInt;
use som_core::bytecode::{BC_SIZE_1_ARG, BC_SIZE_NO_ARGS};
use som_gc::gcref::Gc;
use som_value::value;
use std::mem::offset_of;

pub struct CodeGen<'ctx> {
    pub context: &'ctx inkwell::context::Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub frame_type: StructType<'ctx>,
    // jit_fns: HashMap<String, FunctionValue<'ctx>>,
}

pub type IncFunc = unsafe extern "C" fn(*mut *mut Value, *mut u16) -> u8;
pub type PushLocalFunc = unsafe extern "C" fn(*const Frame, *const u8, *mut *mut Value, *mut u16);
pub type PushArgFunc = unsafe extern "C" fn(*const Frame, *const u8, *mut *mut Value, *mut u16);
pub type PopFunc = unsafe extern "C" fn(*mut *mut Value, *mut u16);

/// We do NaN tagging, so there's no double tag. But 0 isn't a tag in use so we use it to mean a value "of type double"
const UNOFFICIAL_GREAT_DOUBLE_TAG: u64 = 0;

#[no_mangle]
pub extern "C" fn value_new(x: u64, tag: u64) -> u64 {
    match tag {
        UNOFFICIAL_GREAT_DOUBLE_TAG => Value::new_double(f64::from_bits(x)).as_u64(),
        value::INTEGER_TAG => Value::new_integer(x as i32).as_u64(),
        value::BIG_INTEGER_TAG => Value::new_big_integer(x.into()).as_u64(),
        tag => todo!("value_new not implemented for tag {}", tag),
    }
}

#[no_mangle]
pub extern "C" fn value_is(val: u64, tag: u64) -> bool {
    let v = Value::from(val);
    match tag {
        value::INTEGER_TAG => v.is_integer(),
        UNOFFICIAL_GREAT_DOUBLE_TAG => v.is_double(),
        value::BIG_INTEGER_TAG => v.is_big_integer(),
        tag => todo!("value_is not implemented for tag {}", tag),
    }
}

#[no_mangle]
pub extern "C" fn value_extract(val: u64, tag: u64) -> u64 {
    match tag {
        0 => Value::from(val).as_double().unwrap().to_bits(),
        value::INTEGER_TAG => Value::from(val).as_integer().unwrap() as u64,
        value::BIG_INTEGER_TAG => Value::from(val).as_big_integer::<Gc<BigInt>>().unwrap().ptr as u64,
        t => todo!(
            "value_extract not implemented for tag {} (actual value tag: {})",
            t,
            Value::from(val).tag()
        ),
    }
}

/// # Safety
///
/// It's not safe. But believe in yourself.
#[no_mangle]
pub unsafe extern "C" fn bigint_increment(big_int: *mut BigInt) {
    *big_int += 1;
}

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn init(context: &'ctx inkwell::context::Context) -> Self {
        let module = context.create_module("interp");
        let builder = context.create_builder();

        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();

        let frame_type = Self::create_runtime_types(context);

        let codegen = CodeGen {
            context,
            module,
            builder,
            execution_engine,
            frame_type,
            // jit_fns: Default::default(),
        };

        // --- helper functions
        {
            // let i8_type = codegen.context.i8_type();
            // let i32_type = codegen.context.i32_type();
            let i64_type = codegen.context.i64_type();
            let bool_type = codegen.context.bool_type();
            let ptr_type = codegen.context.ptr_type(AddressSpace::default());
            let void_type = codegen.context.void_type();

            let fn_value_new = i64_type.fn_type(&[i64_type.into(), i64_type.into()], false);
            let llvm_value_new_integer = codegen.module.add_function("value_new", fn_value_new, None);
            codegen.execution_engine.add_global_mapping(&llvm_value_new_integer, value_new as usize);

            let fn_value_is = bool_type.fn_type(&[i64_type.into(), i64_type.into()], false);
            let llvm_value_is_integer = codegen.module.add_function("value_is", fn_value_is, None);
            codegen.execution_engine.add_global_mapping(&llvm_value_is_integer, value_is as usize);

            let fn_value_extract = i64_type.fn_type(&[i64_type.into(), i64_type.into()], false);
            let llvm_value_is_integer = codegen.module.add_function("value_extract", fn_value_extract, None);
            codegen.execution_engine.add_global_mapping(&llvm_value_is_integer, value_extract as usize);

            let fn_bigint_increment = void_type.fn_type(&[ptr_type.into()], false);
            let llvm_value_bigint_increment = codegen.module.add_function("bigint_increment", fn_bigint_increment, None);
            codegen.execution_engine.add_global_mapping(&llvm_value_bigint_increment, bigint_increment as usize);
        }

        codegen
    }

    fn create_runtime_types(context: &'ctx Context) -> StructType<'ctx> {
        let ptr = context.ptr_type(AddressSpace::default());

        // MethodInfo
        // let method_info = context.struct_type(
        //     &[
        //         basic_method_info.into(), // 0: basic_method_info
        //         vec_type.into(),          // 1: literals
        //         vec_type.into(),          // 2: body
        //         vec_type.into(),          // 3: inline_cache
        //         context.i8_type().into(), // 4: nbr_locals
        //         context.i8_type().into(), // 5: nbr_args
        //     ],
        //     false,
        // );

        // Frame
        let frame = context.struct_type(
            &[
                ptr.into(),                // 0: prev_frame
                ptr.into(),                // 1: context
                context.i16_type().into(), // 2: bytecode_idx
            ],
            false,
        );

        frame
    }

    fn build_is_value_type(&self, value: BasicValueEnum<'ctx>, tag: u64) -> BasicValueEnum<'ctx> {
        let tag_as_val = self.context.i64_type().const_int(tag, false);
        let value_is_call = self
            .builder
            .build_call(
                self.module.get_function("value_is").unwrap(),
                &[value.into(), tag_as_val.into()],
                "result",
            )
            .ok()
            .unwrap();
        let is_integer = value_is_call.try_as_basic_value().basic().unwrap();
        is_integer
    }

    fn build_value_extract(&self, value: BasicValueEnum<'ctx>, tag: u64) -> BasicValueEnum<'ctx> {
        let tag_as_val = self.context.i64_type().const_int(tag, false);
        let value_extract_call = self
            .builder
            .build_call(
                self.module.get_function("value_extract").unwrap(),
                &[value.into(), tag_as_val.into()],
                "int_val",
            )
            .ok()
            .unwrap();
        value_extract_call.try_as_basic_value().basic().unwrap()
    }

    fn build_increment_bc_idx(&self, bc_idx_ptr: PointerValue, bc_size: u64) {
        let i64_type = self.context.i64_type();
        let bc_idx_val = self.builder.build_load(i64_type, bc_idx_ptr, "bc_idx_val").unwrap().into_int_value();
        let bc_size_no_args = i64_type.const_int(bc_size, false);
        let new_bc_idx = self.builder.build_int_add(bc_idx_val, bc_size_no_args, "new_bc_idx").ok().unwrap();
        self.builder.build_store(bc_idx_ptr, new_bc_idx).ok().unwrap();
    }

    /// stack_push!(sp, value)
    fn build_stack_push(&self, sp_ref: PointerValue, value: IntValue) {
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let sp = self.builder.build_load(ptr_type, sp_ref, "sp").unwrap().into_pointer_value();
        self.builder.build_store(sp, value).unwrap();
        let new_sp = unsafe { self.builder.build_gep(i64_type, sp, &[i64_type.const_int(1i64 as u64, false)], "new_sp").unwrap() };
        self.builder.build_store(sp_ref, new_sp).unwrap();
    }

    /// stack_last!(sp)
    fn build_stack_last(&self, sp: PointerValue<'ctx>) -> BasicValueEnum<'ctx> {
        let i64_type = self.context.i64_type();
        let tos_ptr = unsafe { self.builder.build_gep(i64_type, sp, &[i64_type.const_int(-1i64 as u64, true)], "last_ptr").unwrap() };
        self.builder.build_load(i64_type, tos_ptr, "last_value").unwrap()
    }

    /// stack_drop!(sp)
    fn build_stack_drop(&self, sp_ref: PointerValue<'ctx>) {
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let sp = self.builder.build_load(ptr_type, sp_ref, "sp").unwrap().into_pointer_value();
        let tos_ptr = unsafe { self.builder.build_gep(i64_type, sp, &[i64_type.const_int(-1i64 as u64, true)], "sp_minus_1").unwrap() };
        self.builder.build_store(sp_ref, tos_ptr).unwrap();
    }

    /// bytecodes[self.bytecode_idx as usize + N];
    fn build_get_bytecode_arg(&self, bytecodes: PointerValue<'ctx>, bc_idx: IntValue<'ctx>, n: u64) -> BasicValueEnum<'ctx> {
        let i8_type = self.context.i8_type();

        let idx_arg = self.builder.build_int_add(bc_idx, self.context.i64_type().const_int(n, false), "idx_arg").unwrap();

        let element_ptr = unsafe { self.builder.build_gep(i8_type, bytecodes, &[idx_arg], "element_ptr").unwrap() };
        self.builder.build_load(i8_type, element_ptr, "element").unwrap()
    }

    /// frame.lookup_local(idx) or frame.lookup_arg(idx)
    /// Also `is_local` being a boolean and not an enum is yucky
    fn build_get_frame_value(&self, frame: PointerValue<'ctx>, idx: IntValue<'ctx>, is_local: bool) -> PointerValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i8_type = self.context.i8_type();
        let i64_type = self.context.i64_type();

        // (frame as *mut u8) + OFFSET_TO_VALUES
        let value_heap = {
            let offset = i64_type.const_int(crate::vm_objects::frame::OFFSET_TO_VALUES as u64, false);
            unsafe { self.builder.build_gep(i8_type, frame, &[offset], "value_heap").unwrap() }
        };

        // if it's an argument, they're already right there. if it's a local, we need to add `nbr_args * size_of::<Value>()` first
        let value_start = match is_local {
            true => {
                let method_info = {
                    let context_ptr = self.builder.build_struct_gep(self.frame_type, frame, 1, "context_ptr").unwrap();
                    self.builder.build_load(ptr_type, context_ptr, "method_info").unwrap().into_pointer_value()
                };

                let nbr_args = {
                    let offset_nbr_args = i64_type.const_int(offset_of!(MethodInfo, nbr_args) as u64, false);
                    let nbr_args_ptr = unsafe { self.builder.build_gep(i8_type, method_info, &[offset_nbr_args], "nbr_args_ptr").unwrap() };
                    let nbr_args = self.builder.build_load(i8_type, nbr_args_ptr, "nbr_args").unwrap().into_int_value();
                    self.builder.build_int_z_extend(nbr_args, i64_type, "nbr_args_i64").unwrap()
                };
                let locals = unsafe { self.builder.build_gep(i64_type, value_heap, &[nbr_args], "locals").unwrap() };
                locals
            }
            false => value_heap,
        };

        let idx = self.builder.build_int_z_extend(idx, i64_type, "idx_i64").unwrap();
        unsafe { self.builder.build_gep(i64_type, value_start, &[idx], "local").unwrap() }
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn jit_compile_inc(&mut self) {
        //     let last: &mut Value = stack_last!(self.sp);
        //
        //     if let Some(int) = last.as_integer() {
        //         *last = Value::new_integer(int + 1);
        //     } else if let Some(double) = last.as_double() {
        //         *last = Value::new_double(double + 1.0);
        //     } else if let Some(mut big_int) = last.as_big_integer::<Gc<BigInt>>() {
        //         *big_int += 1;
        //     } else {
        //         panic!("Invalid type in Inc")
        //     };
        //     self.bytecode_idx += BC_SIZE_NO_ARGS;
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let inc_jit_fn_val = {
            let inc_type = self.context.i8_type().fn_type(&[ptr_type.into(), ptr_type.into()], false);
            self.module.add_function("inc", inc_type, None)
        };

        let entry = self.context.append_basic_block(inc_jit_fn_val, "inc_entry");
        let is_double_block = self.context.append_basic_block(inc_jit_fn_val, "inc_is_double_blk");
        let is_bigint_block = self.context.append_basic_block(inc_jit_fn_val, "inc_is_bigint_blk");
        let integer_block = self.context.append_basic_block(inc_jit_fn_val, "inc_int_blk");
        let double_block = self.context.append_basic_block(inc_jit_fn_val, "inc_double_blk");
        let big_int_block = self.context.append_basic_block(inc_jit_fn_val, "inc_big_int_blk");
        let failure_block = self.context.append_basic_block(inc_jit_fn_val, "inc_no_dice");
        let return_block = self.context.append_basic_block(inc_jit_fn_val, "inc_return");

        // ---- entry
        self.builder.position_at_end(entry);

        let sp = {
            let sp_ref = inc_jit_fn_val.get_nth_param(0).unwrap().into_pointer_value();
            self.builder.build_load(ptr_type, sp_ref, "sp").unwrap().into_pointer_value()
        };
        let bc_idx_ptr = inc_jit_fn_val.get_nth_param(1).unwrap().into_pointer_value();

        let tos_value = self.build_stack_last(sp);

        let is_integer = self.build_is_value_type(tos_value, value::INTEGER_TAG).into_int_value();
        // let is_int_result = self
        //     .builder
        //     .build_int_compare(IntPredicate::EQ, is_integer, self.context.bool_type().const_int(0, false), "is_int")
        //     .unwrap();
        self.builder.build_conditional_branch(is_integer, integer_block, is_double_block).unwrap();

        // ---- is_double ----
        {
            self.builder.position_at_end(is_double_block);

            let is_double = self.build_is_value_type(tos_value, UNOFFICIAL_GREAT_DOUBLE_TAG).into_int_value();
            self.builder.build_conditional_branch(is_double, double_block, is_bigint_block).unwrap();
        }

        // ---- is_bigint ----
        {
            self.builder.position_at_end(is_bigint_block);
            let is_bigint = self.build_is_value_type(tos_value, value::BIG_INTEGER_TAG).into_int_value();
            self.builder.build_conditional_branch(is_bigint, big_int_block, failure_block).unwrap();
        }

        // ---- return ----
        {
            self.builder.position_at_end(failure_block);
            let false_const = self.context.i8_type().const_int(0, false);
            self.builder.build_return(Some(&false_const)).unwrap();
        }

        // ---- int_blk ----
        let new_value_int = {
            self.builder.position_at_end(integer_block);

            let int_tag = i64_type.const_int(value::INTEGER_TAG, false);
            let value = self.build_value_extract(tos_value, value::INTEGER_TAG).into_int_value();

            let new_value_int = {
                // int + 1
                let one = i64_type.const_int(1, false);
                let incremented = self.builder.build_int_add(value, one, "inc").ok().unwrap();

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("value_new").unwrap(),
                        &[incremented.into(), int_tag.into()],
                        "new_value",
                    )
                    .ok()
                    .unwrap();
                call.try_as_basic_value().basic().unwrap().into_int_value()
            };
            self.builder.build_unconditional_branch(return_block).unwrap();
            new_value_int
        };
        // ------

        // ---- double_blk ----
        let new_value_double = {
            self.builder.position_at_end(double_block);

            let double_tag = i64_type.const_int(UNOFFICIAL_GREAT_DOUBLE_TAG, false);
            let double_bits = self.build_value_extract(tos_value, UNOFFICIAL_GREAT_DOUBLE_TAG).into_int_value();

            let new_value_float = {
                let float_type = self.context.f64_type();
                let value_as_float = self.builder.build_bit_cast(double_bits, float_type, "double_val").unwrap().into_float_value();
                let incremented = self.builder.build_float_add(value_as_float, float_type.const_float(1.0), "inc").unwrap();
                let result_bits = self.builder.build_bit_cast(incremented, i64_type, "result_bits").unwrap().into_int_value();

                let call = self
                    .builder
                    .build_call(
                        self.module.get_function("value_new").unwrap(),
                        &[result_bits.into(), double_tag.into()],
                        "new_value",
                    )
                    .unwrap();

                call.try_as_basic_value().basic().unwrap().into_int_value()
            };
            self.builder.build_unconditional_branch(return_block).unwrap();
            new_value_float
        };
        // ------

        // ---- big_int_blk ----
        {
            self.builder.position_at_end(big_int_block);

            let value = self.build_value_extract(tos_value, value::BIG_INTEGER_TAG).into_int_value();

            {
                let value_as_ptr = self.builder.build_int_to_ptr(value, ptr_type, "value_ptr").unwrap();
                let _call = self
                    .builder
                    .build_call(self.module.get_function("bigint_increment").unwrap(), &[value_as_ptr.into()], "new_value")
                    .ok()
                    .unwrap();
            };

            self.build_increment_bc_idx(bc_idx_ptr, BC_SIZE_NO_ARGS as u64);

            let true_const = self.context.i8_type().const_int(1, false);
            self.builder.build_return(Some(&true_const)).ok().unwrap();
        }
        // ------

        // --- RETURN BLOCK
        {
            self.builder.position_at_end(return_block);
            let phi = self.builder.build_phi(i64_type, "new_value").unwrap();
            phi.add_incoming(&[(&new_value_int, integer_block), (&new_value_double, double_block)]);

            // *last = new_value
            let tos_ptr = unsafe { self.builder.build_gep(i64_type, sp, &[i64_type.const_int(-1i64 as u64, true)], "last_ptr").unwrap() };
            self.builder.build_store(tos_ptr, phi.as_basic_value()).ok().unwrap();

            self.build_increment_bc_idx(bc_idx_ptr, BC_SIZE_NO_ARGS as u64);

            let true_const = self.context.i8_type().const_int(1, false);
            self.builder.build_return(Some(&true_const)).ok().unwrap();
        }
        // ----

        self.module.verify().unwrap();
    }

    pub(crate) fn jit_compile_push_local(&mut self) {
        // let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
        // let value = *frame.lookup_local(idx as usize);
        // stack_push!(self.sp, value);
        // self.bytecode_idx += BC_SIZE_1_ARG;

        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let push_local_jit_fn_val = {
            let push_local_type = self.context.void_type().fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into(), ptr_type.into()], false);
            self.module.add_function("push_local", push_local_type, None)
        };

        let entry = self.context.append_basic_block(push_local_jit_fn_val, "push_local_entry");
        self.builder.position_at_end(entry);

        let frame = push_local_jit_fn_val.get_nth_param(0).unwrap().into_pointer_value();
        let bytecodes = push_local_jit_fn_val.get_nth_param(1).unwrap().into_pointer_value();
        let sp_ref = push_local_jit_fn_val.get_nth_param(2).unwrap().into_pointer_value();
        let bc_idx_ptr = push_local_jit_fn_val.get_nth_param(3).unwrap().into_pointer_value();

        let bc_idx = self.builder.build_load(i64_type, bc_idx_ptr, "bc_idx_val").unwrap().into_int_value();
        let arg = self.build_get_bytecode_arg(bytecodes, bc_idx, 1);

        // let value = *frame.lookup_local(idx as usize);
        let value = self
            .builder
            .build_load(i64_type, self.build_get_frame_value(frame, arg.into_int_value(), true), "value")
            .unwrap()
            .into_int_value();

        self.build_stack_push(sp_ref, value);

        self.build_increment_bc_idx(bc_idx_ptr, BC_SIZE_1_ARG as u64);

        self.builder.build_return(None).ok().unwrap();
    }

    pub(crate) fn jit_compile_push_arg(&mut self) {
        // let idx: u8 = bytecodes[self.bytecode_idx as usize + 1];
        // let value = *frame.lookup_local(idx as usize);
        // stack_push!(self.sp, value);
        // self.bytecode_idx += BC_SIZE_1_ARG;

        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let push_local_jit_fn_val = {
            let push_local_type = self.context.void_type().fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into(), ptr_type.into()], false);
            self.module.add_function("push_arg", push_local_type, None)
        };

        let entry = self.context.append_basic_block(push_local_jit_fn_val, "push_arg");
        self.builder.position_at_end(entry);

        let frame = push_local_jit_fn_val.get_nth_param(0).unwrap().into_pointer_value();
        let bytecodes = push_local_jit_fn_val.get_nth_param(1).unwrap().into_pointer_value();
        let sp_ref = push_local_jit_fn_val.get_nth_param(2).unwrap().into_pointer_value();
        let bc_idx_ptr = push_local_jit_fn_val.get_nth_param(3).unwrap().into_pointer_value();

        let bc_idx = self.builder.build_load(i64_type, bc_idx_ptr, "bc_idx_val").unwrap().into_int_value();
        let arg = self.build_get_bytecode_arg(bytecodes, bc_idx, 1);

        let value = self
            .builder
            .build_load(i64_type, self.build_get_frame_value(frame, arg.into_int_value(), false), "value")
            .unwrap()
            .into_int_value();

        self.build_stack_push(sp_ref, value);

        self.build_increment_bc_idx(bc_idx_ptr, BC_SIZE_1_ARG as u64);

        self.builder.build_return(None).ok().unwrap();
    }

    pub(crate) fn jit_compile_pop(&mut self) {
        let pop_fn_jit_fn = {
            let ptr_type = self.context.ptr_type(AddressSpace::default());
            let push_local_type = self.context.void_type().fn_type(&[ptr_type.into(), ptr_type.into()], false);
            self.module.add_function("pop", push_local_type, None)
        };

        let entry = self.context.append_basic_block(pop_fn_jit_fn, "pop_entry");
        self.builder.position_at_end(entry);

        let sp_ref = pop_fn_jit_fn.get_nth_param(0).unwrap().into_pointer_value();
        let bc_idx_ptr = pop_fn_jit_fn.get_nth_param(1).unwrap().into_pointer_value();

        self.build_stack_drop(sp_ref);

        self.build_increment_bc_idx(bc_idx_ptr, BC_SIZE_NO_ARGS as u64);

        self.builder.build_return(None).ok().unwrap();
    }
}
