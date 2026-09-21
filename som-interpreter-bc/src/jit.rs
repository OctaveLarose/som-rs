// # Safety
//
// There is no safety. This is a JIT.
#![allow(clippy::missing_safety_doc)]

use crate::value::Value;
use inkwell::builder::Builder;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use num_bigint::BigInt;
use som_core::bytecode::BC_SIZE_NO_ARGS;
use som_gc::gcref::Gc;
use som_value::value;

pub struct CodeGen<'ctx> {
    pub(crate) context: &'ctx inkwell::context::Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    // jit_fns: HashMap<String, FunctionValue<'ctx>>,
}

pub type IncFunc = unsafe extern "C" fn(*mut *mut Value, *mut u16) -> u8;

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

        let codegen = CodeGen {
            context,
            module,
            builder,
            execution_engine,
            // jit_fns: Default::default(),
        };

        // add helper functions

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

        codegen
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

        let inc_type = self.context.i8_type().fn_type(&[ptr_type.into(), ptr_type.into()], false);
        let inc_jit_fn_val = self.module.add_function("inc", inc_type, None);

        let entry = self.context.append_basic_block(inc_jit_fn_val, "entry");
        let is_double_block = self.context.append_basic_block(inc_jit_fn_val, "is_double_blk");
        let is_bigint_block = self.context.append_basic_block(inc_jit_fn_val, "is_bigint_blk");
        let integer_block = self.context.append_basic_block(inc_jit_fn_val, "int_blk");
        let double_block = self.context.append_basic_block(inc_jit_fn_val, "double_blk");
        let big_int_block = self.context.append_basic_block(inc_jit_fn_val, "big_int_blk");
        let failure_block = self.context.append_basic_block(inc_jit_fn_val, "no_dice");
        let return_block = self.context.append_basic_block(inc_jit_fn_val, "return");

        // ---- entry
        self.builder.position_at_end(entry);

        let sp = {
            let sp_ref = inc_jit_fn_val.get_nth_param(0).unwrap().into_pointer_value();
            self.builder.build_load(ptr_type, sp_ref, "sp").unwrap().into_pointer_value()
        };
        let bc_idx_ptr = inc_jit_fn_val.get_nth_param(1).unwrap().into_pointer_value();

        // sp - 1 element
        let tos_ptr = unsafe { self.builder.build_gep(i64_type, sp, &[i64_type.const_int(-1i64 as u64, true)], "last_ptr").unwrap() };
        let tos_value = self.builder.build_load(i64_type, tos_ptr, "last_value").unwrap();

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
            self.builder.build_store(tos_ptr, phi.as_basic_value()).ok().unwrap();

            self.build_increment_bc_idx(bc_idx_ptr, BC_SIZE_NO_ARGS as u64);

            let true_const = self.context.i8_type().const_int(1, false);
            self.builder.build_return(Some(&true_const)).ok().unwrap();
        }
        // ----

        self.module.verify().unwrap();
    }
}
