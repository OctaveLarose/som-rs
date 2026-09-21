use inkwell::builder::Builder;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};
use crate::value::Value;

pub struct CodeGen<'ctx> {
    pub(crate) context: &'ctx inkwell::context::Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    // jit_fns: HashMap<String, FunctionValue<'ctx>>,
}

pub type IncFunc = unsafe extern "C" fn(*mut Value) -> u8;

#[no_mangle]
pub extern "C" fn value_new_integer(x: i32) -> u64 {
    // dbg!(Value::new_integer(x).as_u64());
    // dbg!("");
    Value::new_integer(x).as_u64()
}

#[no_mangle]
pub extern "C" fn value_as_integer(val: u64) -> u64 {
    match Value::from(val).as_integer() {
        Some(x) => (x as u32 as u64) | (1u64 << 32),
        None => 0,
    }
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

        let i8_type = codegen.context.i8_type();
        let i32_type = codegen.context.i32_type();
        let i64_type = codegen.context.i64_type();
        let bool_type = codegen.context.bool_type();

        let fn_new_integer_type = i64_type.fn_type(&[i32_type.into()], false);
        let llvm_value_new_integer = codegen.module.add_function("value_new_integer", fn_new_integer_type, None);
        codegen.execution_engine.add_global_mapping(&llvm_value_new_integer, value_new_integer as usize);

        let fn_as_integer_type = i64_type.fn_type(&[i64_type.into()], false);
        let llvm_value_as_integer = codegen.module.add_function("value_as_integer", fn_as_integer_type, None);
        codegen.execution_engine.add_global_mapping(&llvm_value_as_integer, value_as_integer as usize);

        codegen
    }
    pub(crate) fn jit_compile_inc(&mut self) {
        // if let Some(val) = last.as_integer() {
        // *last = value_new_integer(val + 1)

        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // inc(ptr, i32) -> void
        let inc_type = self.context.i8_type().fn_type(&[ptr_type.into()], false);
        let inc_jit_fn_val = self.module.add_function("inc", inc_type, None);

        let entry = self.context.append_basic_block(inc_jit_fn_val, "entry");
        self.builder.position_at_end(entry);

        let last = inc_jit_fn_val.get_nth_param(0).unwrap().into_pointer_value();

        let last_value = self.builder.build_load(i64_type, last, "last_value").unwrap();

        // value_as_integer(i64) -> i64
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let i1_type = self.context.bool_type();

        let value_as_integer = self.module.get_function("value_as_integer").unwrap();

        let call = self.builder.build_call(value_as_integer, &[last_value.into()], "result").ok().unwrap();

        let result = call.try_as_basic_value().basic().unwrap().into_int_value();

        // has_value = (result >> 32) & 1
        let shifted = self.builder.build_right_shift(result, i64_type.const_int(32, false), false, "has_value_shifted").ok().unwrap();
        let has_value = self.builder.build_int_truncate(shifted, i1_type, "has_value").ok().unwrap();

        // value = result as i32
        let value = self.builder.build_int_truncate(result, i32_type, "value").ok().unwrap();

        let is_none = self
            .builder
            .build_int_compare(IntPredicate::EQ, has_value, self.context.bool_type().const_int(0, false), "is_none")
            .unwrap();

        let return_block = self.context.append_basic_block(inc_jit_fn_val, "return");
        let continue_block = self.context.append_basic_block(inc_jit_fn_val, "continue");

        a
        self.builder.build_conditional_branch(is_none, return_block, continue_block).unwrap();

        // ---- return ----
        self.builder.position_at_end(return_block);
        let false_const = self.context.i8_type().const_int(0, false);
        self.builder.build_return(Some(&false_const)).unwrap();

        // ---- continue ----
        self.builder.position_at_end(continue_block);

        // int + 1
        let one = i32_type.const_int(1, false);
        let incremented = self.builder.build_int_add(value, one, "inc").ok().unwrap();

        // value_new_integer(i32) -> i64
        let call = self
            .builder
            .build_call(self.module.get_function("value_new_integer").unwrap(), &[incremented.into()], "new_value")
            .ok()
            .unwrap();

        let new_value = call.try_as_basic_value().basic().unwrap().into_int_value();

        // *last = new_value
        self.builder.build_store(last, new_value).ok().unwrap();

        let true_const = self.context.i8_type().const_int(1, false);
        self.builder.build_return(Some(&true_const)).ok().unwrap();

        self.module.verify().unwrap();
    }
}
