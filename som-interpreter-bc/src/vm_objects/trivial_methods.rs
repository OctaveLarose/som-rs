use crate::compiler::{value_from_literal, Literal};
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::instance::Instance;
use crate::{stack_drop, stack_last, stack_pop, stack_push};
use som_value::interned::Interned;
use std::cell::Cell;

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialLiteralMethod {
    pub(crate) literal: Literal,
}

impl TrivialLiteralMethod {
    pub fn invoke(&self, universe: &mut Universe) {
        let value_from_literal = value_from_literal(&self.literal, &mut universe.gc_interface);
        stack_push!(universe.sp, value_from_literal);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialGlobalMethod {
    pub(crate) global_name: Interned,
    pub(crate) cached_entry: Cell<Option<Value>>,
}

impl TrivialGlobalMethod {
    pub fn invoke(&self, universe: &mut Universe) {
        stack_drop!(universe.sp); // receiver off the stack.

        if let Some(cached_entry) = self.cached_entry.get() {
            stack_push!(universe.sp, cached_entry);
            return;
        }

        universe
            .lookup_global(self.global_name)
            .map(|v| {
                stack_push!(universe.sp, v);
                self.cached_entry.replace(Some(v));
            })
            .or_else(|| {
                let frame = universe.get_current_frame();
                let self_value = frame.get_self();
                universe.unknown_global(self_value, self.global_name)
            })
            .unwrap_or_else(|| panic!("global not found and unknown_global call failed somehow?"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialGetterMethod {
    pub(crate) field_idx: u8,
}

impl TrivialGetterMethod {
    pub fn invoke(&self, universe: &mut Universe) {
        let arg = stack_pop!(universe.sp);

        if let Some(cls) = arg.as_class() {
            stack_push!(universe.sp, cls.class().lookup_field(self.field_idx as usize));
        } else if let Some(instance) = arg.as_instance() {
            stack_push!(universe.sp, *Instance::lookup_field(&instance, self.field_idx as usize));
        } else {
            panic!("trivial getter not called on a class/instance?")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TrivialSetterMethod {
    pub(crate) field_idx: u8,
}

impl TrivialSetterMethod {
    pub fn invoke(&self, universe: &mut Universe) {
        let val = stack_pop!(universe.sp);
        let rcvr = stack_last!(universe.sp);

        if let Some(cls) = rcvr.as_class() {
            cls.class().assign_field(self.field_idx as usize, val);
        } else if let Some(instance) = rcvr.as_instance() {
            Instance::assign_field(&instance, self.field_idx as usize, val)
        } else {
            panic!("trivial getter not called on a class/instance?")
        }
    }
}
