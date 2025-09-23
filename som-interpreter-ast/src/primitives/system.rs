use crate::gc::VecValue;
use crate::get_args_from_stack;
use crate::primitives::{PrimInfo, PrimitiveFn};
use crate::universe::{GlobalValueStack, Universe};
use crate::value::convert::FromArgs;
use crate::value::convert::{Primitive, StringLike};
use crate::value::Value;
use crate::vm_objects::class::Class;
use anyhow::{bail, Context, Error};
use num_bigint::BigInt;
use once_cell::sync::Lazy;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcref::Gc;
use som_value::interned::Interned;
use std::convert::TryFrom;
use std::fs;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| {
    Box::new([
        ("loadFile:", self::load_file.into_func(), true),
        ("printString:", self::print_string.into_func(), true),
        ("printNewline", self::print_newline.into_func(), true),
        ("errorPrint:", self::error_print.into_func(), true),
        ("errorPrintln:", self::error_println.into_func(), true),
        ("load:", self::load.into_func(), true),
        ("ticks", self::ticks.into_func(), true),
        ("time", self::time.into_func(), true),
        ("fullGC", self::full_gc.into_func(), true),
        ("gcStats", self::gc_stats.into_func(), true),
        ("exit:", self::exit.into_func(), true),
        ("global:", self::global.into_func(), true),
        ("global:put:", self::global_put.into_func(), true),
        ("hasGlobal:", self::has_global.into_func(), true),
        ("printStackTrace", self::print_stack_trace.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

fn load_file(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, _a => Value, path => StringLike);
    let path = match path {
        StringLike::String(ref string) => string,
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    match fs::read_to_string(path) {
        Ok(value) => Ok(Value::String(universe.gc_interface.alloc(value, AllocSiteMarker::String))),
        Err(_) => Ok(Value::NIL),
    }
}

fn print_string(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, system => Value, string => StringLike);
    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    print!("{}", string);
    Ok(system)
}

fn print_newline(_: Value) -> Result<Value, Error> {
    println!();
    Ok(Value::NIL)
}

fn error_print(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, system => Value, string => StringLike);

    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprint!("{}", string);
    Ok(system)
}

fn error_println(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const _: &str = "System>>#errorPrintln:";

    get_args_from_stack!(stack, system => Value, string => StringLike);
    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Char(char) => &*String::from(char),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprintln!("{}", string);
    Ok(system)
}

fn load(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "System>>#load:";

    get_args_from_stack!(stack, _a => Value, class_name => Interned);
    if let Some(cached_class) = universe.lookup_global(class_name) {
        if cached_class.is_ptr::<Class, Gc<Class>>() {
            return Ok(cached_class);
        }
    }

    let class_name_str = universe.lookup_symbol(class_name).to_string();
    match universe.load_class(class_name_str) {
        Ok(class) => Ok(Value::Class(class)),
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    }
}

fn has_global(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, _a => Value, name => Interned);
    Ok(Value::Boolean(universe.has_global(name)))
}

fn global(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, _a => Value, sym => Interned);
    Ok(universe.lookup_global(sym).unwrap_or(Value::NIL))
}

fn global_put(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, _a => Value, name => Interned, value => Value);
    universe.assign_global(name, &value);
    Ok(value)
}

fn exit(_: Value, status: i32) -> Result<Value, Error> {
    std::process::exit(status)
}

fn ticks(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "System>>#ticks";

    get_args_from_stack!(stack, _a => Value);

    let x = universe
        .start_time
        .elapsed()
        .as_micros()
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `i128` to `i32`"))
        .unwrap();

    Ok(Value::Integer(x))
}

fn time(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    const SIGNATURE: &str = "System>>#time";

    get_args_from_stack!(stack, _a => Value);
    match i32::try_from(universe.start_time.elapsed().as_millis()) {
        Ok(micros) => Ok(Value::Integer(micros)),
        Err(err) => bail!(format!("'{}': {}", SIGNATURE, err)),
    }
}

// this function is unusable after my recent changes to the frame. needs to be fixed when a compilation flag for frame debug info is enabled
fn print_stack_trace(_: Value) -> Result<bool, Error> {
    // const SIGNATURE: &str = "System>>#printStackTrace";

    dbg!("printStackTrace is broken (on purpose). It can be fixed and reenabled with a debug flag, though.");
    /*
            for frame in &universe.frames {
            // let class = frame.borrow().get_method_holder(universe);
            // let signature = frame.borrow().get_method_signature();
            // let signature = universe.lookup_symbol(signature);
            let signature = "we do not support method signatures in stack traces anymore...";
            // let block = match frame.borrow().kind() {
            //     FrameKind::Block { .. } => "$block",
            //     _ => "",
            // };
            // println!("{}>>#{}{}", class.borrow().name(), signature, block);
            println!("{}>>#{}", class.borrow().name(), signature);
        }
    */
    Ok(true)
}

fn full_gc(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<Value, Error> {
    get_args_from_stack!(stack, _a => Value);
    Ok(Value::Boolean(universe.gc_interface.full_gc_request()))
}

fn gc_stats(universe: &mut Universe, stack: &mut GlobalValueStack) -> Result<VecValue, Error> {
    get_args_from_stack!(stack, _a => Value);
    let gc_interface = &mut universe.gc_interface;

    let (total_gc, total_gc_time, total_bytes_bigint) = {
        // ----- INTENDED BEHAVIOR -----
        //let total_gc = gc_interface.get_nbr_collections();
        //let total_gc_time = gc_interface.alloc(BigInt::from(gc_interface.get_total_gc_time()), AllocSiteMarker::BigInt);
        //let total_bytes_bigint = gc_interface.alloc(BigInt::from(gc_interface.get_used_bytes()), AllocSiteMarker::BigInt);

        // -----  PROGRAM REPR SIZE -----
        let total_gc: usize = 24;
        let total_gc_time = gc_interface.alloc(BigInt::from(-2424), AllocSiteMarker::BigInt);
        let total_bytes_bigint = gc_interface.alloc(BigInt::from(gc_interface.total_program_repr_size), AllocSiteMarker::BigInt);

        // -----  TOTAL MEM USAGE -----
        //let total_gc: usize = 42;
        //let total_gc_time = gc_interface.alloc(BigInt::from(-4242), AllocSiteMarker::BigInt);
        //let total_bytes_bigint = gc_interface.alloc(
        //    BigInt::from(
        //        gc_interface.total_program_repr_size + gc_interface.total_other_memory_size + (stack.get_capacity() * size_of::<Value>()) as u128,
        //    ),
        //    AllocSiteMarker::BigInt,
        //);

        (total_gc, total_gc_time, total_bytes_bigint)
    };

    Ok(VecValue(universe.gc_interface.alloc_slice(
        &[
            Value::Integer(total_gc as i32),
            Value::BigInteger(total_gc_time),
            Value::BigInteger(total_bytes_bigint),
        ],
        AllocSiteMarker::VecValue,
    )))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
