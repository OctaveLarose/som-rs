use std::convert::TryInto;
use std::fs;
use std::io::Write;

use crate::gc::VecValue;
use crate::interpreter::Interpreter;
use crate::pop_args_from_stack;
use crate::primitives::PrimInfo;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::{Nil, Primitive, StringLike};
use crate::value::Value;
use crate::vm_objects::class::Class;
use anyhow::{Context, Error};
use num_bigint::BigInt;
use once_cell::sync::Lazy;
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcref::Gc;
use som_value::interned::Interned;

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

fn load_file(interpreter: &mut Interpreter, universe: &mut Universe) -> Result<Option<Gc<String>>, Error> {
    pop_args_from_stack!(interpreter, _a => Value, path => StringLike);
    let path = path.as_str(|sym| universe.lookup_symbol(sym));

    let Ok(value) = fs::read_to_string(&*path) else {
        return Ok(None);
    };

    Ok(Some(universe.gc_interface.alloc(value, AllocSiteMarker::String)))
}

fn print_string(interp: &mut Interpreter, universe: &mut Universe) -> Result<Value, Error> {
    pop_args_from_stack!(interp, system => Value, string => StringLike);

    let string = string.as_str(|sym| universe.lookup_symbol(sym));
    print!("{string}");
    std::io::stdout().flush()?;

    Ok(system)
}

fn print_newline(_: Value) -> Result<Nil, Error> {
    println!();
    Ok(Nil)
}

fn error_print(interp: &mut Interpreter, universe: &mut Universe) -> Result<Value, Error> {
    pop_args_from_stack!(interp, system => Value, string => StringLike);

    let string = string.as_str(|sym| universe.lookup_symbol(sym));

    eprint!("{string}");
    std::io::stderr().flush()?;

    Ok(system)
}

fn error_println(interp: &mut Interpreter, universe: &mut Universe) -> Result<Value, Error> {
    pop_args_from_stack!(interp, system => Value, string => StringLike);
    let string = string.as_str(|sym| universe.lookup_symbol(sym));
    eprintln!("{string}");
    Ok(system)
}

fn load(interp: &mut Interpreter, universe: &mut Universe) -> Result<Gc<Class>, Error> {
    pop_args_from_stack!(interp, _a => Value, class_name => Interned);
    let class_name = universe.lookup_symbol(class_name).to_string();
    let class = universe.load_class(class_name)?;

    Ok(class)
}

fn has_global(interp: &mut Interpreter, universe: &mut Universe) -> Result<bool, Error> {
    pop_args_from_stack!(interp, _a => Value, name => Interned);
    Ok(universe.has_global(name))
}

fn global(interp: &mut Interpreter, universe: &mut Universe) -> Result<Option<Value>, Error> {
    pop_args_from_stack!(interp, _a => Value, name => Interned);

    Ok(universe.lookup_global(name))
}

fn global_put(interp: &mut Interpreter, universe: &mut Universe) -> Result<Option<Value>, Error> {
    pop_args_from_stack!(interp, _a => Value, name => Interned, value => Value);
    universe.assign_global(name, value);
    Ok(Some(value))
}

fn exit(interp: &mut Interpreter, _: &mut Universe) -> Result<(), Error> {
    pop_args_from_stack!(interp, _a => Value, status => i32);
    std::process::exit(status);
}

fn ticks(interpreter: &mut Interpreter, _: &mut Universe) -> Result<i32, Error> {
    pop_args_from_stack!(interpreter, _a => Value);

    const SIGNATURE: &str = "System>>#ticks";

    interpreter
        .start_time
        .elapsed()
        .as_micros()
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `i128` to `i32`"))
}

fn time(interpreter: &mut Interpreter, _: &mut Universe) -> Result<i32, Error> {
    const SIGNATURE: &str = "System>>#time";
    pop_args_from_stack!(interpreter, _a => Value);

    interpreter
        .start_time
        .elapsed()
        .as_millis()
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `i128` to `i32`"))
}

fn print_stack_trace(interpreter: &mut Interpreter, _: &mut Universe) -> Result<bool, Error> {
    pop_args_from_stack!(interpreter, _a => Value);

    let frame_stack = {
        let mut frame_stack = vec![];
        let mut current_frame = interpreter.get_current_frame();
        while !current_frame.is_empty() {
            frame_stack.push(current_frame.clone());
            current_frame = current_frame.prev_frame.clone();
        }
        frame_stack
    };

    println!("Stack trace:");
    for (frame_idx, frame) in frame_stack.iter().enumerate() {
        let class = frame.get_method_holder();
        println!(
            "\t{}: {}>>#{} @bi: {}",
            frame_idx,
            class.name(),
            frame.current_context.signature(),
            frame.bytecode_idx
        );
    }
    println!("----------------");

    Ok(true)
}

fn full_gc(interp: &mut Interpreter, universe: &mut Universe) -> Result<bool, Error> {
    pop_args_from_stack!(interp, _a => Value);
    Ok(universe.gc_interface.full_gc_request())
}

fn gc_stats(interp: &mut Interpreter, universe: &mut Universe) -> Result<VecValue, Error> {
    pop_args_from_stack!(interp, _a => Value);

    let gc_interface = &mut universe.gc_interface;

    let (total_gc, total_gc_time, total_bytes_bigint) = {
        // ----- INTENDED BEHAVIOR -----
        let total_gc = gc_interface.get_nbr_collections();
        let total_gc_time = gc_interface.alloc(BigInt::from(gc_interface.get_total_gc_time()), AllocSiteMarker::BigInt);
        let total_bytes_bigint = gc_interface.alloc(BigInt::from(gc_interface.get_used_bytes()), AllocSiteMarker::BigInt);

        // -----  PROGRAM REPR SIZE -----
        //let total_gc: usize = 24;
        //let total_gc_time = gc_interface.alloc(BigInt::from(-2424), AllocSiteMarker::BigInt);
        //let total_bytes_bigint = gc_interface.alloc(BigInt::from(gc_interface.total_program_repr_size), AllocSiteMarker::BigInt);

        // -----  TOTAL MEM USAGE -----
        //let total_gc: usize = 42;
        //let total_gc_time = gc_interface.alloc(BigInt::from(-4242), AllocSiteMarker::BigInt);
        //let total_bytes_bigint = gc_interface.alloc(
        //    BigInt::from(gc_interface.total_program_repr_size + gc_interface.total_other_memory_size),
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
