use std::convert::TryInto;
use std::fs;
use std::io::Write;

use crate::gc::VecValue;
use crate::interpreter::Interpreter;
use crate::primitives::PrimInfo;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::{Nil, Primitive, StringLike, System};
use crate::value::Value;
use crate::vm_objects::class::Class;
use anyhow::{Context, Error};
use num_bigint::BigInt;
use once_cell::sync::Lazy;
use som_core::interner::Interned;
use som_gc::gcref::Gc;

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

fn load_file(_: &mut Interpreter, universe: &mut Universe, _: Value, path: StringLike) -> Result<Option<Gc<String>>, Error> {
    const _: &str = "System>>#loadFie:";

    let path = match path {
        StringLike::String(ref string) => string.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let Ok(value) = fs::read_to_string(path) else {
        return Ok(None);
    };

    Ok(Some(universe.gc_interface.alloc(value)))
}

fn print_string(_: &mut Interpreter, universe: &mut Universe, _: Value, string: StringLike) -> Result<System, Error> {
    const _: &str = "System>>#printString:";

    let string = match string {
        StringLike::String(ref string) => string.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    print!("{string}");
    std::io::stdout().flush()?;

    Ok(System)
}

fn print_newline(_: &mut Interpreter, _: &mut Universe, _: Value) -> Result<Nil, Error> {
    println!();
    Ok(Nil)
}

fn error_print(_: &mut Interpreter, universe: &mut Universe, _: Value, string: StringLike) -> Result<System, Error> {
    const _: &str = "System>>#errorPrint:";

    let string = match string {
        StringLike::String(ref string) => string.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprint!("{string}");
    std::io::stderr().flush()?;

    Ok(System)
}

fn error_println(_: &mut Interpreter, universe: &mut Universe, _: Value, string: StringLike) -> Result<System, Error> {
    const _: &str = "System>>#errorPrintln:";

    let string = match string {
        StringLike::String(ref string) => string.as_str(),
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprintln!("{string}");

    Ok(System)
}

fn load(_: &mut Interpreter, universe: &mut Universe, _: Value, class_name: Interned) -> Result<Gc<Class>, Error> {
    const _: &str = "System>>#load:";

    let class_name = universe.lookup_symbol(class_name).to_string();
    let class = universe.load_class(class_name)?;

    Ok(class)
}

fn has_global(_: &mut Interpreter, universe: &mut Universe, _: Value, name: Interned) -> Result<bool, Error> {
    const _: &str = "System>>#hasGlobal:";

    Ok(universe.has_global(name))
}

fn global(_: &mut Interpreter, universe: &mut Universe, _: Value, name: Interned) -> Result<Option<Value>, Error> {
    const _: &str = "System>>#global:";

    Ok(universe.lookup_global(name))
}

fn global_put(_: &mut Interpreter, universe: &mut Universe, _: Value, name: Interned, value: Value) -> Result<Option<Value>, Error> {
    const _: &str = "System>>#global:put:";
    universe.assign_global(name, value);
    Ok(Some(value))
}

fn exit(_: &mut Interpreter, _: &mut Universe, status: i32) -> Result<(), Error> {
    std::process::exit(status);
}

fn ticks(interpreter: &mut Interpreter, _: &mut Universe, _: Value) -> Result<i32, Error> {
    const SIGNATURE: &str = "System>>#ticks";

    interpreter
        .start_time
        .elapsed()
        .as_micros()
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `i128` to `i32`"))
}

fn time(interpreter: &mut Interpreter, _: &mut Universe, _: Value) -> Result<i32, Error> {
    const SIGNATURE: &str = "System>>#time";

    interpreter
        .start_time
        .elapsed()
        .as_millis()
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `i128` to `i32`"))
}

fn print_stack_trace(interpreter: &mut Interpreter, _: &mut Universe, _: Value) -> Result<bool, Error> {
    const _: &str = "System>>#printStackTrace";

    let frame_stack = {
        let mut frame_stack = vec![];
        let mut current_frame = interpreter.current_frame;
        while !current_frame.is_empty() {
            frame_stack.push(current_frame);
            current_frame = current_frame.prev_frame;
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

fn full_gc(_: &mut Interpreter, universe: &mut Universe, _: Value) -> Result<bool, Error> {
    Ok(universe.gc_interface.full_gc_request())
}

fn gc_stats(_: &mut Interpreter, universe: &mut Universe, _: Value) -> Result<Gc<VecValue>, Error> {
    let gc_interface = &mut universe.gc_interface;

    let total_gc = gc_interface.get_nbr_collections();
    let total_gc_time = gc_interface.alloc(BigInt::from(gc_interface.get_total_gc_time()));
    let total_bytes_bigint = gc_interface.alloc(BigInt::from(gc_interface.get_used_bytes()));

    Ok(universe.gc_interface.alloc(VecValue(vec![
        Value::Integer(total_gc as i32),
        Value::BigInteger(total_gc_time),
        Value::BigInteger(total_bytes_bigint),
    ])))
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
}
