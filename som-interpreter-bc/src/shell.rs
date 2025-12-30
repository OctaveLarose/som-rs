#![allow(dead_code)]
#![allow(unused_imports)]
use std::io;
use std::io::{BufRead, Write};
use std::time::Instant;

use anyhow::Error;
use som_gc::gcref::Gc;
use som_interpreter_bc::compiler;
use som_interpreter_bc::compiler::compile::compile_class;
use som_interpreter_bc::vm_objects::frame::Frame;
use som_interpreter_bc::vm_objects::method::Invoke;
use som_interpreter_bc::{interpreter::Interpreter, universe::Universe, value::Value};
use som_lexer::{Lexer, Token};
use som_parser::lang;

/// Launches an interactive Read-Eval-Print-Loop within the given universe.
pub fn interactive(universe: &mut Universe, verbose: bool) -> Result<(), Error> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    let mut counter = 0;
    let method_name = universe.intern_symbol("run:");
    let mut line = String::new();

    let mut last_value = Value::NIL;

    loop {
        write!(&mut stdout, "({}) SOM Shell | ", counter)?;
        stdout.flush()?;
        line.clear();
        stdin.read_line(&mut line)?;
        if line.is_empty() {
            writeln!(&mut stdout, "exit")?;
            break;
        }
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line == "exit" {
            break;
        }

        let line = format!("ShellClass_{counter} = ( run: it = ( | tmp | tmp := ({line}). 'it = ' print. ^tmp println ) )");

        let start = Instant::now();
        let tokens: Vec<Token> = Lexer::new(line.as_str()).skip_comments(true).skip_whitespace(true).collect();
        let elapsed = start.elapsed();
        if verbose {
            writeln!(&mut stdout, "Lexing time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros(),)?;
        }

        let start = Instant::now();
        let class_def = match som_parser::apply(lang::class_def(), tokens.as_slice()) {
            Some(class_def) => class_def,
            None => {
                println!("ERROR: could not fully parse the given expression");
                continue;
            }
        };
        let elapsed = start.elapsed();
        if verbose {
            writeln!(&mut stdout, "Parsing time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros(),)?;
        }

        let object_class = universe.core.object_class();
        let mut class = match compile_class(&mut universe.interner, &class_def, Some(&object_class), &mut universe.gc_interface) {
            Some(class) => class,
            None => {
                writeln!(&mut stdout, "could not compile expression")?;
                continue;
            }
        };
        let metaclass_class = universe.core.metaclass_class();
        class.set_super_class(&object_class);
        class.class().set_super_class(&object_class.class());
        class.class().set_class(&metaclass_class);

        let method = class.lookup_method(method_name).expect("method not found ??").as_method_info();
        let start = Instant::now();

        let frame_ptr = Frame::alloc_initial_method(method, &[Value::Class(class), last_value], &mut universe.gc_interface);
        let mut interpreter = Interpreter::new(frame_ptr);
        last_value = interpreter.run(universe).expect("failed to run");

        let elapsed = start.elapsed();
        if verbose {
            writeln!(&mut stdout, "Execution time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros(),)?;
            writeln!(&mut stdout)?;
        }

        counter += 1;
    }

    Ok(())
}
