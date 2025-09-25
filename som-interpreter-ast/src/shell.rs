use std::io;
use std::io::{BufRead, Write};
use std::time::Instant;

use anyhow::Error;

use som_interpreter_ast::value::Value;
use som_interpreter_ast::vm_objects::class::Class;
use som_lexer::{Lexer, Token};
use som_parser::lang;

use som_interpreter_ast::invokable::{Invoke, Return};
use som_interpreter_ast::universe::{GlobalValueStack, Universe};

/// Launches an interactive Read-Eval-Print-Loop within the given universe.
pub fn interactive(universe: &mut Universe, verbose: bool) -> Result<(), Error> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    let mut counter = 0;
    let mut line = String::new();
    let mut last_value = Value::NIL;
    // let signature = universe.intern_symbol("run:");

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

        let stmt = format!("ShellClass_{counter} = ( run: it = ( | tmp | tmp := ({line}). 'it = ' print. ^tmp println ) )");
        let start = Instant::now();
        let tokens: Vec<Token> = Lexer::new(stmt).skip_comments(true).skip_whitespace(true).collect();
        let elapsed = start.elapsed();
        if verbose {
            writeln!(&mut stdout, "Lexing time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros(),)?;
        }

        let start = Instant::now();
        let classdef = match som_parser::apply(lang::class_def(), tokens.as_slice()) {
            Some(expr) => expr,
            None => {
                println!("ERROR: could not fully parse the given expression");
                continue;
            }
        };
        let class_expr = Class::from_class_def(classdef, None, universe.gc_interface, &mut universe.interner).unwrap();
        let elapsed = start.elapsed();
        if verbose {
            writeln!(&mut stdout, "Parsing time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros(),)?;
        }

        let mut method = class_expr.lookup_method(universe.interner.reverse_lookup("run:").unwrap()).unwrap();

        let start = Instant::now();

        let mut value_stack = GlobalValueStack::from(vec![Value::Class(class_expr), last_value]);
        last_value = {
            match method.invoke(universe, &mut value_stack, 2) {
                Return::Local(v) => v,
                Return::NonLocal(v, _) => v,
                #[cfg(feature = "inlining-disabled")]
                Return::Restart => todo!(),
            }
        };

        let elapsed = start.elapsed();
        if verbose {
            writeln!(&mut stdout, "Execution time: {} ms ({} µs)", elapsed.as_millis(), elapsed.as_micros(),)?;
            writeln!(&mut stdout)?;
        }

        counter += 1;
    }

    Ok(())
}
