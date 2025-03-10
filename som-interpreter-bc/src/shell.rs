#![allow(dead_code)]
#![allow(unused_imports)]

// /// Launches an interactive Read-Eval-Print-Loop within the given universe.
/*
pub fn interactive(
    interpreter: &mut Interpreter,
    universe: &mut UniverseBC,
    verbose: bool,
) -> Result<(), Error> {
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

        let line = format!("ShellExpr{} = ( run: it = ( ^ ( {} ) ) )", counter, line);

        let start = Instant::now();
        let tokens: Vec<Token> = Lexer::new(line.as_str())
            .skip_comments(true)
            .skip_whitespace(true)
            .collect();
        let elapsed = start.elapsed();
        if verbose {
            writeln!(
                &mut stdout,
                "Lexing time: {} ms ({} µs)",
                elapsed.as_millis(),
                elapsed.as_micros(),
            )?;
        }

        let start = Instant::now();
        let class_def = match som_parser::apply(lang::class_def(), tokens.as_slice(), Some(universe)) {
            Some(class_def) => class_def,
            None => {
                println!("ERROR: could not fully parse the given expression");
                continue;
            }
        };
        let elapsed = start.elapsed();
        if verbose {
            writeln!(
                &mut stdout,
                "Parsing time: {} ms ({} µs)",
                elapsed.as_millis(),
                elapsed.as_micros(),
            )?;
        }

        let object_class = universe.object_class();
        let class = match compiler::compile_class(
            &mut universe.interner,
            &class_def,
            Some(&object_class),
        ) {
            Some(class) => class,
            None => {
                writeln!(&mut stdout, "could not compile expression")?;
                continue;
            }
        };
        let metaclass_class = universe.metaclass_class();
        class.set_super_class(&object_class);
        class

            .class()

            .set_super_class(&object_class.class());
        class

            .class()

            .set_class(&metaclass_class);

        let method = class
            .lookup_method(method_name)
            .expect("method not found ??");
        let start = Instant::now();
        interpreter.push_method_frame(method, vec![Value::System, last_value.clone()]);
        if let Some(value) = interpreter.run(universe) {
            writeln!(
                &mut stdout,
                "returned: {} ({:?})",
                value.to_string(&universe),
                value
            )?;
            last_value = value;
        }
        // , |universe| {
        //     universe
        //         .current_frame()
        //         .borrow_mut()
        //         .bindings
        //         .insert("it".into(), last_value.clone());

        //     expr.evaluate(universe, stack_args)
        // });
        let elapsed = start.elapsed();
        if verbose {
            writeln!(
                &mut stdout,
                "Execution time: {} ms ({} µs)",
                elapsed.as_millis(),
                elapsed.as_micros(),
            )?;
            writeln!(&mut stdout)?;
        }

        // match output {
        //     Return::Local(value) => {
        //         writeln!(&mut stdout, "returned: {} ({:?})", value.to_string(&universe), value)?;
        //         last_value = value;
        //     }
        //     Return::NonLocal(value, frame) => {
        //         writeln!(&mut stdout,
        //             "returned (non-local, escaped): {} ({:?})",
        //             value.to_string(&universe),
        //             value
        //         )?;
        //         writeln!(&mut stdout, "intended for frame: {:?}", frame)?;
        //         last_value = value;
        //     }
        //     Return::Exception(message) => println!("ERROR: {}", message),
        //     Return::Restart => println!("ERROR: asked for a restart to the top-level"),
        // }
        counter += 1;
    }

    Ok(())
}*/
