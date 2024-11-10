//!
//! This is the interpreter for the Simple Object Machine.
//!
#![warn(missing_docs)]

use std::path::PathBuf;

use anyhow::anyhow;
#[cfg(feature = "jemalloc")]
use jemallocator::Jemalloc;
use structopt::StructOpt;

mod shell;

use som_interpreter_ast::invokable::Return;
use som_interpreter_ast::universe::Universe;
use som_interpreter_ast::value::Value;
use som_interpreter_ast::UNIVERSE_RAW_PTR;

#[cfg(feature = "jemalloc")]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Debug, Clone, PartialEq, StructOpt)]
#[structopt(about, author)]
struct Options {
    /// Files to evaluate.
    #[structopt(name = "FILE")]
    file: Option<PathBuf>,

    #[structopt(name = "ARGS")]
    args: Vec<String>,

    /// Set search path for application classes.
    #[structopt(short, long)]
    classpath: Vec<PathBuf>,

    /// Enable verbose output (with timing information).
    #[structopt(short = "v")]
    verbose: bool,

    /// Enable verbose output (with timing information).
    #[structopt(long, short = "hs")]
    heap_size: Option<usize>,
}

fn main() -> anyhow::Result<()> {
    let opts: Options = Options::from_args();

    match opts.file {
        None => {
            let mut universe = Universe::with_classpath(opts.classpath)?;
            shell::interactive(&mut universe, opts.verbose)?
        }
        Some(file) => {
            let file_stem = file.file_stem().ok_or_else(|| anyhow!("the given path has no file stem"))?;
            let file_stem = file_stem.to_str().ok_or_else(|| anyhow!("the given path contains invalid UTF-8 in its file stem"))?;

            let mut classpath = opts.classpath;
            if let Some(directory) = file.parent() {
                classpath.push(directory.to_path_buf());
            }

            let mut universe = {
                match opts.heap_size {
                    Some(heap_size) => Universe::with_classpath_and_heap_size(classpath, heap_size)?,
                    None => Universe::with_classpath(classpath)?,
                }
            };

            unsafe {
                UNIVERSE_RAW_PTR = &mut universe;
            }

            let args = std::iter::once(String::from(file_stem))
                .chain(opts.args.iter().cloned())
                .map(|str| Value::String(universe.gc_interface.alloc(str)))
                .collect();

            let output = universe.initialize(args).unwrap_or_else(|| Return::Exception("could not find 'System>>#initialize:'".to_string()));

            // let class = universe.load_class_from_path(file)?;
            // let instance = Instance::from_class(class);
            // let instance = Value::Instance(Rc::new(RefCell::new(instance)));

            // let invokable = instance.lookup_method(&universe, "run").unwrap();
            // let output = invokable.invoke(&mut universe, vec![instance]);

            match output {
                Return::Exception(message) => println!("ERROR: {}", message),
                Return::Restart => println!("ERROR: asked for a restart to the top-level"),
                _ => {}
            }
        }
    }

    Ok(())
}
