//!
//! This is the interpreter for the Simple Object Machine.
//!
#![warn(missing_docs)]

use std::sync::atomic::Ordering;

use anyhow::anyhow;
#[cfg(feature = "jemalloc")]
use jemallocator::Jemalloc;
use som_core::cli_parser::CLIOptions;

mod shell;

use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
#[cfg(feature = "inlining-disabled")]
use som_interpreter_ast::invokable::Return;

use som_interpreter_ast::universe::{GlobalValueStack, Universe};
use som_interpreter_ast::value::Value;
use som_interpreter_ast::{STACK_ARGS_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};

#[cfg(feature = "jemalloc")]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

fn main() -> anyhow::Result<()> {
    let opts: CLIOptions = CLIOptions::parse();

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

            let mut value_stack = GlobalValueStack::from(Vec::with_capacity(1000));

            UNIVERSE_RAW_PTR_CONST.store(&mut universe, Ordering::SeqCst);
            STACK_ARGS_RAW_PTR_CONST.store(&mut value_stack, Ordering::SeqCst);

            let args = std::iter::once(String::from(file_stem))
                .chain(opts.args.iter().cloned())
                .map(|str| Value::String(universe.gc_interface.alloc(str, AllocSiteMarker::String)))
                .collect();

            let output = universe.initialize(args, &mut value_stack).unwrap_or_else(|| panic!("could not find 'System>>#initialize:'"));

            //dbg!(universe.gc_interface.get_total_nbr_frames());
            //dbg!(&universe.gc_interface.alloc_map[&AllocSiteMarker::Instance]);
            //dbg!(&universe.gc_interface.alloc_map);

            debug_assert!(value_stack.is_empty());

            match output {
                #[cfg(feature = "inlining-disabled")]
                Return::Restart => println!("ERROR: asked for a restart to the top-level"),
                _ => {}
            }
        }
    }

    Ok(())
}
