use crate::compiler::compile::compile_class;
use crate::universe::Universe;
use crate::value::Value;
use crate::vm_objects::frame::Frame;
use crate::vm_objects::method::Method;
use crate::UNIVERSE_RAW_PTR_CONST;
use rstest::{fixture, rstest};
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcref::Gc;
use som_lexer::{Lexer, Token};
use som_parser::lang;
use std::cell::OnceCell;
use std::path::PathBuf;
use std::sync::atomic::Ordering;

// TODO: instead of a universe cell, these should all use some mocks..
static mut UNIVERSE_CELL: OnceCell<Universe> = OnceCell::new();

#[fixture]
pub fn universe<'a>() -> &'a mut Universe {
    #[allow(static_mut_refs)]
    unsafe {
        UNIVERSE_CELL.get_or_init(|| {
            let classpath = vec![
                PathBuf::from("../core-lib/Smalltalk"),
                PathBuf::from("../core-lib/TestSuite"),
                PathBuf::from("../core-lib/Examples/Benchmarks"),
                PathBuf::from("../core-lib/Examples/Benchmarks/Json"),
                PathBuf::from("../core-lib/Examples/Benchmarks/DeltaBlue"),
                PathBuf::from("../core-lib/Examples/Benchmarks/Richards"),
                PathBuf::from("../core-lib/TestSuite/BasicInterpreterTests"),
            ];
            Universe::with_classpath(classpath).expect("could not setup test universe")
        });

        let mut_universe_ref = UNIVERSE_CELL.get_mut().unwrap();
        UNIVERSE_RAW_PTR_CONST.store(mut_universe_ref, Ordering::SeqCst);
        mut_universe_ref
    }
}

fn get_method(method_txt: &str, method_name: &str, universe: &mut Universe) -> Gc<Method> {
    let method_name_interned = universe.intern_symbol(method_name);

    let class_txt = format!("Foo = ( {} )", method_txt);

    let mut lexer = Lexer::new(class_txt).skip_comments(true).skip_whitespace(true);
    let tokens: Vec<Token> = lexer.by_ref().collect();
    assert!(lexer.text().is_empty(), "could not fully tokenize test expression");

    let class_def = som_parser::apply(lang::class_def(), tokens.as_slice()).unwrap();

    let object_class = universe.core.object_class();
    let class = compile_class(&mut universe.interner, &class_def, Some(&object_class), &mut universe.gc_interface);
    assert!(class.is_some(), "could not compile test expression");

    class.unwrap().lookup_method(method_name_interned).expect("method not found somehow?")
}

#[rstest]
fn frame_basic_local_access(universe: &mut Universe) {
    let method_ref = get_method("foo = ( | a b c | ^ 1 + 1 )", "foo", universe);

    let mut frame = Frame::alloc_initial_method(method_ref, &[], &mut universe.gc_interface);

    frame.assign_local(0, Value::Integer(42));
    assert_eq!(frame.lookup_local(0).as_integer(), Some(42));

    frame.assign_local(0, Value::Integer(24));
    assert_eq!(frame.lookup_local(0).as_integer(), Some(24));

    frame.assign_local(0, Value::Double(400.004));
    frame.assign_local(1, Value::NIL);

    let str_ptr = universe.gc_interface.alloc(String::from("abcd"), AllocSiteMarker::String);
    frame.assign_local(2, Value::String(str_ptr.clone()));

    assert_eq!(frame.lookup_local(0).as_double(), Some(400.004));
    assert_eq!(frame.lookup_local(1), &Value::NIL);
    assert_eq!(frame.lookup_local(2).as_string(), Some(str_ptr));
}

#[rstest]
fn frame_basic_arg_access(universe: &mut Universe) {
    let method_ref = get_method("foo: a and: b also: c = ( ^ false )", "foo:and:also:", universe);

    let mut frame = Frame::alloc_initial_method(
        method_ref,
        &[Value::NIL, Value::INTEGER_ZERO, Value::INTEGER_ONE],
        &mut universe.gc_interface,
    );

    assert_eq!(frame.get_nbr_args(), 4); // 3 + self

    assert_eq!(frame.lookup_argument(0), &Value::NIL);
    assert_eq!(frame.lookup_argument(1), &Value::INTEGER_ZERO);
    assert_eq!(frame.lookup_argument(2), &Value::INTEGER_ONE);

    frame.assign_arg(2, Value::Boolean(true));
    assert_eq!(frame.lookup_argument(2).as_boolean(), Some(true));
}

#[rstest]
fn frame_mixed_local_and_arg_access(universe: &mut Universe) {
    let method_ref = get_method("foo: a and: b = ( | a b c | ^ false )", "foo:and:", universe);

    let mut frame = Frame::alloc_initial_method(
        method_ref,
        &[Value::NIL, Value::Double(1000.0), Value::Integer(42)],
        &mut universe.gc_interface,
    );

    assert_eq!(frame.get_nbr_args(), 3); // 2 + self

    assert_eq!(frame.lookup_argument(1), &Value::Double(1000.0));
    assert_eq!(frame.lookup_argument(2), &Value::Integer(42));
    assert_eq!(frame.lookup_local(0), &Value::NIL);
    assert_eq!(frame.lookup_local(1), &Value::NIL);
    assert_eq!(frame.lookup_local(2), &Value::NIL);

    frame.assign_arg(0, Value::Boolean(true));
    frame.assign_local(0, Value::Boolean(false));

    assert_eq!(frame.lookup_argument(0).as_boolean(), Some(true));
    assert_eq!(frame.lookup_local(0).as_boolean(), Some(false));

    frame.assign_arg(1, Value::Integer(42));
    frame.assign_local(2, Value::Double(42.42));

    assert_eq!(frame.lookup_argument(1).as_integer(), Some(42));
    assert_eq!(frame.lookup_local(2).as_double(), Some(42.42));
}
