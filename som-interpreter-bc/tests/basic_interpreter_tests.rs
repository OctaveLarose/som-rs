use rstest::{fixture, rstest};
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_interpreter_bc::compiler::compile::compile_class;
use som_interpreter_bc::interpreter::Interpreter;
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::value::Value;
use som_interpreter_bc::vm_objects::frame::Frame;
use som_interpreter_bc::vm_objects::instance::Instance;
use som_interpreter_bc::{INTERPRETER_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use som_lexer::{Lexer, Token};
use som_parser::lang;
use std::cell::OnceCell;
use std::path::PathBuf;
use std::sync::atomic::Ordering;

#[allow(static_mut_refs)]
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
                // PathBuf::from("../core-lib/Examples/Benchmarks/LanguageFeatures"), // breaks basic tests?
                PathBuf::from("../core-lib/TestSuite/BasicInterpreterTests"),
            ];
            Universe::with_classpath(classpath).expect("could not setup test universe")
        });

        let mut_universe_ref = UNIVERSE_CELL.get_mut().unwrap();
        UNIVERSE_RAW_PTR_CONST.store(mut_universe_ref, Ordering::SeqCst);

        mut_universe_ref
    }
}

#[rstest]
fn basic_interpreter_tests(universe: &mut Universe) {
    let return_class_ptr = universe.load_class("Return").unwrap();
    let compiler_simplification_class_ptr = universe.load_class("CompilerSimplification").unwrap();

    let method_name = universe.intern_symbol("run");

    let tests: &[(&str, Value)] = &[
        // {"Self", "assignSuper", 42, ProgramDefinitionError.class},
        ("MethodCall test", Value::Integer(42)),
        ("MethodCall test2", Value::Integer(42)),
        ("NonLocalReturn test1", Value::Integer(42)),
        ("NonLocalReturn test2", Value::Integer(43)),
        ("NonLocalReturn test3", Value::Integer(3)),
        ("NonLocalReturn test4", Value::Integer(42)),
        ("NonLocalReturn test5", Value::Integer(22)),
        ("Blocks testArg1", Value::Integer(42)),
        ("Blocks testArg2", Value::Integer(77)),
        ("Blocks testArgAndLocal", Value::Integer(8)),
        ("Blocks testArgAndContext", Value::Integer(8)),
        ("Blocks testEmptyZeroArg", Value::Integer(1)),
        ("Blocks testEmptyOneArg", Value::Integer(1)),
        ("Blocks testEmptyTwoArg", Value::Integer(1)),
        ("Return testReturnSelf", Value::Class(return_class_ptr.clone())),
        ("Return testReturnSelfImplicitly", Value::Class(return_class_ptr.clone())),
        ("Return testNoReturnReturnsSelf", Value::Class(return_class_ptr.clone())),
        ("Return testBlockReturnsImplicitlyLastValue", Value::Integer(4)),
        ("IfTrueIfFalse test", Value::Integer(42)),
        ("IfTrueIfFalse test2", Value::Integer(33)),
        ("IfTrueIfFalse test3", Value::Integer(4)),
        (
            "CompilerSimplification testReturnConstantSymbol",
            Value::Symbol(universe.intern_symbol("constant")),
        ),
        ("IfTrueIfFalse testIfTrueTrueResult", Value::Class(universe.core.integer_class())),
        ("IfTrueIfFalse testIfTrueFalseResult", Value::Class(universe.core.nil_class())),
        ("IfTrueIfFalse testIfFalseTrueResult", Value::Class(universe.core.nil_class())),
        ("IfTrueIfFalse testIfFalseFalseResult", Value::Class(universe.core.integer_class())),
        ("CompilerSimplification testReturnConstantInt", Value::Integer(42)),
        (
            "CompilerSimplification testReturnSelf",
            Value::Class(compiler_simplification_class_ptr.clone()),
        ),
        (
            "CompilerSimplification testReturnSelfImplicitly",
            Value::Class(compiler_simplification_class_ptr),
        ),
        ("CompilerSimplification testReturnArgumentN", Value::Integer(55)),
        ("CompilerSimplification testReturnArgumentA", Value::Integer(44)),
        ("CompilerSimplification testSetField", Value::Symbol(universe.intern_symbol("foo"))),
        ("CompilerSimplification testGetField", Value::Integer(40)),
        ("Hash testHash", Value::Integer(444)),
        ("Arrays testEmptyToInts", Value::Integer(3)),
        ("Arrays testPutAllInt", Value::Integer(5)),
        ("Arrays testPutAllNil", Value::Class(universe.core.nil_class())),
        ("Arrays testPutAllBlock", Value::Integer(3)),
        ("Arrays testNewWithAll", Value::Integer(1)),
        ("BlockInlining testNoInlining", Value::Integer(1)),
        ("BlockInlining testOneLevelInlining", Value::Integer(1)),
        ("BlockInlining testOneLevelInliningWithLocalShadowTrue", Value::Integer(2)),
        ("BlockInlining testOneLevelInliningWithLocalShadowFalse", Value::Integer(1)),
        ("BlockInlining testShadowDoesntStoreWrongLocal", Value::Integer(33)),
        ("BlockInlining testShadowDoesntReadUnrelated", Value::Class(universe.core.nil_class())),
        ("BlockInlining testBlockNestedInIfTrue", Value::Integer(2)),
        ("BlockInlining testBlockNestedInIfFalse", Value::Integer(42)),
        ("BlockInlining testStackDisciplineTrue", Value::Integer(1)),
        ("BlockInlining testStackDisciplineFalse", Value::Integer(2)),
        ("BlockInlining testDeepNestedInlinedIfTrue", Value::Integer(3)),
        ("BlockInlining testDeepNestedInlinedIfFalse", Value::Integer(42)),
        ("BlockInlining testDeepNestedBlocksInInlinedIfTrue", Value::Integer(5)),
        ("BlockInlining testDeepNestedBlocksInInlinedIfFalse", Value::Integer(43)),
        ("BlockInlining testDeepDeepNestedTrue", Value::Integer(9)),
        ("BlockInlining testDeepDeepNestedFalse", Value::Integer(43)),
        ("BlockInlining testToDoNestDoNestIfTrue", Value::Integer(2)),
        ("NonLocalVars testWriteDifferentTypes", Value::Double(3.75)),
        ("ObjectCreation test", Value::Integer(1000000)),
        ("Regressions testSymbolEquality", Value::Integer(1)),
        ("Regressions testSymbolReferenceEquality", Value::Integer(1)),
        ("Regressions testUninitializedLocal", Value::Integer(1)),
        ("Regressions testUninitializedLocalInBlock", Value::Integer(1)),
        ("BinaryOperation test", Value::Integer(3 + 8)),
        ("NumberOfTests numberOfTests", Value::Integer(65)),
    ];

    let system_value = universe.lookup_global(universe.interner.reverse_lookup("system").unwrap()).unwrap();

    for (counter, (expr, expected)) in tests.iter().enumerate() {
        println!("testing: '{}'", expr);

        let line = format!("BasicInterpreterTest{} = ( run = ( ^ ( {} ) ) )", counter, expr);

        let mut lexer = Lexer::new(line).skip_comments(true).skip_whitespace(true);
        let tokens: Vec<Token> = lexer.by_ref().collect();
        assert!(lexer.text().is_empty(), "could not fully tokenize test expression");

        let class_def = som_parser::apply(lang::class_def(), tokens.as_slice()).unwrap();

        let object_class = universe.core.object_class();
        let class = compile_class(&mut universe.interner, &class_def, Some(&object_class), universe.gc_interface);
        assert!(class.is_some(), "could not compile test expression");
        let mut class = class.unwrap();

        let metaclass_class = universe.core.metaclass_class();
        class.set_super_class(&object_class);
        class.class().set_super_class(&object_class.class());
        class.class().set_class(&metaclass_class);

        let method = class.lookup_method(method_name).expect("method not found ??");

        let frame = Frame::alloc_initial_method(method, &[system_value], universe.gc_interface);
        let mut interpreter = Interpreter::new(frame);
        if let Some(output) = interpreter.run(universe) {
            assert_eq!(&output, expected, "unexpected test output value");
        }
    }
}

/// Runs the TestHarness, which handles many basic tests written in SOM
#[rstest]
fn test_harness(universe: &mut Universe) {
    let args = ["TestHarness"]
        .iter()
        .map(|str| Value::String(universe.gc_interface.alloc(String::from(*str), AllocSiteMarker::String)))
        .collect();

    let mut interpreter = universe.initialize(args).unwrap();

    // needed for GC
    INTERPRETER_RAW_PTR_CONST.store(&mut interpreter, Ordering::SeqCst);

    assert_eq!(interpreter.run(universe), Some(Value::INTEGER_ZERO))
}

#[rstest]
#[case::bounce("Bounce")]
#[case::mandelbrot("Mandelbrot")]
#[case::treesort("TreeSort")]
#[case::list("List")]
#[case::permute("Permute")]
#[case::queens("Queens")]
#[case::quicksort("QuickSort")]
#[case::sieve("Sieve")]
#[case::fannkuch("Fannkuch")]
#[case::json_small("JsonSmall")]
#[case::deltablue("DeltaBlue")]
// #[case::richards("Richards")]
#[case::towers("Towers")]
fn basic_benchmark_runner(universe: &mut Universe, #[case] benchmark_name: &str) {
    let args = ["BenchmarkHarness", benchmark_name, "1", "1"]
        .iter()
        .map(|str| Value::String(universe.gc_interface.alloc(String::from(*str), AllocSiteMarker::String)))
        .collect();

    let mut interpreter = universe.initialize(args).unwrap();

    INTERPRETER_RAW_PTR_CONST.store(&mut interpreter, Ordering::SeqCst);

    let output = interpreter.run(universe);

    let intern_id = universe.interner.reverse_lookup("BenchmarkHarness");
    assert!(intern_id.is_some());
    assert!(universe.has_global(intern_id.unwrap()));
    let benchmark_harness_class = universe.lookup_global(intern_id.unwrap()).unwrap().as_class().unwrap();

    assert!(output.unwrap().is_value_ptr::<Instance>());
    assert_eq!(output.unwrap().as_instance().unwrap().class, benchmark_harness_class)
}
