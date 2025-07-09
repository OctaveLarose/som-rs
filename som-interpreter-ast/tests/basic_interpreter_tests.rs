use rstest::{fixture, rstest};
use som_gc::gc_interface::{AllocSiteMarker, SOMAllocator};
use som_gc::gcref::Gc;
use som_interpreter_ast::compiler::compile::AstMethodCompilerCtxt;
use som_interpreter_ast::invokable::Return;
use som_interpreter_ast::universe::{GlobalValueStack, Universe};
use som_interpreter_ast::value::Value;
use som_interpreter_ast::vm_objects::instance::Instance;
use som_interpreter_ast::{STACK_ARGS_RAW_PTR_CONST, UNIVERSE_RAW_PTR_CONST};
use som_lexer::{Lexer, Token};
use som_parser::lang;
use std::cell::OnceCell;
use std::path::PathBuf;
use std::sync::atomic::Ordering;

static mut UNIVERSE_CELL: OnceCell<Universe> = OnceCell::new();
static mut STACK_CELL: OnceCell<GlobalValueStack> = OnceCell::new();

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

#[fixture]
pub fn stack<'a>() -> &'a mut GlobalValueStack {
    #[allow(static_mut_refs)]
    unsafe {
        STACK_CELL.get_or_init(|| GlobalValueStack::from(vec![]));

        let mut_stack_ref = STACK_CELL.get_mut().unwrap();
        STACK_ARGS_RAW_PTR_CONST.store(mut_stack_ref, Ordering::SeqCst);
        mut_stack_ref
    }
}

#[rstest]
fn basic_interpreter_tests(universe: &mut Universe, stack: &mut GlobalValueStack) {
    let return_class = Value::Class(universe.load_class("Return").unwrap());
    let compiler_simplification_class = Value::Class(universe.load_class("CompilerSimplification").unwrap());

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
        ("Return testReturnSelf", return_class),
        ("Return testReturnSelfImplicitly", return_class),
        ("Return testNoReturnReturnsSelf", return_class),
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
        ("CompilerSimplification testReturnSelf", compiler_simplification_class),
        ("CompilerSimplification testReturnSelfImplicitly", compiler_simplification_class),
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

    for (expr, expected) in tests {
        println!("testing: '{}'", expr);

        let mut lexer = Lexer::new(expr).skip_comments(true).skip_whitespace(true);
        let tokens: Vec<Token> = lexer.by_ref().collect();
        assert!(lexer.text().is_empty(), "could not fully tokenize test expression");

        let ast_parser = som_parser::apply(lang::expression(), tokens.as_slice()).unwrap();
        let mut compiler = AstMethodCompilerCtxt::new(universe.gc_interface, &mut universe.interner);
        let mut ast = compiler.parse_expression(&ast_parser);

        stack.push(system_value);
        let output = universe.eval_with_frame(stack, 0, 1, &mut ast);

        match &output {
            Return::Local(output) => assert_eq!(output, expected, "unexpected test output value"),
            ret => panic!("unexpected non-local return from basic interpreter test: {:?}", ret),
        }
    }
}

/// Runs the TestHarness, which handles many basic tests written in SOM
#[rstest]
fn test_harness(universe: &mut Universe, stack: &mut GlobalValueStack) {
    let args = ["TestHarness"]
        .iter()
        .map(|str| Value::String(universe.gc_interface.alloc(String::from(*str), AllocSiteMarker::String)))
        .collect();

    let output = universe.initialize(args, stack).unwrap();

    match output {
        Return::Local(val) => assert_eq!(val, Value::INTEGER_ZERO),
        ret => panic!("Unexpected result from test harness: {:?}", ret),
    }
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
fn basic_benchmark_runner(universe: &mut Universe, stack: &mut GlobalValueStack, #[case] benchmark_name: &str) {
    let args = ["BenchmarkHarness", benchmark_name, "1", "1"]
        .iter()
        .map(|str| Value::String(universe.gc_interface.alloc(String::from(*str), AllocSiteMarker::String)))
        .collect();

    let output = universe.initialize(args, stack).unwrap();
    let benchmark_harness_str = universe.intern_symbol("BenchmarkHarness");
    let benchmark_harness_class = universe.lookup_global(benchmark_harness_str).unwrap().as_class().unwrap();

    match output {
        Return::Local(val) => {
            assert!(val.is_ptr::<Instance, Gc<Instance>>());
            assert_eq!(val.as_instance().unwrap().class, benchmark_harness_class)
            // TODO: is that correct/enough?
        }
        ret => panic!("Unexpected result from test harness: {:?}", ret),
    }
}
