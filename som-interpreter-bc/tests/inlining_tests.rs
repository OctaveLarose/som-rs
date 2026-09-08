use som_core::bytecode::{BcEntry, Bytecode::*, BytecodeIter};
use std::path::PathBuf;

use som_interpreter_bc::compiler::compile::compile_class;
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::vm_objects::method::Method;
use som_lexer::{Lexer, Token};
use som_parser::lang;

fn setup_universe() -> Universe {
    let classpath = vec![
        PathBuf::from("../core-lib/Smalltalk"),
        PathBuf::from("../core-lib/TestSuite/BasicInterpreterTests"),
    ];
    Universe::with_classpath(classpath).expect("could not setup test universe")
}

fn get_bytecodes_from_method(class_txt: &str, method_name: &str) -> Vec<BcEntry> {
    let mut universe = setup_universe();

    let method_name_interned = universe.intern_symbol(method_name);

    let mut lexer = Lexer::new(class_txt).skip_comments(true).skip_whitespace(true);
    let tokens: Vec<Token> = lexer.by_ref().collect();
    assert!(lexer.text().is_empty(), "could not fully tokenize test expression");

    let class_def = som_parser::apply(lang::class_def(), tokens.as_slice()).unwrap();

    let object_class = universe.core.object_class();
    let class = compile_class(&mut universe.interner, &class_def, Some(&object_class), &mut universe.gc_interface);
    assert!(class.is_some(), "could not compile test expression");

    let class = class.unwrap();
    let method = class.lookup_method(method_name_interned).expect("method not found ??");

    match &*method {
        Method::Defined(m) => BytecodeIter::init(&m.body, 0).collect(),
        _ => unreachable!(),
    }
}

fn expect_bytecode_sequence(bytecodes: &[BcEntry], expected_bc_sequence: &[BcEntry]) {
    assert!(
        bytecodes.windows(expected_bc_sequence.len()).any(|window| window == expected_bc_sequence),
        "Wrong BC sequence: \n{:?}\nExpected:\n{:?}",
        bytecodes,
        expected_bc_sequence
    )
}

#[test]
fn if_true_or_false_inlining_ok() {
    let class_txt = "Foo = ( run = (
        true ifTrue: [ ^true ].
        ^ false
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::U16Arg(JumpOnFalseTopNil, 6),
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::NoArg(ReturnLocal),
            BcEntry::NoArg(Pop),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::NoArg(ReturnLocal),
        ],
    );

    let class_txt2 = "Foo = ( run = (
        false ifFalse: [ ^false ].
        ^ true
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::U16Arg(JumpOnTrueTopNil, 6),
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::NoArg(ReturnLocal),
            BcEntry::NoArg(Pop),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::NoArg(ReturnLocal),
        ],
    );
}

#[test]
fn if_true_if_false_inlining_ok() {
    let class_txt = "Foo = ( run = ( true ifTrue: [ ^true ] ifFalse: [ ^false]. ))";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::U16Arg(JumpOnFalsePop, 9),
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::NoArg(ReturnLocal),
            BcEntry::U16Arg(Jump, 6),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::NoArg(ReturnLocal),
            BcEntry::NoArg(Pop),
            BcEntry::NoArg(ReturnSelf),
        ],
    );

    let class_txt2 = "Foo = ( run = ( true ifFalse: [ ^false ] ifTrue: [ ^ true]. ))";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::U16Arg(JumpOnTruePop, 9),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::NoArg(ReturnLocal),
            BcEntry::U16Arg(Jump, 6),
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::NoArg(ReturnLocal),
            BcEntry::NoArg(Pop),
            BcEntry::NoArg(ReturnSelf),
        ],
    );
}

#[test]
fn while_true_false_inlining_ok() {
    let class_txt = "Foo = ( run = (
        | cnt |
        cnt := 42.
        [ cnt < 1000000 ] whileTrue: [
            cnt := cnt + 1.
        ]
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushLocal, 0),
            BcEntry::OneArg(PushConstant, 1),
            BcEntry::U16Arg(Send2, 73),
            BcEntry::U16Arg(JumpOnFalsePop, 12),
            BcEntry::OneArg(PushLocal, 0),
            BcEntry::NoArg(Inc),
            BcEntry::TwoArgs(PopLocal, 0, 0),
            BcEntry::U16Arg(JumpBackward, 16),
        ],
    );

    // let class_txt_2 = class_txt.replace("whileTrue", "whileFalse");
    // let bytecodes = get_bytecodes_from_method(class_txt_2.as_str(), "run");
    //
    // expect_bytecode_sequence(&bytecodes, &[
    //     JumpOnTruePop(8),
    //     PushLocal(0, 0),
    //     PushConstant(3),
    //     Send2(4),
    //     Dup,
    //     PopLocal(0, 0),
    //     Pop,
    //     JumpBackward(10)
    // ])
}

#[test]
fn or_and_inlining_ok() {
    let class_txt = "Foo = ( run = (
        ^ (true or: [ false ])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::U16Arg(JumpOnTruePop, 8),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::U16Arg(Jump, 5),
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::NoArg(ReturnLocal),
        ],
    );

    let class_txt2 = "Foo = ( run = (
        ^ (true and: [ false ])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::U16Arg(JumpOnFalsePop, 8),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::U16Arg(Jump, 5),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::NoArg(ReturnLocal),
        ],
    );
}

// test for if we decide to inline or/and not just when it's blocks. part of a breaking benchmark (TreeSort) when we do inline more than just blocks with or/and
#[ignore]
#[test]
fn or_and_no_block_inlining_ok() {
    let class_txt = "Foo = ( run = (
            ^(left  isNil || [ (left  value <  value) && left check ]) &&
             (right isNil || [ (right value >= value) && right check ])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::U16Arg(JumpOnTruePop, 3),
            BcEntry::OneArg(PushGlobal, 1),
            BcEntry::U16Arg(Jump, 2),
            BcEntry::OneArg(PushGlobal, 0),
            BcEntry::NoArg(ReturnLocal),
        ],
    );
}

#[test]
fn inlining_pyramid() {
    let class_txt = "Foo = ( run = (
        | a b c d e f g |
        ^ (a ifTrue: [b ifTrue: [c ifTrue: [d ifTrue: [e ifTrue: [f ifTrue: [g]]]]]])
    ))
    ";

    let class_txt2 = "Foo = ( run = (
        | a |
        ^ (a ifTrue: [| b | b ifTrue: [| c | c ifTrue: [| d | d ifTrue: [| e | e ifTrue: [| f | f ifTrue: [| g | g]]]]]])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    let bytecodes2 = get_bytecodes_from_method(class_txt2, "run");

    let expected_bc = &[
        BcEntry::OneArg(PushLocal, 0),
        BcEntry::U16Arg(JumpOnFalseTopNil, 30),
        BcEntry::OneArg(PushLocal, 1),
        BcEntry::U16Arg(JumpOnFalseTopNil, 25),
        BcEntry::OneArg(PushLocal, 2),
        BcEntry::U16Arg(JumpOnFalseTopNil, 20),
        BcEntry::OneArg(PushLocal, 3),
        BcEntry::U16Arg(JumpOnFalseTopNil, 15),
        BcEntry::OneArg(PushLocal, 4),
        BcEntry::U16Arg(JumpOnFalseTopNil, 10),
        BcEntry::OneArg(PushLocal, 5),
        BcEntry::U16Arg(JumpOnFalseTopNil, 5),
        BcEntry::OneArg(PushLocal, 6),
        BcEntry::NoArg(ReturnLocal),
    ];

    expect_bytecode_sequence(&bytecodes, expected_bc);
    expect_bytecode_sequence(&bytecodes2, expected_bc);
}
#[test]
fn to_do_inlining_ok() {
    let class_txt = "Test = ( run = (
        | cnt |
        cnt := 0.
        1 to: 100 do: [ :i |
            cnt := cnt + i.
        ].
        cnt println.
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::NoArg(Push0),
            BcEntry::TwoArgs(PopLocal, 0, 0),
            BcEntry::NoArg(Push1),
            BcEntry::OneArg(PushConstant, 0),
            BcEntry::NoArg(Dup2),
            BcEntry::U16Arg(JumpIfGreater, 21),
            BcEntry::NoArg(Dup),
            BcEntry::TwoArgs(PopLocal, 0, 1),
            BcEntry::OneArg(PushLocal, 0),
            BcEntry::OneArg(PushLocal, 1),
            BcEntry::U16Arg(Send2, 12),
            BcEntry::TwoArgs(PopLocal, 0, 0),
            BcEntry::NoArg(Inc),
            BcEntry::U16Arg(JumpBackward, 18),
            BcEntry::NoArg(Pop),
        ],
    );
}
