use som_core::bytecode::{BcEntry, Bytecode::*, BytecodeIter};
use som_interpreter_bc::compiler::compile::compile_class;
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::vm_objects::method::Method;
use som_lexer::{Lexer, Token};
use som_parser::lang;
use std::path::PathBuf;

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
    // could replace all the Interned with a "0" here to avoid hard coding their values in tests
    assert!(
        bytecodes.windows(expected_bc_sequence.len()).any(|window| window == expected_bc_sequence),
        "Wrong BC sequence: \n{:?}\nExpected:\n{:?}",
        bytecodes,
        expected_bc_sequence
    )
}

#[test]
fn push_0_1_nil_bytecodes() {
    let class_txt = "Foo = ( run = (
        | a b c |
        a := 0.
        b := 1.
        c := nil.
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::NoArg(Push0),
            BcEntry::TwoArgs(PopLocal, 0, 0),
            BcEntry::NoArg(Push1),
            BcEntry::TwoArgs(PopLocal, 0, 1),
            BcEntry::NoArg(PushNil),
            BcEntry::TwoArgs(PopLocal, 0, 2),
        ],
    );
}

#[test]
fn push_constant_bytecodes() {
    let class_txt = "Foo = ( run = (
        | a b c d e f |
        a := 'abc'.
        b := 'def'.
        c := 'ghi'.
        d := 'abc'.
        e := 'def'.
        f := 'ghi'.
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::OneArg(PushConstant, 0),
            BcEntry::TwoArgs(PopLocal, 0, 0),
            BcEntry::OneArg(PushConstant, 1),
            BcEntry::TwoArgs(PopLocal, 0, 1),
            BcEntry::OneArg(PushConstant, 2),
            BcEntry::TwoArgs(PopLocal, 0, 2),
            BcEntry::OneArg(PushConstant, 0),
            BcEntry::TwoArgs(PopLocal, 0, 3),
            BcEntry::OneArg(PushConstant, 1),
            BcEntry::TwoArgs(PopLocal, 0, 4),
            BcEntry::OneArg(PushConstant, 2),
            BcEntry::TwoArgs(PopLocal, 0, 5),
        ],
    );
}

#[test]
fn send_bytecodes() {
    let class_txt = "Foo = (
        send: a three: b = (
            ^ false
        )

        send: a with: b four: c = (
            ^ false
        )

        run = (
            1 abs.
            1 + 2.
            self send: 1 three: 1.
            self send: 1 with: 1 four: 1.
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(&bytecodes, &[BcEntry::NoArg(Push1), BcEntry::U16Arg(Send1, 96)]);

    // we do a "+ 2" to not have the bytecode INC replace a Send2.
    expect_bytecode_sequence(
        &bytecodes,
        &[BcEntry::NoArg(Push1), BcEntry::OneArg(PushConstant, 0), BcEntry::U16Arg(Send2, 12)],
    );

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::NoArg(PushSelf),
            BcEntry::NoArg(Push1),
            BcEntry::NoArg(Push1),
            BcEntry::U16Arg(Send3, 191),
        ],
    );

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::NoArg(PushSelf),
            BcEntry::NoArg(Push1),
            BcEntry::NoArg(Push1),
            BcEntry::NoArg(Push1),
            BcEntry::U16Arg(SendN, 192),
        ],
    );
}

#[test]
fn super_send_bytecodes() {
    let class_txt = "Foo = (
        run = (
            super send1.
            super sendtwo: 1.
            super send: 1 three: 1.
            super send: 1 with: 1 four: 1.
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(&bytecodes, &[BcEntry::NoArg(PushSelf), BcEntry::U16Arg(SuperSend, 191)]);

    expect_bytecode_sequence(
        &bytecodes,
        &[BcEntry::NoArg(PushSelf), BcEntry::NoArg(Push1), BcEntry::U16Arg(SuperSend, 192)],
    );

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::NoArg(PushSelf),
            BcEntry::NoArg(Push1),
            BcEntry::NoArg(Push1),
            BcEntry::U16Arg(SuperSend, 193),
        ],
    );

    expect_bytecode_sequence(
        &bytecodes,
        &[
            BcEntry::NoArg(PushSelf),
            BcEntry::NoArg(Push1),
            BcEntry::NoArg(Push1),
            BcEntry::NoArg(Push1),
            BcEntry::U16Arg(SuperSend, 194),
        ],
    );
}

#[test]
fn return_self_bytecode_implicit() {
    let class_txt_implicit_return = "Foo = (
        run = (
            42.
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt_implicit_return, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[BcEntry::OneArg(PushConstant, 0), BcEntry::NoArg(Pop), BcEntry::NoArg(ReturnSelf)],
    );
}

#[test]
fn return_self_bytecode_explicit() {
    let class_txt_explicit_return = "Foo = (
        run = (
            ^ self.
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt_explicit_return, "run");

    assert_eq!(bytecodes.len(), 1);
    expect_bytecode_sequence(&bytecodes, &[BcEntry::NoArg(ReturnSelf)]);
}

#[ignore]
#[test]
fn something_jump_bug_popx() {
    // TODO: this test is about jump BC pointing to redundant dup/popx/pop sequences...
    // ...therefore breaking when they're optimized and the jump doesn't know what to do.
    // this issue is currently being circumvented by straight up not removing the sequence when it's a jump target.
    // but this needs to be changed in the future. there's likely an underlying issue that this test right there exemplifies?

    let class_txt = "Foo = (
        testIfTrueTrueResult = (
          | result |
          result := true ifTrue: [ 1 ].
          ^ result class
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "testIfTrueTrueResult");

    let _bc_no_removal = &[
        BcEntry::OneArg(PushGlobal, 0),
        BcEntry::U16Arg(JumpOnFalseTopNil, 2),
        BcEntry::NoArg(Push1),
        BcEntry::NoArg(Dup),
        BcEntry::TwoArgs(PopLocal, 0, 0),
        BcEntry::NoArg(Pop),
        BcEntry::OneArg(PushLocal, 0),
        BcEntry::U16Arg(Send1, 2),
        BcEntry::OneArg(ReturnNonLocal, 1),
        BcEntry::NoArg(Pop),
        BcEntry::NoArg(ReturnSelf),
    ];

    let expected_bytecodes = &[
        BcEntry::OneArg(PushGlobal, 0),
        BcEntry::U16Arg(JumpOnFalseTopNil, 2),
        BcEntry::NoArg(Push1),
        BcEntry::TwoArgs(PopLocal, 0, 0),
        BcEntry::OneArg(PushLocal, 0),
        BcEntry::U16Arg(Send1, 2),
        BcEntry::OneArg(ReturnNonLocal, 1),
        BcEntry::NoArg(Pop),
        BcEntry::NoArg(ReturnSelf),
    ];

    expect_bytecode_sequence(&bytecodes, expected_bytecodes);
}
