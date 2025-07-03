use rstest::{fixture, rstest};
use som_core::interner::Interner;
use som_gc::gc_interface::GCInterface;
use som_interpreter_ast::compiler::compile::AstMethodCompilerCtxt;
use som_interpreter_ast::universe::DEFAULT_HEAP_SIZE;
use som_interpreter_ast::{ast::AstMethodDef, gc::get_callbacks_for_gc};
use som_lexer::{Lexer, Token};
use som_parser::lang;

#[fixture]
fn interner() -> Interner {
    Interner::with_capacity(20)
}

fn get_ast(class_txt: &str, interner: &mut Interner) -> AstMethodDef {
    let mut lexer = Lexer::new(class_txt).skip_comments(true).skip_whitespace(true);
    let tokens: Vec<Token> = lexer.by_ref().collect();
    assert!(lexer.text().is_empty(), "could not fully tokenize test expression");

    let method_def = som_parser::apply(lang::instance_method_def(), tokens.as_slice()).unwrap();

    AstMethodCompilerCtxt::parse_method_def(&method_def, None, GCInterface::init(DEFAULT_HEAP_SIZE, get_callbacks_for_gc()), interner)
}

#[rstest]
fn if_true_inlining_ok(mut interner: Interner) {
    let very_basic = "run = (
        true ifTrue: [ ^true ].
        ^ false
    )";

    let resolve = get_ast(very_basic, &mut interner);

    let ast_answer = "
        Method run (0 locals):AstBody:
            IfInlinedNode (expected bool: true):
                condition expr:
                    GlobalRead(Interned(0))
                body block:
                    AstBody:
                        LocalExit
                            GlobalRead(Interned(0))

            LocalExit
                GlobalRead(Interned(1))
        ";

    let cleaned_ast_answer: String = ast_answer.chars().filter(|c| !c.is_whitespace()).collect();
    let cleaned_resolve: String = resolve.to_string().chars().filter(|c| !c.is_whitespace()).collect();

    assert_eq!(cleaned_ast_answer, cleaned_resolve);
}

#[rstest]
fn if_false_inlining_ok(mut interner: Interner) {
    // based on the method of the same name defined in System
    let method_txt2 = "resolve: a = (
        | class |
        (class == nil) ifFalse: [
            ^class ].
    )";

    let resolve = get_ast(method_txt2, &mut interner);

    let ast_answer = "
        Method resolve: (1 locals):AstBody:
            IfInlinedNode (expected bool: false):
                condition expr:
                    BinaryDispatch \"Interned(0)\":
                        Receiver:
                            LocalVarRead(0)
                        arg:
                            GlobalRead(Interned(1))
                body block:
                    AstBody:
                        LocalExit
                            LocalVarRead(0)
        ";

    let cleaned_ast_answer: String = ast_answer.chars().filter(|c| !c.is_whitespace()).collect();
    let cleaned_resolve: String = resolve.to_string().chars().filter(|c| !c.is_whitespace()).collect();

    assert_eq!(cleaned_ast_answer, cleaned_resolve);
}

#[rstest]
pub fn recursive_inlining(mut interner: Interner) {
    // from Hashtable.
    let contains_key_txt = "containsKey: key = ( 
        | idx e | 
        e isNil ifFalse: [ 
            e keys do: 
                [ :k | 
                    k = key ifTrue: [ 
                        ^true 
                    ] 
                ] 
        ]. 
        )";

    let ast_answer = "Method containsKey: (2 locals):
        AstBody:
            IfInlinedNode (expected bool: false):
                condition expr:\
                    UnaryDispatch \"Interned(0)\":
                        Receiver:
                            LocalVarRead(1)
                body block:
                    AstBody:
                        BinaryDispatch \"Interned(1)\":
                            Receiver:
                                UnaryDispatch \"Interned(2)\":
                                    Receiver:
                                        LocalVarRead(1)
                            arg:
                                Block:
                                    AstBlock(1 params, 0 locals):
                                        IfInlinedNode (expected bool: true):
                                            condition expr:
                                                BinaryDispatch \"Interned(3)\":
                                                    Receiver:
                                                        ArgRead(0, 1)
                                                    arg:
                                                        ArgRead(1, 1)
                                            body block:
                                                AstBody:
                                                    NonLocalExit(1)
                                                        GlobalRead(Interned(4))";

    let resolve = get_ast(contains_key_txt, &mut interner);

    let cleaned_ast_answer: String = ast_answer.chars().filter(|c| !c.is_whitespace()).collect();
    let cleaned_resolve: String = resolve.to_string().chars().filter(|c| !c.is_whitespace()).collect();

    assert_eq!(cleaned_ast_answer, cleaned_resolve);
}

#[rstest]
fn to_do_inlining_ok(mut interner: Interner) {
    let to_do_str = "run = (
        | a |
        a := 42.
        1 to: 50 do: [ :i | (a + i) println ].
    )";

    let resolve = get_ast(to_do_str, &mut interner);

    let ast_answer = "
        Method run (2 locals):AstBody:
            LocalVarWrite(0):
                Literal(Integer(42))
            TodoInlinedNode:
                start:
                    Literal(Integer(1))
                end:
                    Literal(Integer(50))
                body block:
                    AstBody:
                        UnaryDispatch \"Interned(0)\":
                            Receiver:
                                BinaryDispatch \"Interned(1)\":
                                    Receiver:
                                        LocalVarRead(0)
                                    arg:
                                        LocalVarRead(1)
                acc. idx: 1
        ";

    let cleaned_ast_answer: String = ast_answer.chars().filter(|c| !c.is_whitespace()).collect();
    let cleaned_resolve: String = resolve.to_string().chars().filter(|c| !c.is_whitespace()).collect();

    assert_eq!(cleaned_ast_answer, cleaned_resolve);
}
