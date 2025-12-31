use rstest::{fixture, rstest};
use som_core::interner::Interner;
use som_gc::gc_interface::GCInterface;
use som_interpreter_ast::ast::AstExpression::*;
use som_interpreter_ast::ast::InlinedNode::IfInlined;
use som_interpreter_ast::ast::{AstBinaryDispatch, AstBody, AstDispatchNode, AstLiteral, AstMethodDef, AstUnaryDispatch, InlinedNode};
use som_interpreter_ast::compiler::AstMethodCompilerCtxt;
use som_interpreter_ast::nodes::global_read::GlobalNode;
use som_interpreter_ast::nodes::inlined::if_inlined_node::IfInlinedNode;
use som_interpreter_ast::nodes::inlined::to_do_inlined_node::ToDoInlinedNode;
use som_interpreter_ast::universe::DEFAULT_HEAP_SIZE;
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

    let mut gc_interface = Box::new(GCInterface::init(DEFAULT_HEAP_SIZE));
    AstMethodCompilerCtxt::parse_method_def(&method_def, None, &mut gc_interface, interner)
}

#[rstest]
fn if_true_inlining_ok(mut interner: Interner) {
    let very_basic = "run = (
        true ifTrue: [ ^true ].
        ^ false
    )";

    let ast = get_ast(very_basic, &mut interner);

    assert_eq!(
        ast,
        AstMethodDef {
            signature: "run".to_string(),
            locals_nbr: 0,
            body: AstBody {
                exprs: vec![
                    InlinedCall(Box::new(IfInlined(IfInlinedNode {
                        expected_bool: true,
                        cond_expr: GlobalRead(Box::new(GlobalNode::from(interner.reverse_lookup("true").unwrap()))),
                        body_instrs: AstBody {
                            exprs: vec![LocalExit(Box::new(GlobalRead(Box::new(GlobalNode::from(
                                interner.reverse_lookup("true").unwrap()
                            )))))],
                        },
                    },),)),
                    LocalExit(Box::new(GlobalRead(Box::new(GlobalNode::from(
                        interner.reverse_lookup("false").unwrap()
                    ))))),
                ],
            },
        }
    );
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

    assert_eq!(
        resolve,
        AstMethodDef {
            signature: "resolve:".to_string(),
            locals_nbr: 1,
            body: AstBody {
                exprs: vec![InlinedCall(Box::from(IfInlined(IfInlinedNode {
                    expected_bool: false,
                    cond_expr: BinaryDispatch(Box::new(AstBinaryDispatch {
                        dispatch_node: AstDispatchNode {
                            signature: interner.reverse_lookup("==").unwrap(),
                            receiver: LocalVarRead(0),
                            inline_cache: None
                        },
                        arg: GlobalRead(Box::new(GlobalNode::from(interner.reverse_lookup("nil").unwrap()))),
                    }),),
                    body_instrs: AstBody {
                        exprs: vec![LocalExit(Box::new(LocalVarRead(0)))]
                    },
                },)),),],
            },
        }
    );
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
                                    AstBlock(1 args, 0 locals):
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
    let ast = get_ast(to_do_str, &mut interner);

    assert_eq!(
        ast,
        AstMethodDef {
            signature: "run".to_string(),
            locals_nbr: 2,
            body: AstBody {
                exprs: vec![
                    LocalVarWrite(0, Box::new(Literal(AstLiteral::Integer(42)))),
                    InlinedCall(Box::new(InlinedNode::ToDoInlined(ToDoInlinedNode {
                        start: Literal(AstLiteral::Integer(1)),
                        end: Literal(AstLiteral::Integer(50)),
                        body: AstBody {
                            exprs: vec![UnaryDispatch(Box::new(AstUnaryDispatch {
                                dispatch_node: AstDispatchNode {
                                    signature: interner.reverse_lookup("println").unwrap(),
                                    receiver: BinaryDispatch(Box::new(AstBinaryDispatch {
                                        dispatch_node: AstDispatchNode {
                                            signature: interner.reverse_lookup("+").unwrap(),
                                            receiver: LocalVarRead(0),
                                            inline_cache: None,
                                        },
                                        arg: LocalVarRead(1)
                                    })),
                                    inline_cache: None,
                                },
                            }))]
                        },
                        accumulator_idx: 1,
                    })))
                ],
            },
        }
    );
}
