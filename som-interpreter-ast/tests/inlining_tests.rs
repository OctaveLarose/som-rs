use som_interpreter_ast::ast::{AstBinaryOp, AstBody, AstMethodBody};
use som_interpreter_ast::ast::AstExpression::*;
use som_interpreter_ast::ast::InlinedNode::IfInlined;
use som_interpreter_ast::compiler::AstMethodCompilerCtxt;
use som_interpreter_ast::specialized::if_inlined_node::IfInlinedNode;
use som_lexer::{Lexer, Token};
use som_parser::lang;

fn get_ast(class_txt: &str) -> AstMethodBody {
    let mut lexer = Lexer::new(class_txt)
        .skip_comments(true)
        .skip_whitespace(true);
    let tokens: Vec<Token> = lexer.by_ref().collect();
    assert!(lexer.text().is_empty(), "could not fully tokenize test expression");

    let method_def = som_parser::apply(lang::instance_method_def(), tokens.as_slice(), None).unwrap();

    AstMethodCompilerCtxt::parse_method_def(&method_def).body
}

#[test]
fn if_true_inlining_ok() {
    let very_basic = "run = (
        true ifTrue: [ ^true ].
        ^ false
    )";

    let ast = get_ast(very_basic);

    assert_eq!(ast, AstMethodBody::Body {
        locals_nbr: 0,
        body: AstBody {
            exprs: vec![
                InlinedCall(
                    Box::new(IfInlined(
                        IfInlinedNode {
                            expected_bool: true,
                            cond_instrs: AstBody {
                                exprs: vec![GlobalRead("true".to_string())],
                            },
                            body_instrs: AstBody {
                                exprs: vec![Exit(Box::new(GlobalRead("true".to_string())), 0)],
                            },
                        },
                    ),
                    )),
                Exit(Box::new(GlobalRead("false".to_string())), 0),
            ],
        },
    }
    );
}

#[test]
fn if_false_inlining_ok() {
    // based on the method of the same name defined in System
    let method_txt2 = "resolve: a = (
        | class |
        (class == nil) ifFalse: [
            ^class ].
    )";

    let resolve = get_ast(method_txt2);

    dbg!(&resolve);
    assert_eq!(resolve,
               AstMethodBody::Body {
                   locals_nbr: 1,
                   body: AstBody {
        exprs: vec![
            InlinedCall(
                Box::from(IfInlined(
                    IfInlinedNode {
                        expected_bool: false,
                        cond_instrs: AstBody {
                            exprs: vec![
                                BinaryOp(
                                    Box::new(AstBinaryOp {
                                        op: "==".to_string(),
                                        lhs: LocalVarRead(0),
                                        rhs: GlobalRead("nil".to_string()),
                                    }),
                                ),
                            ],
                        },
                        body_instrs: AstBody { exprs: vec![Exit(Box::new(LocalVarRead(0)), 0)] },
                    },
                )),
            ),
        ],
    }});
}