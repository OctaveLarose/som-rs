use som_core::ast::*;
use som_lexer::{Lexer, Token};
use som_parser_core::combinators::*;
use som_parser_core::Parser;
use som_parser_symbols::lang::*;
use som_parser_symbols::AstGenCtxtData;
use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

#[test]
fn literal_tests() {
    let tokens: Vec<Token> = Lexer::new("1.2 5 #foo 'test'").skip_whitespace(true).collect();

    let result = many(literal()).parse(tokens.as_slice(), Rc::new(RefCell::new(AstGenCtxtData::init())));

    assert!(result.is_some(), "input did not parse successfully");
    let (literals, rest, _) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    let mut iter = literals.into_iter();
    assert_eq!(iter.next(), Some(Literal::Double(1.2)));
    assert_eq!(iter.next(), Some(Literal::Integer(5)));
    assert_eq!(iter.next(), Some(Literal::Symbol(String::from("foo"))));
    assert_eq!(iter.next(), Some(Literal::String(String::from("test"))));
    assert_eq!(iter.next(), None);
}

#[test]
fn expression_test_1() {
    let tokens: Vec<Token> = Lexer::new("3 + counter get").skip_whitespace(true).collect();

    let result = expression().parse(tokens.as_slice(), Rc::new(RefCell::new(AstGenCtxtData::init())));

    assert!(result.is_some(), "input did not parse successfully");
    let (expression, rest, _) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        expression,
        Expression::Message(Box::new(Message::Regular(RegularMessage {
            signature: String::from("+"),
            receiver: Expression::Literal(Literal::Integer(3)),
            values: vec![Expression::Message(Box::new(Message::Regular(RegularMessage {
                receiver: Expression::Read(String::from("counter")),
                signature: String::from("get"),
                values: vec![],
            })))],
        })))
    );
}

#[test]
fn block_test() {
    let tokens: Vec<Token> = Lexer::new("[ :test | |local| local := 'this is correct'. local println. ]").skip_whitespace(true).collect();

    let result = block().parse(tokens.as_slice(), Rc::new(RefCell::new(AstGenCtxtData::init())));

    assert!(result.is_some(), "input did not parse successfully");
    let (block, rest, _) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        block,
        Expression::Block(Block {
            nbr_params: 1,
            nbr_locals: 1,
            parameters: vec!["test".to_string()],
            locals: vec!["local".to_string()],
            body: Body {
                exprs: vec![
                    Expression::Write(
                        "local".to_string(),
                        Box::new(Expression::Literal(Literal::String(String::from("this is correct"))))
                    ),
                    Expression::Message(Box::new(Message::Regular(RegularMessage {
                        receiver: Expression::Read("local".to_string()),
                        signature: String::from("println"),
                        values: vec![],
                    })))
                ],
                full_stopped: true,
            }
        }),
    );
}

#[cfg(not(feature = "inlining-disabled"))]
#[test]
fn expression_test_inlining() {
    let tokens: Vec<Token> = Lexer::new("( 3 == 3 ) ifTrue: [ 'this is correct' println. ] ifFalse: [ 'oh no' println ]")
        .skip_whitespace(true)
        .collect();

    let result = expression().parse(tokens.as_slice(), Rc::new(RefCell::new(AstGenCtxtData::init())));

    assert!(result.is_some(), "input did not parse successfully");
    let (expression, rest, _) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        expression,
        Expression::Message(Box::new(Message::IfTrueIfFalseInlined(IfTrueIfFalseInlinedMsg {
            expected_bool: true,
            cond_expr: Expression::Message(Box::new(Message::Regular(RegularMessage {
                signature: String::from("=="),
                receiver: Expression::Literal(Literal::Integer(3)),
                values: vec![Expression::Literal(Literal::Integer(3))],
            }))),
            body_1_instrs: vec![Expression::Message(Box::new(Message::Regular(RegularMessage {
                receiver: Expression::Literal(Literal::String(String::from("this is correct"))),
                signature: String::from("println"),
                values: vec![],
            })))],
            body_2_instrs: vec![Expression::Message(Box::new(Message::Regular(RegularMessage {
                receiver: Expression::Literal(Literal::String(String::from("oh no"))),
                signature: String::from("println"),
                values: vec![],
            })))],
        })))
    );
}

#[test]
fn primary_test() {
    let tokens: Vec<Token> = Lexer::new("[ self fib: (n - 1) + (self fib: (n - 2)) ]").skip_whitespace(true).collect();

    let result = primary().parse(tokens.as_slice(), Rc::new(RefCell::new(AstGenCtxtData::init())));

    assert!(result.is_some(), "input did not parse successfully");
    let (primary, rest, _) = result.unwrap();
    assert!(rest.is_empty(), "input did not parse in its entirety");

    assert_eq!(
        primary,
        Expression::Block(Block {
            nbr_params: 0,
            nbr_locals: 0,
            locals: vec![],
            parameters: vec![],
            body: Body {
                exprs: vec![Expression::Message(Box::new(Message::Regular(RegularMessage {
                    receiver: Expression::Read("self".to_string()),
                    signature: String::from("fib:"),
                    values: vec![Expression::Message(Box::new(Message::Regular(RegularMessage {
                        signature: String::from("+"),
                        receiver: Expression::Message(Box::new(Message::Regular(RegularMessage {
                            signature: String::from("-"),
                            receiver: Expression::Read(String::from("n")),
                            values: vec![Expression::Literal(Literal::Integer(1))],
                        }))),
                        values: vec![Expression::Message(Box::new(Message::Regular(RegularMessage {
                            receiver: Expression::Read("self".to_string()),
                            signature: String::from("fib:"),
                            values: vec![Expression::Message(Box::new(Message::Regular(RegularMessage {
                                signature: String::from("-"),
                                receiver: Expression::Read(String::from("n")),
                                values: vec![Expression::Literal(Literal::Integer(2))],
                            })))],
                        })))]
                    })))],
                })))],
                full_stopped: false,
            }
        }),
    );
}
