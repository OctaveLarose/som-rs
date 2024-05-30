use std::rc::Rc;
use som_core::ast::*;
use som_core::ast::MethodDef::{Generic, InlinedIf, InlinedIfTrueIfFalse, InlinedWhile};
use som_lexer::Token;
use som_parser_core::combinators::*;
use som_parser_core::Parser;
use crate::{AstGenCtxt, AstGenCtxtData, AstGenCtxtType};

macro_rules! opaque {
    ($expr:expr) => {{
        move |input: &'a [Token], genctxt: AstGenCtxt| $expr.parse(input, genctxt)
    }};
}

/// A parser that expects to be nothing left in its input.
pub fn eof<'a>() -> impl Parser<(), &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        if input.is_empty() {
            Some(((), input, genctxt))
        } else {
            None
        }
    }
}

pub fn exact<'a>(ch: Token) -> impl Parser<(), &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some(((), tail, genctxt))
        } else {
            None
        }
    }
}

pub fn exact_ident<'a, 'b: 'a>(string: &'b str) -> impl Parser<(), &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(ref ident) if ident.as_str() == string => Some(((), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn big_integer<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (sign, input, genctxt) = optional(exact(Token::Minus)).parse(input, genctxt)?;
        let sign = if sign.is_some() { "-" } else { "" };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitBigInteger(value) => Some((format!("{}{}", sign, value), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn integer<'a>() -> impl Parser<i64, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (sign, input, genctxt) = optional(exact(Token::Minus)).parse(input, genctxt)?;
        let sign = if sign.is_some() { -1 } else { 1 };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitInteger(value) => Some((*value * sign, tail, genctxt)),
            _ => None,
        }
    }
}

pub fn double<'a>() -> impl Parser<f64, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (sign, input, genctxt) = optional(exact(Token::Minus)).parse(input, genctxt)?;
        let sign = if sign.is_some() { -1.0 } else { 1.0 };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitDouble(value) => Some((*value * sign, tail, genctxt)),
            _ => None,
        }
    }
}

pub fn single_operator<'a>() -> impl Parser<&'static str, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Not => Some(("~", tail, genctxt)),
            Token::And => Some(("&", tail, genctxt)),
            Token::Or => Some(("|", tail, genctxt)),
            Token::Star => Some(("*", tail, genctxt)),
            Token::Div => Some(("/", tail, genctxt)),
            Token::Mod => Some(("\\", tail, genctxt)),
            Token::Plus => Some(("+", tail, genctxt)),
            Token::Equal => Some(("=", tail, genctxt)),
            Token::More => Some((">", tail, genctxt)),
            Token::Less => Some(("<", tail, genctxt)),
            Token::Comma => Some((",", tail, genctxt)),
            Token::At => Some(("@", tail, genctxt)),
            Token::Per => Some(("%", tail, genctxt)),
            Token::Minus => Some(("-", tail, genctxt)),
            _ => None,
        }
    }
}

pub fn operator_sequence<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::OperatorSequence(seq) => Some((seq.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    single_operator().map(String::from).or(operator_sequence())
}

pub fn identifier<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(value) => {
                Some((value.clone(), tail, genctxt))
            }
            _ => None,
        }
    }
}

pub fn string<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitString(value) => Some((value.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn symbol<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitSymbol(value) => Some((value.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn array<'a>() -> impl Parser<Vec<Literal>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        between(
            exact(Token::NewArray),
            many(literal()),
            exact(Token::EndTerm),
        )
            .parse(input, genctxt)
    }
}

pub fn literal<'a>() -> impl Parser<Literal, &'a [Token], AstGenCtxt> {
    (double().map(Literal::Double))
        .or(integer().map(Literal::Integer))
        .or(big_integer().map(Literal::BigInteger))
        .or(string().map(Literal::String))
        .or(symbol().map(Literal::Symbol))
        .or(array().map(Literal::Array))
}

pub fn keyword<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Keyword(value) => Some((value.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn unary_send<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    opaque!(primary())
        .and(many(identifier()))
        .map(|(receiver, signatures)| {
            signatures
                .into_iter()
                .fold(receiver, |receiver, signature| {
                    Expression::Message(Message {
                        receiver: Box::new(receiver),
                        signature,
                        values: Vec::new(),
                    })
                })
        })
}

pub fn binary_send<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    unary_send()
        .and(many(operator().and(unary_send().map(Box::new))))
        .map(|(lhs, operands)| {
            operands.into_iter().fold(lhs, |lhs, (op, rhs)| {
                Expression::BinaryOp(BinaryOp {
                    lhs: Box::new(lhs),
                    op,
                    rhs,
                })
            })
        })
}

pub fn positional_send<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    binary_send()
        .and(many(keyword().and(binary_send())))
        .map(|(receiver, pairs)| {
            if pairs.is_empty() {
                receiver
            } else {
                let (signature, values) = pairs.into_iter().unzip();

                Expression::Message(Message {
                    receiver: Box::new(receiver),
                    signature,
                    values,
                })
            }
        })
}

pub fn body<'a>() -> impl Parser<Body, &'a [Token], AstGenCtxt> {
    sep_by(exact(Token::Period), exit().or(statement()))
        .and(optional(exact(Token::Period)))
        .map(|(exprs, stopped)| Body {
            exprs,
            full_stopped: stopped.is_some(),
        })
}

pub fn locals<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (new_locals_names, input, genctxt) = between(exact(Token::Or), many(identifier()), exact(Token::Or)).parse(input, genctxt)?;
        genctxt.borrow_mut().add_locals(&new_locals_names);
        Some((new_locals_names, input, genctxt))
    }
}

pub fn class_instance_locals<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (new_locals_names, input, genctxt) = between(exact(Token::Or), many(identifier()), exact(Token::Or)).parse(input, genctxt)?;
        genctxt.borrow_mut().add_instance_fields(&new_locals_names);
        Some((new_locals_names, input, genctxt))
    }
}

pub fn class_static_locals<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (new_locals_names, input, genctxt) = between(exact(Token::Or), many(identifier()), exact(Token::Or)).parse(input, genctxt)?;
        genctxt.borrow_mut().add_static_fields(&new_locals_names);
        Some((new_locals_names, input, genctxt))
    }
}

pub fn parameter<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    exact(Token::Colon).and_right(identifier())
}

pub fn parameters<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let (param_names, input, genctxt) = some(parameter()).and_left(exact(Token::Or)).parse(input, genctxt)?;
        genctxt.borrow_mut().add_params(&param_names);
        Some((param_names, input, genctxt))
    }
}

pub fn block<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt| {
        let (_, input, genctxt) = exact(Token::NewBlock).parse(input, genctxt)?;

        let new_genctxt = AstGenCtxtData::new_ctxt_from(genctxt, AstGenCtxtType::Block);
        new_genctxt.borrow_mut().set_name("anonymous block".to_string());

        let (((parameters, locals), body), input, genctxt) = default(parameters())
            .and(default(locals()))
            .and(body())
            .parse(input, new_genctxt)?;
        // we unwrap here at the risk of panicking since if it fails we would want to adjust the scope - but atm we just panoc instead of recovering

        let (_, input, genctxt) = exact(Token::EndBlock).parse(input, genctxt)?;

        let new_genctxt = genctxt.borrow().get_outer();

        Some((Expression::Block(Block {
            nbr_params: parameters.len(),
            nbr_locals: locals.len(),
            #[cfg(feature = "block-debug-info")]
            dbg_info: Rc::clone(&new_genctxt).borrow().get_debug_info(),
            body,
        }), input, new_genctxt))
    }
}

pub fn term<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    between(
        exact(Token::NewTerm),
        assignment().or(expression()),
        exact(Token::EndTerm),
    )
}

pub fn exit<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let (expr, input, genctxt) = exact(Token::Exit).and_right(statement()).parse(input, Rc::clone(&genctxt))?;
        let cur_scope = genctxt.borrow().get_method_scope();
        Some((Expression::Exit(Box::new(expr), cur_scope), input, genctxt))
    }
}

pub fn expression<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    positional_send()
}

pub fn primary<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        match identifier().parse(input, Rc::clone(&genctxt)) {
            None => term()
                .or(block())
                .or(literal().map(Expression::Literal))
                .parse(input, genctxt),
            Some((name, input, genctxt)) => {
                Some((genctxt.borrow().get_var_read(&name), input, Rc::clone(&genctxt)))
            }
        }
    }
}


pub fn assignment<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        match identifier()
            .and_left(exact(Token::Assign))
            .and(opaque!(statement())).parse(input, genctxt) {
            Some(((name, expr), input, genctxt)) => {
                Some((genctxt.borrow().get_var_write(&name, Box::new(expr.clone())), input, Rc::clone(&genctxt)))
            }
            None => None
        }
    }
}

pub fn statement<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<MethodBody, &'a [Token], AstGenCtxt> {
    exact(Token::Primitive).map(|_| MethodBody::Primitive)
}

// making several methods here to avoid having to make the general case be a "move" function, since I think that's a slowdown.
#[cfg(feature = "block-debug-info")]
pub fn method_body<'a>() -> impl Parser<MethodBody, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let ((locals, body), input, genctxt) = between(
            exact(Token::NewTerm),
            default(locals()).and(body()),
            exact(Token::EndTerm),
        ).parse(input, genctxt)?;

        let method_body = MethodBody::Body { 
            locals_nbr: locals.len(),
            body,
            debug_info: genctxt.borrow().get_debug_info()
        };
        
        Some((method_body, input, genctxt))
    }
}

#[cfg(not(feature = "block-debug-info"))]
pub fn method_body<'a>() -> impl Parser<MethodBody, &'a [Token], AstGenCtxt> {
    between(
        exact(Token::NewTerm),
        default(locals()).and(body()),
        exact(Token::EndTerm),
    )
    .map(|(locals, body)| MethodBody::Body {
        locals_nbr: locals.len(),
        body
    })
}

pub fn unary_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    identifier()
        .and_left(exact(Token::Equal))
        .and(primitive().or(method_body()))
        .map(|(signature, body)| Generic(GenericMethodDef {
            kind: MethodKind::Unary,
            signature,
            body,
        }))
}

pub fn positional_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let (pairs, input, genctxt) = some(keyword().and(identifier())).and_left(exact(Token::Equal)).parse(input, genctxt)?;
        let (signature, parameters): (String, Vec<String>) = pairs.into_iter().unzip();

        genctxt.borrow_mut().set_name(signature.clone());
        genctxt.borrow_mut().add_params(&parameters);

        let (body, input, genctxt) = primitive().or(method_body()).parse(input, genctxt)?;

        let method_def = match signature.as_str() {
            "whileTrue:" => {
                InlinedWhile(GenericMethodDef {
                    kind: MethodKind::Positional { parameters },
                    signature,
                    body,
                }, true)
            },
            "whileFalse:" => {
                InlinedWhile(GenericMethodDef {
                    kind: MethodKind::Positional { parameters },
                    signature,
                    body,
                }, false)
            }
            "ifTrue:" => {
                InlinedIf(GenericMethodDef {
                    kind: MethodKind::Positional { parameters },
                    signature,
                    body,
                }, true)
            },
            "ifFalse:" => {
                InlinedIf(GenericMethodDef {
                    kind: MethodKind::Positional { parameters },
                    signature,
                    body,
                }, false)
            },
            "ifTrue:ifFalse:" => {
                InlinedIfTrueIfFalse(GenericMethodDef {
                    kind: MethodKind::Positional { parameters },
                    signature,
                    body,
                })
            },
            _ => Generic(GenericMethodDef {
                kind: MethodKind::Positional { parameters: parameters.clone() },
                signature,
                body,
            })
        };

        Some((method_def, input, genctxt))
    }
}

pub fn operator_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let ((op, rhs), input, genctxt) = operator().and(identifier()).and_left(exact(Token::Equal)).parse(input, genctxt)?;

        genctxt.borrow_mut().add_params(&vec![rhs.clone()]);

        primitive().or(method_body())
            .map(|body| Generic(GenericMethodDef {
                kind: MethodKind::Operator { rhs: rhs.clone() },
                signature: op.clone(),
                body,
            })).parse(input, genctxt)
    }
}

pub fn method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let genctxt = AstGenCtxtData::new_ctxt_from(genctxt, AstGenCtxtType::Method);

        match unary_method_def()
            .or(positional_method_def())
            .or(operator_method_def())
            .parse(input, Rc::clone(&genctxt)) {
            Some((method_def, input, genctxt)) => {
                let original_genctxt = genctxt.borrow().get_outer();
                Some((method_def, input, original_genctxt))
            },
            None => None,
        }
    }
}

pub fn class_def<'a>() -> impl Parser<ClassDef, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], genctxt: AstGenCtxt| {
        let (name, input, genctxt) = identifier().and_left(exact(Token::Equal)).parse(input, genctxt)?;

        genctxt.borrow_mut().set_name(name.clone());

        optional(identifier())
            .and(between(
                exact(Token::NewTerm),
                default(class_instance_locals()).and(many(method_def())).and(default(
                    exact(Token::Separator).and_right(default(class_static_locals()).and(many(method_def()))),
                )),
                exact(Token::EndTerm),
            ))
            .map(|(super_class, (instance_defns, static_defns))| {
                let (instance_locals, instance_methods) = instance_defns;
                let (static_locals, static_methods) = static_defns;

                ClassDef {
                    name: name.clone(),
                    super_class,
                    instance_locals,
                    instance_methods,
                    static_locals,
                    static_methods,
                }
            }).parse(input, genctxt)
    }
}

pub fn file<'a>() -> impl Parser<ClassDef, &'a [Token], AstGenCtxt> {
    class_def().and_left(eof())
}
