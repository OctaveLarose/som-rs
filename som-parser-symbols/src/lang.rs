use som_core::ast::*;
use som_lexer::Token;
use som_parser_core::combinators::*;
use som_parser_core::{Parser};
use crate::{AstGenCtxt, AstGenCtxtType};

macro_rules! opaque {
    ($expr:expr) => {{
        move |input: &'a [Token], mgctxt| $expr.parse(input, mgctxt)
    }};
}

/// A parser that expects to be nothing left in its input.
pub fn eof<'a>() -> impl Parser<(), &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt: AstGenCtxt| {
        if input.is_empty() {
            Some(((), input, mgctxt))
        } else {
            None
        }
    }
}

pub fn exact<'a>(ch: Token) -> impl Parser<(), &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt: AstGenCtxt| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some(((), tail, mgctxt))
        } else {
            None
        }
    }
}

pub fn exact_ident<'a, 'b: 'a>(string: &'b str) -> impl Parser<(), &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(ref ident) if ident.as_str() == string => Some(((), tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn big_integer<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (sign, input, mgctxt) = optional(exact(Token::Minus)).parse(input, mgctxt)?;
        let sign = if sign.is_some() { "-" } else { "" };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitBigInteger(value) => Some((format!("{}{}", sign, value), tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn integer<'a>() -> impl Parser<i64, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (sign, input, mgctxt) = optional(exact(Token::Minus)).parse(input, mgctxt)?;
        let sign = if sign.is_some() { -1 } else { 1 };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitInteger(value) => Some((*value * sign, tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn double<'a>() -> impl Parser<f64, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (sign, input, mgctxt) = optional(exact(Token::Minus)).parse(input, mgctxt)?;
        let sign = if sign.is_some() { -1.0 } else { 1.0 };

        let (head, tail) = input.split_first()?;
        match head {
            Token::LitDouble(value) => Some((*value * sign, tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn single_operator<'a>() -> impl Parser<&'static str, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Not => Some(("~", tail, mgctxt)),
            Token::And => Some(("&", tail, mgctxt)),
            Token::Or => Some(("|", tail, mgctxt)),
            Token::Star => Some(("*", tail, mgctxt)),
            Token::Div => Some(("/", tail, mgctxt)),
            Token::Mod => Some(("\\", tail, mgctxt)),
            Token::Plus => Some(("+", tail, mgctxt)),
            Token::Equal => Some(("=", tail, mgctxt)),
            Token::More => Some((">", tail, mgctxt)),
            Token::Less => Some(("<", tail, mgctxt)),
            Token::Comma => Some((",", tail, mgctxt)),
            Token::At => Some(("@", tail, mgctxt)),
            Token::Per => Some(("%", tail, mgctxt)),
            Token::Minus => Some(("-", tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn operator_sequence<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::OperatorSequence(seq) => Some((seq.clone(), tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    single_operator().map(String::from).or(operator_sequence())
}

pub fn identifier<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt: AstGenCtxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(value) => {
                Some((value.clone(), tail, mgctxt))
            }
            _ => None,
        }
    }
}

pub fn string<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitString(value) => Some((value.clone(), tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn symbol<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitSymbol(value) => Some((value.clone(), tail, mgctxt)),
            _ => None,
        }
    }
}

pub fn array<'a>() -> impl Parser<Vec<Literal>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        between(
            exact(Token::NewArray),
            many(literal()),
            exact(Token::EndTerm),
        )
            .parse(input, mgctxt)
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
    move |input: &'a [Token], mgctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Keyword(value) => Some((value.clone(), tail, mgctxt)),
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
    move |input: &'a [Token], mgctxt| {
        let (new_locals_names, input, mut mgctxt) = between(exact(Token::Or), many(identifier()), exact(Token::Or)).parse(input, mgctxt)?;
        mgctxt = mgctxt.add_locals(&new_locals_names);
        Some((new_locals_names, input, mgctxt))
    }
}

pub fn class_locals<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt| {
        let (new_locals_names, input, mut mgctxt) = between(exact(Token::Or), many(identifier()), exact(Token::Or)).parse(input, mgctxt)?;
        mgctxt = mgctxt.add_fields(&new_locals_names);
        Some((new_locals_names, input, mgctxt))
    }
}

pub fn parameter<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt> {
    exact(Token::Colon).and_right(identifier())
}

pub fn parameters<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt: AstGenCtxt| {
        let (param_names, input, mut mgctxt) = some(parameter()).and_left(exact(Token::Or)).parse(input, mgctxt)?;
        mgctxt = mgctxt.add_params(&param_names);
        Some((param_names, input, mgctxt))
    }
}

pub fn block<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    // between(
    //     exact(Token::NewBlock),
    //     default(parameters()).and(default(locals())).and(body()),
    //     exact(Token::EndBlock),
    // )
    //     .map(|((parameters, locals), body)| {
    //         Expression::Block(Block {
    //             parameters,
    //             locals,
    //             body,
    //         })
    //     })


    move |input: &'a [Token], mgctxt| {
        let (_, input, mut mgctxt) = exact(Token::NewBlock).parse(input, mgctxt)?;
        mgctxt = mgctxt.new_ctxt_from_itself(AstGenCtxtType::Block);
        let (((parameters, locals), body), input, mgctxt) = default(parameters())
            .and(default(locals()))
            .and(body())
            .parse(input, mgctxt).unwrap(); // we unwrap here at the risk of panicking since if it panics, well, we would want to adjust the scope. but atm we just crash
        let (_, input, mut mgctxt) = exact(Token::EndBlock).parse(input, mgctxt)?;
        mgctxt = mgctxt.get_outer();

        Some((Expression::Block(Block {
            parameters,
            locals,
            body,
        }), input, mgctxt))
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
    exact(Token::Exit)
        .and_right(statement())
        .map(|expr| Expression::Exit(Box::new(expr)))
}

pub fn expression<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    positional_send()
}

pub fn primary<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt: AstGenCtxt| {
        let name_opt = identifier().parse(input, mgctxt.clone());

        if name_opt.is_none() {
            return term()
                .or(block())
                .or(literal().map(Expression::Literal)).parse(input, mgctxt);
        }

        let (name, input, mgctxt) = name_opt.unwrap();

        return mgctxt.get_var(&name).and_then(|(_, scope)|
            {
                match scope {
                    0 => Some((Expression::LocalVarRead(name.clone()), input, mgctxt.clone())),
                    _ => Some((Expression::NonLocalVarRead(name.clone(), scope), input, mgctxt.clone()))
                }
            })
            .or(mgctxt.get_param(&name).and_then(|_| Some((Expression::ArgRead(name.clone()), input, mgctxt.clone()))))
            .or(mgctxt.class_fields.iter().find(|v| **v == name).and_then(|_| Some((Expression::FieldRead(name.clone()), input, mgctxt.clone()))))
            .or(Some((Expression::GlobalRead(name.clone()), input, mgctxt.clone())));
    }
}

pub fn assignment<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    identifier()
        .and_left(exact(Token::Assign))
        .and(opaque!(statement()))
        .map(|(name, expr)| Expression::Assignment(name, Box::new(expr)))
}

pub fn statement<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<MethodBody, &'a [Token], AstGenCtxt> {
    exact(Token::Primitive).map(|_| MethodBody::Primitive)
}

pub fn method_body<'a>() -> impl Parser<MethodBody, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt: AstGenCtxt| {
        let (_, input, mut mgctxt) = exact(Token::NewTerm).parse(input, mgctxt)?;
        let (locals, input, _) = default(locals()).parse(input, mgctxt.clone())?;

        mgctxt = mgctxt.new_ctxt_from_itself(AstGenCtxtType::Method);
        let (body, input, mgctxt) = body().parse(input, mgctxt)?;
        let (_, input, mut mgctxt) = exact(Token::EndTerm).parse(input, mgctxt)?;
        mgctxt = mgctxt.get_outer();

        Some((MethodBody::Body { locals, body }, input, mgctxt))
    }

    // between(
    //     exact(Token::NewTerm),
    //     default(locals()).and(body()),
    //     exact(Token::EndTerm),
    // )
    // .map(|(locals, body)| MethodBody::Body { locals, body })
}

pub fn unary_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    identifier()
        .and_left(exact(Token::Equal))
        .and(primitive().or(method_body()))
        .map(|(signature, body)| MethodDef {
            kind: MethodKind::Unary,
            signature,
            body,
        })
}

pub fn positional_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    move |input: &'a [Token], mgctxt: AstGenCtxt| {
        let (method_def, input, mut mgctxt) = some(keyword().and(identifier()))
            .and_left(exact(Token::Equal))
            .and(primitive().or(method_body()))
            .map(|(pairs, body)| {
                let (signature, parameters) = pairs.into_iter().unzip();

                MethodDef {
                    kind: MethodKind::Positional { parameters },
                    signature,
                    body,
                }
            }).parse(input, mgctxt)?;

        mgctxt = match &method_def.kind {
            MethodKind::Positional { parameters } => {
                mgctxt.add_params(&parameters)
            },
            _ => unreachable!()
        };

        // dbg!(&mgctxt.params);

        Some((method_def, input, mgctxt.clone()))
    }
}

pub fn operator_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    operator()
        .and(identifier())
        .and_left(exact(Token::Equal))
        .and(primitive().or(method_body()))
        .map(|((signature, rhs), body)| MethodDef {
            kind: MethodKind::Operator { rhs },
            signature,
            body,
        })
}

pub fn method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt> {
    unary_method_def()
        .or(positional_method_def())
        .or(operator_method_def())
}

pub fn class_def<'a>() -> impl Parser<ClassDef, &'a [Token], AstGenCtxt> {
    identifier()
        .and_left(exact(Token::Equal))
        .and(optional(identifier()))
        .and(between(
            exact(Token::NewTerm),
            default(class_locals()).and(many(method_def())).and(default(
                exact(Token::Separator).and_right(default(class_locals()).and(many(method_def()))),
            )),
            exact(Token::EndTerm),
        ))
        .map(|((name, super_class), (instance_defns, static_defns))| {
            let (instance_locals, instance_methods) = instance_defns;
            let (static_locals, static_methods) = static_defns;

            ClassDef {
                name,
                super_class,
                instance_locals,
                instance_methods,
                static_locals,
                static_methods,
            }
        })
}

pub fn file<'a>() -> impl Parser<ClassDef, &'a [Token], AstGenCtxt> {
    class_def().and_left(eof())
}
