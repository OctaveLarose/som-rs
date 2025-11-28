use crate::{AstGenCtxt, AstGenCtxtData, AstGenCtxtType, AstMethodGenCtxtType};
use som_core::ast::*;
use som_lexer::Token;
use som_parser_core::combinators::*;
use som_parser_core::Parser;
use std::rc::Rc;
#[cfg(not(feature = "inlining-disabled"))]
use crate::inliner::PrimMessageInliner;

macro_rules! opaque {
    ($expr:expr) => {{
        move |input: &'a [Token], genctxt: AstGenCtxt<'a>| $expr.parse(input, genctxt)
    }};
}

/// A parser that expects to be nothing left in its input.
pub fn eof<'a>() -> impl Parser<(), &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        if input.is_empty() {
            Some(((), input, genctxt))
        } else {
            None
        }
    }
}

pub fn exact<'a>(ch: Token) -> impl Parser<(), &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let (head, tail) = input.split_first()?;
        if *head == ch {
            Some(((), tail, genctxt))
        } else {
            None
        }
    }
}

pub fn exact_ident<'a, 'b: 'a>(string: &'b str) -> impl Parser<(), &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(ref ident) if ident.as_str() == string => Some(((), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn big_integer<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
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

pub fn integer<'a>() -> impl Parser<i32, &'a [Token], AstGenCtxt<'a>> {
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

pub fn double<'a>() -> impl Parser<f64, &'a [Token], AstGenCtxt<'a>> {
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

pub fn single_operator<'a>() -> impl Parser<&'static str, &'a [Token], AstGenCtxt<'a>> {
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

pub fn operator_sequence<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::OperatorSequence(seq) => Some((seq.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn operator<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    single_operator().map(String::from).or(operator_sequence())
}

pub fn identifier<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(value) => Some((value.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn super_class<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Identifier(value) => {
                genctxt.borrow_mut().super_class_name = Some(value.clone());
                Some((value.clone(), tail, genctxt))
            }
            _ => None,
        }
    }
}

pub fn string<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitString(value) => Some((value.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn symbol<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::LitSymbol(value) => Some((value.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn array<'a>() -> impl Parser<Vec<Literal>, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| between(exact(Token::NewArray), many(literal()), exact(Token::EndTerm)).parse(input, genctxt)
}

pub fn literal<'a>() -> impl Parser<Literal, &'a [Token], AstGenCtxt<'a>> {
    (double().map(Literal::Double))
        .or(integer().map(Literal::Integer))
        .or(big_integer().map(Literal::BigInteger))
        .or(string().map(Literal::String))
        .or(symbol().map(Literal::Symbol))
        .or(array().map(Literal::Array))
}

pub fn keyword<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        let (head, tail) = input.split_first()?;
        match head {
            Token::Keyword(value) => Some((value.clone(), tail, genctxt)),
            _ => None,
        }
    }
}

pub fn unary_send<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let ((receiver, signatures), input, genctxt) = primary().and(many(identifier())).parse(input, genctxt)?;

        let a = signatures.into_iter().fold(receiver, |receiver, signature| {
            Expression::Message(Box::new(Message::Regular(RegularMessage {
                receiver,
                signature,
                values: Vec::new(),
            })))
        });
        Some((a, input, genctxt))
    }
}

pub fn binary_send<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    unary_send().and(many(operator().and(unary_send()))).map(|(lhs, operands)| {
        operands.into_iter().fold(lhs, |lhs, (op, rhs)| {
            let msg = RegularMessage {
                receiver: lhs,
                signature: op,
                values: vec![rhs],
            };
            Expression::Message(Box::new(Message::Regular(msg)))
        })
    })
}

pub fn positional_send<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        #[allow(unused_mut)] // if inlining is disabled, there's no need for the genctxt to be mutable.
        let ((receiver, pairs), input, mut genctxt) = binary_send().and(many(keyword().and(binary_send()))).parse(input, genctxt)?;

        if pairs.is_empty() {
            Some((receiver, input, genctxt))
        } else {
            let (signature, values) = pairs.into_iter().unzip();
            let msg = RegularMessage { receiver, signature, values };

            #[cfg(not(feature = "inlining-disabled"))]
            let msg = {
                match genctxt.inline_if_possible(&msg) {
                    Some(inlined_msg) => inlined_msg,
                    None => Message::Regular(msg),
                }
            };

            #[cfg(feature = "inlining-disabled")]
            let msg = Message::Regular(msg);

            Some((Expression::Message(Box::new(msg)), input, genctxt))
        }
    }
}

pub fn body<'a>() -> impl Parser<Body, &'a [Token], AstGenCtxt<'a>> {
    sep_by(exact(Token::Period), exit().or(statement())).and(optional(exact(Token::Period))).map(|(exprs, stopped)| Body {
        exprs,
        full_stopped: stopped.is_some(),
    })
}

pub fn locals<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        let (new_locals_names, input, genctxt) = between(exact(Token::Or), many(identifier()), exact(Token::Or)).parse(input, genctxt)?;
        genctxt.borrow_mut().add_locals(&new_locals_names);
        Some((new_locals_names, input, genctxt))
    }
}

pub fn class_locals<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt<'a>> {
    between(exact(Token::Or), many(identifier()), exact(Token::Or))
}

pub fn parameter<'a>() -> impl Parser<String, &'a [Token], AstGenCtxt<'a>> {
    exact(Token::Colon).and_right(identifier())
}

pub fn parameters<'a>() -> impl Parser<Vec<String>, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let (param_names, input, genctxt) = some(parameter()).and_left(exact(Token::Or)).parse(input, genctxt)?;
        genctxt.borrow_mut().add_params(&param_names);
        Some((param_names, input, genctxt))
    }
}

pub fn block<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt| {
        let (_, input, genctxt) = exact(Token::NewBlock).parse(input, genctxt)?;

        let new_genctxt = AstGenCtxtData::new_ctxt_from(genctxt, AstGenCtxtType::Block);

        let (((_parameters, _locals), body), input, genctxt) = default(parameters()).and(default(locals())).and(body()).parse(input, new_genctxt)?;
        // we unwrap here at the risk of panicking since if it fails we would want to adjust the scope - but atm we just panic instead of recovering

        let (_, input, genctxt) = exact(Token::EndBlock).parse(input, genctxt)?;

        let parameters = genctxt.borrow().param_names.clone();
        let locals = genctxt.borrow().local_names.clone();

        let new_genctxt = genctxt.borrow_mut().get_outer();

        Some((
            Expression::Block(Block {
                nbr_params: parameters.len(),
                nbr_locals: locals.len(),
                parameters,
                locals,
                body,
            }),
            input,
            new_genctxt,
        ))
    }
}

pub fn term<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    between(exact(Token::NewTerm), assignment().or(expression()), exact(Token::EndTerm))
}

pub fn exit<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let (expr, input, genctxt) = exact(Token::Exit).and_right(statement()).parse(input, Rc::clone(&genctxt))?;
        Some((Expression::Exit(Box::new(expr)), input, genctxt))
    }
}

pub fn expression<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    positional_send()
}

pub fn primary<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    (identifier().map(Expression::Read)).or(term()).or(block()).or(literal().map(Expression::Literal))
}

pub fn assignment<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    identifier()
        .and_left(exact(Token::Assign))
        .and(opaque!(statement()))
        .map(|(name, expr)| Expression::Write(name, Box::new(expr)))
}

pub fn statement<'a>() -> impl Parser<Expression, &'a [Token], AstGenCtxt<'a>> {
    assignment().or(expression())
}

pub fn primitive<'a>() -> impl Parser<MethodBody, &'a [Token], AstGenCtxt<'a>> {
    exact(Token::Primitive).map(|_| MethodBody::Primitive)
}

pub fn method_body<'a>() -> impl Parser<MethodBody, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let ((_locals, body), input, genctxt) =
            between(exact(Token::NewTerm), default(locals()).and(body()), exact(Token::EndTerm)).parse(input, genctxt)?;

        let method_body = MethodBody::Body {
            locals_nbr: genctxt.borrow().local_names.len(),
            locals: genctxt.borrow().local_names.clone(),
            body,
        };

        Some((method_body, input, genctxt))
    }
}

pub fn unary_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt<'a>> {
    identifier().and_left(exact(Token::Equal)).and(primitive().or(method_body())).map(|(signature, body)| MethodDef {
        args: vec![],
        signature,
        body,
    })
}

pub fn positional_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let (pairs, input, genctxt) = some(keyword().and(identifier())).and_left(exact(Token::Equal)).parse(input, genctxt)?;
        let (signature, parameters): (String, Vec<String>) = pairs.into_iter().unzip();

        genctxt.borrow_mut().name.clone_from(&signature);
        genctxt.borrow_mut().add_params(&parameters);

        let (body, input, genctxt) = primitive().or(method_body()).parse(input, genctxt)?;

        let method_def = MethodDef {
            args: parameters,
            signature,
            body,
        };

        Some((method_def, input, genctxt))
    }
}

pub fn operator_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let ((op, rhs), input, genctxt) = operator().and(identifier()).and_left(exact(Token::Equal)).parse(input, genctxt)?;

        genctxt.borrow_mut().add_params(&[rhs.clone()]);

        primitive()
            .or(method_body())
            .map(|body| MethodDef {
                args: vec![rhs.clone()],
                signature: op.clone(),
                body,
            })
            .parse(input, genctxt)
    }
}

pub fn instance_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let genctxt = AstGenCtxtData::new_ctxt_from(genctxt, AstGenCtxtType::Method(AstMethodGenCtxtType::INSTANCE));

        match unary_method_def().or(positional_method_def()).or(operator_method_def()).parse(input, Rc::clone(&genctxt)) {
            Some((method_def, input, genctxt)) => {
                let original_genctxt = genctxt.borrow_mut().get_outer();
                Some((method_def, input, original_genctxt))
            }
            None => None,
        }
    }
}

pub fn class_method_def<'a>() -> impl Parser<MethodDef, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let genctxt = AstGenCtxtData::new_ctxt_from(genctxt, AstGenCtxtType::Method(AstMethodGenCtxtType::CLASS));

        match unary_method_def().or(positional_method_def()).or(operator_method_def()).parse(input, Rc::clone(&genctxt)) {
            Some((method_def, input, genctxt)) => {
                let original_genctxt = genctxt.borrow_mut().get_outer();
                Some((method_def, input, original_genctxt))
            }
            None => None,
        }
    }
}

pub fn class_def<'a>() -> impl Parser<ClassDef, &'a [Token], AstGenCtxt<'a>> {
    move |input: &'a [Token], genctxt: AstGenCtxt<'a>| {
        let (name, input, genctxt) = identifier().and_left(exact(Token::Equal)).parse(input, genctxt)?;

        genctxt.borrow_mut().name = name.clone();

        optional(super_class())
            .and(between(
                exact(Token::NewTerm),
                default(class_locals()).and(many(instance_method_def())).and(default(
                    exact(Token::Separator).and_right(default(class_locals()).and(many(class_method_def()))),
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
            })
            .parse(input, genctxt)
    }
}

pub fn file<'a>() -> impl Parser<ClassDef, &'a [Token], AstGenCtxt<'a>> {
    class_def().and_left(eof())
}
