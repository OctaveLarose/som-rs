use crate::{Parser};

/// Represents a value of either type A (Left) or type B (Right).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

/// Transforms a parser into a non-consuming one, allowing to parse ahead without consuming anything.
pub fn peek<A, I: Clone, GCTXT>(mut parser: impl Parser<A, I, GCTXT>) -> impl Parser<A, I, GCTXT> {
    move |input: I, genctxt: GCTXT| {
        let (value, _, genctxt) = parser.parse(input.clone(), genctxt)?;
        Some((value, input, genctxt))
    }
}

/// Runs the given parser, fails if it succeeded, and succeeds otherwise.
pub fn not<A, I: Clone, GCTXT: Clone>(mut parser: impl Parser<A, I, GCTXT>) -> impl Parser<(), I, GCTXT> {
    move |input: I, genctxt: GCTXT| match parser.parse(input.clone(), genctxt.clone()) {
        Some(_) => None,
        None => Some(((), input, genctxt)),
    }
}

/// Sequences two parsers, one after the other, collecting both results.
pub fn sequence<A, B, I, GCTXT>(
    mut fst: impl Parser<A, I, GCTXT>,
    mut snd: impl Parser<B, I, GCTXT>,
) -> impl Parser<(A, B), I, GCTXT> {
    // equivalent to: `fst.and(snd)`
    move |input: I, genctxt: GCTXT| {
        let (a, input, genctxt) = fst.parse(input, genctxt)?;
        let (b, input, genctxt) = snd.parse(input, genctxt)?;
        Some(((a, b), input, genctxt))
    }
}

/// Tries to apply the first parser, if it fails, it tries to apply the second parser.
pub fn alternative<A, I: Clone, GCTXT: Clone>(
    mut fst: impl Parser<A, I, GCTXT>,
    mut snd: impl Parser<A, I, GCTXT>,
) -> impl Parser<A, I, GCTXT> {
    move |input: I, genctxt: GCTXT| fst.parse(input.clone(), genctxt.clone()).or_else(|| snd.parse(input, genctxt))
}

/// Same as `either`, but allows for different output types for the parsers.
pub fn either<A, B, I: Clone, GCTXT: Clone>(
    mut fst: impl Parser<A, I, GCTXT>,
    mut snd: impl Parser<B, I, GCTXT>,
) -> impl Parser<Either<A, B>, I, GCTXT> {
    move |input: I, genctxt: GCTXT| {
        if let Some((a, input, genctxt)) = fst.parse(input.clone(), genctxt.clone()) {
            Some((Either::Left(a), input, genctxt))
        } else if let Some((b, input, genctxt)) = snd.parse(input, genctxt) {
            Some((Either::Right(b), input, genctxt))
        } else {
            None
        }
    }
}

/// Tries to apply a parser, or fallback to a constant value (making it an always-succeeding parser).
pub fn fallback<A: Clone, I: Clone, GCTXT: Clone>(def: A, mut parser: impl Parser<A, I, GCTXT>) -> impl Parser<A, I, GCTXT> {
    move |input: I, genctxt: GCTXT| {
        parser
            .parse(input.clone(), genctxt.clone())
            .or_else(|| Some((def.clone(), input, genctxt)))
    }
}

/// Tries to apply a parser, or fallback to its default value (making it an always-succeeding parser).
pub fn default<A: Default, I: Clone, GCTXT: Clone>(parser: impl Parser<A, I, GCTXT>) -> impl Parser<A, I, GCTXT> {
    optional(parser).map(Option::unwrap_or_default)
}

/// Tries every parser in a slice, from left to right, and returns the output of the first succeeding one.
pub fn any<'a, A, I: Clone, GCTXT: Clone>(parsers: &'a mut [impl Parser<A, I, GCTXT>]) -> impl Parser<A, I, GCTXT> + 'a {
    move |input: I, genctxt: GCTXT| {
        parsers
            .iter_mut()
            .find_map(|parser| parser.parse(input.clone(), genctxt.clone()))
    }
}

/// Applies every parser in a slice, from left to right, and returns the output from all of them.
/// If one parser fails, the whole sequence is considered failed.
pub fn all<'a, A, I, GCTXT>(parsers: &'a mut [impl Parser<A, I, GCTXT>]) -> impl Parser<Vec<A>, I, GCTXT> + 'a {
    move |input: I, genctxt: GCTXT| {
        let output = Vec::<A>::with_capacity(parsers.len());
        parsers
            .iter_mut()
            .try_fold((output, input, genctxt), |(mut output, input, genctxt), parser| {
                let (value, input, genctxt) = parser.parse(input, genctxt)?;
                output.push(value);
                Some((output, input, genctxt))
            })
    }
}

/// Tries to apply a parser, but fails gracefully (with an `Option` output).
pub fn optional<A, I: Clone, GCTXT: Clone>(mut parser: impl Parser<A, I, GCTXT>) -> impl Parser<Option<A>, I, GCTXT> {
    move |input: I, genctxt: GCTXT| {
        if let Some((value, input, genctxt)) = parser.parse(input.clone(), genctxt.clone()) {
            Some((Some(value), input, genctxt))
        } else {
            Some((None, input, genctxt.clone()))
        }
    }
}

/// Applies a parser zero or more times.
pub fn many<A, I: Clone, GCTXT: Clone>(mut parser: impl Parser<A, I, GCTXT>) -> impl Parser<Vec<A>, I, GCTXT> {
    move |mut input: I, genctxt: GCTXT| {
        let mut output = Vec::<A>::new();
        let mut genctxt2 = genctxt;
        loop {
            let Some((value, next, modified_ctxt)) = parser.parse(input.clone(), genctxt2.clone()) else { break };
            genctxt2 = modified_ctxt;
            input = next;
            output.push(value);
        }
        Some((output, input, genctxt2))
    }
}

/// Applies a parser one or more times.
pub fn some<A, I: Clone, GCTXT: Clone>(mut parser: impl Parser<A, I, GCTXT>) -> impl Parser<Vec<A>, I, GCTXT> {
    move |input: I, genctxt| {
        let (value, mut input, genctxt) = parser.parse(input, genctxt)?;
        let mut output = vec![value];
        let mut genctxt2 = genctxt;

        loop {
            let Some((value, next, new_ctxt)) = parser.parse(input.clone(), genctxt2.clone()) else { break };
            genctxt2 = new_ctxt;
            input = next;
            output.push(value);
        }

        Some((output, input, genctxt2))
    }
}

/// Parses something that is enclosed between two other things.
pub fn between<A, B, C, I, GCTXT>(
    mut before: impl Parser<A, I, GCTXT>,
    mut within: impl Parser<B, I, GCTXT>,
    mut after: impl Parser<C, I, GCTXT>,
) -> impl Parser<B, I, GCTXT> {
    move |input: I, genctxt| {
        let (_, input, genctxt) = before.parse(input, genctxt)?;
        let (value, input, genctxt) = within.parse(input, genctxt)?;
        let (_, input, genctxt) = after.parse(input, genctxt)?;
        Some((value, input, genctxt))
    }
}

/// Parses zero or more things, separated by an arbitrary delimiter.
pub fn sep_by<A, B, I: Clone, GCTXT: Clone>(
    mut delim: impl Parser<A, I, GCTXT>,
    mut within: impl Parser<B, I, GCTXT>,
) -> impl Parser<Vec<B>, I, GCTXT> {
    move |input: I, genctxt: GCTXT| {
        let mut output = Vec::<B>::new();
        let l1 = within.parse(input.clone(), genctxt.clone());
        if l1.is_some() {
            let (value, mut input, mut genctxt2) = l1.unwrap();
            output.push(value);

            loop {
                let l2 = delim
                    .parse(input.clone(), genctxt2.clone())
                    .and_then(|(_, input, genctxt3)| within.parse(input, genctxt3));

                if l2.is_none() {
                    break;
                } else {
                    let (value, next, new_mg) = l2.unwrap();
                    input = next;
                    output.push(value);
                    genctxt2 = new_mg;
                }
            }

            // while let Some((value, next, genctxt)) = delim
            //     .parse(input.clone(), genctxt.clone())
            //     .and_then(|(_, input, mg)| within.parse(input, mg))
            // {
            //     input = next;
            //     output.push(value);
            // }
            Some((output, input, genctxt2))
        } else {
            Some((output, input, genctxt.clone()))
        }
    }
}

/// Parses one or more things, separated by an arbitrary delimiter.
pub fn sep_by1<A, B, I: Clone, GCTXT: Clone>(
    mut delim: impl Parser<A, I, GCTXT>,
    mut within: impl Parser<B, I, GCTXT>,
) -> impl Parser<Vec<B>, I, GCTXT> {
    move |input: I, genctxt: GCTXT| {
        let mut output = Vec::<B>::new();
        let (value, mut input, genctxt) = within.parse(input, genctxt)?;
        output.push(value);
        while let Some((value, next, _)) = delim
            .parse(input.clone(), genctxt.clone())
            .and_then(|(_, input, genctxt)| within.parse(input, genctxt))
        {
            input = next;
            output.push(value);
        }
        Some((output, input, genctxt))
    }
}

/// Transforms the output value of a parser.
pub fn map<A, B, I, GCTXT>(mut parser: impl Parser<A, I, GCTXT>, func: impl Fn(A) -> B) -> impl Parser<B, I, GCTXT> {
    move |input: I, genctxt: GCTXT| {
        let (value, input, genctxt) = parser.parse(input, genctxt)?;
        Some((func(value), input, genctxt))
    }
}
