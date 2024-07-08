use std::marker::PhantomData;

/// Generic parser combinators.
pub mod combinators;

/// Defines a parser.
///
/// It is basically a function that takes an input and returns a parsed result along with the rest of input (which can be parsed further).
pub trait Parser<T, I, GCTXT>: Sized {
    /// Applies the parser on some input.
    ///
    /// It returns the parsed value and the rest of the unparsed input as `Some`, if successful.  
    /// Failing that, it returns `None`.  
    fn parse(&mut self, input: I, genctxt: GCTXT) -> Option<(T, I, GCTXT)>;

    /// Sequences two parsers, one after the other, collecting both results.
    fn and<U, P: Parser<U, I, GCTXT>>(self, parser: P) -> And<Self, P> {
        And {
            p1: self,
            p2: parser,
        }
    }

    /// Tries to apply the first parser, if it fails, it tries to apply the second parser.
    fn or<P: Parser<T, I, GCTXT>>(self, parser: P) -> Or<Self, P> {
        Or {
            p1: self,
            p2: parser,
        }
    }

    /// Maps a function over the output value of the parser.
    fn map<F: Fn(T) -> U, U>(self, func: F) -> Map<Self, F, T> {
        Map {
            parser: self,
            func,
            _phantom: PhantomData,
        }
    }

    /// Sequences two parsers, one after the other, but discards the output of the second one.
    fn and_left<P: Parser<U, I, GCTXT>, U>(self, parser: P) -> AndLeft<Self, P, U> {
        AndLeft {
            p1: self,
            p2: parser,
            _phantom: PhantomData,
        }
    }

    /// Sequences two parsers, one after the other, but discards the output of the first one.
    fn and_right<P: Parser<U, I, GCTXT>, U>(self, parser: P) -> AndRight<Self, P, T> {
        AndRight {
            p1: self,
            p2: parser,
            _phantom: PhantomData,
        }
    }
}

/// Sequences two parsers, one after the other, collecting both results.
pub struct And<A, B> {
    p1: A,
    p2: B,
}

impl<T1, T2, A, B, I, GCTXT> Parser<(T1, T2), I, GCTXT> for And<A, B>
where
    A: Parser<T1, I, GCTXT>,
    B: Parser<T2, I, GCTXT>,
{
    fn parse<'a>(&mut self, input: I, genctxt: GCTXT) -> Option<((T1, T2), I, GCTXT)> {
        let (v1, input, genctxt) = self.p1.parse(input, genctxt)?;
        let (v2, input, genctxt) = self.p2.parse(input, genctxt)?;
        Some(((v1, v2), input, genctxt))
    }
}

/// Tries to apply the first parser, if it fails, it tries to apply the second parser.
pub struct Or<A, B> {
    p1: A,
    p2: B,
}

impl<T, A, B, I, GCTXT> Parser<T, I, GCTXT> for Or<A, B>
where
    I: Clone,
    GCTXT: Clone,
    A: Parser<T, I, GCTXT>,
    B: Parser<T, I, GCTXT>,
{
    fn parse(&mut self, input: I, genctxt: GCTXT) -> Option<(T, I, GCTXT)> {
        // self.p1
        //     .parse(input.clone(), genctxt)
        //     .or_else(|| self.p2.parse(input, genctxt))

        let l1 = self.p1.parse(input.clone(), genctxt.clone());

        if l1.is_some() {
            l1
        } else {
            self.p2.parse(input, genctxt)
        }
    }
}

/// Maps a function over the output value of the parser.
pub struct Map<P, F, T> {
    parser: P,
    func: F,
    _phantom: PhantomData<T>,
}

impl<P, T, F, U, I, GCTXT> Parser<U, I, GCTXT> for Map<P, F, T>
where
    P: Parser<T, I, GCTXT>,
    F: Fn(T) -> U,
{
    fn parse<'a>(&mut self, input: I, genctxt: GCTXT) -> Option<(U, I, GCTXT)> {
        let (value, input, genctxt) = self.parser.parse(input, genctxt)?;
        Some(((self.func)(value), input, genctxt))
    }
}

/// Sequences two parsers, one after the other, but discards the output of the second one.
pub struct AndLeft<A, B, U> {
    p1: A,
    p2: B,
    _phantom: PhantomData<U>,
}

impl<A, B, T, U, I, GCTXT> Parser<T, I, GCTXT> for AndLeft<A, B, U>
where
    A: Parser<T, I, GCTXT>,
    B: Parser<U, I, GCTXT>,
{
    fn parse(&mut self, input: I, genctxt: GCTXT) -> Option<(T, I, GCTXT)> {
        let (value, input, genctxt) = self.p1.parse(input, genctxt)?;
        let (_, input, genctxt) = self.p2.parse(input, genctxt)?;
        Some((value, input, genctxt))
    }
}

/// Sequences two parsers, one after the other, but discards the output of the first one.
pub struct AndRight<A, B, T> {
    p1: A,
    p2: B,
    _phantom: PhantomData<T>,
}

impl<A, B, T, U, I, GCTXT> Parser<U, I, GCTXT> for AndRight<A, B, T>
where
    A: Parser<T, I, GCTXT>,
    B: Parser<U, I, GCTXT>,
{
    fn parse(&mut self, input: I, genctxt: GCTXT) -> Option<(U, I, GCTXT)> {
        let (_, input, genctxt) = self.p1.parse(input, genctxt)?;
        let (value, input, genctxt) = self.p2.parse(input, genctxt)?;
        Some((value, input, genctxt))
    }
}

/// Because a `Parser` is basically a function of the following signature.
/// ```text
/// (I) -> (T, I)
/// ```
/// We can implement it for any bare `Fn(I) -> (T, I)`.
impl<T, F, I, GCTXT> Parser<T, I, GCTXT> for F
where
    F: FnMut(I, GCTXT) -> Option<(T, I, GCTXT)>,
{
    fn parse(&mut self, input: I, genctxt: GCTXT) -> Option<(T, I, GCTXT)> {
        (self)(input, genctxt)
    }
}
