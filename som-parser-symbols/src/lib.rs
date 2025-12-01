//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with the tokens outputted by the lexical analyser, instead of directly reading text.
//!

/// SOM-specific parser combinators.
pub mod lang;

/// Inlining SOM control flow methods.
pub mod inliner;

use som_core::ast::ClassDef;
use som_lexer::Token;
use som_parser_core::Parser;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum AstMethodGenCtxtType {
    INSTANCE,
    CLASS,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum AstGenCtxtType {
    Class,
    Block,
    Method(AstMethodGenCtxtType),
}

// #[derive(Debug)]
pub struct AstGenCtxtData<'a> {
    kind: AstGenCtxtType,
    // name: String, // used for debugging
    local_names: Vec<String>,
    param_names: Vec<String>,
    outer_ctxt: Option<AstGenCtxt<'a>>,
}

pub type AstGenCtxt<'a> = Rc<RefCell<AstGenCtxtData<'a>>>;

impl AstGenCtxtData<'_> {
    pub fn init() -> Self {
        AstGenCtxtData {
            kind: AstGenCtxtType::Class,
            // name: "NO NAME".to_string(),
            local_names: vec![],
            param_names: vec![],
            outer_ctxt: None,
        }
    }
}

impl<'a> AstGenCtxtData<'a> {
    pub fn new_ctxt_from(outer: AstGenCtxt, kind: AstGenCtxtType) -> AstGenCtxt {
        Rc::new(RefCell::new(AstGenCtxtData {
            kind,
            // name: "NO NAME".to_string(),
            local_names: vec![],
            param_names: vec![],
            outer_ctxt: Some(Rc::clone(&outer)),
        }))
    }

    // for debugging
    // pub fn get_class_name(&self) -> String {
    //     match &self.kind {
    //         AstGenCtxtType::Class => self.name.clone(),
    //         _ => self.outer_ctxt.as_ref().unwrap().borrow_mut().get_class_name(),
    //     }
    // }

    pub fn get_outer(&mut self) -> AstGenCtxt<'a> {
        let outer = self.outer_ctxt.as_ref().unwrap();
        Rc::clone(outer)
    }

    pub fn has_local(&self, name: &String) -> bool {
        if self.kind == AstGenCtxtType::Class {
            false
        } else if self.local_names.iter().any(|local| local == name) {
            true
        } else {
            match &self.outer_ctxt {
                Some(outer) => outer.borrow().has_local(name),
                None => false,
            }
        }
    }

    pub fn add_locals(&mut self, new_locals_names: &[String]) {
        debug_assert_ne!(self.kind, AstGenCtxtType::Class);
        self.local_names.extend(new_locals_names.iter().cloned());
    }

    pub fn add_params(&mut self, parameters: &[String]) {
        debug_assert_ne!(self.kind, AstGenCtxtType::Class);
        self.param_names.extend(parameters.iter().cloned());
    }
}

/// Parses the input of an entire file into an AST.
pub fn parse_file(input: &[Token]) -> Option<ClassDef> {
    self::apply(lang::file(), input)
}

/// Applies a parser and returns the output value if the entirety of the input has been parsed successfully.
pub fn apply<'a, A, P>(mut parser: P, input: &'a [Token]) -> Option<A>
where
    P: Parser<A, &'a [Token], AstGenCtxt<'a>>,
{
    match parser.parse(input, Rc::new(RefCell::new(AstGenCtxtData::init()))) {
        Some((output, [], _)) => Some(output),
        Some(_) | None => None,
    }
}
