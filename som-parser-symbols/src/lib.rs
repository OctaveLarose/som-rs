//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with the tokens outputted by the lexical analyser, instead of directly reading text.
//!

/// SOM-specific parser combinators.
pub mod lang;

/// Inlining SOM control flow methods.
pub mod inliner;

#[cfg(feature = "block-debug-info")]
use som_core::ast::BlockDebugInfo;
use som_core::ast::{ClassDef, Expression};
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
    name: String, // used for debugging
    super_class_name: Option<String>,
    local_names: Vec<String>,
    param_names: Vec<String>,
    current_scope: usize,
    _is_getting_inlined: bool,
    outer_ctxt: Option<AstGenCtxt<'a>>,
}

pub type AstGenCtxt<'a> = Rc<RefCell<AstGenCtxtData<'a>>>;

#[derive(Debug, PartialEq)]
enum FoundVar {
    Local(usize, usize),
    Argument(usize, usize),
    // Field(usize),
}

impl AstGenCtxtData<'_> {
    pub fn init() -> Self {
        AstGenCtxtData {
            kind: AstGenCtxtType::Class,
            name: "NO NAME".to_string(),
            super_class_name: None,
            local_names: vec![],
            param_names: vec![],
            current_scope: 0,
            outer_ctxt: None,
            _is_getting_inlined: false,
        }
    }
}

impl<'a> AstGenCtxtData<'a> {
    pub fn new_ctxt_from(outer: AstGenCtxt, kind: AstGenCtxtType) -> AstGenCtxt {
        Rc::new(RefCell::new(AstGenCtxtData {
            kind,
            name: "NO NAME".to_string(),
            super_class_name: outer.borrow().super_class_name.clone(),
            local_names: vec![],
            param_names: vec![],
            current_scope: outer.borrow().current_scope + 1,
            _is_getting_inlined: false,
            outer_ctxt: Some(Rc::clone(&outer)),
        }))
    }

    // for debugging
    pub fn get_class_name(&self) -> String {
        match &self.kind {
            AstGenCtxtType::Class => self.name.clone(),
            _ => self.outer_ctxt.as_ref().unwrap().borrow_mut().get_class_name(),
        }
    }

    pub fn get_outer(&mut self) -> AstGenCtxt<'a> {
        let outer = self.outer_ctxt.as_ref().unwrap();
        Rc::clone(outer)
    }

    pub fn get_super_class_name(&self) -> Option<String> {
        match &self.kind {
            AstGenCtxtType::Class => self.super_class_name.clone(),
            AstGenCtxtType::Method(method_type) => {
                let s_cl_name = self.outer_ctxt.as_ref().unwrap().borrow_mut().get_super_class_name();

                match (&s_cl_name, method_type) {
                    (Some(_), _) => s_cl_name,
                    (None, AstMethodGenCtxtType::INSTANCE) => Some(String::from("Object")),
                    (None, AstMethodGenCtxtType::CLASS) => Some(String::from("Class")),
                }
            }
            _ => self.outer_ctxt.as_ref().unwrap().borrow_mut().get_super_class_name(),
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

    pub fn get_local(&self, name: &String) -> Option<usize> {
        self.local_names.iter().position(|local| local == name)
    }

    pub fn get_param(&self, name: &String) -> Option<usize> {
        self.param_names.iter().position(|local| *local == *name)
    }

    fn find_var(&self, name: &String) -> Option<FoundVar> {
        self.get_local(name)
            .map(|idx| FoundVar::Local(0, idx))
            .or_else(|| self.get_param(name).map(|idx| FoundVar::Argument(0, idx)))
            .or_else(|| {
                // check whether it's defined in an outer scope block as a local or arg...
                match &self.outer_ctxt.as_ref() {
                    None => None,
                    Some(outer) => outer.borrow().find_var(name).map(|found| match found {
                        FoundVar::Local(up_idx, idx) => FoundVar::Local(up_idx + 1, idx),
                        FoundVar::Argument(up_idx, idx) => FoundVar::Argument(up_idx + 1, idx),
                        // FoundVar::Field(idx) => FoundVar::Field(idx),
                    }),
                }
            })
    }

    fn get_var_read(&self, name: &String) -> Expression {
        if name == "self" {
            return Expression::ArgRead(self.get_method_scope(), 0);
        }

        match self.find_var(name) {
            None => Expression::GlobalRead(name.clone()),
            Some(v) => {
                match v {
                    FoundVar::Local(up_idx, idx) => Expression::VarRead(up_idx, idx),
                    FoundVar::Argument(up_idx, idx) => Expression::ArgRead(up_idx, idx + 1),
                    // FoundVar::Field(idx) => Expression::FieldRead(idx)
                }
            }
        }
    }

    fn get_var_write(&self, name: &String, expr: Box<Expression>) -> Expression {
        match self.find_var(name) {
            None => Expression::GlobalWrite(name.clone(), expr),
            Some(v) => {
                match v {
                    FoundVar::Local(up_idx, idx) => Expression::VarWrite(up_idx, idx, expr),
                    FoundVar::Argument(up_idx, idx) => Expression::ArgWrite(up_idx, idx + 1, expr), // + 1 to adjust for self
                                                                                                    // FoundVar::Field(idx) => Expression::FieldWrite(idx, expr)
                }
            }
        }
    }

    pub fn get_method_scope_rec(&self, method_scope: usize) -> usize {
        match &self.kind {
            AstGenCtxtType::Class => method_scope - 1, // functionally unreachable branch. maybe reachable in the REPL, when we're technically outside a method, maybe? not sure.
            AstGenCtxtType::Method(_) => method_scope,
            AstGenCtxtType::Block => self.outer_ctxt.as_ref().unwrap().borrow().get_method_scope_rec(method_scope + 1),
        }
    }

    pub fn get_method_scope(&self) -> usize {
        self.get_method_scope_rec(0)
    }

    #[cfg(feature = "block-debug-info")]
    pub fn get_debug_info(&self) -> BlockDebugInfo {
        BlockDebugInfo {
            parameters: {
                let mut parameters_with_self = self.param_names.clone();
                parameters_with_self.insert(0, String::from("self"));
                parameters_with_self
            },
            locals: self.local_names.clone(),
        }
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
