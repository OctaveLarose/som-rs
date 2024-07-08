//!
//! This crate serves as the syntactical analyser (parser) for the Simple Object Machine.
//!
//! This particular version of the parser works with the tokens outputted by the lexical analyser, instead of directly reading text.
//!

/// SOM-specific parser combinators.
pub mod lang;

use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use som_core::ast::{ClassDef, Expression};
#[cfg(feature = "block-debug-info")]
use som_core::ast::BlockDebugInfo;
use som_core::universe::UniverseForParser;
use som_lexer::Token;
use som_parser_core::{Parser};

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
    class_instance_fields: Vec<String>,
    class_static_fields: Vec<String>, // it's possible the distinction between static/instance fields is useless, but i don't think so.
    current_scope: usize,
    outer_ctxt: Option<AstGenCtxt<'a>>,
    universe: Option<&'a mut dyn UniverseForParser>,
}

pub type AstGenCtxt<'a> = Rc<RefCell<AstGenCtxtData<'a>>>;

#[derive(Debug, PartialEq)]
enum FoundVar {
    Local(usize, usize),
    Argument(usize, usize),
    Field(usize),
}

impl<'a> AstGenCtxtData<'a> {
    pub fn init(universe: Option<&'a mut dyn UniverseForParser>) -> Self {
        AstGenCtxtData {
            kind: AstGenCtxtType::Class,
            name: "NO NAME".to_string(),
            super_class_name: None,
            local_names: vec![],
            param_names: vec![],
            class_static_fields: vec![],
            class_instance_fields: vec![],
            current_scope: 0,
            outer_ctxt: None,
            universe,
        }
    }

    pub fn init_no_universe() -> Self {
        AstGenCtxtData {
            kind: AstGenCtxtType::Class,
            name: "NO NAME".to_string(),
            super_class_name: None,
            local_names: vec![],
            param_names: vec![],
            class_static_fields: vec![],
            class_instance_fields: vec![],
            current_scope: 0,
            outer_ctxt: None,
            universe: None,
        }
    }
}

impl<'a> AstGenCtxtData<'a> {
    pub fn new_ctxt_from(outer: AstGenCtxt, kind: AstGenCtxtType) -> AstGenCtxt {
        let universe = mem::take(&mut outer.borrow_mut().universe);

        Rc::new(RefCell::new(
            AstGenCtxtData {
                kind,
                name: "NO NAME".to_string(),
                super_class_name: outer.borrow().super_class_name.clone(),
                local_names: vec![],
                param_names: vec![],
                class_instance_fields: vec![],
                class_static_fields: vec![],
                current_scope: outer.borrow().current_scope + 1,
                outer_ctxt: Some(Rc::clone(&outer)),
                universe,
            }))
    }

    pub fn get_outer(&mut self) -> AstGenCtxt<'a> {
        let outer = self.outer_ctxt.as_ref().unwrap();
        outer.borrow_mut().universe = mem::take(&mut self.universe);
        Rc::clone(outer)
    }
    
    pub fn load_super_class_and_set_fields(&mut self, name: &String) {
        let fields = match self.universe.as_mut() {
            Some(universe) => universe.load_class_and_get_all_fields(name),
            None => panic!("No universe provided even though we need to parse the superclass {}", name)
        };
        self.add_instance_fields(fields.0);
        self.add_static_fields(fields.1);
        self.super_class_name = Some(name.clone());
    }

    pub fn add_instance_fields(&mut self, fields_names: Vec<String>) {
        self.class_instance_fields.extend(fields_names);
    }

    pub fn add_static_fields(&mut self, fields_names: Vec<String>) {
        self.class_static_fields.extend(fields_names);
    }

    pub fn add_locals(&mut self, new_locals_names: &Vec<String>) {
        debug_assert_ne!(self.kind, AstGenCtxtType::Class);
        self.local_names.extend(new_locals_names.iter().cloned());
    }

    pub fn add_params(&mut self, parameters: &Vec<String>) {
        debug_assert_ne!(self.kind, AstGenCtxtType::Class);
        self.param_names.extend(parameters.iter().cloned());
    }

    pub fn get_local(&self, name: &String) -> Option<usize> {
        self.local_names.iter().position(|local| local == name)
    }

    pub fn get_param(&self, name: &String) -> Option<usize> {
        self.param_names.iter().position(|local| *local == *name)
    }
    
    pub fn get_instance_field(&self, name: &String) -> Option<usize> {
        self.class_instance_fields.iter().position(|c| c == name)
    }

    pub fn get_static_field(&self, name: &String) -> Option<usize> {
        self.class_static_fields.iter().position(|c| c == name)
    }

    fn find_var(&self, name: &String) -> Option<FoundVar> {
        self.get_local(name)
            .map(|idx| FoundVar::Local(0, idx))
            .or_else(|| self.get_param(name).map(|idx| FoundVar::Argument(0, idx)))
            .or_else(|| { // check whether it's defined in an outer scope block as a local or arg...
                match &self.outer_ctxt.as_ref() {
                    None => None,
                    Some(outer) => outer.borrow().find_var(name).map(|found|
                        match found {
                            FoundVar::Local(up_idx, idx) => FoundVar::Local(up_idx + 1, idx),
                            FoundVar::Argument(up_idx, idx) => FoundVar::Argument(up_idx + 1, idx),
                            FoundVar::Field(idx) => FoundVar::Field(idx),
                        }
                    )
                }
            })
            .or_else(|| // ...and if we recursively searched and it wasn't in a block, it must be a field (or search fails and it's a global).
                match self.kind {
                    AstGenCtxtType::Method(method_type) => {
                        let class_ctxt = self.outer_ctxt.as_ref().unwrap().borrow();
                        match method_type {
                            AstMethodGenCtxtType::INSTANCE => class_ctxt.get_instance_field(name).map(FoundVar::Field),
                            AstMethodGenCtxtType::CLASS => class_ctxt.get_static_field(name).map(FoundVar::Field),
                        }
                    }
                    _ => None,
                }
            )
    }
    fn get_var_read(&self, name: &String) -> Expression {
        if name == "self" {
            return Expression::ArgRead(self.get_method_scope(), 0);
        }

        match self.find_var(name) {
            None => Expression::GlobalRead(name.clone()),
            Some(v) => {
                match v {
                    FoundVar::Local(up_idx, idx) => {
                        match up_idx {
                            0 => Expression::LocalVarRead(idx),
                            _ => Expression::NonLocalVarRead(up_idx, idx)
                        }
                    }
                    FoundVar::Argument(up_idx, idx) => Expression::ArgRead(up_idx, idx + 1),
                    FoundVar::Field(idx) => Expression::FieldRead(idx)
                }
            }
        }
    }

    fn get_var_write(&self, name: &String, expr: Box<Expression>) -> Expression {
        match self.find_var(name) {
            None => {
                panic!("variable couldn't be found to be written to ({} in {}).", name, self.name)
            }
            Some(v) => {
                match v {
                    FoundVar::Local(up_idx, idx) => {
                        match up_idx {
                            0 => Expression::LocalVarWrite(idx, expr),
                            _ => Expression::NonLocalVarWrite(up_idx, idx, expr)
                        }
                    }
                    FoundVar::Argument(up_idx, idx) => Expression::ArgWrite(up_idx, idx + 1, expr), // + 1 to adjust for self
                    FoundVar::Field(idx) => Expression::FieldWrite(idx, expr)
                }
            }
        }
    }

    pub fn get_method_scope_rec(&self, method_scope: usize) -> usize {
        match &self.kind {
            AstGenCtxtType::Class => method_scope - 1, // functionally unreachable branch. maybe reachable in the REPL, when we're technically outside a method, maybe? not sure.
            AstGenCtxtType::Method(_) => method_scope,
            AstGenCtxtType::Block => self.outer_ctxt.as_ref().unwrap().borrow().get_method_scope_rec(method_scope + 1)
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
pub fn parse_file(input: &[Token], universe: &mut dyn UniverseForParser) -> Option<ClassDef> {
    self::apply(lang::file(), input, Some(universe))
}

/// Parses the input of an entire file into an AST, without access to the universe (system classes are initialized before the Universe itself, and don't need access to it)
pub fn parse_file_no_universe(input: &[Token]) -> Option<ClassDef> {
    self::apply(lang::file(), input, None)
}

/// Applies a parser and returns the output value if the entirety of the input has been parsed successfully.
pub fn apply<'a, A, P>(mut parser: P, input: &'a [Token], universe: Option<&'a mut dyn UniverseForParser>) -> Option<A>
    where
        P: Parser<A, &'a [Token], AstGenCtxt<'a>>,
{
    match parser.parse(input, Rc::new(RefCell::new(AstGenCtxtData::init(universe)))) {
        Some((output, tail, _)) if tail.is_empty() => Some(output),
        Some(_) | None => None,
    }
}
