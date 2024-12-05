use super::inliner::PrimMessageInliner;
use crate::ast::{
    AstBinaryDispatch, AstBlock, AstBody, AstDispatchNode, AstExpression, AstLiteral, AstMethodDef, AstNAryDispatch, AstSuperMessage,
    AstTernaryDispatch, AstUnaryDispatch,
};
use crate::gc::VecAstLiteral;
use crate::primitives::UNIMPLEM_PRIMITIVE;
use crate::specialized::down_to_do_node::DownToDoNode;
use crate::specialized::to_by_do_node::ToByDoNode;
use crate::specialized::to_do_node::ToDoNode;
use crate::specialized::trivial_methods::{TrivialGetterMethod, TrivialGlobalMethod, TrivialLiteralMethod, TrivialSetterMethod};
use crate::vm_objects::class::Class;
use crate::vm_objects::method::{MethodKind, MethodKindSpecialized};
use som_core::ast;
use som_core::ast::{Expression, Literal, MethodBody};
use som_gc::gc_interface::GCInterface;
use som_gc::gcref::Gc;
use std::panic;

pub struct AstMethodCompilerCtxt<'a> {
    /// The class in which context we're compiling. Needed for resolving field accesses. Should always be Some() outside of a testing context.
    pub(crate) class: Option<Gc<Class>>,
    /// The stack of scopes to better reason about inlining.
    pub(crate) scopes: Vec<AstScopeCtxt>,
    /// The interface to the GC to allocate anything we want during parsing.
    pub(crate) gc_interface: &'a mut GCInterface,
}

#[derive(Debug, Default)]
pub(crate) struct AstScopeCtxt {
    nbr_args: usize,
    nbr_locals: usize,
    pub is_getting_inlined: bool,
}

impl AstScopeCtxt {
    pub fn init(nbr_args: usize, nbr_locals: usize, is_getting_inlined: bool) -> Self {
        Self {
            nbr_args,
            nbr_locals,
            is_getting_inlined,
        }
    }

    pub fn get_nbr_locals(&self) -> usize {
        self.nbr_locals
    }

    pub fn add_nbr_locals(&mut self, nbr_to_add: usize) {
        self.nbr_locals += nbr_to_add;
    }
    pub fn get_nbr_args(&self) -> usize {
        self.nbr_args
    }
}

impl<'a> AstMethodCompilerCtxt<'a> {
    pub fn new(gc_interface: &'a mut GCInterface) -> Self {
        Self {
            class: None,
            scopes: vec![],
            gc_interface,
        }
    }

    pub fn get_method_kind(method: &ast::MethodDef, class: Option<Gc<Class>>, gc_interface: &mut GCInterface) -> MethodKind {
        // NB: these If/IfTrueIfFalse/While are very rare cases, since we normally inline those functions.
        // But we don't do inlining when e.g. the condition for ifTrue: isn't a block.
        // so there is *some* occasional benefit in having those specialized method nodes around for those cases.
        match method.signature.as_str() {
            "to:do:" => MethodKind::Specialized(MethodKindSpecialized::ToDo(ToDoNode {})),
            "to:by:do:" => MethodKind::Specialized(MethodKindSpecialized::ToByDo(ToByDoNode {})),
            "downTo:do:" => MethodKind::Specialized(MethodKindSpecialized::DownToDo(DownToDoNode {})),
            _ => match method.body {
                MethodBody::Primitive => MethodKind::Primitive(&*UNIMPLEM_PRIMITIVE),
                MethodBody::Body { .. } => {
                    let ast_method_def = AstMethodCompilerCtxt::parse_method_def(method, class, gc_interface);

                    if let Some(trivial_method_kind) = AstMethodCompilerCtxt::make_trivial_method_if_possible(&ast_method_def) {
                        trivial_method_kind
                    } else {
                        MethodKind::Defined(ast_method_def)
                    }
                }
            },
        }
    }

    pub(crate) fn make_trivial_method_if_possible(method_def: &AstMethodDef) -> Option<MethodKind> {
        if method_def.locals_nbr != 0 || method_def.body.exprs.len() != 1 {
            return None;
        }

        match method_def.body.exprs.first()? {
            AstExpression::LocalExit(expr) => {
                match expr.as_ref() {
                    AstExpression::Literal(lit) => {
                        Some(MethodKind::TrivialLiteral(TrivialLiteralMethod { literal: lit.clone() }))
                        // todo avoid clone by moving code to previous function tbh
                    }
                    AstExpression::GlobalRead(global) => Some(MethodKind::TrivialGlobal(TrivialGlobalMethod {
                        global_name: *global.clone(),
                    })),
                    AstExpression::FieldRead(idx) => Some(MethodKind::TrivialGetter(TrivialGetterMethod { field_idx: *idx })),
                    _ => None,
                }
            }
            AstExpression::FieldWrite(idx, expr) => match expr.as_ref() {
                AstExpression::ArgRead(0, 1) => Some(MethodKind::TrivialSetter(TrivialSetterMethod { field_idx: *idx })),
                _ => None,
            },
            _ => None,
        }
    }

    /// Transforms a generic MethodDef into an AST-specific one.
    /// Note: public since it's used in tests.
    pub fn parse_method_def(method_def: &ast::MethodDef, class: Option<Gc<Class>>, gc_interface: &mut GCInterface) -> AstMethodDef {
        let (body, locals_nbr) = match &method_def.body {
            MethodBody::Primitive => {
                unreachable!("unimplemented primitive")
            }
            MethodBody::Body { locals_nbr, body, .. } => {
                let args_nbr = method_def.signature.chars().filter(|e| *e == ':').count(); // not sure if needed
                let mut ctxt = AstMethodCompilerCtxt {
                    class,
                    scopes: vec![AstScopeCtxt::init(args_nbr, *locals_nbr, false)],
                    gc_interface,
                };

                (ctxt.parse_body(body), ctxt.scopes.last().unwrap().get_nbr_locals() as u8)
            }
        };

        AstMethodDef {
            signature: method_def.signature.clone(),
            locals_nbr,
            body,
        }
    }

    pub fn parse_expression(&mut self, expr: &Expression) -> AstExpression {
        match expr.clone() {
            Expression::GlobalRead(global_name) => self.global_or_field_read_from_superclass(global_name),
            Expression::GlobalWrite(global_name, expr) => self.resolve_global_write_to_field_write(&global_name, expr.as_ref()),
            Expression::LocalVarRead(idx) => AstExpression::LocalVarRead(idx as u8),
            Expression::NonLocalVarRead(scope, idx) => AstExpression::NonLocalVarRead(scope as u8, idx as u8),
            Expression::ArgRead(scope, idx) => AstExpression::ArgRead(scope as u8, idx as u8),
            Expression::LocalVarWrite(a, b) => AstExpression::LocalVarWrite(a as u8, Box::new(self.parse_expression(b.as_ref()))),
            Expression::NonLocalVarWrite(a, b, c) => AstExpression::NonLocalVarWrite(a as u8, b as u8, Box::new(self.parse_expression(c.as_ref()))),
            Expression::ArgWrite(a, b, c) => AstExpression::ArgWrite(a as u8, b as u8, Box::new(self.parse_expression(c.as_ref()))),
            Expression::Message(msg) => self.parse_message(msg.as_ref()),
            Expression::Exit(a, b) => match b {
                0 => AstExpression::LocalExit(Box::new(self.parse_expression(a.as_ref()))),
                _ => AstExpression::NonLocalExit(Box::new(self.parse_expression(a.as_ref())), b as u8),
            },
            Expression::Literal(a) => {
                match &a {
                    // this is to handle a weird corner case where "-2147483648" is considered to be a bigint by the lexer and then parser, when it's in fact just barely in i32 range
                    Literal::BigInteger(big_int_str) => match big_int_str.parse::<i32>() {
                        Ok(x) => AstExpression::Literal(AstLiteral::Integer(x)),
                        _ => AstExpression::Literal(self.parse_literal(&a)),
                    },
                    _ => AstExpression::Literal(self.parse_literal(&a)),
                }
            }
            Expression::Block(a) => {
                let ast_block = self.parse_block(&a);
                AstExpression::Block(self.gc_interface.alloc(ast_block))
            }
        }
    }

    pub fn parse_body(&mut self, body: &ast::Body) -> AstBody {
        AstBody {
            exprs: body.exprs.iter().map(|expr| self.parse_expression(expr)).collect(),
        }
    }

    pub fn parse_block(&mut self, blk: &ast::Block) -> AstBlock {
        self.scopes.push(AstScopeCtxt::init(blk.nbr_params, blk.nbr_locals, false));

        let body = self.parse_body(&blk.body);
        let bl = self.scopes.last().unwrap();
        let output_blk = AstBlock {
            nbr_params: bl.get_nbr_args() as u8,
            nbr_locals: bl.get_nbr_locals() as u8,
            body,
        };

        self.scopes.pop();
        output_blk
    }

    pub fn parse_message(&mut self, msg: &ast::Message) -> AstExpression {
        self.parse_message_with_func(msg, Self::parse_expression)
    }

    pub fn parse_message_with_inlining(&mut self, msg: &ast::Message) -> AstExpression {
        self.parse_message_with_func(msg, Self::parse_expression_with_inlining)
    }

    pub fn parse_message_with_func(
        &mut self,
        msg: &ast::Message,
        expr_parsing_func: fn(&mut AstMethodCompilerCtxt<'a>, &Expression) -> AstExpression,
    ) -> AstExpression {
        #[cfg(not(feature = "inlining-disabled"))]
        {
            let maybe_inlined = self.inline_if_possible(msg);
            if let Some(inlined_node) = maybe_inlined {
                return AstExpression::InlinedCall(Box::new(inlined_node));
            }
        }

        if msg.receiver == Expression::GlobalRead(String::from("super")) {
            return AstExpression::SuperMessage(Box::new(AstSuperMessage {
                super_class: self
                    .class
                    .unwrap()
                    .super_class
                    .unwrap_or_else(|| panic!("no super class set, even though the method has a super call?")),
                signature: msg.signature.clone(),
                values: msg.values.iter().map(|e| expr_parsing_func(self, e)).collect(),
            }));
        }

        let receiver = expr_parsing_func(self, &msg.receiver);
        match msg.values.len() {
            0 => AstExpression::UnaryDispatch(Box::new(AstUnaryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: msg.signature.clone(),
                    inline_cache: None,
                },
            })),
            1 => AstExpression::BinaryDispatch(Box::new(AstBinaryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: msg.signature.clone(),
                    inline_cache: None,
                },
                arg: expr_parsing_func(self, msg.values.first().unwrap()),
            })),
            2 => AstExpression::TernaryDispatch(Box::new(AstTernaryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: msg.signature.clone(),
                    inline_cache: None,
                },
                arg1: expr_parsing_func(self, msg.values.first().unwrap()),
                arg2: expr_parsing_func(self, msg.values.get(1).unwrap()),
            })),
            _ => AstExpression::NAryDispatch(Box::new(AstNAryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: msg.signature.clone(),
                    inline_cache: None,
                },
                values: msg.values.iter().map(|e| expr_parsing_func(self, e)).collect(),
            })),
        }
    }

    pub(crate) fn global_or_field_read_from_superclass(&self, name: String) -> AstExpression {
        if name.as_str() == "super" {
            return AstExpression::ArgRead((self.scopes.len() - 1) as u8, 0);
        }

        if self.class.is_none() {
            return AstExpression::GlobalRead(Box::new(name.clone()));
        }

        match self.class.unwrap().get_field_offset_by_name(&name) {
            Some(offset) => AstExpression::FieldRead(offset as u8),
            _ => AstExpression::GlobalRead(Box::new(name.clone())),
        }
    }

    fn resolve_global_write_to_field_write(&mut self, name: &String, expr: &Expression) -> AstExpression {
        if self.class.is_none() {
            panic!(
                "can't turn the GlobalWrite `{}` into a FieldWrite, and GlobalWrite shouldn't exist at runtime",
                name
            );
        }

        match self.class.unwrap().get_field_offset_by_name(name) {
            Some(offset) => AstExpression::FieldWrite(offset as u8, Box::new(self.parse_expression(expr))),
            _ => panic!(
                "can't turn the GlobalWrite `{}` into a FieldWrite, and GlobalWrite shouldn't exist at runtime",
                name
            ),
        }
    }

    pub(crate) fn parse_literal(&mut self, lit: &ast::Literal) -> AstLiteral {
        match lit {
            Literal::String(str) => {
                let str_ptr = self.gc_interface.alloc(str.clone());
                AstLiteral::String(str_ptr)
            }
            Literal::Symbol(str) => {
                let str_ptr = self.gc_interface.alloc(str.clone());
                AstLiteral::Symbol(str_ptr)
            }
            Literal::Double(double) => AstLiteral::Double(*double),
            Literal::Integer(int) => AstLiteral::Integer(*int),
            Literal::BigInteger(bigint_str) => {
                let bigint_ptr = self.gc_interface.alloc(bigint_str.parse().unwrap());
                AstLiteral::BigInteger(bigint_ptr)
            }
            Literal::Array(arr) => {
                let arr = arr.iter().map(|lit| self.parse_literal(lit)).collect();
                let arr_ptr = self.gc_interface.alloc(VecAstLiteral(arr));
                AstLiteral::Array(arr_ptr)
            }
        }
    }
}
