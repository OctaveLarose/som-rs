//use super::inliner::PrimMessageInliner;
use crate::ast::{
    AstBinaryDispatch, AstBlock, AstBody, AstDispatchNode, AstExpression, AstLiteral, AstMethodDef, AstNAryDispatch, AstSuperMessage,
    AstTernaryDispatch, AstUnaryDispatch,
};
use crate::gc::VecLiteral;
use crate::nodes::global_read::GlobalNode;
use crate::nodes::inlined::and_inlined_node::AndInlinedNode;
use crate::nodes::inlined::if_inlined_node::IfInlinedNode;
use crate::nodes::inlined::if_nil_if_not_nil_inlined_node::IfNilIfNotNilInlinedNode;
use crate::nodes::inlined::if_nil_inlined_node::IfNilInlinedNode;
use crate::nodes::inlined::if_true_if_false_inlined_node::IfTrueIfFalseInlinedNode;
use crate::nodes::inlined::or_inlined_node::OrInlinedNode;
use crate::nodes::inlined::to_do_inlined_node::ToDoInlinedNode;
use crate::nodes::inlined::while_inlined_node::WhileInlinedNode;
use crate::nodes::trivial_methods::{TrivialGetterMethod, TrivialGlobalMethod, TrivialLiteralMethod, TrivialSetterMethod};
use crate::primitives::UNIMPLEM_PRIMITIVE;
use crate::vm_objects::class::Class;
use crate::vm_objects::method::MethodKind;
use indexmap::IndexSet;
use som_core::ast::{self};
use som_core::ast::{Expression, Literal, MethodBody};
use som_core::interner::Interner;
use som_gc::gc_interface::{AllocSiteMarker, GCInterface, SOMAllocator};
use som_gc::gcref::Gc;

#[derive(Debug)]
pub(crate) enum FoundVar {
    Local(u8, u8),
    Argument(u8, u8),
    Field(u8),
}

pub struct AstMethodCompilerCtxt<'a> {
    /// The class in which context we're compiling. Needed for resolving field accesses. Should always be Some() outside of a testing context.
    pub(crate) class: Option<Gc<Class>>,
    /// The stack of scopes to better reason about inlining.
    pub(crate) scopes: Vec<AstScopeCtxt>,
    /// The interface to the GC to allocate anything we want during parsing.
    pub(crate) gc_interface: &'a mut GCInterface,
    /// For string interning during compilation.
    pub(crate) interner: &'a mut Interner,
}

#[derive(Debug, Default)]
pub(crate) struct AstScopeCtxt {
    nbr_args: usize,
    nbr_locals: usize,
    args: IndexSet<String>,
    locals: IndexSet<String>,
}

#[allow(unused)]
impl AstScopeCtxt {
    pub fn init(nbr_args: usize, nbr_locals: usize, locals: IndexSet<String>, args: IndexSet<String>) -> Self {
        Self {
            nbr_args,
            nbr_locals,
            locals,
            args,
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

    pub fn get_arg(&self, name: &str) -> Option<usize> {
        self.args.iter().position(|a| a == name)
    }

    pub fn get_local(&self, name: &str) -> Option<usize> {
        self.args.iter().position(|a| a == name)
    }

    pub fn find_var(&self, name: &str, cur_scope: usize, scopes: &Vec<AstScopeCtxt>) -> Option<FoundVar> {
        let name = match name {
            "super" => "self",
            name => name,
        };
        (self.locals.get_index_of(name))
            .map(|idx| FoundVar::Local(0, idx as u8))
            .or_else(|| (self.args.get_index_of(name)).map(|idx| FoundVar::Argument(0, idx as u8)))
            .or_else(|| {
                scopes.iter().nth_back(cur_scope + 1)?.find_var(name, cur_scope + 1, scopes).map(|found| match found {
                    FoundVar::Local(up_idx, idx) => FoundVar::Local(up_idx + 1, idx),
                    FoundVar::Argument(up_idx, idx) => FoundVar::Argument(up_idx + 1, idx),
                    _ => unreachable!(),
                })
            })
    }
}

impl<'a> AstMethodCompilerCtxt<'a> {
    fn find_var(&self, name: &str) -> Option<FoundVar> {
        if let Some(found_var) = self.scopes.last()?.find_var(name, 0, &self.scopes) {
            return Some(found_var);
        }
        if let Some(cls) = &self.class {
            if let Some(found_field) = cls.field_names.iter().position(|n| n == name) {
                return Some(FoundVar::Field(found_field as u8));
            }
        }
        None
    }

    pub fn new(gc_interface: &'a mut GCInterface, interner: &'a mut Interner) -> Self {
        Self {
            class: None,
            scopes: vec![],
            gc_interface,
            interner,
        }
    }

    pub fn get_method_kind(method: &ast::MethodDef, class: Option<Gc<Class>>, gc_interface: &mut GCInterface, interner: &mut Interner) -> MethodKind {
        match method.body {
            MethodBody::Primitive => MethodKind::Primitive(&*UNIMPLEM_PRIMITIVE),
            MethodBody::Body { .. } => {
                let ast_method_def = AstMethodCompilerCtxt::parse_method_def(method, class, gc_interface, interner);

                // since we allocate the method, we report the size of the method to GC, which is enough for trivial methods
                if let Some(trivial_method_kind) = AstMethodCompilerCtxt::make_trivial_method_if_possible(&ast_method_def, interner) {
                    trivial_method_kind
                } else {
                    // ...but for regular methods, we calculate the total size of the expressions also
                    // we only add the size of the expressions and not the vector since the vector is already counted within the methodkind

                    // NOT the best way to do this, but I thought it'd be nice to do it AFTER all the compilation and inlining is done, to make sure we are accurate
                    #[cfg(feature = "track-allocations")]
                    {
                        fn get_tree_size(expr: &AstExpression) -> u128 {
                            use AstExpression::*;
                            let expr_size = size_of::<AstExpression>() as u128;
                            match expr {
                                LocalVarRead(_)
                                | NonLocalVarRead(_, _)
                                | ArgRead(_, _)
                                | FieldRead(_)
                                | IncLocal(_)
                                | DecLocal(_)
                                | GlobalRead(_)
                                | Literal(_) => expr_size,
                                LocalVarWrite(_, ast_expression)
                                | NonLocalVarWrite(_, _, ast_expression)
                                | ArgWrite(_, _, ast_expression)
                                | LocalExit(ast_expression)
                                | NonLocalExit(ast_expression, _)
                                | FieldWrite(_, ast_expression) => expr_size + get_tree_size(ast_expression),
                                UnaryDispatch(ast_unary_dispatch) => {
                                    // the minus feels silly, but the logic is that since it's not a box in AstUnaryDispatch,
                                    // if we count the size of AstUnaryDispatch but then also invoke get_tree_size on the receiver, the receiver would get counted twice
                                    expr_size + size_of::<AstUnaryDispatch>() as u128 + get_tree_size(&ast_unary_dispatch.dispatch_node.receiver)
                                        - expr_size
                                }
                                BinaryDispatch(ast_binary_dispatch) => {
                                    // here count an AstBinaryDispatch but avoid double counting the expressions
                                    expr_size
                                        + size_of::<AstBinaryDispatch>() as u128
                                        + get_tree_size(&ast_binary_dispatch.arg)
                                        + get_tree_size(&ast_binary_dispatch.dispatch_node.receiver)
                                        - expr_size * 2
                                }
                                TernaryDispatch(ast_ternary_dispatch) => {
                                    expr_size
                                        + size_of::<AstTernaryDispatch>() as u128
                                        + get_tree_size(&ast_ternary_dispatch.dispatch_node.receiver)
                                        + get_tree_size(&ast_ternary_dispatch.arg1)
                                        + get_tree_size(&ast_ternary_dispatch.arg2)
                                        - expr_size * 3
                                }
                                NAryDispatch(ast_nary_dispatch) => {
                                    expr_size
                                        + size_of::<AstNAryDispatch>() as u128
                                        + get_tree_size(&ast_nary_dispatch.dispatch_node.receiver)
                                        + ast_nary_dispatch.values.iter().map(get_tree_size).sum::<u128>()
                                        - expr_size // only one here - it's a Vec (counted within ASTNaryDispatch), so we've not double counted elements
                                }
                                SuperMessage(ast_super_message) => {
                                    expr_size
                                        + size_of::<AstSuperMessage>() as u128
                                        + ast_super_message.values.iter().map(get_tree_size).sum::<u128>()
                                }
                                Block(block) => {
                                    // block is on the GC heap, we only need to consider its body expressions.
                                    expr_size + block.body.exprs.iter().map(get_tree_size).sum::<u128>()
                                }
                                InlinedCall(inlined_node) => {
                                    use crate::ast::InlinedNode;
                                    use crate::ast::InlinedNode::*;
                                    expr_size + size_of::<InlinedNode>() as u128 + {
                                        match &**inlined_node {
                                            IfInlined(if_inlined_node) => if_inlined_node.body_instrs.exprs.iter().map(get_tree_size).sum::<u128>(),
                                            IfTrueIfFalseInlined(if_true_if_false_inlined_node) => {
                                                get_tree_size(&if_true_if_false_inlined_node.cond_expr)
                                                    + if_true_if_false_inlined_node.body_1_instrs.exprs.iter().map(get_tree_size).sum::<u128>()
                                                    + if_true_if_false_inlined_node.body_2_instrs.exprs.iter().map(get_tree_size).sum::<u128>()
                                            }
                                            IfNilInlined(if_nil_inlined_node) => {
                                                get_tree_size(&if_nil_inlined_node.cond_expr)
                                                    + if_nil_inlined_node.body_instrs.exprs.iter().map(get_tree_size).sum::<u128>()
                                            }
                                            IfNilIfNotNilInlined(if_nil_if_not_nil_inlined_node) => {
                                                get_tree_size(&if_nil_if_not_nil_inlined_node.cond_expr)
                                                    + if_nil_if_not_nil_inlined_node.body_1_instrs.exprs.iter().map(get_tree_size).sum::<u128>()
                                                    + if_nil_if_not_nil_inlined_node.body_2_instrs.exprs.iter().map(get_tree_size).sum::<u128>()
                                            }
                                            WhileInlined(while_inlined_node) => {
                                                while_inlined_node.cond_instrs.exprs.iter().map(get_tree_size).sum::<u128>()
                                                    + while_inlined_node.body_instrs.exprs.iter().map(get_tree_size).sum::<u128>()
                                            }
                                            OrInlined(or_inlined_node) => {
                                                get_tree_size(&or_inlined_node.first)
                                                    + or_inlined_node.second.exprs.iter().map(get_tree_size).sum::<u128>()
                                            }
                                            AndInlined(and_inlined_node) => {
                                                get_tree_size(&and_inlined_node.first)
                                                    + and_inlined_node.second.exprs.iter().map(get_tree_size).sum::<u128>()
                                            }
                                            ToDoInlined(to_do_inlined_node) => {
                                                get_tree_size(&to_do_inlined_node.start)
                                                    + get_tree_size(&to_do_inlined_node.end)
                                                    + to_do_inlined_node.body.exprs.iter().map(get_tree_size).sum::<u128>()
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        let total_tree_size: u128 = ast_method_def.body.exprs.iter().map(get_tree_size).sum();
                        gc_interface.total_program_repr_size += total_tree_size;
                    }
                    MethodKind::Defined(ast_method_def)
                }
            }
        }
        // match method.signature.as_str() {
        //     // "to:by:do:" => MethodKind::Specialized(MethodKindSpecialized::ToByDo(ToByDoNode {})),
        //     // "downTo:do:" => MethodKind::Specialized(MethodKindSpecialized::DownToDo(DownToDoNode {})),
        //     _ => match method.body {
    }

    pub(crate) fn make_trivial_method_if_possible(method_def: &AstMethodDef, _interner: &mut Interner) -> Option<MethodKind> {
        if method_def.locals_nbr != 0 || method_def.body.exprs.len() != 1 {
            return None;
        }

        let args_nbr = method_def.signature.chars().filter(|e| *e == ':').count();

        match method_def.body.exprs.first()? {
            AstExpression::LocalExit(expr) => {
                if args_nbr != 0 {
                    return None;
                }

                match expr.as_ref() {
                    AstExpression::Literal(lit) => {
                        Some(MethodKind::TrivialLiteral(TrivialLiteralMethod { literal: lit.clone() }))
                        // todo avoid clone by moving code to previous function tbh
                    }
                    AstExpression::GlobalRead(global) => Some(MethodKind::TrivialGlobal(TrivialGlobalMethod { global_name: global.clone() })),
                    AstExpression::FieldRead(idx) => Some(MethodKind::TrivialGetter(TrivialGetterMethod { field_idx: *idx })),
                    _ => None,
                }
            }
            AstExpression::FieldWrite(idx, expr) => {
                if args_nbr != 1 {
                    return None;
                }

                match expr.as_ref() {
                    AstExpression::ArgRead(0, 1) => Some(MethodKind::TrivialSetter(TrivialSetterMethod { field_idx: *idx })),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Transforms a generic MethodDef into an AST-specific one.
    /// Note: public since it's used in tests.
    pub fn parse_method_def(
        method_def: &ast::MethodDef,
        class: Option<Gc<Class>>,
        gc_interface: &mut GCInterface,
        interner: &mut Interner,
    ) -> AstMethodDef {
        let (body, locals_nbr) = match &method_def.body {
            MethodBody::Primitive => {
                unreachable!("unimplemented primitive")
            }
            MethodBody::Body { locals_nbr, body, locals } => {
                let args_nbr = method_def.signature.chars().filter(|e| *e == ':').count(); // not sure if needed

                let mut locals_set: IndexSet<String> = IndexSet::new();
                for local in locals {
                    locals_set.insert(local.clone());
                }

                let mut args_set: IndexSet<String> = IndexSet::new();
                args_set.insert("self".to_string());
                for arg in &method_def.args {
                    args_set.insert(arg.clone());
                }
                let mut ctxt = AstMethodCompilerCtxt {
                    class,
                    scopes: vec![AstScopeCtxt::init(args_nbr, *locals_nbr, locals_set, args_set)],
                    gc_interface,
                    interner,
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
            Expression::Read(global_name) => match self.find_var(&global_name) {
                Some(FoundVar::Local(scope, idx)) => match scope {
                    0 => AstExpression::LocalVarRead(idx),
                    _ => AstExpression::NonLocalVarRead(scope, idx),
                },
                Some(FoundVar::Argument(scope, idx)) => AstExpression::ArgRead(scope, idx),
                Some(FoundVar::Field(idx)) => AstExpression::FieldRead(idx),
                None => self.global_read(global_name),
            },
            Expression::Write(global_name, expr) => match self.find_var(&global_name) {
                Some(FoundVar::Local(scope, idx)) => match scope {
                    0 => {
                        let local_write_expr = AstExpression::LocalVarWrite(idx, Box::new(self.parse_expression(expr.as_ref())));
                        self.maybe_make_inc_or_dec(&local_write_expr).unwrap_or(local_write_expr)
                    }
                    _ => AstExpression::NonLocalVarWrite(scope, idx, Box::new(self.parse_expression(expr.as_ref()))),
                },
                Some(FoundVar::Argument(scope, idx)) => AstExpression::ArgWrite(scope, idx, Box::new(self.parse_expression(expr.as_ref()))),
                Some(FoundVar::Field(idx)) => AstExpression::FieldWrite(idx, Box::new(self.parse_expression(expr.as_ref()))),
                _ => self.resolve_global_write(&global_name, &expr),
            },
            Expression::Message(msg) => self.parse_message(msg.as_ref()),
            Expression::Exit(expr) => {
                let scope = self.scopes.len() - 1;

                match scope {
                    0 => AstExpression::LocalExit(Box::new(self.parse_expression(expr.as_ref()))),
                    _ => AstExpression::NonLocalExit(Box::new(self.parse_expression(expr.as_ref())), scope as u8),
                }
            }
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
                AstExpression::Block(self.gc_interface.alloc(ast_block, AllocSiteMarker::Block))
            }
        }
    }

    pub fn maybe_make_inc_or_dec(&self, local_var_idx: &AstExpression) -> Option<AstExpression> {
        let (a, b) = match local_var_idx {
            AstExpression::LocalVarWrite(a, b) => (a, b),
            _ => unreachable!(),
        };

        if let AstExpression::BinaryDispatch(message) = &**b {
            let signature = self.interner.lookup(message.dispatch_node.signature);

            if (signature == "+" || signature == "-") && message.arg == AstExpression::Literal(AstLiteral::Integer(1)) {
                if let AstExpression::LocalVarRead(local_idx) = message.dispatch_node.receiver {
                    if local_idx == *a {
                        match signature {
                            "+" => {
                                return Some(AstExpression::IncLocal(*a));
                            }
                            "-" => return Some(AstExpression::DecLocal(*a)),
                            _ => unreachable!(),
                        }
                    }
                }
            }
        }

        None
    }

    pub fn parse_body(&mut self, body: &ast::Body) -> AstBody {
        AstBody {
            exprs: body.exprs.iter().map(|expr| self.parse_expression(expr)).collect(),
        }
    }

    pub fn parse_block(&mut self, blk: &ast::Block) -> AstBlock {
        let mut locals_set: IndexSet<String> = IndexSet::new(); // TODO: can we do better than allocating whole new IndexSets, for here and the method?..
        for local in &blk.locals {
            locals_set.insert(local.clone());
        }

        let mut args_set: IndexSet<String> = IndexSet::new();
        args_set.insert("#blockSelf".to_string());
        for arg in &blk.args {
            args_set.insert(arg.clone());
        }
        self.scopes.push(AstScopeCtxt::init(blk.args.len(), blk.locals.len(), locals_set, args_set));

        let body = self.parse_body(&blk.body);
        let bl = self.scopes.last().unwrap();
        let output_blk = AstBlock {
            nbr_args: bl.get_nbr_args() as u8,
            nbr_locals: bl.get_nbr_locals() as u8,
            body,
        };

        self.scopes.pop();
        output_blk
    }

    pub fn parse_message(&mut self, msg: &ast::Message) -> AstExpression {
        let msg = {
            match msg {
                ast::Message::Regular(reg_msg) => reg_msg,
                ast::Message::IfInlined(if_inlined_msg) => {
                    let ast_inlined_node = IfInlinedNode {
                        expected_bool: if_inlined_msg.expected_bool,
                        cond_expr: self.parse_expression(&if_inlined_msg.cond_expr),
                        body_instrs: AstBody {
                            exprs: if_inlined_msg.body_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                    };
                    return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::IfInlined(ast_inlined_node)));
                }
                ast::Message::IfNilInlined(if_nil_inlined_message) => {
                    let ast_inlined_node = IfNilInlinedNode {
                        expects_nil: if_nil_inlined_message.expects_nil,
                        cond_expr: self.parse_expression(&if_nil_inlined_message.cond_expr),
                        body_instrs: AstBody {
                            exprs: if_nil_inlined_message.body_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                    };
                    return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::IfNilInlined(ast_inlined_node)));
                }
                ast::Message::IfTrueIfFalseInlined(if_true_if_false_inlined_message) => {
                    let ast_inlined_node = IfTrueIfFalseInlinedNode {
                        cond_expr: self.parse_expression(&if_true_if_false_inlined_message.cond_expr),
                        body_1_instrs: AstBody {
                            exprs: if_true_if_false_inlined_message.body_1_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                        body_2_instrs: AstBody {
                            exprs: if_true_if_false_inlined_message.body_2_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                        expected_bool: if_true_if_false_inlined_message.expected_bool,
                    };
                    return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::IfTrueIfFalseInlined(ast_inlined_node)));
                }
                ast::Message::IfNilIfNotNilInlined(if_nil_if_not_nil_inlined_message) => {
                    let ast_inlined_node = IfNilIfNotNilInlinedNode {
                        cond_expr: self.parse_expression(&if_nil_if_not_nil_inlined_message.cond_expr),
                        body_1_instrs: AstBody {
                            exprs: if_nil_if_not_nil_inlined_message.body_1_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                        body_2_instrs: AstBody {
                            exprs: if_nil_if_not_nil_inlined_message.body_2_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                        expects_nil: if_nil_if_not_nil_inlined_message.expects_nil,
                    };
                    return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::IfNilIfNotNilInlined(ast_inlined_node)));
                }
                ast::Message::WhileInlined(while_inlined_message) => {
                    let ast_inlined_node = WhileInlinedNode {
                        expected_bool: while_inlined_message.expected_bool,
                        cond_instrs: AstBody {
                            exprs: while_inlined_message.cond_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                        body_instrs: AstBody {
                            exprs: while_inlined_message.body_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                    };

                    return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::WhileInlined(ast_inlined_node)));
                }
                ast::Message::AndOrInlined(and_or_inlined_message) => match and_or_inlined_message.is_and {
                    true => {
                        let ast_inlined_node = AndInlinedNode {
                            first: self.parse_expression(&and_or_inlined_message.first),
                            second: AstBody {
                                exprs: and_or_inlined_message.second.iter().map(|e| self.parse_expression(e)).collect(),
                            },
                        };

                        return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::AndInlined(ast_inlined_node)));
                    }
                    false => {
                        let ast_inlined_node = OrInlinedNode {
                            first: self.parse_expression(&and_or_inlined_message.first),
                            second: AstBody {
                                exprs: and_or_inlined_message.second.iter().map(|e| self.parse_expression(e)).collect(),
                            },
                        };

                        return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::OrInlined(ast_inlined_node)));
                    }
                },
                ast::Message::ToDoInlined(to_do_inlined_message) => {
                    let accumulator_idx = match self.find_var(&to_do_inlined_message.accumulator_name) {
                        Some(FoundVar::Local(0, a)) => a as usize,
                        invalid => panic!("to do inlining couldn't find a valid index for its accumulator: got {:?}", invalid),
                    };

                    let ast_inlined_node = ToDoInlinedNode {
                        start: self.parse_expression(&to_do_inlined_message.start_expr),
                        end: self.parse_expression(&to_do_inlined_message.end_expr),
                        body: AstBody {
                            exprs: to_do_inlined_message.body_instrs.iter().map(|e| self.parse_expression(e)).collect(),
                        },
                        accumulator_idx,
                    };

                    return AstExpression::InlinedCall(Box::new(crate::ast::InlinedNode::ToDoInlined(ast_inlined_node)));
                }
            }
        };

        let interned_signature = self.interner.intern(msg.signature.as_str());

        if msg.receiver == Expression::Read(String::from("super")) {
            return AstExpression::SuperMessage(Box::new(AstSuperMessage {
                super_class: self
                    .class
                    .as_ref()
                    .unwrap()
                    .super_class
                    .clone()
                    .unwrap_or_else(|| panic!("no super class set, even though the method has a super call?")),
                signature: interned_signature,
                values: msg.values.iter().map(|e| self.parse_expression(e)).collect(),
            }));
        }

        let receiver = self.parse_expression(&msg.receiver);
        match msg.values.len() {
            0 => AstExpression::UnaryDispatch(Box::new(AstUnaryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: interned_signature,
                    inline_cache: None,
                },
            })),
            1 => AstExpression::BinaryDispatch(Box::new(AstBinaryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: interned_signature,
                    inline_cache: None,
                },
                arg: self.parse_expression(msg.values.first().unwrap()),
            })),
            2 => AstExpression::TernaryDispatch(Box::new(AstTernaryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: interned_signature,
                    inline_cache: None,
                },
                arg1: self.parse_expression(msg.values.first().unwrap()),
                arg2: self.parse_expression(msg.values.get(1).unwrap()),
            })),
            _ => AstExpression::NAryDispatch(Box::new(AstNAryDispatch {
                dispatch_node: AstDispatchNode {
                    receiver,
                    signature: interned_signature,
                    inline_cache: None,
                },
                values: msg.values.iter().map(|e| self.parse_expression(e)).collect(),
            })),
        }
    }

    pub(crate) fn global_read(&mut self, name: String) -> AstExpression {
        if name.as_str() == "super" {
            return AstExpression::ArgRead((self.scopes.len() - 1) as u8, 0);
        }

        if self.class.is_none() {
            return AstExpression::GlobalRead(Box::new(GlobalNode::from(self.interner.intern(name.as_str()))));
        }

        match self.class.as_ref().unwrap().get_field_offset_by_name(&name) {
            Some(offset) => AstExpression::FieldRead(offset as u8),
            _ => AstExpression::GlobalRead(Box::new(GlobalNode::from(self.interner.intern(name.as_str())))),
        }
    }

    fn resolve_global_write(&mut self, name: &String, expr: &Expression) -> AstExpression {
        if self.class.is_none() {
            panic!(
                "can't turn the GlobalWrite `{}` into a FieldWrite, and GlobalWrite shouldn't exist at runtime",
                name
            );
        }

        match self.class.as_ref().unwrap().get_field_offset_by_name(name) {
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
                let str_ptr = self.gc_interface.alloc(str.clone(), AllocSiteMarker::StringLiteral);
                AstLiteral::String(str_ptr)
            }
            Literal::Symbol(str) => {
                let interned_sym = self.interner.intern(str.as_str());
                AstLiteral::Symbol(interned_sym)
            }
            Literal::Double(double) => AstLiteral::Double(*double),
            Literal::Integer(int) => AstLiteral::Integer(*int),
            Literal::BigInteger(bigint_str) => {
                let bigint_ptr = self.gc_interface.alloc(bigint_str.parse().unwrap(), AllocSiteMarker::BigInt);
                AstLiteral::BigInteger(bigint_ptr)
            }
            Literal::Array(arr) => {
                let arr_ptr = {
                    let arr: Vec<AstLiteral> = arr.iter().map(|lit| self.parse_literal(lit)).collect();
                    self.gc_interface.alloc_slice(arr.as_slice(), AllocSiteMarker::SliceAstLiteral)
                };
                AstLiteral::Array(VecLiteral(arr_ptr))
            }
        }
    }
}
