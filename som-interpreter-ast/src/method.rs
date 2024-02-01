use som_core::ast;

use crate::class::Class;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::{SOMRef, SOMWeakRef};
use crate::specialized::while_node::WhileNode;
use crate::specialized::if_node::IfNode;
use crate::specialized::if_true_if_false_node::IfTrueIfFalseNode;

// #[derive(Clone)]
// pub struct MethodEnv {
//     pub ast: ast::MethodDef,
//     pub inline_cache: Vec<Option<(*const Class, SOMRef<Method>)>>
//     // pub inline_cache: RefCell<Vec<Option<(*const Class, SOMRef<Method>)>>>
// }

/// The kind of a class method.
#[derive(Clone)]
pub enum MethodKind {
    /// A user-defined method from the AST.
    // Defined(MethodEnv),
    Defined(ast::GenericMethodDef),
    /// An interpreter primitive.
    Primitive(PrimitiveFn),
    /// Specialized: whileTrue/whileFalse node.
    WhileInlined(WhileNode),
    /// Specialized: ifTrue/ifFalse.
    IfInlined(IfNode),
    /// Specialized: ifTrue/ifFalse.
    IfTrueIfFalseInlined(IfTrueIfFalseNode),
    /// A non-implemented primitive.
    NotImplemented(String),
}

impl MethodKind {
    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }
}

/// Represents a class method.
#[derive(Clone)]
pub struct Method {
    pub kind: MethodKind,
    pub holder: SOMWeakRef<Class>,
    pub signature: String,
}

impl Method {
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        if self.is_primitive() {
            universe.primitive_class()
        } else {
            universe.method_class()
        }
    }

    pub fn kind(&self) -> &MethodKind {
        &self.kind
    }

    pub fn holder(&self) -> &SOMWeakRef<Class> {
        &self.holder
    }

    pub fn signature(&self) -> &str {
        self.signature.as_str()
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }
}
