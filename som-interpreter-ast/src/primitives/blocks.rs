use crate::expect_args;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseAST;
use crate::value::Value;

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use crate::evaluate::Evaluate;
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
        ("value", self::value, true),
        ("restart", self::restart, false),
    ];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let block = match args.first() {
            Some(Value::Block(b)) => b.clone(),
            _ => panic!("Calling value: on a block... not on a block?")
        };

        let nbr_locals = block.borrow().block.borrow().nbr_locals;
        universe.with_frame(
            nbr_locals,
            args,
            |universe| block.borrow_mut().evaluate(universe),
        )
    }

    // TODO: with inlining, this is never called. Maybe it could be removed for better perf since we could forego Return::Restart? but this wouldn't be fully valid interpreter behaviour.
    fn restart(_: &mut UniverseAST, args: Vec<Value>) -> Return {
        const SIGNATURE: &str = "Block>>#restart";

        expect_args!(SIGNATURE, args, [Value::Block(_)]);

        Return::Restart
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}

/// Primitives for the **Block2** class.
pub mod block2 {
    use crate::evaluate::Evaluate;
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[("value:", self::value, true)];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let block = match args.first() {
            Some(Value::Block(b)) => b.clone(),
            _ => panic!("Calling value: on a block... not on a block?")
        };

        let nbr_locals = block.borrow().block.borrow().nbr_locals;
        
        universe.with_frame(
            nbr_locals,
            args,
            |universe| block.borrow_mut().evaluate(universe),
        )
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}

/// Primitives for the **Block3** class.
pub mod block3 {
    use crate::evaluate::Evaluate;
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] =
        &[("value:with:", self::value_with, true)];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value_with(universe: &mut UniverseAST, args: Vec<Value>) -> Return {
        let block = match args.first() {
            Some(Value::Block(b)) => b.clone(),
            _ => panic!("Calling value: on a block... not on a block?")
        };

        let nbr_locals = block.borrow().block.borrow().nbr_locals;
        
        universe.with_frame(
            nbr_locals,
            args,
            |universe| block.borrow_mut().evaluate(universe),
        )
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}
