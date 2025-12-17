use super::PrimInfo;
use crate::invokable::Return;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use once_cell::sync::Lazy;

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use super::*;
    use crate::universe::GlobalValueStack;
    use crate::value::convert::Primitive;
    use crate::value::Value;
    use anyhow::Error;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> =
        Lazy::new(|| Box::new([("value", self::value.into_func(), true), ("restart", self::restart.into_func(), false)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

    fn value(universe: &mut Universe, value_stack: &mut GlobalValueStack) -> Result<Return, Error> {
        let nbr_locals = value_stack.last().as_block().unwrap().block.nbr_locals;
        Ok(universe.eval_block_with_frame(value_stack, nbr_locals, 1))
    }

    fn restart(_: Value) -> Result<Return, Error> {
        Ok(Return::Restart)
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
    }
}

/// Primitives for the **Block2** class.
pub mod block2 {
    use super::*;
    use crate::universe::GlobalValueStack;
    use crate::value::convert::Primitive;
    use anyhow::Error;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([("value:", self::value.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

    fn value(universe: &mut Universe, value_stack: &mut GlobalValueStack) -> Result<Return, Error> {
        let nbr_locals = value_stack.iter().nth_back(1).unwrap().as_block().unwrap().block.nbr_locals;
        Ok(universe.eval_block_with_frame(value_stack, nbr_locals, 2))
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
    }
}

/// Primitives for the **Block3** class.
pub mod block3 {
    use super::*;
    use crate::universe::GlobalValueStack;
    use crate::value::convert::Primitive;
    use anyhow::Error;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([("value:with:", self::value_with.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

    fn value_with(universe: &mut Universe, value_stack: &mut GlobalValueStack) -> Result<Return, Error> {
        let nbr_locals = value_stack.iter().nth_back(2).unwrap().as_block().unwrap().block.nbr_locals;
        Ok(universe.eval_block_with_frame(value_stack, nbr_locals, 3))
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES.iter().find(|it| it.0 == signature).map(|it| it.1)
    }
}
