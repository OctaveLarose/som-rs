use anyhow::Error;
use once_cell::sync::Lazy;

use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::convert::Primitive;

use crate::primitives::PrimInfo;

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use super::*;
    use crate::interpreter::push_block_frame;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> =
        Lazy::new(|| Box::new([("value", self::value.into_func(), true), ("restart", self::restart.into_func(), false)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

    fn value(universe: &mut Universe) -> Result<(), Error> {
        push_block_frame(universe, 1);
        Ok(())
    }

    fn restart(universe: &mut Universe) -> Result<(), Error> {
        // interpreter.current_frame.bytecode_idx = 0;
        universe.bytecode_idx = 0;
        Ok(())
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
    use crate::interpreter::push_block_frame;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([("value:", self::value.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

    fn value(universe: &mut Universe) -> Result<(), Error> {
        push_block_frame(universe, 2);
        Ok(())
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
    use crate::interpreter::push_block_frame;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([("value:with:", self::value_with.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[PrimInfo]>> = Lazy::new(|| Box::new([]));

    fn value_with(universe: &mut Universe) -> Result<(), Error> {
        push_block_frame(universe, 3);
        Ok(())
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
