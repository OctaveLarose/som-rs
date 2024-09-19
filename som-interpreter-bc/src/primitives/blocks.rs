use anyhow::Error;
use once_cell::sync::Lazy;

use crate::block::Block;
use crate::convert::Primitive;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseBC;
use crate::value::Value;

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use super::*;
    use som_core::gc::GCRef;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| {
            Box::new([
                ("value", self::value.into_func(), true),
                ("restart", self::restart.into_func(), false),
            ])
        });
    pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([]));

    fn value(
        interpreter: &mut Interpreter,
        universe: &mut UniverseBC,
        receiver: GCRef<Block>,
    ) -> Result<(), Error> {
        const _: &str = "Block1>>#value";

        interpreter.push_block_frame(receiver, vec![Value::Block(receiver)], &mut universe.gc_interface);

        Ok(())
    }

    fn restart(
        interpreter: &mut Interpreter,
        _: &mut UniverseBC,
        _: GCRef<Block>,
    ) -> Result<(), Error> {
        const _: &str = "Block>>#restart";

        interpreter
            .current_frame
            .to_obj()
            .bytecode_idx = 0;

        Ok(())
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}

/// Primitives for the **Block2** class.
pub mod block2 {
    use super::*;
    use som_core::gc::GCRef;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([("value:", self::value.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([]));

    fn value(
        interpreter: &mut Interpreter,
        universe: &mut UniverseBC,
        receiver: GCRef<Block>,
        argument: Value,
    ) -> Result<(), Error> {
        const _: &str = "Block2>>#value:";

        interpreter.push_block_frame(receiver, vec![Value::Block(receiver), argument], &mut universe.gc_interface);

        Ok(())
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}

/// Primitives for the **Block3** class.
pub mod block3 {
    use super::*;
    use som_core::gc::GCRef;

    pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([("value:with:", self::value_with.into_func(), true)]));
    pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
        Lazy::new(|| Box::new([]));

    fn value_with(
        interpreter: &mut Interpreter,
        universe: &mut UniverseBC,
        receiver: GCRef<Block>,
        argument1: Value,
        argument2: Value,
    ) -> Result<(), Error> {
        const _: &str = "Block3>>#value:with:";

        interpreter.push_block_frame(receiver, vec![Value::Block(receiver), argument1, argument2], &mut universe.gc_interface);

        Ok(())
    }

    /// Search for an instance primitive matching the given signature.
    pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        INSTANCE_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }

    /// Search for a class primitive matching the given signature.
    pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
        CLASS_PRIMITIVES
            .iter()
            .find(|it| it.0 == signature)
            .map(|it| it.1)
    }
}
