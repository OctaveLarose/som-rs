use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::UniverseBC;
use crate::value::Value;
use crate::expect_args;

/// Primitives for the **Block** and **Block1** class.
pub mod block1 {
    use std::rc::Rc;
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
        ("value", self::value, true),
        ("restart", self::restart, false),
    ];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];
    
    fn value(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
        const SIGNATURE: &str = "Block1>>#value";

        expect_args!(SIGNATURE, args, [Value::Block(block)]);

        interpreter.push_block_frame(Rc::clone(&block), vec![Value::Block(block.clone())]);
        
        // match interpreter.stack.pop() {
        //     Some(Value::Block(block)) => interpreter.push_block_frame(Rc::clone(&block), vec![Value::Block(block)]),
        //     _ => panic!("Expected a blockself when calling a block")
        // };
    }

    pub fn restart(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
        const SIGNATURE: &str = "Block>>#restart";

        expect_args!(SIGNATURE, args, [Value::Block(_)]);

        // let frame = interpreter.current_frame().expect("no current frame");
        interpreter.bytecode_idx = 0;
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
    use std::rc::Rc;
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[("value:", self::value, true)];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
        const SIGNATURE: &str = "Block2>>#value:";

        expect_args!(SIGNATURE, args, [
            Value::Block(block),
            argument
        ]);

        interpreter.push_block_frame(Rc::clone(&block), vec![Value::Block(block.clone()), argument.clone()]);
        
        // NB: what follows is a potentially sliiiiightly faster way of handling things, but didn't lead to visible speedups, so eh.
        
        // let args = interpreter.stack.split_off(interpreter.stack.len() - 2);
        
        // match args.first() {
        //     Some(Value::Block(block)) => interpreter.push_block_frame(Rc::clone(&block), args),
        //     _ => panic!("Expected a blockself when calling a block")
        // };
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
    use std::rc::Rc;
    use super::*;

    pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] =
        &[("value:with:", self::value_with, true)];
    pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

    fn value_with(interpreter: &mut Interpreter, args: Vec<Value>, _: &mut UniverseBC) {
        const SIGNATURE: &str = "Block3>>#value:with:";
        
        expect_args!(SIGNATURE, args, [
            Value::Block(block),
            argument1,
            argument2
        ]);

        interpreter.push_block_frame(Rc::clone(&block), vec![Value::Block(block.clone()), argument1.clone(), argument2.clone()]);

        // let args = interpreter.stack.split_off(interpreter.stack.len() - 3);

        // match args.first() {
        //     Some(Value::Block(block)) => interpreter.push_block_frame(Rc::clone(&block), args),
        //     _ => panic!("Expected a blockself when calling a block")
        // };
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
