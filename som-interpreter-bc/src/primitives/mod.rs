mod blocks;

/// Primitives for the **Array** class.
pub mod array;
/// Primitives for the **Class** class.
pub mod class;
/// Primitives for the **Double** class.
pub mod double;
/// Primitives for the **Integer** class.
pub mod integer;
/// Primitives for the **Method** class and the **Primitive** class.
pub mod method;
/// Primitives for the **Object** class.
pub mod object;
/// Primitives for the **String** class.
pub mod string;
/// Primitives for the **Symbol** class.
pub mod symbol;
/// Primitives for the **System** class.
pub mod system;

pub use self::blocks::{block1, block2, block3};
use anyhow::Error;
use once_cell::sync::Lazy;
use crate::convert::Primitive;
use crate::interpreter::Interpreter;
use crate::universe::Universe;

// TODO we're missing several primitive changes from master originally

pub type PrimitiveFn = dyn Fn(&mut Interpreter, &mut Universe) -> Result<(), Error>
+ Send
+ Sync
+ 'static;

pub fn get_class_primitives(
    class_name: &str,
) -> Option<&'static [(&'static str, &'static PrimitiveFn, bool)]> {
    match class_name {
        "Array" => Some(self::array::CLASS_PRIMITIVES.as_ref()),
        "Block1" => Some(self::block1::CLASS_PRIMITIVES.as_ref()),
        "Block2" => Some(self::block2::CLASS_PRIMITIVES.as_ref()),
        "Block3" => Some(self::block3::CLASS_PRIMITIVES.as_ref()),
        "Class" => Some(self::class::CLASS_PRIMITIVES.as_ref()),
        "Double" => Some(self::double::CLASS_PRIMITIVES.as_ref()),
        "Integer" => Some(self::integer::CLASS_PRIMITIVES.as_ref()),
        "Method" => Some(self::method::CLASS_PRIMITIVES.as_ref()),
        "Primitive" => Some(self::method::CLASS_PRIMITIVES.as_ref()),
        "Object" => Some(self::object::CLASS_PRIMITIVES.as_ref()),
        "String" => Some(self::string::CLASS_PRIMITIVES.as_ref()),
        "Symbol" => Some(self::symbol::CLASS_PRIMITIVES.as_ref()),
        "System" => Some(self::system::CLASS_PRIMITIVES.as_ref()),
        _ => None,
    }
}

pub fn get_instance_primitives(
    class_name: &str,
) -> Option<&'static [(&'static str, &'static PrimitiveFn, bool)]> {
    match class_name {
        "Array" => Some(self::array::INSTANCE_PRIMITIVES.as_ref()),
        "Block1" => Some(self::block1::INSTANCE_PRIMITIVES.as_ref()),
        "Block2" => Some(self::block2::INSTANCE_PRIMITIVES.as_ref()),
        "Block3" => Some(self::block3::INSTANCE_PRIMITIVES.as_ref()),
        "Class" => Some(self::class::INSTANCE_PRIMITIVES.as_ref()),
        "Double" => Some(self::double::INSTANCE_PRIMITIVES.as_ref()),
        "Integer" => Some(self::integer::INSTANCE_PRIMITIVES.as_ref()),
        "Method" => Some(self::method::INSTANCE_PRIMITIVES.as_ref()),
        "Primitive" => Some(self::method::INSTANCE_PRIMITIVES.as_ref()),
        "Object" => Some(self::object::INSTANCE_PRIMITIVES.as_ref()),
        "String" => Some(self::string::INSTANCE_PRIMITIVES.as_ref()),
        "Symbol" => Some(self::symbol::INSTANCE_PRIMITIVES.as_ref()),
        "System" => Some(self::system::INSTANCE_PRIMITIVES.as_ref()),
        _ => None,
    }
}

/// Function called for an unimplemented primitive.
fn unimplem_prim_fn(_: &mut Interpreter, _: &mut Universe, _: i32) -> Result<i32, Error> {
    panic!("called an unimplemented primitive")
}

pub static UNIMPLEM_PRIMITIVE: Lazy<Box<&'static PrimitiveFn>> = Lazy::new(|| {
    Box::new(unimplem_prim_fn.into_func())
});