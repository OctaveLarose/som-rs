use std::fmt::{Debug, Formatter};

pub const SYSTEM_CLASS_NAMES: &[&str; 25] = &[
    "Array", 
    "Block", 
    "Block1", 
    "Block2",
    "Block3",
    "Boolean",
    "Class",
    "Dictionary",
    "Double",
    "False",
    "HashEntry",
    "Hashtable",
    "Integer",
    "Metaclass",
    "Method",
    "Nil",
    "Object",
    "Pair",
    "Primitive",
    "Set",
    "String",
    "Symbol",
    "System",
    "True",
    "Vector"
];


pub trait UniverseForParser {
    fn load_class_and_get_all_fields(&mut self, class_name: &str) -> (Vec<String>, Vec<String>);
}

impl Debug for dyn UniverseForParser {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Universe for parser")
    }
}