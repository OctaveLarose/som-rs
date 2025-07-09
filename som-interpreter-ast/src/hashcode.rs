use std::hash::{Hash, Hasher};
use std::ops::Deref;

use crate::value::Value;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;
use num_bigint::BigInt;
use som_gc::gcref::Gc;

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        if self.is_nil() {
            hasher.write(b"#nil#");
        } else if let Some(value) = self.as_boolean() {
            hasher.write(b"#bool#");
            value.hash(hasher);
        } else if let Some(value) = self.as_integer() {
            hasher.write(b"#int#");
            value.hash(hasher);
        } else if let Some(value) = self.as_big_integer::<Gc<BigInt>>() {
            hasher.write(b"#bigint#");
            value.hash(hasher);
        } else if let Some(value) = self.as_double() {
            hasher.write(b"#double#");
            let raw_bytes: &[u8] = unsafe { std::slice::from_raw_parts((&value as *const f64) as *const u8, std::mem::size_of::<f64>()) };
            hasher.write(raw_bytes);
        } else if let Some(value) = self.as_symbol() {
            hasher.write(b"#sym#");
            value.hash(hasher);
        } else if let Some(value) = self.as_string::<Gc<String>>() {
            hasher.write(b"#string#");
            value.hash(hasher);
        } else if let Some(value) = self.as_array() {
            hasher.write(b"#arr#");
            value.0.iter().for_each(|elem| elem.hash(hasher));
        } else if let Some(value) = self.as_block() {
            hasher.write(b"#blk#");
            value.hash(hasher);
        } else if let Some(value) = self.as_class() {
            hasher.write(b"#cls#");
            value.deref().hash(hasher);
        } else if let Some(instance) = self.as_instance() {
            hasher.write(b"#inst#");
            instance.class.hash(hasher);
            for i in 0..instance.class.fields.len() {
                Instance::lookup_field(&instance, i as u8).hash(hasher)
            }
        } else if let Some(value) = self.as_invokable() {
            hasher.write(b"#mthd#");
            value.hash(hasher);
        } else {
            panic!("Unexpected Value variant encountered!");
        }
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
        self.fields.iter().for_each(|val| val.hash(hasher))
    }
}

impl Hash for Instance {
    fn hash<H: Hasher>(&self, _hasher: &mut H) {
        todo!()
    }
}

impl Hash for Block {
    fn hash<H: Hasher>(&self, _hasher: &mut H) {
        todo!()
    }
}

impl Hash for Method {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.holder.hash(hasher);
        hasher.write(b">>");
        self.signature.hash(hasher);
    }
}
