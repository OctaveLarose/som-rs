use std::hash::{Hash, Hasher};

use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::instance::Instance;
use crate::vm_objects::method::Method;

// impl Hash for ValueEnum {
//     fn hash<H: Hasher>(&self, hasher: &mut H) {
//         NaNBoxedVal::from(self.clone()).hash(hasher)
//     }
// }

impl Hash for Class {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
        self.fields.iter().for_each(|value| {
            value.hash(hasher);
        });
    }
}

impl Hash for Instance {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.class.hash(hasher);
        // todo better hash that actually reads the values
        // self.locals.iter().for_each(|value| {
        //     value.hash(hasher);
        // });
    }
}

impl Hash for Block {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        let blk_info = self.blk_info;
        blk_info.literals.iter().for_each(|it| it.hash(hasher));
        blk_info.nbr_locals.hash(hasher);
        // self.blk_info.locals.iter().for_each(|it| it.hash(hasher));
        blk_info.nbr_params.hash(hasher);
        blk_info.body.iter().for_each(|it| it.hash(hasher));
    }
}

impl Hash for Method {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.holder.hash(hasher);
        hasher.write(b">>");
        self.signature.hash(hasher);
    }
}
