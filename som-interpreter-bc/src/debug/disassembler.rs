use crate::compiler::Literal;
use crate::universe::Universe;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::method::MethodInfo;
use som_core::bytecode::{BcEntry, Bytecode, BytecodeIter, BC_SIZE_1_ARG, BC_SIZE_2_ARG, BC_SIZE_NO_ARGS, BC_SIZE_U16_ARG};
use som_value::interned::Interned;

pub fn disassemble_method_body(universe: &Universe, class: &Class, env: &MethodInfo) {
    disassemble_body(universe, class, 1, &mut vec![env]);
    #[cfg(not(feature = "frame-debug-info"))]
    eprintln!("------- Used disassembler without debug symbols. While it could be possible, it's likely not desired. -------");
}

fn disassemble_body(universe: &Universe, class: &Class, level: usize, env: &mut Vec<&dyn FrameEnv>) {
    let padding = "  |".repeat(level);
    let current = env.last().copied().unwrap();
    let bc_iter = BytecodeIter::init(current.get_body(), 0);
    let mut cur_idx = 0;
    for bytecode in bc_iter {
        let extra_spaces_nbr = if cur_idx >= 100 {
            0
        } else if (10..=99).contains(&cur_idx) {
            1
        } else {
            2
        };

        match bytecode {
            BcEntry::NoArg(bc) | BcEntry::OneArg(bc, _) | BcEntry::TwoArgs(bc, _, _) | BcEntry::U16Arg(bc, _) => {
                print!("{cur_idx} {0} {padding} {1}", " ".repeat(extra_spaces_nbr), bc.padded_name());
            }
        };

        match bytecode {
            BcEntry::NoArg(_) => {
                println!();
            }
            BcEntry::OneArg(Bytecode::PushLocal, idx) => {
                print!(" {idx}");
                let local_str = env.last().unwrap().resolve_local(idx);
                println!(" (`{0}`)", local_str);
            }
            BcEntry::TwoArgs(Bytecode::PushNonLocal, up_idx, idx) | BcEntry::TwoArgs(Bytecode::PopLocal, up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                let local_str = env.iter().rev().nth(usize::from(up_idx)).map(|env| env.resolve_local(idx));
                println!(" (`{0}`)", local_str.unwrap()); // code's kinda all over the place, it was a quick and easy refactor. could/should be cleaned
            }
            BcEntry::OneArg(Bytecode::PushField, idx) | BcEntry::OneArg(Bytecode::PopField, idx) => {
                print!(" {idx}");
                let Some(name) = class.field_names.get(usize::from(idx)) else {
                    println!(" (invalid field)");
                    continue;
                };
                println!(" (`{0}`)", universe.lookup_symbol(*name));
            }
            BcEntry::OneArg(Bytecode::PushArg, idx) => {
                print!(" {idx}");
                let arg_str = env.last().unwrap().resolve_argument(idx);
                println!(" (`{0}`)", arg_str);
            }
            BcEntry::TwoArgs(Bytecode::PushNonLocalArg, up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                let arg_str = (env.iter().rev().nth(usize::from(up_idx))).map(|env| env.resolve_argument(idx));
                println!(" (`{0}`)", arg_str.unwrap());
            }
            BcEntry::OneArg(Bytecode::PushBlock, idx) => {
                print!(" {idx}");
                let Some(Literal::Block(blk)) = current.resolve_literal(idx) else {
                    println!("({padding}  | (invalid block)");
                    continue;
                };
                println!();
                env.push(&**blk);
                disassemble_body(universe, class, level + 1, env);
                env.pop();
            }
            BcEntry::OneArg(Bytecode::PushConstant, idx) => {
                print!(" {idx}");
                let Some(literal) = current.resolve_literal(idx) else {
                    println!(" (invalid constant)");
                    continue;
                };
                match literal {
                    Literal::Symbol(symbol) => {
                        println!(" (Symbol(#{0}))", universe.lookup_symbol(*symbol));
                    }
                    _ => {
                        println!(" ({literal:?})");
                    }
                }
            }
            BcEntry::OneArg(Bytecode::PushGlobal, idx) => {
                print!(" {idx}");
                let Some(Literal::Symbol(signature)) = current.resolve_literal(idx) else {
                    println!(" (invalid global)");
                    continue;
                };
                println!(" (`{0}`)", universe.lookup_symbol(*signature));
            }
            BcEntry::TwoArgs(Bytecode::PopArg, up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                let arg_str = (env.iter().rev().nth(usize::from(up_idx))).map(|env| env.resolve_argument(idx));
                println!(" (`{0}`)", arg_str.unwrap());
            }
            BcEntry::U16Arg(Bytecode::Send1, idx)
            | BcEntry::U16Arg(Bytecode::Send2, idx)
            | BcEntry::U16Arg(Bytecode::Send3, idx)
            | BcEntry::U16Arg(Bytecode::SendN, idx)
            | BcEntry::U16Arg(Bytecode::SuperSend, idx) => {
                print!(" {idx}");
                println!(" (#{0})", universe.lookup_symbol(Interned(idx)));
            }
            BcEntry::OneArg(Bytecode::ReturnNonLocal, up_idx) => {
                println!(" {}", up_idx);
            }
            BcEntry::U16Arg(Bytecode::Jump, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnFalsePop, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnTruePop, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnFalseTopNil, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnTrueTopNil, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnNilTopTop, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnNotNilTopTop, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnNilPop, idx)
            | BcEntry::U16Arg(Bytecode::JumpOnNotNilPop, idx)
            | BcEntry::U16Arg(Bytecode::JumpIfGreater, idx) => {
                println!(" {} (jump to bytecode index {})", idx, cur_idx + idx as usize);
            }
            BcEntry::U16Arg(Bytecode::JumpBackward, idx) => {
                println!(" {} (jump to bytecode index {})", idx, cur_idx - idx as usize);
            }
            a => panic!("Unhandled bytecode {:?}", a),
        }

        match bytecode {
            BcEntry::NoArg(..) => cur_idx += BC_SIZE_NO_ARGS as usize,
            BcEntry::OneArg(..) => cur_idx += BC_SIZE_1_ARG as usize,
            BcEntry::TwoArgs(..) => cur_idx += BC_SIZE_2_ARG as usize,
            BcEntry::U16Arg(..) => cur_idx += BC_SIZE_U16_ARG as usize,
        }
    }
}

trait FrameEnv {
    fn get_body(&self) -> &[Bytecode];
    fn resolve_local(&self, idx: u8) -> String;
    fn resolve_literal(&self, idx: u8) -> Option<&Literal>;
    fn resolve_argument(&self, idx: u8) -> String;
}

impl FrameEnv for MethodInfo {
    fn get_body(&self) -> &[Bytecode] {
        &self.body
    }
    #[cfg(feature = "frame-debug-info")]
    fn resolve_local(&self, idx: u8) -> String {
        match self.block_debug_info.locals.get(usize::from(idx)) {
            None => String::from("(local not found)"),
            Some(s) => s.clone(),
        }
    }

    #[cfg(not(feature = "frame-debug-info"))]
    fn resolve_local(&self, _idx: u8) -> String {
        String::from("(unknown local - no debug info)")
    }

    fn resolve_literal(&self, idx: u8) -> Option<&Literal> {
        self.literals.get(usize::from(idx))
    }

    #[cfg(feature = "frame-debug-info")]
    fn resolve_argument(&self, idx: u8) -> String {
        match self.block_debug_info.parameters.get(usize::from(idx)) {
            None => String::from("(argument not found)"),
            Some(s) => s.clone(),
        }
    }

    #[cfg(not(feature = "frame-debug-info"))]
    fn resolve_argument(&self, _idx: u8) -> String {
        String::from("(unknown argument - no debug info)")
    }
}

impl FrameEnv for Block {
    fn get_body(&self) -> &[Bytecode] {
        &self.blk_info.body
    }
    #[cfg(feature = "frame-debug-info")]
    fn resolve_local(&self, idx: u8) -> String {
        match self.blk_info.get_block_debug_info().locals.get(usize::from(idx)) {
            None => String::from("(local not found)"),
            Some(s) => s.clone(),
        }
    }

    #[cfg(not(feature = "frame-debug-info"))]
    fn resolve_local(&self, _idx: u8) -> String {
        String::from("(unknown local)")
    }

    fn resolve_literal(&self, idx: u8) -> Option<&Literal> {
        self.blk_info.literals.get(usize::from(idx))
    }

    #[cfg(feature = "frame-debug-info")]
    fn resolve_argument(&self, idx: u8) -> String {
        match self.blk_info.get_block_debug_info().parameters.get(usize::from(idx)) {
            None => String::from("(argument not found)"),
            Some(s) => s.clone(),
        }
    }

    #[cfg(not(feature = "frame-debug-info"))]
    fn resolve_argument(&self, _idx: u8) -> String {
        String::from("(unknown argument - no debug info)")
    }
}
