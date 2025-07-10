use som_core::bytecode::Bytecode;

use crate::compiler::Literal;
use crate::universe::Universe;
use crate::vm_objects::block::Block;
use crate::vm_objects::class::Class;
use crate::vm_objects::method::MethodInfo;

pub fn disassemble_method_body(universe: &Universe, class: &Class, env: &MethodInfo) {
    disassemble_body(universe, class, 1, &mut vec![env]);
    #[cfg(not(feature = "frame-debug-info"))]
    eprintln!("------- Used disassembler without debug symbols. While it could be possible, it's likely not desired. -------");
}

fn disassemble_body(universe: &Universe, class: &Class, level: usize, env: &mut Vec<&dyn FrameEnv>) {
    let padding = "  |".repeat(level);
    let current = env.last().copied().unwrap();
    for (cur_idx, bytecode) in current.get_body().iter().copied().enumerate() {
        let extra_spaces_nbr = if cur_idx >= 100 {
            0
        } else if (10..=99).contains(&cur_idx) {
            1
        } else {
            2
        };
        print!("{cur_idx} {0} {padding} {1}", " ".repeat(extra_spaces_nbr), bytecode.padded_name());
        // print!("{padding} {0}", bytecode.padded_name());

        match bytecode {
            Bytecode::Dup | Bytecode::Dup2 => {
                println!();
            }
            Bytecode::Inc => {
                println!();
            }
            Bytecode::Dec => {
                println!();
            }
            Bytecode::PushLocal(idx) => {
                print!(" {idx}");
                let local_str = env.last().unwrap().resolve_local(idx);
                println!(" (`{0}`)", local_str);
            }
            Bytecode::PushNonLocal(up_idx, idx) | Bytecode::PopLocal(up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                let local_str = env.iter().rev().nth(usize::from(up_idx)).map(|env| env.resolve_local(idx));
                println!(" (`{0}`)", local_str.unwrap()); // code's kinda all over the place, it was a quick and easy refactor. could/should be cleaned
            }
            Bytecode::PushField(idx) | Bytecode::PopField(idx) => {
                print!(" {idx}");
                let Some(name) = class.field_names.get(usize::from(idx)) else {
                    println!(" (invalid field)");
                    continue;
                };
                println!(" (`{0}`)", universe.lookup_symbol(*name));
            }
            Bytecode::PushArg(idx) => {
                print!(" {idx}");
                let arg_str = env.last().unwrap().resolve_argument(idx);
                println!(" (`{0}`)", arg_str);
            }
            Bytecode::PushNonLocalArg(up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                let arg_str = (env.iter().rev().nth(usize::from(up_idx))).map(|env| env.resolve_argument(idx));
                println!(" (`{0}`)", arg_str.unwrap());
            }
            Bytecode::PushBlock(idx) => {
                print!(" {idx}");
                let Some(Literal::Block(blk)) = current.resolve_literal(idx) else {
                    println!("({padding}  | (invalid block)");
                    continue;
                };
                println!(" - (max stack size: {})", blk.blk_info.get_env().max_stack_size);
                env.push(&**blk);
                disassemble_body(universe, class, level + 1, env);
                env.pop();
            }
            Bytecode::PushConstant(idx) => {
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
            Bytecode::PushSelf => {
                println!();
            }
            Bytecode::PushGlobal(idx) => {
                print!(" {idx}");
                let Some(Literal::Symbol(signature)) = current.resolve_literal(idx) else {
                    println!(" (invalid global)");
                    continue;
                };
                println!(" (`{0}`)", universe.lookup_symbol(*signature));
            }
            Bytecode::Pop => {
                println!();
            }
            Bytecode::PopArg(up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                let arg_str = (env.iter().rev().nth(usize::from(up_idx))).map(|env| env.resolve_argument(idx));
                println!(" (`{0}`)", arg_str.unwrap());
            }
            Bytecode::Send1(idx) | Bytecode::Send2(idx) | Bytecode::Send3(idx) | Bytecode::SendN(idx) | Bytecode::SuperSend(idx) => {
                print!(" {idx}");
                println!(" (#{0})", universe.lookup_symbol(idx));
            }
            Bytecode::ReturnLocal | Bytecode::ReturnSelf => {
                println!();
            }
            Bytecode::ReturnNonLocal(up_idx) => {
                println!(" {}", up_idx);
            }
            Bytecode::Jump(idx)
            | Bytecode::JumpOnFalsePop(idx)
            | Bytecode::JumpOnTruePop(idx)
            | Bytecode::JumpOnFalseTopNil(idx)
            | Bytecode::JumpOnTrueTopNil(idx)
            | Bytecode::JumpOnNilTopTop(idx)
            | Bytecode::JumpOnNotNilTopTop(idx)
            | Bytecode::JumpOnNilPop(idx)
            | Bytecode::JumpOnNotNilPop(idx)
            | Bytecode::JumpIfGreater(idx) => {
                println!(" {} (jump to bytecode index {})", idx, cur_idx + idx as usize);
            }
            Bytecode::JumpBackward(idx) => {
                println!(" {} (jump to bytecode index {})", idx, cur_idx - idx as usize);
            }
            Bytecode::Push0 | Bytecode::Push1 | Bytecode::PushNil => {
                println!();
            }
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
        self.literals.get_checked(usize::from(idx))
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
        &self.blk_info.get_env().body
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
        self.blk_info.get_env().literals.get_checked(usize::from(idx))
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
