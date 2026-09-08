use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::fmt;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bytecode {
    Dup,
    PushLocal,
    PushNonLocal,
    PushArg,
    PushNonLocalArg,
    PushField,
    PushBlock,
    PushConstant,
    PushGlobal,
    Inc,
    Dec,
    Push0,
    Push1,
    PushNil,
    PushSelf,
    Pop,
    PopLocal,
    PopArg,
    PopField,
    Send1,
    Send2,
    Send3,
    SendN,
    SuperSend,
    ReturnSelf,
    ReturnLocal,
    ReturnNonLocal,
    Jump,
    JumpBackward,
    JumpOnTrueTopNil,
    JumpOnFalseTopNil,
    JumpOnNilTopTop,
    JumpOnNotNilTopTop,
    JumpOnNilPop,
    JumpOnNotNilPop,
    JumpOnTruePop,
    JumpOnFalsePop,
    JumpIfGreater,
    Dup2,
}

// `u16`s for easy addition to bytecode index.
pub const BC_SIZE_NO_ARGS: u16 = 1;
pub const BC_SIZE_1_ARG: u16 = 2;
pub const BC_SIZE_2_ARG: u16 = 3;
pub const BC_SIZE_U16_ARG: u16 = 3;

enum BcType {
    NoArgs,
    OneArg,
    TwoArg,
    U16Arg,
}

static BYTECODE_MAP: Lazy<HashMap<Bytecode, BcType>> = Lazy::new(|| {
    HashMap::from([
        (Bytecode::Dup, BcType::NoArgs),
        (Bytecode::PushLocal, BcType::OneArg),
        (Bytecode::PushNonLocal, BcType::TwoArg),
        (Bytecode::PushArg, BcType::OneArg),
        (Bytecode::PushNonLocalArg, BcType::TwoArg),
        (Bytecode::PushField, BcType::OneArg),
        (Bytecode::PushBlock, BcType::OneArg),
        (Bytecode::PushConstant, BcType::OneArg),
        (Bytecode::PushGlobal, BcType::OneArg),
        (Bytecode::Inc, BcType::NoArgs),
        (Bytecode::Dec, BcType::NoArgs),
        (Bytecode::Push0, BcType::NoArgs),
        (Bytecode::Push1, BcType::NoArgs),
        (Bytecode::PushNil, BcType::NoArgs),
        (Bytecode::PushSelf, BcType::NoArgs),
        (Bytecode::Pop, BcType::NoArgs),
        (Bytecode::PopLocal, BcType::TwoArg),
        (Bytecode::PopArg, BcType::TwoArg),
        (Bytecode::PopField, BcType::OneArg),
        (Bytecode::Send1, BcType::U16Arg),
        (Bytecode::Send2, BcType::U16Arg),
        (Bytecode::Send3, BcType::U16Arg),
        (Bytecode::SendN, BcType::U16Arg),
        (Bytecode::SuperSend, BcType::U16Arg),
        (Bytecode::ReturnSelf, BcType::NoArgs),
        (Bytecode::ReturnLocal, BcType::NoArgs),
        (Bytecode::ReturnNonLocal, BcType::OneArg),
        (Bytecode::Jump, BcType::U16Arg),
        (Bytecode::JumpBackward, BcType::U16Arg),
        (Bytecode::JumpOnTrueTopNil, BcType::U16Arg),
        (Bytecode::JumpOnFalseTopNil, BcType::U16Arg),
        (Bytecode::JumpOnNilTopTop, BcType::U16Arg),
        (Bytecode::JumpOnNotNilTopTop, BcType::U16Arg),
        (Bytecode::JumpOnNilPop, BcType::U16Arg),
        (Bytecode::JumpOnNotNilPop, BcType::U16Arg),
        (Bytecode::JumpOnTruePop, BcType::U16Arg),
        (Bytecode::JumpOnFalsePop, BcType::U16Arg),
        (Bytecode::JumpIfGreater, BcType::U16Arg),
        (Bytecode::Dup2, BcType::NoArgs),
    ])
});

#[inline(always)]
pub fn read_u16(bytecodes: &[Bytecode], idx: usize) -> u16 {
    (bytecodes[idx + 1] as u16) | ((bytecodes[idx] as u16) << 8)
}

pub fn split_u16(val: u16) -> (u8, u8) {
    let high_byte: u8 = (val >> 8) as u8;
    let low_byte: u8 = (val & 0xff) as u8;
    (high_byte, low_byte)
}

impl Bytecode {
    /// Get the instruction's name padded so that every padded names are of the same length.
    #[rustfmt::skip]
    pub fn padded_name(self) -> &'static str {
        match self {
            Self::Dup                => "DUP                    ",
            Self::Inc                => "INC                    ",
            Self::Dec                => "DEC                    ",
            Self::PushLocal          => "PUSH_LOCAL             ",
            Self::PushNonLocal       => "PUSH_NON_LOCAL         ",
            Self::PushArg            => "PUSH_ARG               ",
            Self::PushNonLocalArg    => "PUSH_NON_LOCAL_ARG     ",
            Self::PushField          => "PUSH_FIELD             ",
            Self::PushBlock          => "PUSH_BLOCK             ",
            Self::PushConstant       => "PUSH_CONSTANT          ",
            Self::PushGlobal         => "PUSH_GLOBAL            ",
            Self::Push0              => "PUSH_0                 ",
            Self::Push1              => "PUSH_1                 ",
            Self::PushNil            => "PUSH_NIL               ",
            Self::PushSelf           => "PUSH_SELF              ",
            Self::Pop                => "POP                    ",
            Self::PopLocal           => "POP_LOCAL              ",
            Self::PopArg             => "POP_ARG                ",
            Self::PopField           => "POP_FIELD              ",
            Self::Send1              => "SEND_1                 ",
            Self::Send2              => "SEND_2                 ",
            Self::Send3              => "SEND_3                 ",
            Self::SendN              => "SEND_N                 ",
            Self::SuperSend          => "SUPER_SEND             ",
            Self::ReturnSelf         => "RETURN_SELF            ",
            Self::ReturnLocal        => "RETURN_LOCAL           ",
            Self::ReturnNonLocal     => "RETURN_NON_LOCAL       ",
            Self::Jump               => "JUMP                   ",
            Self::JumpBackward       => "JUMP_BACKWARD          ",
            Self::JumpOnTrueTopNil   => "JUMP_ON_TRUE_TOP_NIL   ",
            Self::JumpOnFalseTopNil  => "JUMP_ON_FALSE_TOP_NIL  ",
            Self::JumpOnNilTopTop    => "JUMP_ON_NIL_TOP_TOP    ",
            Self::JumpOnNotNilTopTop => "JUMP_ON_NOT_NIL_TOP_TOP",
            Self::JumpOnTruePop      => "JUMP_ON_TRUE_POP       ",
            Self::JumpOnFalsePop     => "JUMP_ON_FALSE_POP      ",
            Self::JumpOnNilPop       => "JUMP_ON_NIL_POP        ",
            Self::JumpOnNotNilPop    => "JUMP_ON_NOT_NIL_POP    ",
            Self::Dup2               => "DUP2                   ",
            Self::JumpIfGreater      => "JUMP_IF_GREATER        ",
        }
    }
}

pub struct BytecodeIter<'a> {
    bytecodes: &'a [Bytecode],
    cur_idx: usize,
}

impl<'a> BytecodeIter<'a> {
    pub fn init(bytecodes: &'a [Bytecode], cur_idx: usize) -> Self {
        Self { bytecodes, cur_idx }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum BcEntry {
    NoArg(Bytecode),
    OneArg(Bytecode, u8),
    TwoArgs(Bytecode, u8, u8),
    U16Arg(Bytecode, u16),
}

impl Iterator for BytecodeIter<'_> {
    type Item = BcEntry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur_idx >= self.bytecodes.len() {
            return None;
        }

        let bc = self.bytecodes[self.cur_idx];
        let bc_type = BYTECODE_MAP.get(&bc).unwrap();

        match bc_type {
            BcType::NoArgs => {
                let ret = BcEntry::NoArg(bc);
                self.cur_idx += BC_SIZE_NO_ARGS as usize;
                Some(ret)
            }
            BcType::OneArg => {
                let arg = self.bytecodes[self.cur_idx + 1] as u8;
                let ret = BcEntry::OneArg(bc, arg);
                self.cur_idx += BC_SIZE_1_ARG as usize;
                Some(ret)
            }
            BcType::TwoArg => {
                let arg1 = self.bytecodes[self.cur_idx + 1] as u8;
                let arg2 = self.bytecodes[self.cur_idx + 2] as u8;
                let ret = BcEntry::TwoArgs(bc, arg1, arg2);
                self.cur_idx += BC_SIZE_2_ARG as usize;
                Some(ret)
            }
            BcType::U16Arg => {
                let big_arg = read_u16(self.bytecodes, self.cur_idx + 1);
                let ret = BcEntry::U16Arg(bc, big_arg);
                self.cur_idx += BC_SIZE_U16_ARG as usize;
                Some(ret)
            }
        }
    }
}

impl fmt::Display for Bytecode {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: remove this hack. maybe we ditch bytecode display entirely
        let idx = 42;
        let up_idx = 42;
        let scope = 42;
        match self {
            Self::Dup                       => write!(f, "DUP"),
            Self::Inc                       => write!(f, "INC"),
            Self::Dec                       => write!(f, "DEC"),
            Self::PushLocal    => write!(f, "PUSH_LOCAL {}", idx),
            Self::PushNonLocal    => write!(f, "PUSH_NON_LOCAL {}, {}", up_idx, idx),
            Self::PushArg => write!(f, "PUSH_ARG {}", idx),
            Self::PushNonLocalArg => write!(f, "PUSH_NON_LOCAL_ARG {}, {}", up_idx, idx),
            Self::PushField            => write!(f, "PUSH_FIELD {}", idx),
            Self::PushBlock            => write!(f, "PUSH_BLOCK {}", idx),
            Self::PushConstant         => write!(f, "PUSH_CONSTANT {}", idx),
            Self::PushGlobal         => write!(f, "PUSH_GLOBAL {}", idx),
            Self::Push0                         => write!(f, "PUSH_0"),
            Self::Push1                         => write!(f, "PUSH_1"),
            Self::PushNil                       => write!(f, "PUSH_NIL"),
            Self::PushSelf                      => write!(f, "PUSH_SELF"),
            Self::Pop                           => write!(f, "POP"),
            Self::PopLocal     => write!(f, "POP_LOCAL {}, {}", up_idx, idx),
            Self::PopArg  => write!(f, "POP_ARG {}, {}", up_idx, idx),
            Self::PopField             => write!(f, "POP_FIELD {}", idx),
            Self::Send1                 => write!(f, "SEND_1 {}", idx),
            Self::Send2                 => write!(f, "SEND_2 {}", idx),
            Self::Send3                 => write!(f, "SEND_3 {}", idx),
            Self::SendN                 => write!(f, "SEND_N {}", idx),
            Self::SuperSend            => write!(f, "SUPER_SEND {}", idx),
            Self::ReturnSelf                    => write!(f, "RETURN_SELF", ),
            Self::ReturnLocal               => write!(f, "RETURN_LOCAL", ),
            Self::ReturnNonLocal      => write!(f, "RETURN_NON_LOCAL {}", scope),
            Self::Jump             => write!(f, "JUMP {}", idx),
            Self::JumpBackward             => write!(f, "JUMP_BACKWARD {}", idx),
            Self::JumpOnFalseTopNil => write!(f, "JUMP_ON_FALSE_TOP_NIL {}", idx),
            Self::JumpOnTrueTopNil => write!(f, "JUMP_ON_TRUE_TOP_NIL {}", idx),
            Self::JumpOnNilTopTop    => write!(f, "JUMP_ON_NIL_TOP_TOP {}", idx),
            Self::JumpOnNotNilTopTop => write!(f, "JUMP_ON_NOT_NIL_TOP_TOP {}", idx),
            Self::JumpOnTruePop => write!(f, "JUMP_ON_TRUE_POP {}", idx),
            Self::JumpOnFalsePop => write!(f, "JUMP_ON_FALSE_POP {}", idx),
            Self::JumpOnNilPop => write!(f, "JUMP_ON_NIL_POP {}", idx),
            Self::JumpOnNotNilPop => write!(f, "JUMP_ON_NOT_NIL_POP {}", idx),
            Self::Dup2                  => write!(f, "DUP2"),
            Self::JumpIfGreater      => write!(f, "JUMP_IF_GREATER {}", idx),
        }
    }
}
