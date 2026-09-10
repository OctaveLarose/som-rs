use once_cell::sync::Lazy;
use std::collections::HashMap;

/// Bytecode structure, mostly just used to have a Bytecode::* namespace for different bytecodes.
pub struct Bytecode;

impl Bytecode {
    pub const DUP: u8 = 0;
    pub const PUSH_LOCAL: u8 = 1;
    pub const PUSH_NON_LOCAL: u8 = 2;
    pub const PUSH_ARG: u8 = 3;
    pub const PUSH_NON_LOCAL_ARG: u8 = 4;
    pub const PUSH_FIELD: u8 = 5;
    pub const PUSH_BLOCK: u8 = 6;
    pub const PUSH_CONSTANT: u8 = 7;
    pub const PUSH_GLOBAL: u8 = 8;
    pub const INC: u8 = 9;
    pub const DEC: u8 = 10;
    pub const PUSH_0: u8 = 11;
    pub const PUSH_1: u8 = 12;
    pub const PUSH_NIL: u8 = 13;
    pub const PUSH_SELF: u8 = 14;
    pub const POP: u8 = 15;
    pub const POP_LOCAL: u8 = 16;
    pub const POP_ARG: u8 = 17;
    pub const POP_FIELD: u8 = 18;
    pub const SEND_1: u8 = 19;
    pub const SEND_2: u8 = 20;
    pub const SEND_3: u8 = 21;
    pub const SEND_N: u8 = 22;
    pub const SUPER_SEND: u8 = 23;
    pub const RETURN_SELF: u8 = 24;
    pub const RETURN_LOCAL: u8 = 25;
    pub const RETURN_NON_LOCAL: u8 = 26;
    pub const JUMP: u8 = 27;
    pub const JUMP_BACKWARD: u8 = 28;
    pub const JUMP_ON_TRUE_TOP_NIL: u8 = 29;
    pub const JUMP_ON_FALSE_TOP_NIL: u8 = 30;
    pub const JUMP_ON_NIL_TOP_TOP: u8 = 31;
    pub const JUMP_ON_NOT_NIL_TOP_TOP: u8 = 32;
    pub const JUMP_ON_NIL_POP: u8 = 33;
    pub const JUMP_ON_NOT_NIL_POP: u8 = 34;
    pub const JUMP_ON_TRUE_POP: u8 = 35;
    pub const JUMP_ON_FALSE_POP: u8 = 36;
    pub const JUMP_IF_GREATER: u8 = 37;
    pub const DUP_2: u8 = 38;
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

static BYTECODE_MAP: Lazy<HashMap<u8, BcType>> = Lazy::new(|| {
    HashMap::from([
        (Bytecode::DUP, BcType::NoArgs),
        (Bytecode::PUSH_LOCAL, BcType::OneArg),
        (Bytecode::PUSH_NON_LOCAL, BcType::TwoArg),
        (Bytecode::PUSH_ARG, BcType::OneArg),
        (Bytecode::PUSH_NON_LOCAL_ARG, BcType::TwoArg),
        (Bytecode::PUSH_FIELD, BcType::OneArg),
        (Bytecode::PUSH_BLOCK, BcType::OneArg),
        (Bytecode::PUSH_CONSTANT, BcType::OneArg),
        (Bytecode::PUSH_GLOBAL, BcType::OneArg),
        (Bytecode::INC, BcType::NoArgs),
        (Bytecode::DEC, BcType::NoArgs),
        (Bytecode::PUSH_0, BcType::NoArgs),
        (Bytecode::PUSH_1, BcType::NoArgs),
        (Bytecode::PUSH_NIL, BcType::NoArgs),
        (Bytecode::PUSH_SELF, BcType::NoArgs),
        (Bytecode::POP, BcType::NoArgs),
        (Bytecode::POP_LOCAL, BcType::TwoArg),
        (Bytecode::POP_ARG, BcType::TwoArg),
        (Bytecode::POP_FIELD, BcType::OneArg),
        (Bytecode::SEND_1, BcType::U16Arg),
        (Bytecode::SEND_2, BcType::U16Arg),
        (Bytecode::SEND_3, BcType::U16Arg),
        (Bytecode::SEND_N, BcType::U16Arg),
        (Bytecode::SUPER_SEND, BcType::U16Arg),
        (Bytecode::RETURN_SELF, BcType::NoArgs),
        (Bytecode::RETURN_LOCAL, BcType::NoArgs),
        (Bytecode::RETURN_NON_LOCAL, BcType::OneArg),
        (Bytecode::JUMP, BcType::U16Arg),
        (Bytecode::JUMP_BACKWARD, BcType::U16Arg),
        (Bytecode::JUMP_ON_TRUE_TOP_NIL, BcType::U16Arg),
        (Bytecode::JUMP_ON_FALSE_TOP_NIL, BcType::U16Arg),
        (Bytecode::JUMP_ON_NIL_TOP_TOP, BcType::U16Arg),
        (Bytecode::JUMP_ON_NOT_NIL_TOP_TOP, BcType::U16Arg),
        (Bytecode::JUMP_ON_NIL_POP, BcType::U16Arg),
        (Bytecode::JUMP_ON_NOT_NIL_POP, BcType::U16Arg),
        (Bytecode::JUMP_ON_TRUE_POP, BcType::U16Arg),
        (Bytecode::JUMP_ON_FALSE_POP, BcType::U16Arg),
        (Bytecode::JUMP_IF_GREATER, BcType::U16Arg),
        (Bytecode::DUP_2, BcType::NoArgs),
    ])
});

#[inline(always)]
pub fn read_u16(bytecodes: &[u8], idx: usize) -> u16 {
    (bytecodes[idx + 1] as u16) | ((bytecodes[idx] as u16) << 8)
}

pub fn split_u16(val: u16) -> (u8, u8) {
    let high_byte: u8 = (val >> 8) as u8;
    let low_byte: u8 = (val & 0xff) as u8;
    (high_byte, low_byte)
}

impl Bytecode {
    pub fn get_iter(bytecodes: &[u8]) -> BytecodeIter {
        BytecodeIter { bytecodes, cur_idx: 0 }
    }

    /// Get the instruction's name padded so that every padded names are of the same length.
    #[rustfmt::skip]
    pub fn padded_name(bc: u8) -> &'static str {
        match bc {
            Self::DUP                       => "DUP                    ",
            Self::INC                       => "INC                    ",
            Self::DEC                       => "DEC                    ",
            Self::PUSH_LOCAL                => "PUSH_LOCAL             ",
            Self::PUSH_NON_LOCAL            => "PUSH_NON_LOCAL         ",
            Self::PUSH_ARG                  => "PUSH_ARG               ",
            Self::PUSH_NON_LOCAL_ARG        => "PUSH_NON_LOCAL_ARG     ",
            Self::PUSH_FIELD                => "PUSH_FIELD             ",
            Self::PUSH_BLOCK                => "PUSH_BLOCK             ",
            Self::PUSH_CONSTANT             => "PUSH_CONSTANT          ",
            Self::PUSH_GLOBAL               => "PUSH_GLOBAL            ",
            Self::PUSH_0                    => "PUSH_0                 ",
            Self::PUSH_1                    => "PUSH_1                 ",
            Self::PUSH_NIL                  => "PUSH_NIL               ",
            Self::PUSH_SELF                 => "PUSH_SELF              ",
            Self::POP                       => "POP                    ",
            Self::POP_LOCAL                 => "POP_LOCAL              ",
            Self::POP_ARG                   => "POP_ARG                ",
            Self::POP_FIELD                 => "POP_FIELD              ",
            Self::SEND_1                    => "SEND_1                 ",
            Self::SEND_2                    => "SEND_2                 ",
            Self::SEND_3                    => "SEND_3                 ",
            Self::SEND_N                    => "SEND_N                 ",
            Self::SUPER_SEND                => "SUPER_SEND             ",
            Self::RETURN_SELF               => "RETURN_SELF            ",
            Self::RETURN_LOCAL              => "RETURN_LOCAL           ",
            Self::RETURN_NON_LOCAL          => "RETURN_NON_LOCAL       ",
            Self::JUMP                      => "JUMP                   ",
            Self::JUMP_BACKWARD             => "JUMP_BACKWARD          ",
            Self::JUMP_ON_TRUE_TOP_NIL      => "JUMP_ON_TRUE_TOP_NIL   ",
            Self::JUMP_ON_FALSE_TOP_NIL     => "JUMP_ON_FALSE_TOP_NIL  ",
            Self::JUMP_ON_NIL_TOP_TOP       => "JUMP_ON_NIL_TOP_TOP    ",
            Self::JUMP_ON_NOT_NIL_TOP_TOP   => "JUMP_ON_NOT_NIL_TOP_TOP",
            Self::JUMP_ON_TRUE_POP          => "JUMP_ON_TRUE_POP       ",
            Self::JUMP_ON_FALSE_POP         => "JUMP_ON_FALSE_POP      ",
            Self::JUMP_ON_NIL_POP           => "JUMP_ON_NIL_POP        ",
            Self::JUMP_ON_NOT_NIL_POP       => "JUMP_ON_NOT_NIL_POP    ",
            Self::DUP_2                     => "DUP2                   ",
            Self::JUMP_IF_GREATER           => "JUMP_IF_GREATER        ",
            invalid_bc => panic!("requested invalid bytecode {}", invalid_bc)
        }
    }
}

pub struct BytecodeIter<'a> {
    bytecodes: &'a [u8],
    cur_idx: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub enum BcEntry {
    NoArg(u8),
    OneArg(u8, u8),
    TwoArgs(u8, u8, u8),
    U16Arg(u8, u16),
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
                let arg = self.bytecodes[self.cur_idx + 1];
                let ret = BcEntry::OneArg(bc, arg);
                self.cur_idx += BC_SIZE_1_ARG as usize;
                Some(ret)
            }
            BcType::TwoArg => {
                let arg1 = self.bytecodes[self.cur_idx + 1];
                let arg2 = self.bytecodes[self.cur_idx + 2];
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
