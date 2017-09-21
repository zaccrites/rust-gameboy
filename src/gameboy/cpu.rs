
use std::rc::Rc;
use std::cell::RefCell;

use super::memory::MemoryUnit;




#[derive(Debug, Clone, Copy)]
struct FlagsRegister {
    zero: bool,
    negative: bool,
    half_carry: bool,
    carry: bool,
}

impl FlagsRegister {

    // TODO: Is this function needed?
    fn new() -> FlagsRegister {
        FlagsRegister {
            zero: false,
            negative: false,
            half_carry: false,
            carry: false,
        }
    }

    // TODO: Replace with ::from_byte(u8) method?
    fn write_byte(&mut self, value: u8) {
        self.zero = (value & (1 << 7)) != 0;
        self.negative = (value & (1 << 6)) != 0;
        self.half_carry = (value & (1 << 5)) != 0;
        self.carry = (value & (1 << 4)) != 0;
    }

    fn read_byte(&self) -> u8 {
        ((self.zero as u8) << 7) |
        ((self.negative as u8) << 6) |
        ((self.half_carry as u8) << 5) |
        ((self.carry as u8) << 4)
    }
}




struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    flags: FlagsRegister,
}

impl Registers {
    fn new() -> Registers {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            flags: FlagsRegister::new(),
        }
    }
}



#[derive(Debug, Clone, Copy)]
enum ByteAddress {
    Register(Register),
    DoubleRegisterIndirect(DoubleRegister),
    DoubleRegisterIndirectInc(DoubleRegister),
    DoubleRegisterIndirectDec(DoubleRegister),
    Immediate,
    // ImmediateIndirect,
}

#[derive(Debug, Clone, Copy)]
enum WordAddress {
    DoubleRegister(DoubleRegister),
    ProgramCounter,
    StackPointer,
    Immediate,
}


#[derive(Debug, Clone, Copy)]
enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Flags,
}

#[derive(Debug, Clone, Copy)]
enum DoubleRegister {
    AF,
    BC,
    DE,
    HL,
}




pub struct Cpu<'a> {
    program_counter: u16,
    stack_pointer: u16,
    registers: Registers,

    // TODO: Is this needed? Is it better to just pass it in to
    // the fetch_and_execute function directly? The reference could
    // be made part of the ByteAddress, etc.
    memory: Rc<RefCell<MemoryUnit<'a>>>,
}

impl<'a> Cpu<'a> {

    pub fn new(memory: Rc<RefCell<MemoryUnit<'a>>>) -> Cpu<'a> {
        Cpu {
            program_counter: 0,
            stack_pointer: 0,
            registers: Registers::new(),
            memory: memory,
        }
    }

    pub fn reset(&mut self) {
        // http://problemkaputt.de/pandocs.htm#powerupsequence
        self.memory.borrow_mut().reset();
        self.program_counter = 0x0100;
        self.write_double_register(DoubleRegister::AF, 0x01b0);
        self.write_double_register(DoubleRegister::BC, 0x0013);
        self.write_double_register(DoubleRegister::DE, 0x00d8);
        self.write_double_register(DoubleRegister::HL, 0x014d);
        self.stack_pointer = 0xfffe;
        // TODO: Reset IO ports
    }



    // TODO: Instruction timing. Probably via a table, since calculating it is too much work.
    // TODO: Instruction disassembly printing
    // pub fn fetch_and_execute(&mut self) {
    pub fn fetch_and_execute(&mut self) {
        let opcode = self.memory.borrow().read_byte(self.program_counter);
        self.program_counter += 1;

        match opcode {

            // TODO: Reorganize these instructions

            0x20 => self.jr(JumpCondition::NonZero),
            0x30 => self.jr(JumpCondition::NoCarry),

            0x18 => self.jr(JumpCondition::Unconditional),
            0x28 => self.jr(JumpCondition::Zero),
            0x38 => self.jr(JumpCondition::Carry),

            0x01 => self.ld16(WordAddress::DoubleRegister(DoubleRegister::BC), WordAddress::Immediate),
            0x11 => self.ld16(WordAddress::DoubleRegister(DoubleRegister::DE), WordAddress::Immediate),
            0x21 => self.ld16(WordAddress::DoubleRegister(DoubleRegister::HL), WordAddress::Immediate),
            0x31 => self.ld16(WordAddress::StackPointer, WordAddress::Immediate),

            0x02 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::BC), ByteAddress::Register(Register::A)),
            0x12 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::DE), ByteAddress::Register(Register::A)),
            0x22 => self.ld8(ByteAddress::DoubleRegisterIndirectInc(DoubleRegister::HL), ByteAddress::Register(Register::A)),
            0x32 => self.ld8(ByteAddress::DoubleRegisterIndirectDec(DoubleRegister::HL), ByteAddress::Register(Register::A)),

            0x0e => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Immediate),
            0x1e => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Immediate),
            0x2e => self.ld8(ByteAddress::Register(Register::L), ByteAddress::Immediate),
            0x3e => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Immediate),

            0x0a => self.ld8(ByteAddress::Register(Register::A), ByteAddress::DoubleRegisterIndirect(DoubleRegister::BC)),
            0x1a => self.ld8(ByteAddress::Register(Register::A), ByteAddress::DoubleRegisterIndirect(DoubleRegister::DE)),
            0x2a => self.ld8(ByteAddress::Register(Register::A), ByteAddress::DoubleRegisterIndirectInc(DoubleRegister::HL)),
            0x3a => self.ld8(ByteAddress::Register(Register::A), ByteAddress::DoubleRegisterIndirectDec(DoubleRegister::HL)),

            0x06 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Immediate),
            0x16 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Immediate),
            0x26 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Immediate),
            0x36 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Immediate),

            0x04 => self.inc8(ByteAddress::Register(Register::B)),
            0x14 => self.inc8(ByteAddress::Register(Register::D)),
            0x24 => self.inc8(ByteAddress::Register(Register::H)),
            0x34 => self.inc8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),

            0x05 => self.dec8(ByteAddress::Register(Register::B)),
            0x15 => self.dec8(ByteAddress::Register(Register::D)),
            0x25 => self.dec8(ByteAddress::Register(Register::H)),
            0x35 => self.dec8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),

            0x0c => self.inc8(ByteAddress::Register(Register::C)),
            0x1c => self.inc8(ByteAddress::Register(Register::E)),
            0x2c => self.inc8(ByteAddress::Register(Register::L)),
            0x3c => self.inc8(ByteAddress::Register(Register::A)),

            0x0d => self.dec8(ByteAddress::Register(Register::C)),
            0x1d => self.dec8(ByteAddress::Register(Register::E)),
            0x2d => self.dec8(ByteAddress::Register(Register::L)),
            0x3d => self.dec8(ByteAddress::Register(Register::A)),



            0x07 => self.rlca(),
            0x17 => self.rla(),

            0x0f => self.rrca(),
            0x1f => self.rra(),



            0x40 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::B)),
            0x41 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::C)),
            0x42 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::D)),
            0x43 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::E)),
            0x44 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::H)),
            0x45 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::L)),
            0x46 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x47 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::A)),

            0x48 => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::B)),
            0x49 => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::C)),
            0x4a => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::D)),
            0x4b => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::E)),
            0x4c => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::H)),
            0x4d => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::L)),
            0x4e => self.ld8(ByteAddress::Register(Register::C), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x4f => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::A)),

            0x50 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Register(Register::B)),
            0x51 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Register(Register::C)),
            0x52 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Register(Register::D)),
            0x53 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Register(Register::E)),
            0x54 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Register(Register::H)),
            0x55 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Register(Register::L)),
            0x56 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x57 => self.ld8(ByteAddress::Register(Register::D), ByteAddress::Register(Register::A)),

            0x58 => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Register(Register::B)),
            0x59 => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Register(Register::C)),
            0x5a => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Register(Register::D)),
            0x5b => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Register(Register::E)),
            0x5c => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Register(Register::H)),
            0x5d => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Register(Register::L)),
            0x5e => self.ld8(ByteAddress::Register(Register::E), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x5f => self.ld8(ByteAddress::Register(Register::E), ByteAddress::Register(Register::A)),

            0x60 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::B)),
            0x61 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::C)),
            0x62 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::D)),
            0x63 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::E)),
            0x64 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::H)),
            0x65 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::L)),
            0x66 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x67 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::A)),

            0x68 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::B)),
            0x69 => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::C)),
            0x6a => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::D)),
            0x6b => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::E)),
            0x6c => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::H)),
            0x6d => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::L)),
            0x6e => self.ld8(ByteAddress::Register(Register::H), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x6f => self.ld8(ByteAddress::Register(Register::H), ByteAddress::Register(Register::A)),

            0x70 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Register(Register::B)),
            0x71 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Register(Register::C)),
            0x72 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Register(Register::D)),
            0x73 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Register(Register::E)),
            0x74 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Register(Register::H)),
            0x75 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Register(Register::L)),
            // 0x76 => self.halt(),
            0x77 => self.ld8(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL), ByteAddress::Register(Register::A)),

            0x78 => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Register(Register::B)),
            0x79 => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Register(Register::C)),
            0x7a => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Register(Register::D)),
            0x7b => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Register(Register::E)),
            0x7c => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Register(Register::H)),
            0x7d => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Register(Register::L)),
            0x7e => self.ld8(ByteAddress::Register(Register::A), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x7f => self.ld8(ByteAddress::Register(Register::A), ByteAddress::Register(Register::A)),


            0x70 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::B)),
            0x71 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::C)),
            0x72 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::D)),
            0x73 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::E)),
            0x74 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::H)),
            0x75 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::L)),
            0x76 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x77 => self.ld8(ByteAddress::Register(Register::B), ByteAddress::Register(Register::A)),

            0x78 => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::B)),
            0x79 => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::C)),
            0x7a => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::D)),
            0x7b => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::E)),
            0x7c => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::H)),
            0x7d => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::L)),
            0x7e => self.ld8(ByteAddress::Register(Register::C), ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x7f => self.ld8(ByteAddress::Register(Register::C), ByteAddress::Register(Register::A)),



            // 0x08 => self.ld16(WordAddress::ImmediateIndirect, WordAddress::StackPointer),

            0x80 => self.add(ByteAddress::Register(Register::B)),
            0x81 => self.add(ByteAddress::Register(Register::C)),
            0x82 => self.add(ByteAddress::Register(Register::D)),
            0x83 => self.add(ByteAddress::Register(Register::E)),
            0x84 => self.add(ByteAddress::Register(Register::H)),
            0x85 => self.add(ByteAddress::Register(Register::L)),
            0x86 => self.add(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x87 => self.add(ByteAddress::Register(Register::A)),

            0x88 => self.adc(ByteAddress::Register(Register::B)),
            0x89 => self.adc(ByteAddress::Register(Register::C)),
            0x8a => self.adc(ByteAddress::Register(Register::D)),
            0x8b => self.adc(ByteAddress::Register(Register::E)),
            0x8c => self.adc(ByteAddress::Register(Register::H)),
            0x8d => self.adc(ByteAddress::Register(Register::L)),
            0x8e => self.adc(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x8f => self.adc(ByteAddress::Register(Register::A)),

            0xa0 => self.and(ByteAddress::Register(Register::B)),
            0xa1 => self.and(ByteAddress::Register(Register::C)),
            0xa2 => self.and(ByteAddress::Register(Register::D)),
            0xa3 => self.and(ByteAddress::Register(Register::E)),
            0xa4 => self.and(ByteAddress::Register(Register::H)),
            0xa5 => self.and(ByteAddress::Register(Register::L)),
            0xa6 => self.and(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xa7 => self.and(ByteAddress::Register(Register::A)),

            0xa8 => self.xor(ByteAddress::Register(Register::B)),
            0xa9 => self.xor(ByteAddress::Register(Register::C)),
            0xaa => self.xor(ByteAddress::Register(Register::D)),
            0xab => self.xor(ByteAddress::Register(Register::E)),
            0xac => self.xor(ByteAddress::Register(Register::H)),
            0xad => self.xor(ByteAddress::Register(Register::L)),
            0xae => self.xor(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xaf => self.xor(ByteAddress::Register(Register::A)),


            0xb0 => self.or(ByteAddress::Register(Register::B)),
            0xb1 => self.or(ByteAddress::Register(Register::C)),
            0xb2 => self.or(ByteAddress::Register(Register::D)),
            0xb3 => self.or(ByteAddress::Register(Register::E)),
            0xb4 => self.or(ByteAddress::Register(Register::H)),
            0xb5 => self.or(ByteAddress::Register(Register::L)),
            0xb6 => self.or(ByteAddress::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xb7 => self.or(ByteAddress::Register(Register::A)),

            0xf6 => self.or(ByteAddress::Immediate),
            0xe6 => self.and(ByteAddress::Immediate),

            0xce => self.adc(ByteAddress::Immediate),






            0xc3 => self.jp(JumpCondition::Unconditional),

            0xc2 => self.jp(JumpCondition::NonZero),
            0xd2 => self.jp(JumpCondition::NoCarry),

            0xca => self.jp(JumpCondition::Zero),
            0xda => self.jp(JumpCondition::Carry),



            0xc0 => self.ret(JumpCondition::NonZero),
            0xd0 => self.ret(JumpCondition::NoCarry),

            0xc7 => self.rst(0x00),
            0xd7 => self.rst(0x10),
            0xe7 => self.rst(0x20),
            0xf7 => self.rst(0x30),

            0xc8 => self.ret(JumpCondition::Zero),
            0xd8 => self.ret(JumpCondition::Carry),

            0xc9 => self.ret(JumpCondition::Unconditional),

            0xcf => self.rst(0x08),
            0xdf => self.rst(0x18),
            0xef => self.rst(0x28),
            0xff => self.rst(0x38),





            _ => panic!("Unimplemented opcode: 0x{:02x}", opcode),
        }
    }


    /// Push a word onto the stack.
    fn push_word(&mut self, value: u16) {
        self.memory.borrow_mut().write_word(self.stack_pointer, value);
        let (sp, _) = self.stack_pointer.overflowing_sub(2);
        self.stack_pointer = sp;
    }

    /// Pop a word off the stack.
    fn pop_word(&mut self) -> u16 {
        let (sp, _) = self.stack_pointer.overflowing_add(2);
        self.stack_pointer = sp;
        self.memory.borrow().read_word(self.stack_pointer)
    }

    /// Push a byte onto the stack.
    fn push_byte(&mut self, value: u8) {
        self.memory.borrow_mut().write_byte(self.stack_pointer, value);
        let (sp, _) = self.stack_pointer.overflowing_sub(1);
        self.stack_pointer = sp;
    }

    /// Pop a byte off the stack.
    fn pop_byte(&mut self) -> u8 {
        let (sp, _) = self.stack_pointer.overflowing_sub(1);
        self.stack_pointer = sp;
        self.memory.borrow().read_byte(self.stack_pointer)

    }



    fn write_byte(&mut self, address: ByteAddress, value: u8) {
        match address {
            ByteAddress::Register(Register::A) => self.registers.a = value,
            ByteAddress::Register(Register::B) => self.registers.b = value,
            ByteAddress::Register(Register::C) => self.registers.c = value,
            ByteAddress::Register(Register::D) => self.registers.d = value,
            ByteAddress::Register(Register::E) => self.registers.e = value,
            ByteAddress::Register(Register::H) => self.registers.h = value,
            ByteAddress::Register(Register::L) => self.registers.l = value,
            ByteAddress::Register(Register::Flags) => self.registers.flags.write_byte(value),

            ByteAddress::DoubleRegisterIndirect(double_register) => {
                let address = self.read_double_register(double_register);
                self.memory.borrow_mut().write_byte(address, value)
            },

            ByteAddress::DoubleRegisterIndirectInc(double_register) => {
                let address = self.read_double_register(double_register);
                self.increment_double_register(double_register);
                self.memory.borrow_mut().write_byte(address, value)
            },

            ByteAddress::DoubleRegisterIndirectDec(double_register) => {
                let address = self.read_double_register(double_register);
                self.decrement_double_register(double_register);
                self.memory.borrow_mut().write_byte(address, value)
            }

            // TODO: That this is even possible is a sign the type
            // system is permitting something bad. Can generics help,
            // like how is done with the SDL2 library?
            ByteAddress::Immediate => panic!("Tried to write to immediate byte!"),

        }
    }

    fn read_byte(&mut self, address: ByteAddress) -> u8 {
        match address {
            ByteAddress::Register(Register::A) => self.registers.a,
            ByteAddress::Register(Register::B) => self.registers.b,
            ByteAddress::Register(Register::C) => self.registers.c,
            ByteAddress::Register(Register::D) => self.registers.d,
            ByteAddress::Register(Register::E) => self.registers.e,
            ByteAddress::Register(Register::H) => self.registers.h,
            ByteAddress::Register(Register::L) => self.registers.l,
            ByteAddress::Register(Register::Flags) => self.registers.flags.read_byte(),

            ByteAddress::DoubleRegisterIndirect(double_register) => {
                let address = self.read_double_register(double_register);
                self.memory.borrow().read_byte(address)
            }

            ByteAddress::DoubleRegisterIndirectInc(double_register) => {
                let address = self.read_double_register(double_register);
                self.increment_double_register(double_register);
                self.memory.borrow_mut().read_byte(address)
            },

            ByteAddress::DoubleRegisterIndirectDec(double_register) => {
                let address = self.read_double_register(double_register);
                self.decrement_double_register(double_register);
                self.memory.borrow_mut().read_byte(address)
            }

            ByteAddress::Immediate => {
                let value = self.memory.borrow().read_byte(self.program_counter);
                self.program_counter += 1;
                value
            }
        }
    }


    fn write_word(&mut self, address: WordAddress, value: u16) {
        match address {
            WordAddress::DoubleRegister(double_register) => self.write_double_register(double_register, value),
            WordAddress::ProgramCounter => self.program_counter = value,
            WordAddress::StackPointer => self.stack_pointer = value,

            WordAddress::Immediate => panic!("Tried to write to immediate word!"),
        }
    }

    fn read_word(&mut self, address: WordAddress) -> u16 {
        match address {
            WordAddress::DoubleRegister(double_register) => self.read_double_register(double_register),
            WordAddress::ProgramCounter => self.program_counter,
            WordAddress::StackPointer => self.stack_pointer,

            WordAddress::Immediate => {
                let value = self.memory.borrow().read_word(self.program_counter);
                self.program_counter += 2;
                value
            }
        }
    }


    /// Read the contents of a double register.
    fn read_double_register(&self, double_register: DoubleRegister) -> u16 {
        let (high_byte, low_byte) = match double_register {
            DoubleRegister::AF => (self.registers.a, self.registers.flags.read_byte()),
            DoubleRegister::HL => (self.registers.h, self.registers.l),
            DoubleRegister::BC => (self.registers.b, self.registers.c),
            DoubleRegister::DE => (self.registers.d, self.registers.e),
        };
        ((high_byte as u16) << 8) | (low_byte as u16)
    }

    fn write_double_register(&mut self, double_register: DoubleRegister, value: u16) {
        let high_byte = (value >> 8) as u8;
        let low_byte = (value & 0x0f) as u8;
        match double_register {
            DoubleRegister::AF => {
                self.registers.a = high_byte;
                self.registers.flags.write_byte(low_byte);
            },
            DoubleRegister::HL => {
                self.registers.h = high_byte;
                self.registers.l = low_byte;
            },
            DoubleRegister::BC => {
                self.registers.b = high_byte;
                self.registers.c = low_byte;
            },
            DoubleRegister::DE => {
                self.registers.d = high_byte;
                self.registers.e = low_byte;
            }
        }
    }

    fn increment_double_register(&mut self, double_register: DoubleRegister) {
        let current_value = self.read_double_register(double_register);
        let (new_value, _) = current_value.overflowing_add(1);
        self.write_double_register(double_register, new_value);
    }

    fn decrement_double_register(&mut self, double_register: DoubleRegister) {
        let current_value = self.read_double_register(double_register);
        let (new_value, _) = current_value.overflowing_sub(1);
        self.write_double_register(double_register, new_value);
    }


    fn add(&mut self, operand: ByteAddress) {
        let operand = self.read_byte(operand);
        let half_carry = ((self.registers.a & 0x0f) + (operand & 0x0f)) & 0x10 != 0;
        let (result, overflowed) = self.registers.a.overflowing_add(operand);

        self.registers.a = result;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        self.registers.flags.carry = overflowed;
    }

    fn adc(&mut self, operand: ByteAddress) {
        let operand = self.read_byte(operand);
        let half_carry = ((self.registers.a & 0x0f) + (operand & 0x0f)) & 0x10 != 0;
        let (mut result, overflowed) = self.registers.a.overflowing_add(operand);
        if self.registers.flags.carry {
            result += 1;
        }

        self.registers.a = result;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        self.registers.flags.carry = overflowed;
    }

    fn and(&mut self, operand: ByteAddress) {
        let operand = self.read_byte(operand);

        self.registers.a &= operand;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = true;
        self.registers.flags.carry = false;
    }

    fn or(&mut self, operand: ByteAddress) {
        let operand = self.read_byte(operand);
        self.registers.a |= operand;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = false;
    }

    fn xor(&mut self, operand: ByteAddress) {
        let operand = self.read_byte(operand);
        self.registers.a ^= operand;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = false;
    }


    fn inc8(&mut self, operand: ByteAddress) {
        let start_value = self.read_byte(operand);
        let (new_value, overflowed) = start_value.overflowing_add(1);
        let half_carry = (start_value & 0x0f) == 0x0f;

        self.registers.a = new_value;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        // Carry flag not affected
    }

    fn dec8(&mut self, operand: ByteAddress) {
        let start_value = self.read_byte(operand);
        let (new_value, overflowed) = start_value.overflowing_sub(1);
        let half_carry = (start_value & 0x0f) == 0;

        self.registers.a = new_value;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        // Carry flag not affected
    }


    // Rotates and Shifts
    // See "Rotates & Shifts" section for awesome diagrams.
    // http://gameboy.mongenel.com/dmg/opcodes.html

    fn rlca(&mut self) {
        /*
                    +--------------+
             ___    |    ______    |
            |CY|<---+---|7<--0|<---+
        */

        let old_value = self.registers.a;
        self.registers.a = (old_value << 1) | (old_value >> 7);

        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = self.registers.a & 0x80 == 0x80;
    }

    fn rla(&mut self) {
        /*
           +--------------------------+
           |    ___         ______    |
           +---|CY|<-------|7<--0|<---+
        */

        let old_value = self.registers.a;
        let carry = if self.registers.flags.carry { 0x01 } else { 0 };
        self.registers.a = (old_value << 1) | carry;

        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = self.registers.a & 0x80 == 0x80;
    }

    fn rrca(&mut self) {
        /*
           +-------------+
           |             |
           +---|7-->0|---+--->|CY|
        */

        let old_value = self.registers.a;
        self.registers.a = (old_value >> 1) | (old_value << 7);

        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = self.registers.a & 0x01 == 0x01;
    }

    fn rra(&mut self) {
        /*
           +--------------------------+
           |    ______         ___    |
           +---|7-->0|------->|CY|<---+
        */

        let old_value = self.registers.a;
        let carry = if self.registers.flags.carry { 0x80 } else { 0 };
        self.registers.a = (old_value >> 1) | carry;

        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = self.registers.a & 0x01 == 0x01;
    }







    // Jump Instructions

    fn jp(&mut self, condition: JumpCondition) {
        let target = self.read_word(WordAddress::Immediate);
        if condition.should_jump(self.registers.flags) {
            self.program_counter = target;
        }
    }

    fn jr(&mut self, condition: JumpCondition) {
        let offset = self.read_byte(ByteAddress::Immediate) as u16;
        let target = self.program_counter + offset;
        if condition.should_jump(self.registers.flags) {
            self.program_counter = target;
        }
    }

    fn rst(&mut self, target: u8) {
        let pc = self.program_counter;
        self.push_word(pc);
        self.program_counter = 0x0000 + (target as u16);
    }

    fn ret(&mut self, condition: JumpCondition) {
        let target = self.pop_word();
        if condition.should_jump(self.registers.flags) {
            self.program_counter = target;
        }
    }


    // Load Instructions

    fn ld16(&mut self, destination: WordAddress, source: WordAddress) {
        let value = self.read_word(source);
        self.write_word(destination, value);
    }

    fn ld8(&mut self, destination: ByteAddress, source: ByteAddress) {
        let value = self.read_byte(source);
        self.write_byte(destination, value);
    }


}



#[derive(Debug, Clone, Copy)]
enum JumpCondition {
    Unconditional,
    Carry,
    NoCarry,
    Zero,
    NonZero,
}

impl JumpCondition {
    fn should_jump(self, flags: FlagsRegister) -> bool {
        match self {
            JumpCondition::Unconditional => true,
            JumpCondition::Carry => flags.carry,
            JumpCondition::NoCarry => ! flags.carry,
            JumpCondition::Zero => flags.zero,
            JumpCondition::NonZero => ! flags.zero,
        }
    }
}




// struct AdcInstruction {
//     operand: ByteAddress,
// }

// impl AdcInstruction {
//     // Return number of cycles?
//     fn execute(&self, cpu: &mut Cpu) {
//         let operand = cpu.read_byte(self.operand);
//         let half_carry = (cpu.registers.a & 0x0f) + (operand & 0x0f) & 0x10 != 0;
//         let (result, overflowed) = cpu.registers.a.overflowing_add(operand);

//         cpu.registers.flags.zero = (cpu.registers.a == 0);
//         cpu.registers.flags.negative = false;
//         cpu.registers.flags.half_carry = half_carry;
//         cpu.registers.flags.carry = overflowed;

//         cpu.registers.a = result;
//     }
// }





#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_flags_register() {
        let flags = FlagsRegister {zero: true, negative: false, half_carry: false, carry: false};
        assert_eq!(0x80, flags.read_byte());
        let flags = FlagsRegister {zero: false, negative: true, half_carry: false, carry: false};
        assert_eq!(0x40, flags.read_byte());
        let flags = FlagsRegister {zero: false, negative: false, half_carry: true, carry: false};
        assert_eq!(0x20, flags.read_byte());
        let flags = FlagsRegister {zero: false, negative: false, half_carry: false, carry: true};
        assert_eq!(0x10, flags.read_byte());

        // let flags = FlagsRegister::from_byte();
    }

}
