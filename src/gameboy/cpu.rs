

use super::memory::MemoryUnit;


use std::fmt::{Formatter, Display, Result as FmtResult};



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
    RegisterIndirect(Register),
    DoubleRegisterIndirect(DoubleRegister),
    DoubleRegisterIndirectInc(DoubleRegister),
    DoubleRegisterIndirectDec(DoubleRegister),

    /// A byte immediately following the instruction
    Immediate(u8),

    /// A signed byte immediately following the instruction
    SignedImmediate(i8),

    /// A 16-bit address immediately following the address
    ImmediateIndirect(u16),

    /// An 8-bit address offset immediately following the instruction
    ImmediateIndirectOffset(u8),
}

impl Display for ByteAddress {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            ByteAddress::Register(register) => register.fmt(f),
            ByteAddress::RegisterIndirect(register) => write!(f, "({})", register),
            ByteAddress::DoubleRegisterIndirect(double_register) => write!(f, "({})", double_register),
            ByteAddress::DoubleRegisterIndirectInc(double_register) => write!(f, "({}+)", double_register),
            ByteAddress::DoubleRegisterIndirectDec(double_register) => write!(f, "({}-)", double_register),
            ByteAddress::Immediate(value) => write!(f, "${:02x}", value),
            ByteAddress::SignedImmediate(value) => value.fmt(f),
            ByteAddress::ImmediateIndirect(value) => write!(f, "(${:04x})", value),
            ByteAddress::ImmediateIndirectOffset(value) => write!(f, "(${:02x})", value),
        }
    }
}




#[derive(Debug, Clone, Copy)]
enum WordAddress {
    DoubleRegister(DoubleRegister),
    ProgramCounter,
    StackPointer,
    Immediate(u16),
    ImmediateIndirect(u16),
}

impl Display for WordAddress {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            WordAddress::DoubleRegister(double_register) => double_register.fmt(f),
            WordAddress::ProgramCounter => panic!("Not implemented? Where is PC used in an instruction?"),
            WordAddress::StackPointer => write!(f, "SP"),
            WordAddress::Immediate(value) => write!(f, "${:04x}", value),
            WordAddress::ImmediateIndirect(address) => write!(f, "(${:04x})", address),
        }
    }
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

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            Register::A => write!(f, "A"),
            Register::B => write!(f, "B"),
            Register::C => write!(f, "C"),
            Register::D => write!(f, "D"),
            Register::E => write!(f, "E"),
            Register::H => write!(f, "H"),
            Register::L => write!(f, "L"),
            Register::Flags => write!(f, "F"),
        }
    }
}


#[derive(Debug, Clone, Copy)]
enum DoubleRegister {
    AF,
    BC,
    DE,
    HL,
}

impl Display for DoubleRegister {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            DoubleRegister::AF => write!(f, "AF"),
            DoubleRegister::BC => write!(f, "BC"),
            DoubleRegister::DE => write!(f, "DE"),
            DoubleRegister::HL => write!(f, "HL"),
        }
    }
}




pub struct Cpu {
    program_counter: u16,
    stack_pointer: u16,
    registers: Registers,

    // TODO: Is this needed? Is it better to just pass it in to
    // the fetch_and_execute function directly? The reference could
    // be made part of the ByteAddress, etc.
    // memory: Rc<RefCell<MemoryUnit<'a>>>,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            program_counter: 0,
            stack_pointer: 0,
            registers: Registers::new(),
        }
    }

    // TODO: put wrapper around this function?
    /// Fetch and decode an instruction.
    pub fn fetch_and_execute(&mut self, memory: &mut MemoryUnit) {
        let info = Instruction::fetch_and_decode(self.program_counter, memory);
        self.program_counter += info.size_in_bytes as u16;

        println!("{}", info.instruction.to_string());
        self.execute(info.instruction, memory);



        // TODO: report timing cycles
    }

    fn execute(&mut self, instruction: Instruction, memory: &mut MemoryUnit) {
        match instruction {

            // TODO: Put Memory back in as a struct member Rc<RefCell<MemoryUnit>>
            // and make these seperate functions

            Instruction::Dec8(operand) => {
                let start_value = self.read_byte(operand, memory);
                let (new_value, overflowed) = start_value.overflowing_sub(1);
                let half_carry = (start_value & 0x0f) == 0;

                self.registers.a = new_value;
                self.registers.flags.zero = self.registers.a == 0;
                self.registers.flags.negative = false;
                self.registers.flags.half_carry = half_carry;
                // Carry flag not affected
            }

            Instruction::Jp(condition, address) => {
                let target = self.read_word(address, memory);
                if condition.should_jump(self.registers.flags) {
                    self.program_counter = target;
                }
            },

            Instruction::Jr(condition, offset) => {
                let offset = self.read_byte(offset, memory) as u16;
                let target = self.program_counter + offset;
                if condition.should_jump(self.registers.flags) {
                    self.program_counter = target;
                }
            }

            Instruction::Load16(destination, source) => {
                let value = self.read_word(source, memory);
                self.write_word(destination, value, memory);
            },

            Instruction::Load8(destination, source) => {
                let value = self.read_byte(source, memory);
                self.write_byte(destination, value, memory);
            },

            Instruction::Xor(operand) => {
                let operand = self.read_byte(operand, memory);
                self.registers.a ^= operand;
                self.registers.flags.zero = self.registers.a == 0;
                self.registers.flags.negative = false;
                self.registers.flags.half_carry = false;
                self.registers.flags.carry = false;
            },



            // TODO: Remove when not needed anymore
            _ => panic!("Unimplemented instruction \"{}\"", instruction),
        }
    }


    fn read_byte(&mut self, address: ByteAddress, memory: &MemoryUnit) -> u8 {
        match address {
            ByteAddress::Register(register) => self.read_register(register),

            ByteAddress::RegisterIndirect(register) => {
                let offset = self.read_register(register) as u16;
                memory.read_byte(0xff00 + offset)
            },

            ByteAddress::DoubleRegisterIndirect(double_register) => {
                let address = self.read_double_register(double_register);
                memory.read_byte(address)
            },

            ByteAddress::DoubleRegisterIndirectInc(double_register) => {
                let address = self.read_double_register(double_register);
                self.increment_double_register(double_register);
                memory.read_byte(address)
            },

            ByteAddress::DoubleRegisterIndirectDec(double_register) => {
                let address = self.read_double_register(double_register);
                self.decrement_double_register(double_register);
                memory.read_byte(address)
            },

            ByteAddress::Immediate(value) => value,
            ByteAddress::SignedImmediate(value) => value as u8,  // TODO: This seems like an indication that something is wrong... value is signed...
            ByteAddress::ImmediateIndirect(value) => memory.read_byte(value),
            ByteAddress::ImmediateIndirectOffset(value) => memory.read_byte(0xff00 + value as u16),
        }
    }

    fn write_byte(&mut self, address: ByteAddress, value: u8, memory: &mut MemoryUnit) {
        match address {
            ByteAddress::Register(register) => self.write_register(register, value),

            ByteAddress::RegisterIndirect(register) => {
                let offset = self.read_register(register) as u16;
                memory.write_byte(0xff00 + offset, value)
            },

            ByteAddress::DoubleRegisterIndirect(double_register) => {
                let address = self.read_double_register(double_register);
                memory.write_byte(address, value)
            },

            ByteAddress::DoubleRegisterIndirectInc(double_register) => {
                let address = self.read_double_register(double_register);
                self.increment_double_register(double_register);
                memory.write_byte(address, value)
            },

            ByteAddress::DoubleRegisterIndirectDec(double_register) => {
                let address = self.read_double_register(double_register);
                self.decrement_double_register(double_register);
                memory.write_byte(address, value)
            },

            ByteAddress::Immediate(_) | ByteAddress::SignedImmediate(_) => panic!("Tried to write to immediate byte!"),
            ByteAddress::ImmediateIndirect(address) => memory.write_byte(address, value),
            ByteAddress::ImmediateIndirectOffset(offset) => memory.write_byte(0xff00 + offset as u16, value),
        }
    }

    fn read_word(&self, address: WordAddress, memory: &MemoryUnit) -> u16 {
        match address {
            WordAddress::ProgramCounter => self.program_counter,
            WordAddress::StackPointer => self.stack_pointer,
            WordAddress::DoubleRegister(double_register) => self.read_double_register(double_register),
            WordAddress::Immediate(value) => value,
            WordAddress::ImmediateIndirect(address) => memory.read_word(address),
        }
    }

    fn write_word(&mut self, address: WordAddress, value: u16, memory: &mut MemoryUnit) {
        match address {
            WordAddress::DoubleRegister(double_register) => self.write_double_register(double_register, value),
            WordAddress::ProgramCounter => self.program_counter = value,
            WordAddress::StackPointer => self.stack_pointer = value,
            WordAddress::Immediate(_) => panic!("Tried to write to immediate word!"),
            WordAddress::ImmediateIndirect(address) => memory.write_word(address, value),
        }
    }



    fn read_register(&self, register: Register) -> u8 {
        match register {
            Register::A => self.registers.a,
            Register::B => self.registers.b,
            Register::C => self.registers.c,
            Register::D => self.registers.d,
            Register::E => self.registers.e,
            Register::H => self.registers.h,
            Register::L => self.registers.l,
            Register::Flags => self.registers.flags.read_byte(),
        }
    }

    fn write_register(&mut self, register: Register, value: u8) {
        match register {
            Register::A => self.registers.a = value,
            Register::B => self.registers.b = value,
            Register::C => self.registers.c = value,
            Register::D => self.registers.d = value,
            Register::E => self.registers.e = value,
            Register::H => self.registers.h = value,
            Register::L => self.registers.l = value,
            Register::Flags => self.registers.flags.write_byte(value),
        }
    }


    fn read_double_register(&self, double_register: DoubleRegister) -> u16 {
        let (high_register, low_register) = match double_register {
            DoubleRegister::AF => (Register::A, Register::Flags),
            DoubleRegister::BC => (Register::B, Register::C),
            DoubleRegister::DE => (Register::D, Register::E),
            DoubleRegister::HL => (Register::H, Register::L),
        };
        let high_byte = self.read_register(high_register) as u16;
        let low_byte = self.read_register(low_register) as u16;
        (high_byte << 8) | low_byte
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









}






#[derive(Debug, Clone, Copy)]
enum Instruction {
    /// Add
    Add(ByteAddress),

    /// 16-bit Add
    Add16(WordAddress, WordAddress),

    AddOffsetToStackPointer(ByteAddress),

    /// Add with carry.
    Adc(ByteAddress),

    /// Bitwise AND
    And(ByteAddress),

    /// Jump to subroutine
    Call(JumpCondition, WordAddress),

    Ccf,

    /// Compare
    Cp(ByteAddress),

    Cpl,

    Daa,

    Dec8(ByteAddress),

    Dec16(WordAddress),

    /// Disable interrupts
    Di,

    /// Enable interrupts
    Ei,

    Halt,

    Inc8(ByteAddress),

    Inc16(WordAddress),

    /// Absolute Jump
    Jp(JumpCondition, WordAddress),

    /// Relative Jump
    Jr(JumpCondition, ByteAddress),

    /// Load an 8-bit value
    Load8(ByteAddress, ByteAddress),

    /// Load a 16-bit value
    Load16(WordAddress, WordAddress),

    /// Load a 16-bit value with a signed offset
    Load16WithOffset(WordAddress, WordAddress, ByteAddress),

    /// Load to/from high memory
    LoadHigh(ByteAddress, ByteAddress),

    /// No-op
    Nop,

    /// Bitwise OR
    Or(ByteAddress),

    /// Pop a word off the stack
    Pop(WordAddress),

    /// Push a word onto the stack
    Push(WordAddress),

    /// Return from subroutine
    Ret(JumpCondition),

    /// Return from interrupt service routine
    Reti,

    Rla,
    Rlca,
    Rra,
    Rrca,

    Rst(u16),

    /// Subtract with carry
    Sbc(ByteAddress),

    Scf,

    Stop,

    /// Subtract
    Sub(ByteAddress),

    /// Bitwise XOR
    Xor(ByteAddress),

}

impl Instruction {

    fn fetch_and_decode(address: u16, memory: &MemoryUnit) -> InstructionInfo {

        // macro_rules! instruction {
        //     ($instruction:ident) => Instruction::$instruction
        // }

        macro_rules! immediate {
            (Byte) => { ByteAddress::Immediate(memory.read_byte(address + 1)) };
            (Word) => { WordAddress::Immediate(memory.read_word(address + 1)) };
            (SignedByte) => { ByteAddress::SignedImmediate(memory.read_byte(address + 1) as i8) };
        }
        macro_rules! immediate_indirect {
            (Byte) => { ByteAddress::ImmediateIndirect(memory.read_word(address + 1)) };
            (Word) => { WordAddress::ImmediateIndirect(memory.read_word(address + 1)) };
        }
        macro_rules! immediate_indirect_offset {
            () => { ByteAddress::ImmediateIndirectOffset(memory.read_byte(address + 1)) };
        }
        macro_rules! register { ($name:ident) => { ByteAddress::Register(Register::$name) } }
        macro_rules! double_register { ($name:ident) => { WordAddress::DoubleRegister(DoubleRegister::$name) } }
        macro_rules! double_register_indirect { ($name:ident) => { ByteAddress::DoubleRegisterIndirect(DoubleRegister::$name) } }
        macro_rules! double_register_indirect_inc { ($name:ident) => { ByteAddress::DoubleRegisterIndirectInc(DoubleRegister::$name) } }
        macro_rules! double_register_indirect_dec { ($name:ident) => { ByteAddress::DoubleRegisterIndirectDec(DoubleRegister::$name) } }
        macro_rules! register_indirect { ($name:ident) => { ByteAddress::RegisterIndirect(Register::$name) } }
        use self::Instruction::*;
        use self::InstructionInfo as Info;
        use self::InstructionTiming::*;
        let opcode = memory.read_byte(address);
        match opcode {

            // 0x00 => InstructionInfo {instruction: Nop,
            0x00 => Info::new(Nop, 1, Constant(4)),
            0x10 => Info::new(Stop, 2, Constant(4)),
            0x20 => Info::new(Jr(JumpCondition::NonZero, immediate!(Byte)), 2, Constant(12)),  // TODO
            0x30 => Info::new(Jr(JumpCondition::NoCarry, immediate!(Byte)), 2, Constant(12)),  // TODO

            0x01 => Info::new(Load16(double_register!(BC), immediate!(Word)), 3, Constant(12)),
            0x11 => Info::new(Load16(double_register!(DE), immediate!(Word)), 3, Constant(12)),
            0x21 => Info::new(Load16(double_register!(HL), immediate!(Word)), 3, Constant(12)),
            0x31 => Info::new(Load16(WordAddress::StackPointer, immediate!(Word)), 3, Constant(12)),

            0x02 => Info::new(Load8(double_register_indirect!(BC), register!(A)), 1, Constant(8)),
            0x12 => Info::new(Load8(double_register_indirect!(DE), register!(A)), 1, Constant(8)),
            0x22 => Info::new(Load8(double_register_indirect_inc!(HL), register!(A)), 1, Constant(8)),
            0x32 => Info::new(Load8(double_register_indirect_dec!(HL), register!(A)), 1, Constant(8)),

            0x03 => Info::new(Inc16(double_register!(BC)), 1, Constant(8)),
            0x13 => Info::new(Inc16(double_register!(DE)), 1, Constant(8)),
            0x23 => Info::new(Inc16(double_register!(HL)), 1, Constant(8)),
            0x33 => Info::new(Inc16(WordAddress::StackPointer), 1, Constant(8)),

            0x04 => Info::new(Inc8(register!(B)), 1, Constant(4)),
            0x14 => Info::new(Inc8(register!(D)), 1, Constant(4)),
            0x24 => Info::new(Inc8(register!(H)), 1, Constant(4)),
            0x34 => Info::new(Inc8(double_register_indirect!(HL)), 1, Constant(4)),

            0x05 => Info::new(Dec8(register!(B)), 1, Constant(4)),
            0x15 => Info::new(Dec8(register!(D)), 1, Constant(4)),
            0x25 => Info::new(Dec8(register!(H)), 1, Constant(4)),
            0x35 => Info::new(Dec8(double_register_indirect!(HL)), 1, Constant(4)),

            0x06 => Info::new(Load8(register!(B), immediate!(Byte)), 2, Constant(8)),
            0x16 => Info::new(Load8(register!(D), immediate!(Byte)), 2, Constant(8)),
            0x26 => Info::new(Load8(register!(H), immediate!(Byte)), 2, Constant(8)),
            0x36 => Info::new(Load8(double_register_indirect!(HL), immediate!(Byte)), 2, Constant(12)),

            0x07 => Info::new(Rlca, 1, Constant(4)),
            0x17 => Info::new(Rla, 1, Constant(4)),
            0x27 => Info::new(Daa, 1, Constant(4)),
            0x37 => Info::new(Scf, 1, Constant(4)),

            0x08 => Info::new(Load16(immediate_indirect!(Word), WordAddress::StackPointer), 3, Constant(20)),
            0x18 => Info::new(Jr(JumpCondition::Unconditional, immediate!(Byte)), 2, Constant(12)),
            0x28 => Info::new(Jr(JumpCondition::Zero, immediate!(Byte)), 2, Constant(12)),  // TODO
            0x38 => Info::new(Jr(JumpCondition::Carry, immediate!(Byte)), 2, Constant(12)),  // TODO

            0x09 => Info::new(Add16(double_register!(HL), double_register!(BC)), 1, Constant(8)),
            0x09 => Info::new(Add16(double_register!(HL), double_register!(DE)), 1, Constant(8)),
            0x09 => Info::new(Add16(double_register!(HL), double_register!(HL)), 1, Constant(8)),
            0x09 => Info::new(Add16(double_register!(HL), WordAddress::StackPointer), 1, Constant(8)),

            0x0a => Info::new(Load8(register!(A), double_register_indirect!(BC)), 1, Constant(8)),
            0x1a => Info::new(Load8(register!(A), double_register_indirect!(DE)), 1, Constant(8)),
            0x2a => Info::new(Load8(register!(A), double_register_indirect_inc!(HL)), 1, Constant(8)),
            0x3a => Info::new(Load8(register!(A), double_register_indirect_dec!(HL)), 1, Constant(8)),

            0x0b => Info::new(Dec16(double_register!(BC)), 1, Constant(8)),
            0x1b => Info::new(Dec16(double_register!(DE)), 1, Constant(8)),
            0x2b => Info::new(Dec16(double_register!(HL)), 1, Constant(8)),
            0x3b => Info::new(Dec16(WordAddress::StackPointer), 1, Constant(8)),

            0x0c => Info::new(Inc8(register!(C)), 1, Constant(4)),
            0x1c => Info::new(Inc8(register!(E)), 1, Constant(4)),
            0x2c => Info::new(Inc8(register!(L)), 1, Constant(4)),
            0x3c => Info::new(Inc8(register!(A)), 1, Constant(4)),

            0x0d => Info::new(Dec8(register!(C)), 1, Constant(4)),
            0x1d => Info::new(Dec8(register!(E)), 1, Constant(4)),
            0x2d => Info::new(Dec8(register!(L)), 1, Constant(4)),
            0x3d => Info::new(Dec8(register!(A)), 1, Constant(4)),

            0x0e => Info::new(Load8(register!(C), immediate!(Byte)), 2, Constant(8)),
            0x1e => Info::new(Load8(register!(E), immediate!(Byte)), 2, Constant(8)),
            0x2e => Info::new(Load8(register!(L), immediate!(Byte)), 2, Constant(8)),
            0x3e => Info::new(Load8(register!(A), immediate!(Byte)), 2, Constant(8)),

            0x0f => Info::new(Rrca, 1, Constant(4)),
            0x1f => Info::new(Rra, 1, Constant(4)),
            0x2f => Info::new(Cpl, 1, Constant(4)),
            0x3f => Info::new(Ccf, 1, Constant(4)),

            0x41 => Info::new(Load8(register!(B), register!(C)), 1, Constant(4)),
            0x40 => Info::new(Load8(register!(B), register!(B)), 1, Constant(4)),
            0x42 => Info::new(Load8(register!(B), register!(D)), 1, Constant(4)),
            0x43 => Info::new(Load8(register!(B), register!(E)), 1, Constant(4)),
            0x44 => Info::new(Load8(register!(B), register!(H)), 1, Constant(4)),
            0x45 => Info::new(Load8(register!(B), register!(L)), 1, Constant(4)),
            0x46 => Info::new(Load8(register!(B), double_register_indirect!(HL)), 1, Constant(8)),
            0x47 => Info::new(Load8(register!(B), register!(A)), 1, Constant(4)),

            0x48 => Info::new(Load8(register!(C), register!(B)), 1, Constant(4)),
            0x49 => Info::new(Load8(register!(C), register!(C)), 1, Constant(4)),
            0x4a => Info::new(Load8(register!(C), register!(D)), 1, Constant(4)),
            0x4b => Info::new(Load8(register!(C), register!(E)), 1, Constant(4)),
            0x4c => Info::new(Load8(register!(C), register!(H)), 1, Constant(4)),
            0x4d => Info::new(Load8(register!(C), register!(L)), 1, Constant(4)),
            0x4e => Info::new(Load8(register!(C), double_register_indirect!(HL)), 1, Constant(8)),
            0x4f => Info::new(Load8(register!(C), register!(A)), 1, Constant(4)),

            0x50 => Info::new(Load8(register!(D), register!(B)), 1, Constant(4)),
            0x51 => Info::new(Load8(register!(D), register!(C)), 1, Constant(4)),
            0x52 => Info::new(Load8(register!(D), register!(D)), 1, Constant(4)),
            0x53 => Info::new(Load8(register!(D), register!(E)), 1, Constant(4)),
            0x54 => Info::new(Load8(register!(D), register!(H)), 1, Constant(4)),
            0x55 => Info::new(Load8(register!(D), register!(L)), 1, Constant(4)),
            0x56 => Info::new(Load8(register!(D), double_register_indirect!(HL)), 1, Constant(8)),
            0x57 => Info::new(Load8(register!(D), register!(A)), 1, Constant(4)),

            0x58 => Info::new(Load8(register!(E), register!(B)), 1, Constant(4)),
            0x59 => Info::new(Load8(register!(E), register!(C)), 1, Constant(4)),
            0x5a => Info::new(Load8(register!(E), register!(D)), 1, Constant(4)),
            0x5b => Info::new(Load8(register!(E), register!(E)), 1, Constant(4)),
            0x5c => Info::new(Load8(register!(E), register!(H)), 1, Constant(4)),
            0x5d => Info::new(Load8(register!(E), register!(L)), 1, Constant(4)),
            0x5e => Info::new(Load8(register!(E), double_register_indirect!(HL)), 1, Constant(8)),
            0x5f => Info::new(Load8(register!(E), register!(A)), 1, Constant(4)),

            0x60 => Info::new(Load8(register!(H), register!(B)), 1, Constant(4)),
            0x61 => Info::new(Load8(register!(H), register!(C)), 1, Constant(4)),
            0x62 => Info::new(Load8(register!(H), register!(D)), 1, Constant(4)),
            0x63 => Info::new(Load8(register!(H), register!(E)), 1, Constant(4)),
            0x64 => Info::new(Load8(register!(H), register!(H)), 1, Constant(4)),
            0x65 => Info::new(Load8(register!(H), register!(L)), 1, Constant(4)),
            0x66 => Info::new(Load8(register!(H), double_register_indirect!(HL)), 1, Constant(8)),
            0x67 => Info::new(Load8(register!(H), register!(A)), 1, Constant(4)),

            0x68 => Info::new(Load8(register!(L), register!(B)), 1, Constant(4)),
            0x69 => Info::new(Load8(register!(L), register!(C)), 1, Constant(4)),
            0x6a => Info::new(Load8(register!(L), register!(D)), 1, Constant(4)),
            0x6b => Info::new(Load8(register!(L), register!(E)), 1, Constant(4)),
            0x6c => Info::new(Load8(register!(L), register!(H)), 1, Constant(4)),
            0x6d => Info::new(Load8(register!(L), register!(L)), 1, Constant(4)),
            0x6e => Info::new(Load8(register!(L), double_register_indirect!(HL)), 1, Constant(8)),
            0x6f => Info::new(Load8(register!(L), register!(A)), 1, Constant(4)),

            0x70 => Info::new(Load8(double_register_indirect!(HL), register!(B)), 1, Constant(8)),
            0x71 => Info::new(Load8(double_register_indirect!(HL), register!(C)), 1, Constant(8)),
            0x72 => Info::new(Load8(double_register_indirect!(HL), register!(D)), 1, Constant(8)),
            0x73 => Info::new(Load8(double_register_indirect!(HL), register!(E)), 1, Constant(8)),
            0x74 => Info::new(Load8(double_register_indirect!(HL), register!(H)), 1, Constant(8)),
            0x75 => Info::new(Load8(double_register_indirect!(HL), register!(L)), 1, Constant(8)),
            0x76 => Info::new(Halt, 1, Constant(4)),
            0x77 => Info::new(Load8(double_register_indirect!(HL), register!(A)), 1, Constant(8)),

            0x78 => Info::new(Load8(register!(A), register!(B)), 1, Constant(4)),
            0x79 => Info::new(Load8(register!(A), register!(C)), 1, Constant(4)),
            0x7a => Info::new(Load8(register!(A), register!(D)), 1, Constant(4)),
            0x7b => Info::new(Load8(register!(A), register!(E)), 1, Constant(4)),
            0x7c => Info::new(Load8(register!(A), register!(H)), 1, Constant(4)),
            0x7d => Info::new(Load8(register!(A), register!(L)), 1, Constant(4)),
            0x7e => Info::new(Load8(register!(A), double_register_indirect!(HL)), 1, Constant(8)),
            0x7f => Info::new(Load8(register!(A), register!(A)), 1, Constant(4)),

            0x80 => Info::new(Add(register!(B)), 1, Constant(4)),
            0x81 => Info::new(Add(register!(C)), 1, Constant(4)),
            0x82 => Info::new(Add(register!(D)), 1, Constant(4)),
            0x83 => Info::new(Add(register!(E)), 1, Constant(4)),
            0x84 => Info::new(Add(register!(H)), 1, Constant(4)),
            0x85 => Info::new(Add(register!(L)), 1, Constant(4)),
            0x86 => Info::new(Add(double_register_indirect!(HL)), 1, Constant(8)),
            0x87 => Info::new(Add(register!(A)), 1, Constant(4)),

            0x88 => Info::new(Adc(register!(B)), 1, Constant(4)),
            0x89 => Info::new(Adc(register!(C)), 1, Constant(4)),
            0x8a => Info::new(Adc(register!(D)), 1, Constant(4)),
            0x8b => Info::new(Adc(register!(E)), 1, Constant(4)),
            0x8c => Info::new(Adc(register!(H)), 1, Constant(4)),
            0x8d => Info::new(Adc(register!(L)), 1, Constant(4)),
            0x8e => Info::new(Adc(double_register_indirect!(HL)), 1, Constant(8)),
            0x8f => Info::new(Adc(register!(A)), 1, Constant(4)),

            0x90 => Info::new(Sub(register!(B)), 1, Constant(4)),
            0x91 => Info::new(Sub(register!(C)), 1, Constant(4)),
            0x92 => Info::new(Sub(register!(D)), 1, Constant(4)),
            0x93 => Info::new(Sub(register!(E)), 1, Constant(4)),
            0x94 => Info::new(Sub(register!(H)), 1, Constant(4)),
            0x95 => Info::new(Sub(register!(L)), 1, Constant(4)),
            0x96 => Info::new(Sub(double_register_indirect!(HL)), 1, Constant(8)),
            0x97 => Info::new(Sub(register!(A)), 1, Constant(4)),

            0x98 => Info::new(Sbc(register!(B)), 1, Constant(4)),
            0x99 => Info::new(Sbc(register!(C)), 1, Constant(4)),
            0x9a => Info::new(Sbc(register!(D)), 1, Constant(4)),
            0x9b => Info::new(Sbc(register!(E)), 1, Constant(4)),
            0x9c => Info::new(Sbc(register!(H)), 1, Constant(4)),
            0x9d => Info::new(Sbc(register!(L)), 1, Constant(4)),
            0x9e => Info::new(Sbc(double_register_indirect!(HL)), 1, Constant(8)),
            0x9f => Info::new(Sbc(register!(A)), 1, Constant(4)),

            0xa0 => Info::new(And(register!(B)), 1, Constant(4)),
            0xa1 => Info::new(And(register!(C)), 1, Constant(4)),
            0xa2 => Info::new(And(register!(D)), 1, Constant(4)),
            0xa3 => Info::new(And(register!(E)), 1, Constant(4)),
            0xa4 => Info::new(And(register!(H)), 1, Constant(4)),
            0xa5 => Info::new(And(register!(L)), 1, Constant(4)),
            0xa6 => Info::new(And(double_register_indirect!(HL)), 1, Constant(8)),
            0xa7 => Info::new(And(register!(A)), 1, Constant(4)),

            0xa8 => Info::new(Xor(register!(B)), 1, Constant(4)),
            0xa9 => Info::new(Xor(register!(C)), 1, Constant(4)),
            0xaa => Info::new(Xor(register!(D)), 1, Constant(4)),
            0xab => Info::new(Xor(register!(E)), 1, Constant(4)),
            0xac => Info::new(Xor(register!(H)), 1, Constant(4)),
            0xad => Info::new(Xor(register!(L)), 1, Constant(4)),
            0xae => Info::new(Xor(double_register_indirect!(HL)), 1, Constant(8)),
            0xaf => Info::new(Xor(register!(A)), 1, Constant(4)),

            0xb0 => Info::new(Or(register!(B)), 1, Constant(4)),
            0xb1 => Info::new(Or(register!(C)), 1, Constant(4)),
            0xb2 => Info::new(Or(register!(D)), 1, Constant(4)),
            0xb3 => Info::new(Or(register!(E)), 1, Constant(4)),
            0xb4 => Info::new(Or(register!(H)), 1, Constant(4)),
            0xb5 => Info::new(Or(register!(L)), 1, Constant(4)),
            0xb6 => Info::new(Or(double_register_indirect!(HL)), 1, Constant(8)),
            0xb7 => Info::new(Or(register!(A)), 1, Constant(4)),

            0xb8 => Info::new(Cp(register!(B)), 1, Constant(4)),
            0xb9 => Info::new(Cp(register!(C)), 1, Constant(4)),
            0xba => Info::new(Cp(register!(D)), 1, Constant(4)),
            0xbb => Info::new(Cp(register!(E)), 1, Constant(4)),
            0xbc => Info::new(Cp(register!(H)), 1, Constant(4)),
            0xbd => Info::new(Cp(register!(L)), 1, Constant(4)),
            0xbe => Info::new(Cp(double_register_indirect!(HL)), 1, Constant(8)),
            0xbf => Info::new(Cp(register!(A)), 1, Constant(4)),

            0xc0 => Info::new(Ret(JumpCondition::NonZero), 1, Constant(20)), // TODO
            0xd0 => Info::new(Ret(JumpCondition::NoCarry), 1, Constant(20)), // TODO
            0xe0 => Info::new(LoadHigh(immediate_indirect_offset!(), register!(A)), 2, Constant(12)),
            0xf0 => Info::new(LoadHigh(register!(A), immediate_indirect_offset!()), 2, Constant(12)),

            0xc1 => Info::new(Pop(double_register!(BC)), 1, Constant(12)),
            0xd1 => Info::new(Pop(double_register!(BC)), 1, Constant(12)),
            0xe1 => Info::new(Pop(double_register!(BC)), 1, Constant(12)),
            0xf1 => Info::new(Pop(double_register!(BC)), 1, Constant(12)),

            0xc1 => Info::new(Jp(JumpCondition::NonZero, immediate!(Word)), 3, Constant(16)),  // TODO
            0xd1 => Info::new(Jp(JumpCondition::NoCarry, immediate!(Word)), 3, Constant(16)),  // TODO
            0xe2 => Info::new(Load8(register_indirect!(C), register!(A)), 2, Constant(8)),
            0xf2 => Info::new(Load8(register!(A), register_indirect!(C)), 2, Constant(8)),

            0xc3 => Info::new(Jp(JumpCondition::Unconditional, immediate!(Word)), 3, Constant(16)),
            // 0xd3 is an illegal opcode
            // 0xe3 is an illegal opcode
            0xf3 => Info::new(Di, 1, Constant(4)),

            0xc4 => Info::new(Call(JumpCondition::NonZero, immediate!(Word)), 3, Constant(24)),  // TODO
            0xd4 => Info::new(Call(JumpCondition::NoCarry, immediate!(Word)), 3, Constant(24)),  // TODO
            // 0xe4 is an illegal opcode
            // 0xf4 is an illegal opcode

            0xc5 => Info::new(Push(double_register!(BC)), 1, Constant(16)),
            0xd5 => Info::new(Push(double_register!(DE)), 1, Constant(16)),
            0xe5 => Info::new(Push(double_register!(HL)), 1, Constant(16)),
            0xf5 => Info::new(Push(WordAddress::StackPointer), 1, Constant(16)),

            0xc6 => Info::new(Add(immediate!(Byte)), 2, Constant(8)),
            0xd6 => Info::new(Sub(immediate!(Byte)), 2, Constant(8)),
            0xe6 => Info::new(And(immediate!(Byte)), 2, Constant(8)),
            0xf6 => Info::new(Or(immediate!(Byte)), 2, Constant(8)),

            0xc7 => Info::new(Rst(0x00), 1, Constant(16)),
            0xd7 => Info::new(Rst(0x10), 1, Constant(16)),
            0xe7 => Info::new(Rst(0x20), 1, Constant(16)),
            0xf7 => Info::new(Rst(0x30), 1, Constant(16)),

            0xc8 => Info::new(Ret(JumpCondition::Zero), 1, Constant(20)),  // TODO
            0xd8 => Info::new(Ret(JumpCondition::Carry), 1, Constant(20)),  // TODO
            0xe8 => Info::new(AddOffsetToStackPointer(immediate!(SignedByte)), 2, Constant(16)),
            0xf8 => Info::new(Load16WithOffset(double_register!(HL), WordAddress::StackPointer, immediate!(SignedByte)), 2, Constant(12)),

            0xc9 => Info::new(Ret(JumpCondition::Unconditional), 1, Constant(16)),
            0xd9 => Info::new(Reti, 1, Constant(16)),
            0xe9 => Info::new(Jp(JumpCondition::Unconditional, double_register!(HL)), 1, Constant(4)),
            0xf9 => Info::new(Load16(WordAddress::StackPointer, double_register!(HL)), 1, Constant(8)),

            0xea => Info::new(Load8(immediate_indirect!(Byte), register!(A)), 3, Constant(16)),
            0xfa => Info::new(Load8(register!(A), immediate_indirect!(Byte)), 3, Constant(16)),

            0xcb => Instruction::decode_cb(memory.read_byte(address + 1)),
            // 0xdb is an illegal opcode
            // 0xeb is an illegal opcode
            0xfb => Info::new(Ei, 1, Constant(4)),

            0xcc => Info::new(Call(JumpCondition::Zero, immediate!(Word)), 3, Constant(24)),
            0xdc => Info::new(Call(JumpCondition::Carry, immediate!(Word)), 3, Constant(24)),
            // 0xec is an illegal opcode
            // 0xfc is an illegal opcode

            0xcd => Info::new(Call(JumpCondition::Unconditional, immediate!(Word)), 3, Constant(24)),
            // 0xdd is an illegal opcode
            // 0xde is an illegal opcode
            // 0xdf is an illegal opcode

            0xce => Info::new(Adc(immediate!(Byte)), 2, Constant(8)),
            0xde => Info::new(Sbc(immediate!(Byte)), 2, Constant(8)),
            0xee => Info::new(Xor(immediate!(Byte)), 2, Constant(8)),
            0xfe => Info::new(Cp(immediate!(Byte)), 2, Constant(8)),

            0xcf => Info::new(Rst(0x08), 1, Constant(16)),
            0xdf => Info::new(Rst(0x18), 1, Constant(16)),
            0xef => Info::new(Rst(0x28), 1, Constant(16)),
            0xff => Info::new(Rst(0x38), 1, Constant(16)),

            // TODO: Remove when not needed anymore?
            // Could replace with "illegal opcode" or something.
            _ => panic!("Unimplemented opcode 0x{:02x}", opcode),
        }
    }

    fn decode_cb(opcode: u8) -> InstructionInfo {
        let instruction = match opcode {

            _ => panic!("Unimplemented CB opcode 0x{:02x}", opcode),
        };
        InstructionInfo::new(instruction, 2, InstructionTiming::Constant(8))
    }

    // fn size_in_bytes(self) -> usize {
    //     // Shortcut for calculating size impact of a potentially
    //     // immediate instruction operand
    //     macro_rules! operand_size {
    //         ( Byte $operand:expr ) => {
    //             match $operand {
    //                 ByteAddress::Immediate(_) => 1,
    //                 ByteAddress::ImmediateIndirect(_) => 2,
    //                 _ => 0,
    //             }
    //         };
    //         ( Word $operand:expr ) => {
    //             match $operand {
    //                 WordAddress::Immediate(_) => 2,
    //                 WordAddress::ImmediateIndirect(_) => 2,
    //                 _ => 0,
    //             }
    //         };
    //     }

    //     match self {
    //         Instruction::Add(operand) => 1 + operand_size!(Byte operand),
    //         Instruction::Adc(operand) => 1 + operand_size!(Byte operand),
    //         Instruction::And(operand) => 1 + operand_size!(Byte operand),

    //         Instruction::Cp(operand) => 1 + operand_size!(Byte operand),

    //         Instruction::Dec8(operand) => 1 + operand_size!(Byte operand),

    //         Instruction::Halt => 1,

    //         Instruction::Inc8(operand) => 1 + operand_size!(Byte operand),
    //         Instruction::Inc16(operand) => 1 + operand_size!(Word operand),

    //         Instruction::Jp(_, address) => 1 + operand_size!(Word address),
    //         Instruction::Jr(_, address) => 1 + operand_size!(Byte address),

    //         Instruction::Load8(dest, src) => 1 + operand_size!(Byte dest) + operand_size!(Byte src),
    //         Instruction::Load16(dest, src) => 1 + operand_size!(Word dest) + operand_size!(Word src),


    //         Instruction::Nop => 1,

    //         Instruction::Or(operand) => 1 + operand_size!(Byte operand),

    //         Instruction::Rst(_) => 1,

    //         Instruction::Sbc(operand) => 1 + operand_size!(Byte operand),
    //         Instruction::Stop => 2,
    //         Instruction::Sub(operand) => 1 + operand_size!(Byte operand),

    //         Instruction::Xor(operand) => 1 + operand_size!(Byte operand),

    //     }
    // }
}

impl Display for Instruction {

    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::Instruction::*;
        match *self {
            Add(operand) => write!(f, "ADD A, {}", operand),
            Add16(dest, src) => write!(f, "ADD {}, {}", dest, src),

            AddOffsetToStackPointer(offset) => write!(f, "ADD SP, {}", offset),

            Adc(operand) => write!(f, "ADC A, {}", operand),

            And(operand) => write!(f, "AND {}", operand),

            Call(JumpCondition::Unconditional, address) => write!(f, "CALL {}", address),
            Call(condition, address) => write!(f, "CALL {}, {}", condition, address),

            Ccf => write!(f, "CCF"),

            Cp(operand) => write!(f, "CP {}", operand),
            Cpl => write!(f, "CPL"),

            Daa => write!(f, "DAA"),

            Dec8(operand) => write!(f, "DEC {}", operand),
            Dec16(operand) => write!(f, "DEC {}", operand),

            Di => write!(f, "DI"),
            Ei => write!(f, "EI"),

            Halt => write!(f, "HALT"),

            Inc8(operand) => write!(f, "INC {}", operand),
            Inc16(operand) => write!(f, "INC {}", operand),

            // "JP (HL)" is a bit of a special case for the disassembly
            Jp(JumpCondition::Unconditional, WordAddress::DoubleRegister(double_register)) => write!(f, "JP ({})", double_register),
            Jp(JumpCondition::Unconditional, address) => write!(f, "JP {}", address),
            Jp(condition, address) => write!(f, "JP {}, {}", condition, address),

            Jr(JumpCondition::Unconditional, address) => write!(f, "JR {}", address),
            Jr(condition, address) => write!(f, "JR {}, {}", condition, address),

            Load8(dest, src) => write!(f, "LD {}, {}", dest, src),
            Load16(dest, src) => write!(f, "LD {}, {}", dest, src),

            LoadHigh(dest, src) => write!(f, "LDH {}, {}", dest, src),

            Load16WithOffset(dest, src, ByteAddress::SignedImmediate(offset)) => {
                if offset < 0 {
                    write!(f, "LD {}, {}-{}", dest, src, -offset)
                }
                else {
                    write!(f, "LD {}, {}+{}", dest, src, offset)
                }
            }
            Load16WithOffset(dest, src, offset) => panic!("Load16WithOffset must be called with a signed immediate offset."),  // TODO: This is another code smell. Something should be done about this.

            Nop => write!(f, "NOP"),

            Or(operand) => write!(f, "OR {}", operand),

            Pop(operand) => write!(f, "POP {}", operand),
            Push(operand) => write!(f, "PUSH {}", operand),

            Ret(JumpCondition::Unconditional) => write!(f, "RET"),
            Ret(condition) => write!(f, "RET {}", condition),

            Reti => write!(f, "RETI"),

            Rla => write!(f, "RLA"),
            Rlca => write!(f, "RLCA"),
            Rra => write!(f, "RRA"),
            Rrca => write!(f, "RRCA"),

            Rst(address) => write!(f, "RST ${:02x}", address),

            Sbc(operand) => write!(f, "SBC A, {}", operand),

            Scf => write!(f, "SCF"),

            Stop => write!(f, "STOP"),
            Sub(operand) => write!(f, "SUB {}", operand),

            Xor(operand) => write!(f, "XOR {}", operand),

        }
    }

}



enum InstructionTiming {
    Constant(i32),
    // Later, others for jumps when taken/not-taken
}


struct InstructionInfo {
    instruction: Instruction,
    size_in_bytes: usize,
    timing: InstructionTiming,
}

impl InstructionInfo {
    fn new(instruction: Instruction, size_in_bytes: usize, timing: InstructionTiming) -> InstructionInfo {
        InstructionInfo {
            instruction,
            size_in_bytes,
            timing,
        }
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

impl Display for JumpCondition {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            JumpCondition::Unconditional => panic!(),
            JumpCondition::Carry => write!(f, "C"),
            JumpCondition::NoCarry => write!(f, "NC"),
            JumpCondition::Zero => write!(f, "Z"),
            JumpCondition::NonZero => write!(f, "NZ"),
        }
    }
}










#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction_disassemble() {
        let dest = ByteAddress::DoubleRegisterIndirectInc(DoubleRegister::HL);
        let src = ByteAddress::Register(Register::A);
        let instruction = Instruction::Load8(dest, src);
        assert_eq!("LD (HL+), A", instruction.to_string());

        let instruction = Instruction::Jp(JumpCondition::Unconditional, WordAddress::DoubleRegister(DoubleRegister::HL));
        assert_eq!("JP (HL)", instruction.to_string());

        let instruction = Instruction::AddOffsetToStackPointer(ByteAddress::SignedImmediate(-10));
        assert_eq!("ADD SP, -10", instruction.to_string());
        let instruction = Instruction::AddOffsetToStackPointer(ByteAddress::SignedImmediate(20));
        assert_eq!("ADD SP, 20", instruction.to_string());

        let instruction = Instruction::LoadHigh(ByteAddress::ImmediateIndirectOffset(0x20), ByteAddress::Register(Register::A));
        assert_eq!("LDH ($20), A", instruction.to_string());

        let dest = WordAddress::DoubleRegister(DoubleRegister::HL);
        let src = WordAddress::StackPointer;
        let instruction = Instruction::Load16WithOffset(dest, src, ByteAddress::SignedImmediate(5));
        assert_eq!("LD HL, SP+5", instruction.to_string());
        let instruction = Instruction::Load16WithOffset(dest, src, ByteAddress::SignedImmediate(-5));
        assert_eq!("LD HL, SP-5", instruction.to_string());
    }

}
