
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Formatter, Display, Result as FmtResult};
use std::convert::From;

use gameboy::memory::MemoryUnit;


#[derive(Debug, Default)]
struct Flags {
    zero: bool,
    negative: bool,
    half_carry: bool,
    carry: bool,
}

impl Flags {
    fn read(&self) -> u8 {
        ((self.zero as u8) << 7) |
        ((self.negative as u8) << 6) |
        ((self.half_carry as u8) << 5) |
        ((self.carry as u8) << 4)
    }

    fn write(&mut self, value: u8) {
        self.zero = (value & (1 << 7)) != 0;
        self.negative = (value & (1 << 6)) != 0;
        self.half_carry = (value & (1 << 5)) != 0;
        self.carry = (value & (1 << 4)) != 0;
    }

    fn meets_condition(&self, condition: Option<FlagCondition>) -> bool {
        match condition {
            None => true,
            Some(FlagCondition::Zero) => self.zero,
            Some(FlagCondition::NonZero) => ! self.zero,
            Some(FlagCondition::Carry) => self.carry,
            Some(FlagCondition::NoCarry) => ! self.carry,
        }
    }
}


#[derive(Debug, Default)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    flags: Flags,
}



/// State for pending IME status changes.
enum ImeChangeStatus {
    /// The change was just requested
    Fresh(bool),
    /// The change is waiting one instruction
    Waiting(bool),
    /// There is no requested change
    None,
}



pub struct Cpu<'a> {
    program_counter: u16,
    stack_pointer: u16,
    registers: Registers,
    memory: Rc<RefCell<MemoryUnit<'a>>>,


    /// Interrupt Master Enable flag
    ime: bool,
    ime_change_status: ImeChangeStatus,
}

impl<'a> Cpu<'a> {
    pub fn new(memory: Rc<RefCell<MemoryUnit<'a>>>) -> Cpu<'a> {
        Cpu {
            program_counter: 0,
            stack_pointer: 0,
            registers: Registers::default(),
            memory,
            ime: false,
            ime_change_status: ImeChangeStatus::None,
        }
    }

    pub fn reset(&mut self) {
        // http://problemkaputt.de/pandocs.htm#powerupsequence
        self.memory.borrow_mut().reset();
        self.program_counter = 0x0100;
        self.registers.a = 0x01;
        self.registers.flags.write(0xb0);
        self.registers.b = 0x00;
        self.registers.c = 0x13;
        self.registers.d = 0x00;
        self.registers.e = 0xd8;
        self.registers.h = 0x01;
        self.registers.l = 0x4d;
        self.stack_pointer = 0xfffe;
        // TODO: Reset IO ports
        self.ime = true;
        self.ime_change_status = ImeChangeStatus::None;
    }

    pub fn fetch_and_execute(&mut self) -> i32 {
        let (instruction, size_in_bytes) = Instruction::decode(self.program_counter, &self.memory.borrow());


        // TODO: What is a better way to do this?
        for i in 0..3 {
            if i < size_in_bytes {
                let address = self.program_counter + i as u16;
                print!("{:02x} ", self.memory.borrow().read_byte(address));
            }
            else {
                print!("   ");
            }
        }


        println!("  {:<15}    PC={:04x}   HL={:04x}  BC={:04x}   IME={} LY={}",
            instruction.to_string(),
            self.program_counter,
            self.read_double_register(DoubleRegister::HL),
            self.read_double_register(DoubleRegister::BC),

            if self.ime { 'Y' } else { 'N' },
            self.memory.borrow().get_io_read_value(0x44),
        );
        self.program_counter += size_in_bytes as u16;
        self.execute(instruction);

        // IME changes wait one until after the following instruction is
        // executed to take effect.
        match self.ime_change_status {
            ImeChangeStatus::Fresh(new_state) => self.ime_change_status = ImeChangeStatus::Waiting(new_state),
            ImeChangeStatus::Waiting(new_state) => {
                self.ime_change_status = ImeChangeStatus::None;
                self.ime = new_state;
            },
            ImeChangeStatus::None => (),
        }


        // TODO: Implement this properly
        let machine_cycles = 4;
        machine_cycles
    }


    fn execute(&mut self, instruction: Instruction) {
        use self::Instruction::*;
        match instruction {

            Call(condition, address) => self.call(condition, address),

            Compare(operand) => self.cp(operand),

            EnableInterrupts => self.ei(),
            DisableInterrupts => self.di(),

            Dec8(operand) => self.dec8(operand),
            Dec16(operand) => self.dec16(operand),

            Inc8(operand) => self.inc8(operand),
            Inc16(operand) => self.inc16(operand),


            Load8(dest, src) => self.load8(dest, src),
            Load16(dest, src) => self.load16(dest, src),

            Nop => (),
            Jump(condition, address) => self.jump(condition, address),
            Xor(operand) => self.xor(operand),
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
            Register::F => self.registers.flags.read(),
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
            Register::F => self.registers.flags.write(value),
        }
    }

    fn read_double_register(&self, double_register: DoubleRegister) -> u16 {
        let (high_register, low_register) = double_register.split();
        let high_byte = self.read_register(high_register) as u16;
        let low_byte = self.read_register(low_register) as u16;
        (high_byte << 8) | low_byte
    }

    fn write_double_register(&mut self, double_register: DoubleRegister, value: u16) {
        let (high_register, low_register) = double_register.split();
        self.write_register(high_register, (value >> 8) as u8);
        self.write_register(low_register, (value & 0xff) as u8);
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












    fn call(&mut self, condition: Option<FlagCondition>, address: u16) {
        if self.registers.flags.meets_condition(condition) {
            let pc = self.program_counter;
            self.push_word(pc);
            self.program_counter = address;
        }
    }


    fn cp(&mut self, operand: Byte) {
        let operand = operand.read(self);
        self.registers.flags.zero = self.registers.a == operand;
        self.registers.flags.negative = true;
        self.registers.flags.half_carry = (self.registers.a & 0x0f) < (operand & 0x0f);
        self.registers.flags.carry = self.registers.a < operand;
    }


    fn ei(&mut self) {
        self.ime_change_status = ImeChangeStatus::Fresh(true);
    }

    fn di(&mut self) {
        self.ime_change_status = ImeChangeStatus::Fresh(false);
    }


    fn dec8(&mut self, operand: MutableByte) {
        let start_value = operand.read(self);
        let new_value = start_value.wrapping_sub(1);
        let half_carry = (start_value & 0x0f) == 0;

        operand.write(new_value, self);
        self.registers.flags.zero = new_value == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        // Carry flag not affected
    }

    fn dec16(&mut self, operand: MutableWord) {
        let new_value = operand.read(self).wrapping_sub(1);
        operand.write(new_value, self)
        // No flags affected
    }

    fn inc8(&mut self, operand: MutableByte) {
        let start_value = operand.read(self);
        let new_value = start_value.wrapping_add(1);
        let half_carry = (start_value & 0x0f) == 0x0f;

        operand.write(new_value, self);
        self.registers.flags.zero = new_value == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        // Carry flag not affected
    }

    fn inc16(&mut self, operand: MutableWord) {
        let new_value = operand.read(self).wrapping_add(1);
        operand.write(new_value, self)
        // No flags affected
    }


    fn load8(&mut self, dest: MutableByte, src: Byte) {
        let value = src.read(self);
        dest.write(value, self);
    }

    fn load16(&mut self, dest: MutableWord, src: Word) {
        let value = src.read(self);
        dest.write(value, self);
    }


    fn jump(&mut self, condition: Option<FlagCondition>, target: JumpTarget) {
        if self.registers.flags.meets_condition(condition) {
            self.program_counter = target.resolve(self)
        }
    }

    fn xor(&mut self, operand: Byte) {
        self.registers.a ^= operand.read(self);
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = false;
    }


}









#[derive(Debug, Clone, Copy)]
enum Register {
    A, B, C, D, E, H, L, F
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
            Register::F => write!(f, "F"),
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

impl DoubleRegister {
    /// Split into high and low registers
    fn split(self) -> (Register, Register) {
        match self {
            DoubleRegister::AF => (Register::A, Register::F),
            DoubleRegister::BC => (Register::B, Register::C),
            DoubleRegister::DE => (Register::D, Register::E),
            DoubleRegister::HL => (Register::H, Register::L),
        }
    }
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




#[derive(Debug, Clone, Copy)]
enum FlagCondition {
    Zero,
    NonZero,
    Carry,
    NoCarry,
}

impl Display for FlagCondition {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::FlagCondition::*;
        match *self {
            Zero => write!(f, "Z"),
            NonZero => write!(f, "NZ"),
            Carry => write!(f, "C"),
            NoCarry => write!(f, "NC"),
        }
    }
}



#[derive(Debug, Clone, Copy)]
enum Byte {
    /// Read a CPU register
    Register(Register),
    /// Read High RAM offset by a CPU register
    RegisterIndirect(Register),
    /// Read from an address given by a double register.
    DoubleRegisterIndirect(DoubleRegister),

    DoubleRegisterIndirectInc(DoubleRegister),
    DoubleRegisterIndirectDec(DoubleRegister),

    /// Read an immediate byte
    Immediate(u8),

    /// Read from memory using the given immediate address
    ImmediateIndirect(u16),

    /// Read a byte from High RAM with the given immediate offset
    ImmediateIndirectHigh(u8),

}

impl Byte {
    fn read(self, cpu: &mut Cpu) -> u8 {
        match self {
            Byte::Register(register) => cpu.read_register(register),
            Byte::RegisterIndirect(register) => cpu.memory.borrow().read_byte(0xff00 + cpu.read_register(register) as u16),
            Byte::DoubleRegisterIndirect(double_register) => cpu.memory.borrow().read_byte(cpu.read_double_register(double_register)),

            Byte::DoubleRegisterIndirectInc(double_register) => {
                let address = cpu.read_double_register(double_register);
                cpu.increment_double_register(double_register);
                cpu.memory.borrow().read_byte(address)
            },

            Byte::DoubleRegisterIndirectDec(double_register) => {
                let address = cpu.read_double_register(double_register);
                cpu.decrement_double_register(double_register);
                cpu.memory.borrow().read_byte(address)
            },


            Byte::Immediate(value) => value,
            Byte::ImmediateIndirect(address) => cpu.memory.borrow().read_byte(address),
            Byte::ImmediateIndirectHigh(offset) => cpu.memory.borrow().read_byte(0xff00 + offset as u16),

        }
    }
}

impl Display for Byte {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            Byte::Register(register) => register.fmt(f),
            Byte::RegisterIndirect(register) => write!(f, "({})", register),
            Byte::DoubleRegisterIndirect(double_register) => write!(f, "({})", double_register),
            Byte::DoubleRegisterIndirectInc(double_register) => write!(f, "({}+)", double_register),
            Byte::DoubleRegisterIndirectDec(double_register) => write!(f, "({}-)", double_register),
            Byte::Immediate(value) => write!(f, "${:02x}", value),
            Byte::ImmediateIndirect(address) => write!(f, "(${:04x})", address),
            Byte::ImmediateIndirectHigh(offset) => write!(f, "(${:02x})", offset),
        }
    }
}

impl From<MutableByte> for Byte {
    fn from(source: MutableByte) -> Byte {
        match source {
            MutableByte::Register(register) => Byte::Register(register),
            MutableByte::RegisterIndirect(register) => Byte::RegisterIndirect(register),
            MutableByte::DoubleRegisterIndirect(double_register) => Byte::DoubleRegisterIndirect(double_register),
            MutableByte::DoubleRegisterIndirectInc(double_register) => Byte::DoubleRegisterIndirectInc(double_register),
            MutableByte::DoubleRegisterIndirectDec(double_register) => Byte::DoubleRegisterIndirectDec(double_register),
            MutableByte::ImmediateIndirect(address) => Byte::ImmediateIndirect(address),
            MutableByte::ImmediateIndirectHigh(offset) => Byte::ImmediateIndirectHigh(offset),
        }
    }
}


#[derive(Debug, Clone, Copy)]
enum MutableByte {
    Register(Register),
    RegisterIndirect(Register),
    DoubleRegisterIndirect(DoubleRegister),
    DoubleRegisterIndirectInc(DoubleRegister),
    DoubleRegisterIndirectDec(DoubleRegister),

    ImmediateIndirect(u16),
    ImmediateIndirectHigh(u8),
}

impl MutableByte {
    fn write(self, value: u8, cpu: &mut Cpu) {
        match self {
            MutableByte::Register(register) => cpu.write_register(register, value),
            MutableByte::RegisterIndirect(register) => {
                let address = 0xff00 + cpu.read_register(register) as u16;
                cpu.memory.borrow_mut().write_byte(address, value);
            }
            MutableByte::DoubleRegisterIndirect(double_register) => {
                let address = cpu.read_double_register(double_register);
                cpu.memory.borrow_mut().write_byte(address, value);
            }

            MutableByte::DoubleRegisterIndirectInc(double_register) => {
                let address = cpu.read_double_register(double_register);
                cpu.increment_double_register(double_register);
                cpu.memory.borrow_mut().write_byte(address, value);
            },

            MutableByte::DoubleRegisterIndirectDec(double_register) => {
                let address = cpu.read_double_register(double_register);
                cpu.decrement_double_register(double_register);
                cpu.memory.borrow_mut().write_byte(address, value);
            },

            MutableByte::ImmediateIndirect(address) => cpu.memory.borrow_mut().write_byte(address, value),
            MutableByte::ImmediateIndirectHigh(offset) => cpu.memory.borrow_mut().write_byte(0xff00 + offset as u16, value),
        }
    }

    fn read(self, cpu: &mut Cpu) -> u8 {
        Byte::from(self).read(cpu)
    }
}

impl Display for MutableByte {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Byte::from(*self).fmt(f)
    }
}



#[derive(Debug, Clone, Copy)]
enum Word {
    StackPointer,
    DoubleRegister(DoubleRegister),
    Immediate(u16),
    ImmediateIndirect(u16),
}

impl Word {
    fn read(self, cpu: &Cpu) -> u16 {
        match self {
            Word::StackPointer => cpu.stack_pointer,
            Word::DoubleRegister(register) => cpu.read_double_register(register),
            Word::Immediate(value) => value,
            Word::ImmediateIndirect(address) => cpu.memory.borrow().read_word(address),
        }
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match *self {
            Word::StackPointer => write!(f, "SP"),
            Word::DoubleRegister(double_register) => double_register.fmt(f),
            Word::Immediate(value) => write!(f, "${:04x}", value),
            Word::ImmediateIndirect(address) => write!(f, "(${:04x})", address),
        }
    }
}

impl From<MutableWord> for Word {
    fn from(source: MutableWord) -> Word {
        match source {
            MutableWord::StackPointer => Word::StackPointer,
            MutableWord::DoubleRegister(double_register) => Word::DoubleRegister(double_register),
            MutableWord::ImmediateIndirect(address) => Word::ImmediateIndirect(address),
        }
    }
}


#[derive(Debug, Clone, Copy)]
enum MutableWord {
    StackPointer,
    DoubleRegister(DoubleRegister),
    ImmediateIndirect(u16),
}

impl MutableWord {
    fn write(self, value: u16, cpu: &mut Cpu) {
        match self {
            MutableWord::StackPointer => cpu.stack_pointer = value,
            MutableWord::DoubleRegister(double_register) => cpu.write_double_register(double_register, value),
            MutableWord::ImmediateIndirect(address) => cpu.memory.borrow_mut().write_word(address, value),
        }
    }

    fn read(self, cpu: &Cpu) -> u16 {
        Word::from(self).read(cpu)
    }
}

impl Display for MutableWord {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Word::from(*self).fmt(f)
    }
}



#[derive(Debug, Clone, Copy)]
enum JumpTarget {
    Absolute(Word),
    Relative(i8),
}

impl JumpTarget {
    fn resolve(self, cpu: &Cpu) -> u16 {
        match self {
            JumpTarget::Absolute(address) => address.read(cpu),
            JumpTarget::Relative(offset) => ((cpu.program_counter as i32) + (offset as i32)) as u16,
        }
    }
}






#[derive(Debug, Clone, Copy)]
enum Instruction {

    Call(Option<FlagCondition>, u16),

    Compare(Byte),

    EnableInterrupts,
    DisableInterrupts,

    Inc8(MutableByte),
    Inc16(MutableWord),

    Dec8(MutableByte),
    Dec16(MutableWord),

    Load8(MutableByte, Byte),
    Load16(MutableWord, Word),

    Nop,
    Xor(Byte),
    Jump(Option<FlagCondition>, JumpTarget),
}

impl Instruction {

    // TODO: Enhancement: derive size in bytes and timing from instruction itself
    fn decode(address: u16, memory: &MemoryUnit) -> (Instruction, usize) {
        let mut size_in_bytes = 0;

        macro_rules! get_next {
            (Byte) => {
                {
                    let next_byte = memory.read_byte(address + size_in_bytes as u16);
                    size_in_bytes += 1;
                    next_byte
                }
            };
            (Word) => {
                {
                    let next_word = memory.read_word(address + size_in_bytes as u16);
                    size_in_bytes += 2;
                    next_word
                }
            }
        }

        macro_rules! immediate {
            (Byte) => { Byte::Immediate(get_next!(Byte)) };
            (Word) => { Word::Immediate(get_next!(Word)) };

            (JumpTarget Relative) => { JumpTarget::Relative(get_next!(Byte) as i8) };
            (JumpTarget Absolute) => { JumpTarget::Absolute(immediate!(Word)) };
        }

        macro_rules! flag {
            (*) => { None };
            (Z) => { Some(FlagCondition::Zero) };
            (NZ) => { Some(FlagCondition::NonZero) };
            (C) => { Some(FlagCondition::Carry) };
            (NC) => { Some(FlagCondition::NoCarry) };
        }

        macro_rules! register {
            ($name:ident) => { Byte::Register(Register::$name) };
            (mut $name:ident) => { MutableByte::Register(Register::$name) };
        }

        macro_rules! double_register {
            ($name:ident) => { Word::DoubleRegister(DoubleRegister::$name) };
            (mut $name:ident) => { MutableWord::DoubleRegister(DoubleRegister::$name) };
        }

        macro_rules! double_register_indirect {
            ($name:ident)      => { Byte::DoubleRegisterIndirect(DoubleRegister::$name) };
            ($name:ident+)     => { Byte::DoubleRegisterIndirectInc(DoubleRegister::$name) };
            ($name:ident-)     => { Byte::DoubleRegisterIndirectDec(DoubleRegister::$name) };
            (mut $name:ident)  => { MutableByte::DoubleRegisterIndirect(DoubleRegister::$name) };
            (mut $name:ident+) => { MutableByte::DoubleRegisterIndirectInc(DoubleRegister::$name) };
            (mut $name:ident-) => { MutableByte::DoubleRegisterIndirectDec(DoubleRegister::$name) };
        }


        use self::Instruction::*;
        let opcode = get_next!(Byte);
        let instruction = match opcode {

            // ZAC (for faster ctrl+f)

            0x00 => Nop,


            0x01 => Load16(double_register!(mut BC), immediate!(Word)),
            0x11 => Load16(double_register!(mut DE), immediate!(Word)),
            0x21 => Load16(double_register!(mut HL), immediate!(Word)),
            0x31 => Load16(MutableWord::StackPointer, immediate!(Word)),


            0x02 => Load8(double_register_indirect!(mut BC), register!(A)),
            0x12 => Load8(double_register_indirect!(mut DE), register!(A)),
            0x22 => Load8(double_register_indirect!(mut HL+), register!(A)),
            0x32 => Load8(double_register_indirect!(mut HL-), register!(A)),

            0x03 => Inc16(double_register!(mut BC)),
            0x13 => Inc16(double_register!(mut DE)),
            0x23 => Inc16(double_register!(mut HL)),
            0x33 => Inc16(MutableWord::StackPointer),

            0x06 => Load8(register!(mut B), immediate!(Byte)),
            0x16 => Load8(register!(mut D), immediate!(Byte)),
            0x26 => Load8(register!(mut H), immediate!(Byte)),
            0x36 => Load8(double_register_indirect!(mut HL), immediate!(Byte)),


            0x0a => Load8(register!(mut A), double_register_indirect!(BC)),
            0x1a => Load8(register!(mut A), double_register_indirect!(DE)),
            0x2a => Load8(register!(mut A), double_register_indirect!(HL+)),
            0x3a => Load8(register!(mut A), double_register_indirect!(HL-)),


            0x0b => Dec16(double_register!(mut BC)),
            0x1b => Dec16(double_register!(mut DE)),
            0x2b => Dec16(double_register!(mut HL)),
            0x3b => Dec16(MutableWord::StackPointer),



            0x02 => Load8(double_register_indirect!(mut BC), register!(A)),
            0x12 => Load8(double_register_indirect!(mut DE), register!(A)),
            0x22 => Load8(double_register_indirect!(mut HL+), register!(A)),
            0x32 => Load8(double_register_indirect!(mut HL-), register!(A)),


            0x0e => Load8(register!(mut C), immediate!(Byte)),
            0x1e => Load8(register!(mut E), immediate!(Byte)),
            0x2e => Load8(register!(mut L), immediate!(Byte)),
            0x3e => Load8(register!(mut A), immediate!(Byte)),



            0x04 => Inc8(register!(mut B)),
            0x14 => Inc8(register!(mut D)),
            0x24 => Inc8(register!(mut H)),
            0x34 => Inc8(double_register_indirect!(mut HL)),

            0x05 => Dec8(register!(mut B)),
            0x15 => Dec8(register!(mut D)),
            0x25 => Dec8(register!(mut H)),
            0x35 => Dec8(double_register_indirect!(mut HL)),



            0xe0 => Load8(MutableByte::ImmediateIndirectHigh(get_next!(Byte)), register!(A)),
            0xf0 => Load8(register!(mut A), Byte::ImmediateIndirectHigh(get_next!(Byte))),


            0x0c => Inc8(register!(mut C)),
            0x1c => Inc8(register!(mut E)),
            0x2c => Inc8(register!(mut L)),
            0x3c => Inc8(register!(mut A)),

            0x0d => Dec8(register!(mut C)),
            0x1d => Dec8(register!(mut E)),
            0x2d => Dec8(register!(mut L)),
            0x3d => Dec8(register!(mut A)),



            0xf3 => DisableInterrupts,
            0xfb => EnableInterrupts,



            0xfe => Compare(immediate!(Byte)),



            // 0x01 => Load16(double_register!(BC), immediate!(Word)),

            // 0x20 => Load8(double_register_indirect!(BC), register!(A)),
            // 0x21 => Load8(double_register_indirect!(DE), register!(A)),

            0x12 => Load8(MutableByte::DoubleRegisterIndirect(DoubleRegister::DE), Byte::Register(Register::A)),


            0x21 => Load16(MutableWord::DoubleRegister(DoubleRegister::HL), immediate!(Word)),


            0xc3 => Jump(flag!(*), immediate!(JumpTarget Absolute)),



            0xa8 => Xor(register!(B)),
            0xa9 => Xor(register!(C)),
            0xaa => Xor(register!(D)),
            0xab => Xor(register!(E)),
            0xac => Xor(register!(H)),
            0xad => Xor(register!(L)),
            0xae => Xor(double_register_indirect!(HL)),
            0xaf => Xor(register!(A)),


            0x20 => Jump(flag!(NZ), immediate!(JumpTarget Relative)),
            0x30 => Jump(flag!(NC), immediate!(JumpTarget Relative)),

            0x18 => Jump(flag!(*), immediate!(JumpTarget Relative)),
            0x28 => Jump(flag!(Z), immediate!(JumpTarget Relative)),
            0x38 => Jump(flag!(C), immediate!(JumpTarget Relative)),



            0xc4 => Call(flag!(NZ), get_next!(Word)),
            0xd4 => Call(flag!(NC), get_next!(Word)),

            0xcc => Call(flag!(Z), get_next!(Word)),
            0xdc => Call(flag!(C), get_next!(Word)),

            0xcd => Call(flag!(*), get_next!(Word)),



            0xea => Load8(MutableByte::ImmediateIndirect(get_next!(Word)), register!(A)),
            0xfa => Load8(register!(mut A), Byte::ImmediateIndirect(get_next!(Word))),



            0xe2 => Load8(MutableByte::RegisterIndirect(Register::C), register!(A)),
            0xf2 => Load8(register!(mut A), Byte::RegisterIndirect(Register::C)),



            // 0x40 => Load8(register!(B), register!(B)),


            _ => panic!("Unimplemented opcode 0x{:02x} at address 0x{:04x}", opcode, address),
        };
        (instruction, size_in_bytes)
    }

}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::Instruction::*;
        match *self {

            Call(None, address) => write!(f, "CALL ${:04x}", address),
            Call(Some(condition), address) => write!(f, "CALL {}, ${:04x}", condition, address),

            Compare(operand) => write!(f, "CP {}", operand),

            EnableInterrupts => write!(f, "EI"),
            DisableInterrupts => write!(f, "DI"),

            Dec8(operand) => write!(f, "DEC {}", operand),
            Dec16(operand) => write!(f, "DEC {}", operand),

            Inc8(operand) => write!(f, "INC {}", operand),
            Inc16(operand) => write!(f, "INC {}", operand),

            Jump(None, JumpTarget::Relative(offset)) => write!(f, "JR {}", offset),
            Jump(Some(condition), JumpTarget::Relative(offset)) => write!(f, "JR {}, {}", condition, offset),

            Jump(None, JumpTarget::Absolute(Word::DoubleRegister(double_register))) => write!(f, "JP ({})", double_register),
            Jump(None, JumpTarget::Absolute(address)) => write!(f, "JP {}", address),
            Jump(Some(condition), JumpTarget::Absolute(address)) => write!(f, "JP {}, {}", condition, address),


            Load8(dest, src) => write!(f, "LD {}, {}", dest, src),
            Load16(dest, src) => write!(f, "LD {}, {}", dest, src),


            Nop => write!(f, "NOP"),
            Xor(operand) => write!(f, "XOR {}", operand),
        }
    }
}
