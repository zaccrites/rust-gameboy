
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Formatter, Display, Result as FmtResult};
use std::convert::From;

use gameboy::memory::MemoryUnit;




fn pause() {
    use std::io;
    use std::io::prelude::*;

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    write!(stdout, ""). unwrap();
    stdout.flush().unwrap();

    let _ = stdin.read(&mut [0u8]).unwrap();
}




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



    stepping: bool,
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


            stepping: false,
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
        let instruction = Instruction::decode(self.program_counter, &self.memory.borrow());

        let size_in_bytes = instruction.size_in_bytes();

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
        // println!("  {:<15}    PC={:04x}   HL={:04x}  AF={:04x}  BC={:04x}   IME={} LY={}   STACK_WORD={:04x}",
        println!("  {:<15}    PC={:04x}   HL={:04x}  AF={:04x}  BC={:04x}   IME={} LY={}   ($0xff80)={:02x}",
            instruction.to_string(),
            self.program_counter,
            self.read_double_register(DoubleRegister::HL),
            self.read_double_register(DoubleRegister::AF),
            self.read_double_register(DoubleRegister::BC),

            if self.ime { 'Y' } else { 'N' },
            self.memory.borrow().get_io_read_value(0x44),

            // self.memory.borrow().read_word(self.stack_pointer.wrapping_add(2)),
            self.memory.borrow().read_byte(0xff80),
        );



        if self.stepping {
            pause();
        }


        self.program_counter += size_in_bytes as u16;
        let machine_cycles = instruction.cycles(&self.registers.flags);
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


        // println!("Machine cycles = {}", machine_cycles);
        machine_cycles
    }


    fn execute(&mut self, instruction: Instruction) {
        use self::Instruction::*;
        match instruction {

            Call(condition, address) => self.call(condition, address),

            Add16(dest, src) => self.add16(dest, src),

            Add(operand) => self.add(operand),
            Adc(operand) => self.adc(operand),
            Sub(operand) => self.sub(operand),
            Sbc(operand) => self.sbc(operand),
            And(operand) => self.and(operand),
            Xor(operand) => self.xor(operand),
            Or(operand) => self.or(operand),
            Cp(operand) => self.cp(operand),

            Cpl => self.cpl(),

            EnableInterrupts => self.ei(),
            DisableInterrupts => self.di(),

            Dec8(operand) => self.dec8(operand),
            Dec16(operand) => self.dec16(operand),

            Inc8(operand) => self.inc8(operand),
            Inc16(operand) => self.inc16(operand),

            Push(operand) => self.push(operand),
            Pop(operand) => self.pop(operand),

            Ret(condition) => self.ret(condition),
            Reti => self.reti(),

            Rst(address) => self.rst(address),


            Load8(dest, src) => self.load8(dest, src),
            Load16(dest, src) => self.load16(dest, src),

            Nop => (),
            Jump(condition, address) => self.jump(condition, address),


            // CB
            Swap(operand) => self.swap(operand),
            Bit(bit, operand) => self.bit(bit, operand),
            Set(bit, operand) => self.set(bit, operand),
            Res(bit, operand) => self.res(bit, operand),

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
        let sp = self.stack_pointer.wrapping_sub(2);
        self.stack_pointer = sp;
    }

    /// Pop a word off the stack.
    fn pop_word(&mut self) -> u16 {
        let sp = self.stack_pointer.wrapping_add(2);
        self.stack_pointer = sp;
        self.memory.borrow().read_word(self.stack_pointer)
    }






    fn push(&mut self, operand: Word) {
        let operand = operand.read(self);
        self.push_word(operand);
    }

    fn pop(&mut self, operand: MutableWord) {
        let value = self.pop_word();
        operand.write(value, self);
    }

    fn swap(&mut self, operand: MutableByte) {
        let original_value = operand.read(self);
        let upper = original_value >> 4;
        let lower = original_value & 0x0f;
        let new_value = (lower << 4) | upper;
        operand.write(new_value, self);
        self.registers.flags.zero = new_value == 0;
        // No other flags affected
    }


    fn bit(&mut self, bit: u8, operand: Byte) {
        let operand = operand.read(self);
        self.registers.flags.zero = operand & (1 << bit) == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = true;
        // Carry flag not affected
    }

    fn set(&mut self, bit: u8, operand: MutableByte) {
        let start_value = operand.read(self);
        operand.write(start_value | (1 << bit), self);
    }

    fn res(&mut self, bit: u8, operand: MutableByte) {
        let start_value = operand.read(self);
        operand.write(start_value & !(1 << bit), self);
    }






    fn add16(&mut self, dest: MutableWord, src: Word) {
        let start_value = dest.read(self);
        let addend = src.read(self);
        let (sum, overflowed) = start_value.overflowing_add(addend);
        let half_carry = ((start_value & 0xff) + (addend & 0xff)) & 0x0100 != 0;

        dest.write(sum, self);
        // Zero flag not affected
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        self.registers.flags.carry = overflowed;
    }




    fn cp(&mut self, operand: Byte) {
        let operand = operand.read(self);
        let half_carry = (self.registers.a & 0x0f) < (operand & 0x0f);
        let (_, overflowed) = self.registers.a.overflowing_sub(operand);

        self.registers.flags.zero = self.registers.a == operand;
        self.registers.flags.negative = true;
        self.registers.flags.half_carry = (operand & 0x0f) > (self.registers.a & 0x0f);
        self.registers.flags.carry = overflowed;
    }

    fn sub(&mut self, operand: Byte) {
        let operand = operand.read(self);
        let half_carry = (self.registers.a & 0x0f) < (operand & 0x0f);
        let (result, overflowed) = self.registers.a.overflowing_sub(operand);

        self.registers.a = result;
        self.registers.flags.zero = self.registers.a == operand;
        self.registers.flags.negative = true;
        self.registers.flags.half_carry = (operand & 0x0f) > (self.registers.a & 0x0f);
        self.registers.flags.carry = overflowed;
    }

    fn sbc(&mut self, operand: Byte) {
        let operand = operand.read(self);
        let half_carry = (self.registers.a & 0x0f) < (operand & 0x0f);
        let (mut result, overflowed) = self.registers.a.overflowing_sub(operand);
        if self.registers.flags.carry {
            result -= 1;  // TODO: Verify that this is right
        }

        self.registers.a = result;
        self.registers.flags.zero = self.registers.a == operand;
        self.registers.flags.negative = true;
        self.registers.flags.half_carry = (operand & 0x0f) > (self.registers.a & 0x0f);
        self.registers.flags.carry = overflowed;
    }

    fn add(&mut self, operand: Byte) {
        let operand = operand.read(self);
        let half_carry = ((self.registers.a & 0x0f) + (operand & 0x0f)) & 0x10 != 0;
        let (result, overflowed) = self.registers.a.overflowing_add(operand);

        self.registers.a = result;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = half_carry;
        self.registers.flags.carry = overflowed;
    }

    fn adc(&mut self, operand: Byte) {
        let operand = operand.read(self);
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

    fn and(&mut self, operand: Byte) {
        let operand = operand.read(self);

        self.registers.a &= operand;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = true;
        self.registers.flags.carry = false;
    }

    fn or(&mut self, operand: Byte) {
        let operand = operand.read(self);
        self.registers.a |= operand;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = false;
    }

    fn xor(&mut self, operand: Byte) {
        let operand = operand.read(self);
        self.registers.a ^= operand;
        self.registers.flags.zero = self.registers.a == 0;
        self.registers.flags.negative = false;
        self.registers.flags.half_carry = false;
        self.registers.flags.carry = false;
    }

    fn cpl(&mut self) {
        self.registers.a = !self.registers.a;
        // Zero flag not affected
        self.registers.flags.negative = true;
        self.registers.flags.half_carry = true;
        // Carry flag not affected
    }




    fn call(&mut self, condition: Option<FlagCondition>, address: u16) {
        if self.registers.flags.meets_condition(condition) {
            let pc = self.program_counter;
            self.push_word(pc);
            self.program_counter = address;
        }
    }

    fn ret(&mut self, condition: Option<FlagCondition>) {
        if self.registers.flags.meets_condition(condition) {
            let pc = self.pop_word();
            self.program_counter = pc;
        }
    }

    fn reti(&mut self) {
        let pc = self.pop_word();
        self.program_counter = pc;
        self.ime = true;
    }

    fn rst(&mut self, address: u8) {
        let pc = self.program_counter;
        self.push_word(pc);
        self.program_counter = 0x0000 + (address as u16);
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
        let do_pause = match (dest, src) {
            // (MutableByte::Register(Register::A), Byte::Register(Register::B)) => true,
            _ => false,
        };

        if do_pause {
            println!("\n     Before:  A = {:02x}, B = {:02x}\n", self.registers.a, self.registers.b);
        }

        let value = src.read(self);
        dest.write(value, self);

        if do_pause {
            println!("     After:   A = {:02x}, B = {:02x}\n", self.registers.a, self.registers.b);
            pause();
        }

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

    /// Access cost in cycles
    fn cycles(self) -> i32 {
        match self {
            Byte::Register(_) => 0,
            Byte::RegisterIndirect(_)
            | Byte::DoubleRegisterIndirect(_)
            | Byte::DoubleRegisterIndirectInc(_)
            | Byte::DoubleRegisterIndirectDec(_) => 4,
            Byte::Immediate(_) => 4,
            Byte::ImmediateIndirect(_) => 12,
            Byte::ImmediateIndirectHigh(_) => 8,
        }
    }

    /// Number of bytes added to instruction length
    fn size_in_bytes(self) -> usize {
        match self {
            Byte::Immediate(_) | Byte::ImmediateIndirectHigh(_) => 1,
            Byte::ImmediateIndirect(_) => 2,
            _ => 0,
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

    fn cycles(self) -> i32 {
        Byte::from(self).cycles()
    }

    fn size_in_bytes(self) -> usize {
        Byte::from(self).size_in_bytes()
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

    fn cycles(self) -> i32 {
        match self {
            Word::StackPointer => 0,
            Word::DoubleRegister(_) => 0,
            Word::Immediate(_) => 8,
            Word::ImmediateIndirect(_) => 12,
        }
    }

    fn size_in_bytes(self) -> usize {
        match self {
            Word::Immediate(_) | Word::ImmediateIndirect(_) => 2,
            _ => 0,
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

    fn cycles(self) -> i32 {
        Word::from(self).cycles()
    }

    fn size_in_bytes(self) -> usize {
        Word::from(self).size_in_bytes()
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

    EnableInterrupts,
    DisableInterrupts,

    Inc8(MutableByte),
    Inc16(MutableWord),

    Dec8(MutableByte),
    Dec16(MutableWord),

    Load8(MutableByte, Byte),
    Load16(MutableWord, Word),

    Add16(MutableWord, Word),

    Add(Byte),
    Adc(Byte),
    Sub(Byte),
    Sbc(Byte),
    And(Byte),
    Xor(Byte),
    Or(Byte),
    Cp(Byte),

    Cpl,


    Push(Word),
    Pop(MutableWord),


    Ret(Option<FlagCondition>),
    Reti,

    Rst(u8),

    Nop,
    Jump(Option<FlagCondition>, JumpTarget),


    // CB
    Swap(MutableByte),
    Bit(u8, Byte),
    Set(u8, MutableByte),
    Res(u8, MutableByte),

}

impl Instruction {

    fn decode(mut address: u16, memory: &MemoryUnit) -> Instruction {
        macro_rules! get_next {
            (Byte) => {
                {
                    let next_byte = memory.read_byte(address);
                    address += 1;
                    next_byte
                }
            };
            (Word) => {
                {
                    let next_word = memory.read_word(address);
                    address += 2;
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
        match opcode {

            // ZAC (for faster ctrl+f)

            0x00 => Nop,
            //
            0x20 => Jump(flag!(NZ), immediate!(JumpTarget Relative)),
            0x30 => Jump(flag!(NC), immediate!(JumpTarget Relative)),

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

            0x04 => Inc8(register!(mut B)),
            0x14 => Inc8(register!(mut D)),
            0x24 => Inc8(register!(mut H)),
            0x34 => Inc8(double_register_indirect!(mut HL)),

            0x05 => Dec8(register!(mut B)),
            0x15 => Dec8(register!(mut D)),
            0x25 => Dec8(register!(mut H)),
            0x35 => Dec8(double_register_indirect!(mut HL)),

            0x06 => Load8(register!(mut B), immediate!(Byte)),
            0x16 => Load8(register!(mut D), immediate!(Byte)),
            0x26 => Load8(register!(mut H), immediate!(Byte)),
            0x36 => Load8(double_register_indirect!(mut HL), immediate!(Byte)),

            // 7

            //
            0x18 => Jump(flag!(*), immediate!(JumpTarget Relative)),
            0x28 => Jump(flag!(Z), immediate!(JumpTarget Relative)),
            0x38 => Jump(flag!(C), immediate!(JumpTarget Relative)),

            0x09 => Add16(double_register!(mut HL), double_register!(BC)),
            0x19 => Add16(double_register!(mut HL), double_register!(DE)),
            0x29 => Add16(double_register!(mut HL), double_register!(HL)),
            0x39 => Add16(double_register!(mut HL), Word::StackPointer),

            0x0a => Load8(register!(mut A), double_register_indirect!(BC)),
            0x1a => Load8(register!(mut A), double_register_indirect!(DE)),
            0x2a => Load8(register!(mut A), double_register_indirect!(HL+)),
            0x3a => Load8(register!(mut A), double_register_indirect!(HL-)),

            0x0b => Dec16(double_register!(mut BC)),
            0x1b => Dec16(double_register!(mut DE)),
            0x2b => Dec16(double_register!(mut HL)),
            0x3b => Dec16(MutableWord::StackPointer),

            0x0c => Inc8(register!(mut C)),
            0x1c => Inc8(register!(mut E)),
            0x2c => Inc8(register!(mut L)),
            0x3c => Inc8(register!(mut A)),

            0x0d => Dec8(register!(mut C)),
            0x1d => Dec8(register!(mut E)),
            0x2d => Dec8(register!(mut L)),
            0x3d => Dec8(register!(mut A)),

            0x0e => Load8(register!(mut C), immediate!(Byte)),
            0x1e => Load8(register!(mut E), immediate!(Byte)),
            0x2e => Load8(register!(mut L), immediate!(Byte)),
            0x3e => Load8(register!(mut A), immediate!(Byte)),

            0x2f => Cpl,

            0x40 => Load8(register!(mut B), register!(B)),
            0x41 => Load8(register!(mut B), register!(C)),
            0x42 => Load8(register!(mut B), register!(D)),
            0x43 => Load8(register!(mut B), register!(E)),
            0x44 => Load8(register!(mut B), register!(H)),
            0x45 => Load8(register!(mut B), register!(L)),
            0x46 => Load8(register!(mut B), double_register_indirect!(HL)),
            0x47 => Load8(register!(mut B), register!(A)),

            0x48 => Load8(register!(mut C), register!(B)),
            0x49 => Load8(register!(mut C), register!(C)),
            0x4a => Load8(register!(mut C), register!(D)),
            0x4b => Load8(register!(mut C), register!(E)),
            0x4c => Load8(register!(mut C), register!(H)),
            0x4d => Load8(register!(mut C), register!(L)),
            0x4e => Load8(register!(mut C), double_register_indirect!(HL)),
            0x4f => Load8(register!(mut C), register!(A)),

            0x50 => Load8(register!(mut D), register!(B)),
            0x51 => Load8(register!(mut D), register!(C)),
            0x52 => Load8(register!(mut D), register!(D)),
            0x53 => Load8(register!(mut D), register!(E)),
            0x54 => Load8(register!(mut D), register!(H)),
            0x55 => Load8(register!(mut D), register!(L)),
            0x56 => Load8(register!(mut D), double_register_indirect!(HL)),
            0x57 => Load8(register!(mut D), register!(A)),

            0x58 => Load8(register!(mut E), register!(B)),
            0x59 => Load8(register!(mut E), register!(C)),
            0x5a => Load8(register!(mut E), register!(D)),
            0x5b => Load8(register!(mut E), register!(E)),
            0x5c => Load8(register!(mut E), register!(H)),
            0x5d => Load8(register!(mut E), register!(L)),
            0x5e => Load8(register!(mut E), double_register_indirect!(HL)),
            0x5f => Load8(register!(mut E), register!(A)),

            0x60 => Load8(register!(mut H), register!(B)),
            0x61 => Load8(register!(mut H), register!(C)),
            0x62 => Load8(register!(mut H), register!(D)),
            0x63 => Load8(register!(mut H), register!(E)),
            0x64 => Load8(register!(mut H), register!(H)),
            0x65 => Load8(register!(mut H), register!(L)),
            0x66 => Load8(register!(mut H), double_register_indirect!(HL)),
            0x67 => Load8(register!(mut H), register!(A)),

            0x68 => Load8(register!(mut L), register!(B)),
            0x69 => Load8(register!(mut L), register!(C)),
            0x6a => Load8(register!(mut L), register!(D)),
            0x6b => Load8(register!(mut L), register!(E)),
            0x6c => Load8(register!(mut L), register!(H)),
            0x6d => Load8(register!(mut L), register!(L)),
            0x6e => Load8(register!(mut L), double_register_indirect!(HL)),
            0x6f => Load8(register!(mut L), register!(A)),

            0x70 => Load8(double_register_indirect!(mut HL), register!(B)),
            0x71 => Load8(double_register_indirect!(mut HL), register!(C)),
            0x72 => Load8(double_register_indirect!(mut HL), register!(D)),
            0x73 => Load8(double_register_indirect!(mut HL), register!(E)),
            0x74 => Load8(double_register_indirect!(mut HL), register!(H)),
            0x75 => Load8(double_register_indirect!(mut HL), register!(L)),
            // 0x76 => Halt,
            0x77 => Load8(double_register_indirect!(mut HL), register!(A)),

            0x78 => Load8(register!(mut A), register!(B)),
            0x79 => Load8(register!(mut A), register!(C)),
            0x7a => Load8(register!(mut A), register!(D)),
            0x7b => Load8(register!(mut A), register!(E)),
            0x7c => Load8(register!(mut A), register!(H)),
            0x7d => Load8(register!(mut A), register!(L)),
            0x7e => Load8(register!(mut A), double_register_indirect!(HL)),
            0x7f => Load8(register!(mut A), register!(A)),

            0x80 => Add(register!(B)),
            0x81 => Add(register!(C)),
            0x82 => Add(register!(D)),
            0x83 => Add(register!(E)),
            0x84 => Add(register!(H)),
            0x85 => Add(register!(L)),
            0x86 => Add(double_register_indirect!(HL)),
            0x87 => Add(register!(A)),

            0x88 => Adc(register!(B)),
            0x89 => Adc(register!(C)),
            0x8a => Adc(register!(D)),
            0x8b => Adc(register!(E)),
            0x8c => Adc(register!(H)),
            0x8d => Adc(register!(L)),
            0x8e => Adc(double_register_indirect!(HL)),
            0x8f => Adc(register!(A)),

            0x90 => Sub(register!(B)),
            0x91 => Sub(register!(C)),
            0x92 => Sub(register!(D)),
            0x93 => Sub(register!(E)),
            0x94 => Sub(register!(H)),
            0x95 => Sub(register!(L)),
            0x96 => Sub(double_register_indirect!(HL)),
            0x97 => Sub(register!(A)),

            0x98 => Sbc(register!(B)),
            0x99 => Sbc(register!(C)),
            0x9a => Sbc(register!(D)),
            0x9b => Sbc(register!(E)),
            0x9c => Sbc(register!(H)),
            0x9d => Sbc(register!(L)),
            0x9e => Sbc(double_register_indirect!(HL)),
            0x9f => Sbc(register!(A)),

            0xa0 => And(register!(B)),
            0xa1 => And(register!(C)),
            0xa2 => And(register!(D)),
            0xa3 => And(register!(E)),
            0xa4 => And(register!(H)),
            0xa5 => And(register!(L)),
            0xa6 => And(double_register_indirect!(HL)),
            0xa7 => And(register!(A)),

            0xa8 => Xor(register!(B)),
            0xa9 => Xor(register!(C)),
            0xaa => Xor(register!(D)),
            0xab => Xor(register!(E)),
            0xac => Xor(register!(H)),
            0xad => Xor(register!(L)),
            0xae => Xor(double_register_indirect!(HL)),
            0xaf => Xor(register!(A)),

            0xb0 => Or(register!(B)),
            0xb1 => Or(register!(C)),
            0xb2 => Or(register!(D)),
            0xb3 => Or(register!(E)),
            0xb4 => Or(register!(H)),
            0xb5 => Or(register!(L)),
            0xb6 => Or(double_register_indirect!(HL)),
            0xb7 => Or(register!(A)),

            0xb8 => Cp(register!(B)),
            0xb9 => Cp(register!(C)),
            0xba => Cp(register!(D)),
            0xbb => Cp(register!(E)),
            0xbc => Cp(register!(H)),
            0xbd => Cp(register!(L)),
            0xbe => Cp(double_register_indirect!(HL)),
            0xbf => Cp(register!(A)),

            0xc0 => Ret(flag!(NZ)),
            0xd0 => Ret(flag!(NC)),
            0xe0 => Load8(MutableByte::ImmediateIndirectHigh(get_next!(Byte)), register!(A)),
            0xf0 => Load8(register!(mut A), Byte::ImmediateIndirectHigh(get_next!(Byte))),

            0xc1 => Pop(double_register!(mut BC)),
            0xd1 => Pop(double_register!(mut DE)),
            0xe1 => Pop(double_register!(mut HL)),
            0xf1 => Pop(double_register!(mut AF)),

            0xc2 => Jump(flag!(NZ), immediate!(JumpTarget Absolute)),
            0xd2 => Jump(flag!(NC), immediate!(JumpTarget Absolute)),
            0xe2 => Load8(MutableByte::RegisterIndirect(Register::C), register!(A)),
            0xf2 => Load8(register!(mut A), Byte::RegisterIndirect(Register::C)),

            0xc3 => Jump(flag!(*), immediate!(JumpTarget Absolute)),
            // 0xd3 is an undefined opcode
            // 0xe3 is an undefined opcode
            0xf3 => DisableInterrupts,

            0xc4 => Call(flag!(NZ), get_next!(Word)),
            0xd4 => Call(flag!(NC), get_next!(Word)),
            // 0xe4 is an undefined opcode
            // 0xf4 is an undefined opcode

            0xc5 => Push(double_register!(BC)),
            0xd5 => Push(double_register!(DE)),
            0xe5 => Push(double_register!(HL)),
            0xf5 => Push(double_register!(AF)),

            0xc6 => Add(immediate!(Byte)),
            0xd6 => Sub(immediate!(Byte)),
            0xe6 => And(immediate!(Byte)),
            0xf6 => Or(immediate!(Byte)),

            0xc7 => Rst(0x00),
            0xd7 => Rst(0x10),
            0xe7 => Rst(0x20),
            0xf7 => Rst(0x30),

            0xc8 => Ret(flag!(Z)),
            0xd8 => Ret(flag!(C)),
            //
            //

            0xc9 => Ret(flag!(*)),
            0xd9 => Reti,
            0xe9 => Jump(flag!(*), JumpTarget::Absolute(double_register!(HL))),
            0xf9 => Load16(MutableWord::StackPointer, double_register!(HL)),

            0xca => Jump(flag!(Z), immediate!(JumpTarget Absolute)),
            0xda => Jump(flag!(C), immediate!(JumpTarget Absolute)),
            0xea => Load8(MutableByte::ImmediateIndirect(get_next!(Word)), register!(A)),
            0xfa => Load8(register!(mut A), Byte::ImmediateIndirect(get_next!(Word))),

            0xcb => Instruction::decode_cb(get_next!(Byte)),
            // 0xfc is an undefined opcode
            // 0xfc is an undefined opcode
            0xfb => EnableInterrupts,

            0xcc => Call(flag!(Z), get_next!(Word)),
            0xdc => Call(flag!(C), get_next!(Word)),
            // 0xec is an undefined opcode
            // 0xfc is an undefined opcode

            0xcd => Call(flag!(*), get_next!(Word)),
            // 0xdd is an undefined opcode
            // 0xed is an undefined opcode
            // 0xfd is an undefined opcode

            0xce => Adc(immediate!(Byte)),
            0xde => Sbc(immediate!(Byte)),
            0xee => Xor(immediate!(Byte)),
            0xfe => Cp(immediate!(Byte)),

            0xcf => Rst(0x08),
            0xdf => Rst(0x18),
            0xef => Rst(0x28),
            0xff => Rst(0x38),

            _ => panic!("Unimplemented opcode 0x{:02x} at address 0x{:04x}", opcode, address),
        }
    }

    fn decode_cb(opcode: u8) -> Instruction {
        use self::Instruction::*;
        match opcode {


            0x30 => Swap(MutableByte::Register(Register::B)),
            0x31 => Swap(MutableByte::Register(Register::C)),
            0x32 => Swap(MutableByte::Register(Register::D)),
            0x33 => Swap(MutableByte::Register(Register::E)),
            0x34 => Swap(MutableByte::Register(Register::H)),
            0x35 => Swap(MutableByte::Register(Register::L)),
            0x36 => Swap(MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x37 => Swap(MutableByte::Register(Register::A)),




            0x40 => Bit(0, Byte::Register(Register::B)),
            0x41 => Bit(0, Byte::Register(Register::C)),
            0x42 => Bit(0, Byte::Register(Register::D)),
            0x43 => Bit(0, Byte::Register(Register::E)),
            0x44 => Bit(0, Byte::Register(Register::H)),
            0x45 => Bit(0, Byte::Register(Register::L)),
            0x46 => Bit(0, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x47 => Bit(0, Byte::Register(Register::A)),

            0x48 => Bit(1, Byte::Register(Register::B)),
            0x49 => Bit(1, Byte::Register(Register::C)),
            0x4a => Bit(1, Byte::Register(Register::D)),
            0x4b => Bit(1, Byte::Register(Register::E)),
            0x4c => Bit(1, Byte::Register(Register::H)),
            0x4d => Bit(1, Byte::Register(Register::L)),
            0x4e => Bit(1, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x4f => Bit(1, Byte::Register(Register::A)),

            0x50 => Bit(2, Byte::Register(Register::B)),
            0x51 => Bit(2, Byte::Register(Register::C)),
            0x52 => Bit(2, Byte::Register(Register::D)),
            0x53 => Bit(2, Byte::Register(Register::E)),
            0x54 => Bit(2, Byte::Register(Register::H)),
            0x55 => Bit(2, Byte::Register(Register::L)),
            0x56 => Bit(2, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x57 => Bit(2, Byte::Register(Register::A)),

            0x58 => Bit(3, Byte::Register(Register::B)),
            0x59 => Bit(3, Byte::Register(Register::C)),
            0x5a => Bit(3, Byte::Register(Register::D)),
            0x5b => Bit(3, Byte::Register(Register::E)),
            0x5c => Bit(3, Byte::Register(Register::H)),
            0x5d => Bit(3, Byte::Register(Register::L)),
            0x5e => Bit(3, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x5f => Bit(3, Byte::Register(Register::A)),

            0x60 => Bit(4, Byte::Register(Register::B)),
            0x61 => Bit(4, Byte::Register(Register::C)),
            0x62 => Bit(4, Byte::Register(Register::D)),
            0x63 => Bit(4, Byte::Register(Register::E)),
            0x64 => Bit(4, Byte::Register(Register::H)),
            0x65 => Bit(4, Byte::Register(Register::L)),
            0x66 => Bit(4, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x67 => Bit(4, Byte::Register(Register::A)),

            0x68 => Bit(5, Byte::Register(Register::B)),
            0x69 => Bit(5, Byte::Register(Register::C)),
            0x6a => Bit(5, Byte::Register(Register::D)),
            0x6b => Bit(5, Byte::Register(Register::E)),
            0x6c => Bit(5, Byte::Register(Register::H)),
            0x6d => Bit(5, Byte::Register(Register::L)),
            0x6e => Bit(5, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x6f => Bit(5, Byte::Register(Register::A)),

            0x70 => Bit(6, Byte::Register(Register::B)),
            0x71 => Bit(6, Byte::Register(Register::C)),
            0x72 => Bit(6, Byte::Register(Register::D)),
            0x73 => Bit(6, Byte::Register(Register::E)),
            0x74 => Bit(6, Byte::Register(Register::H)),
            0x75 => Bit(6, Byte::Register(Register::L)),
            0x76 => Bit(6, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x77 => Bit(6, Byte::Register(Register::A)),

            0x78 => Bit(7, Byte::Register(Register::B)),
            0x79 => Bit(7, Byte::Register(Register::C)),
            0x7a => Bit(7, Byte::Register(Register::D)),
            0x7b => Bit(7, Byte::Register(Register::E)),
            0x7c => Bit(7, Byte::Register(Register::H)),
            0x7d => Bit(7, Byte::Register(Register::L)),
            0x7e => Bit(7, Byte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x7f => Bit(7, Byte::Register(Register::A)),

            0x80 => Res(0, MutableByte::Register(Register::B)),
            0x81 => Res(0, MutableByte::Register(Register::C)),
            0x82 => Res(0, MutableByte::Register(Register::D)),
            0x83 => Res(0, MutableByte::Register(Register::E)),
            0x84 => Res(0, MutableByte::Register(Register::H)),
            0x85 => Res(0, MutableByte::Register(Register::L)),
            0x86 => Res(0, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x87 => Res(0, MutableByte::Register(Register::A)),

            0x88 => Res(1, MutableByte::Register(Register::B)),
            0x89 => Res(1, MutableByte::Register(Register::C)),
            0x8a => Res(1, MutableByte::Register(Register::D)),
            0x8b => Res(1, MutableByte::Register(Register::E)),
            0x8c => Res(1, MutableByte::Register(Register::H)),
            0x8d => Res(1, MutableByte::Register(Register::L)),
            0x8e => Res(1, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x8f => Res(1, MutableByte::Register(Register::A)),

            0x90 => Res(2, MutableByte::Register(Register::B)),
            0x91 => Res(2, MutableByte::Register(Register::C)),
            0x92 => Res(2, MutableByte::Register(Register::D)),
            0x93 => Res(2, MutableByte::Register(Register::E)),
            0x94 => Res(2, MutableByte::Register(Register::H)),
            0x95 => Res(2, MutableByte::Register(Register::L)),
            0x96 => Res(2, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x97 => Res(2, MutableByte::Register(Register::A)),

            0x98 => Res(3, MutableByte::Register(Register::B)),
            0x99 => Res(3, MutableByte::Register(Register::C)),
            0x9a => Res(3, MutableByte::Register(Register::D)),
            0x9b => Res(3, MutableByte::Register(Register::E)),
            0x9c => Res(3, MutableByte::Register(Register::H)),
            0x9d => Res(3, MutableByte::Register(Register::L)),
            0x9e => Res(3, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0x9f => Res(3, MutableByte::Register(Register::A)),

            0xa0 => Res(4, MutableByte::Register(Register::B)),
            0xa1 => Res(4, MutableByte::Register(Register::C)),
            0xa2 => Res(4, MutableByte::Register(Register::D)),
            0xa3 => Res(4, MutableByte::Register(Register::E)),
            0xa4 => Res(4, MutableByte::Register(Register::H)),
            0xa5 => Res(4, MutableByte::Register(Register::L)),
            0xa6 => Res(4, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xa7 => Res(4, MutableByte::Register(Register::A)),

            0xa8 => Res(5, MutableByte::Register(Register::B)),
            0xa9 => Res(5, MutableByte::Register(Register::C)),
            0xaa => Res(5, MutableByte::Register(Register::D)),
            0xab => Res(5, MutableByte::Register(Register::E)),
            0xac => Res(5, MutableByte::Register(Register::H)),
            0xad => Res(5, MutableByte::Register(Register::L)),
            0xae => Res(5, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xaf => Res(5, MutableByte::Register(Register::A)),

            0xb0 => Res(6, MutableByte::Register(Register::B)),
            0xb1 => Res(6, MutableByte::Register(Register::C)),
            0xb2 => Res(6, MutableByte::Register(Register::D)),
            0xb3 => Res(6, MutableByte::Register(Register::E)),
            0xb4 => Res(6, MutableByte::Register(Register::H)),
            0xb5 => Res(6, MutableByte::Register(Register::L)),
            0xb6 => Res(6, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xb7 => Res(6, MutableByte::Register(Register::A)),

            0xb8 => Res(7, MutableByte::Register(Register::B)),
            0xb9 => Res(7, MutableByte::Register(Register::C)),
            0xba => Res(7, MutableByte::Register(Register::D)),
            0xbb => Res(7, MutableByte::Register(Register::E)),
            0xbc => Res(7, MutableByte::Register(Register::H)),
            0xbd => Res(7, MutableByte::Register(Register::L)),
            0xbe => Res(7, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xbf => Res(7, MutableByte::Register(Register::A)),

            0xc0 => Set(0, MutableByte::Register(Register::B)),
            0xc1 => Set(0, MutableByte::Register(Register::C)),
            0xc2 => Set(0, MutableByte::Register(Register::D)),
            0xc3 => Set(0, MutableByte::Register(Register::E)),
            0xc4 => Set(0, MutableByte::Register(Register::H)),
            0xc5 => Set(0, MutableByte::Register(Register::L)),
            0xc6 => Set(0, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xc7 => Set(0, MutableByte::Register(Register::A)),

            0xc8 => Set(1, MutableByte::Register(Register::B)),
            0xc9 => Set(1, MutableByte::Register(Register::C)),
            0xca => Set(1, MutableByte::Register(Register::D)),
            0xcb => Set(1, MutableByte::Register(Register::E)),
            0xcc => Set(1, MutableByte::Register(Register::H)),
            0xcd => Set(1, MutableByte::Register(Register::L)),
            0xce => Set(1, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xcf => Set(1, MutableByte::Register(Register::A)),

            0xd0 => Set(2, MutableByte::Register(Register::B)),
            0xd1 => Set(2, MutableByte::Register(Register::C)),
            0xd2 => Set(2, MutableByte::Register(Register::D)),
            0xd3 => Set(2, MutableByte::Register(Register::E)),
            0xd4 => Set(2, MutableByte::Register(Register::H)),
            0xd5 => Set(2, MutableByte::Register(Register::L)),
            0xd6 => Set(2, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xd7 => Set(2, MutableByte::Register(Register::A)),

            0xd8 => Set(3, MutableByte::Register(Register::B)),
            0xd9 => Set(3, MutableByte::Register(Register::C)),
            0xda => Set(3, MutableByte::Register(Register::D)),
            0xdb => Set(3, MutableByte::Register(Register::E)),
            0xdc => Set(3, MutableByte::Register(Register::H)),
            0xdd => Set(3, MutableByte::Register(Register::L)),
            0xde => Set(3, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xdf => Set(3, MutableByte::Register(Register::A)),

            0xe0 => Set(4, MutableByte::Register(Register::B)),
            0xe1 => Set(4, MutableByte::Register(Register::C)),
            0xe2 => Set(4, MutableByte::Register(Register::D)),
            0xe3 => Set(4, MutableByte::Register(Register::E)),
            0xe4 => Set(4, MutableByte::Register(Register::H)),
            0xe5 => Set(4, MutableByte::Register(Register::L)),
            0xe6 => Set(4, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xe7 => Set(4, MutableByte::Register(Register::A)),

            0xe8 => Set(5, MutableByte::Register(Register::B)),
            0xe9 => Set(5, MutableByte::Register(Register::C)),
            0xea => Set(5, MutableByte::Register(Register::D)),
            0xeb => Set(5, MutableByte::Register(Register::E)),
            0xec => Set(5, MutableByte::Register(Register::H)),
            0xed => Set(5, MutableByte::Register(Register::L)),
            0xee => Set(5, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xef => Set(5, MutableByte::Register(Register::A)),

            0xf0 => Set(6, MutableByte::Register(Register::B)),
            0xf1 => Set(6, MutableByte::Register(Register::C)),
            0xf2 => Set(6, MutableByte::Register(Register::D)),
            0xf3 => Set(6, MutableByte::Register(Register::E)),
            0xf4 => Set(6, MutableByte::Register(Register::H)),
            0xf5 => Set(6, MutableByte::Register(Register::L)),
            0xf6 => Set(6, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xf7 => Set(6, MutableByte::Register(Register::A)),

            0xf8 => Set(7, MutableByte::Register(Register::B)),
            0xf9 => Set(7, MutableByte::Register(Register::C)),
            0xfa => Set(7, MutableByte::Register(Register::D)),
            0xfb => Set(7, MutableByte::Register(Register::E)),
            0xfc => Set(7, MutableByte::Register(Register::H)),
            0xfd => Set(7, MutableByte::Register(Register::L)),
            0xfe => Set(7, MutableByte::DoubleRegisterIndirect(DoubleRegister::HL)),
            0xff => Set(7, MutableByte::Register(Register::A)),

            _ => panic!("Unimplemented CB opcode 0x{:02x}", opcode),
        }
    }

    fn cycles(self, flags: &Flags) -> i32 {

        let jump_cycles = |condition, taken_cycles| {
            if flags.meets_condition(condition) {
                taken_cycles
            }
            else {
                0
            }
        };

        use self::Instruction::*;
        match self {

            Nop => 4,


            EnableInterrupts | DisableInterrupts => 4,


            Inc8(operand) | Dec8(operand) => 4 + operand.cycles() * 2,

            Add(operand)
            | Adc(operand)
            | Sub(operand)
            | Sbc(operand)
            | And(operand)
            | Xor(operand)
            | Or(operand)
            | Cp(operand) => 4 + operand.cycles(),
            Cpl => 4,


            Inc16(_) | Dec16(_) | Add16(_, _) => 8,

            Push(_) => 16,
            Pop(_) => 12,




            Jump(condition, JumpTarget::Absolute(target)) => 4 + target.cycles() + jump_cycles(condition, 4),
            Jump(condition, JumpTarget::Relative(_)) => 8 + jump_cycles(condition, 4),

            Call(condition, _) => 12 + jump_cycles(condition, 12),
            Ret(None) => 16,
            Ret(condition) => 8 + jump_cycles(condition, 12),
            Reti => 16,


            Rst(_) => 16,


            Load8(dest, src) => 4 + dest.cycles() + src.cycles(),
            Load16(dest, src) => 8 + dest.cycles() + src.cycles(),


            Bit(_, operand) => 8 + operand.cycles() * 2,
            Swap(operand)
            | Set(_, operand)
            | Res(_, operand) => 8 + operand.cycles() * 2,


        }
    }

    fn size_in_bytes(self) -> usize {
        use self::Instruction::*;
        1 + match self {
            Load8(dest, src) => dest.size_in_bytes() + src.size_in_bytes(),
            Load16(dest, src) => dest.size_in_bytes() + src.size_in_bytes(),

            Jump(_, JumpTarget::Relative(_)) => 1,
            Jump(_, JumpTarget::Absolute(target)) => target.size_in_bytes(),
            Call(_, _) => 2,



            Add(operand)
            | Adc(operand)
            | Sub(operand)
            | Sbc(operand)
            | And(operand)
            | Xor(operand)
            | Or(operand)
            | Cp(operand) => operand.size_in_bytes(),


            Add16(dest, src) => dest.size_in_bytes() + src.size_in_bytes(),


            Swap(_) |Bit(_, _) | Set(_, _) | Res(_, _) => 1,

            Ret(_) => 0,
            EnableInterrupts | DisableInterrupts => 0,

            Inc8(operand) | Dec8(operand) => operand.size_in_bytes(),
            Inc16(operand) | Dec16(operand) => operand.size_in_bytes(),
            Push(operand) => operand.size_in_bytes(),
            Pop(operand) => operand.size_in_bytes(),

            Cpl => 0,
            Nop => 0,
            Ret(_) | Reti | Rst(_) => 0,

        }
    }

}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::Instruction::*;
        match *self {

            Call(None, address) => write!(f, "CALL ${:04x}", address),
            Call(Some(condition), address) => write!(f, "CALL {}, ${:04x}", condition, address),

            Add16(dest, src) => write!(f, "ADD {}, {}", dest, src),

            Add(operand) => write!(f, "ADD {}", operand),
            Adc(operand) => write!(f, "ADC {}", operand),
            Sub(operand) => write!(f, "SUB {}", operand),
            Sbc(operand) => write!(f, "SBC {}", operand),
            And(operand) => write!(f, "AND {}", operand),
            Xor(operand) => write!(f, "XOR {}", operand),
            Or(operand) => write!(f, "OR {}", operand),
            Cp(operand) => write!(f, "CP {}", operand),

            Cpl => write!(f, "CPL"),

            EnableInterrupts => write!(f, "EI"),
            DisableInterrupts => write!(f, "DI"),

            Dec8(operand) => write!(f, "DEC {}", operand),
            Dec16(operand) => write!(f, "DEC {}", operand),

            Inc8(operand) => write!(f, "INC {}", operand),
            Inc16(operand) => write!(f, "INC {}", operand),

            Push(operand) => write!(f, "PUSH {}", operand),
            Pop(operand) => write!(f, "POP {}", operand),

            Jump(None, JumpTarget::Relative(offset)) => write!(f, "JR {}", offset),
            Jump(Some(condition), JumpTarget::Relative(offset)) => write!(f, "JR {}, {}", condition, offset),

            Jump(None, JumpTarget::Absolute(Word::DoubleRegister(double_register))) => write!(f, "JP ({})", double_register),
            Jump(None, JumpTarget::Absolute(address)) => write!(f, "JP {}", address),
            Jump(Some(condition), JumpTarget::Absolute(address)) => write!(f, "JP {}, {}", condition, address),

            Ret(None) => write!(f, "RET"),
            Ret(Some(condition)) => write!(f, "RET {}", condition),
            Reti => write!(f, "RETI"),

            Rst(address) => write!(f, "RST ${:02x}", address),


            Load8(dest, src) => write!(f, "LD {}, {}", dest, src),
            Load16(dest, src) => write!(f, "LD {}, {}", dest, src),


            Nop => write!(f, "NOP"),


            Swap(operand) => write!(f, "SWAP {}", operand),
            Bit(bit, operand) => write!(f, "BIT {}, {}", bit, operand),
            Set(bit, operand) => write!(f, "SET {}, {}", bit, operand),
            Res(bit, operand) => write!(f, "RES {}, {}", bit, operand),

        }
    }
}
