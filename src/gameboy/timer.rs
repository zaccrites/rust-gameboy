
use std::rc::Rc;
use std::cell::RefCell;

use gameboy::memory::MemoryUnit;


const DIV_PORT_NUMBER: u8 = 0x04;       // Timer Divider Register
const TIMA_PORT_NUMBER: u8 = 0x05;      // Timer Counter Register
const TMA_PORT_NUMBER: u8 = 0x06;       // Timer Modulo Register
const TAC_PORT_NUMBER: u8 = 0x07;       // Timer Control Register


// TODO: Extract this 1_000_000 cycles per second constant somewhere else
const DIVIDER_CYCLES: i32 = 1_000_000 / 16384;

pub struct Timer<'a> {
    memory: Rc<RefCell<MemoryUnit<'a>>>,

    divider_cycles_remaining: i32,
    divider_value: u8,
}

impl<'a> Timer<'a> {

    pub fn new(memory: Rc<RefCell<MemoryUnit<'a>>>) -> Timer {
        Timer {
            memory,

            divider_cycles_remaining: 0,
            divider_value: 0,
        }
    }

    pub fn reset(&mut self) {
        self.divider_cycles_remaining = DIVIDER_CYCLES;
        self.divider_value = 0;
    }

    pub fn step(&mut self, cycles: i32) {

        if let Some(_) = self.memory.borrow_mut().check_for_io_write(DIV_PORT_NUMBER) {
            self.divider_value = 0;
        }
        self.divider_cycles_remaining -= cycles;
        if self.divider_cycles_remaining <= 0 {
            self.divider_cycles_remaining += DIVIDER_CYCLES;
            self.divider_value = self.divider_value.wrapping_add(1);
        }
        self.memory.borrow_mut().set_io_read_value(DIV_PORT_NUMBER, self.divider_value);

    }

}
