
use gameboy::memory::MemoryUnit;


pub const INT_FLAG_ADDRESS: u16 = 0xff0f;
pub const INT_ENABLE_ADDRESS: u16 = 0xffff;


#[derive(Debug, Clone, Copy)]
pub enum Interrupt {
    VBlank = 0,
    LCDStat = 1,
    Timer = 2,
    Serial = 3,
    Joypad = 4,
}

impl Interrupt {

    pub fn request(self, memory: &mut MemoryUnit) {
        let start_value = memory.read_byte(INT_FLAG_ADDRESS);
        let new_value = start_value | (1 << (self as u16));
        // println!("Requested interrupt {:?}   start={:02x}   new={:02x}", self, start_value, new_value);
        memory.write_byte(INT_FLAG_ADDRESS, new_value);
    }

    pub fn reset(self, memory: &mut MemoryUnit) {
        let start_value = memory.read_byte(INT_FLAG_ADDRESS);
        let new_value = start_value & !(1 << (self as u16));
        memory.write_byte(INT_FLAG_ADDRESS, new_value);
    }

    pub fn is_requested_and_enabled(self, memory: &MemoryUnit) -> bool {
        let mask = 1 << (self as u16);
        let is_requested =  memory.read_byte(INT_FLAG_ADDRESS) & mask != 0;
        let is_enabled = memory.read_byte(INT_ENABLE_ADDRESS) & mask != 0;
        is_requested && is_enabled
    }

    pub fn get_handler_address(self) -> u16 {
        0x40 + 8 * (self as u16)
    }

    /// Get the next pending requested and enabled interrupt to service.
    pub fn get_pending_interrupt(memory: &MemoryUnit) -> Option<Interrupt> {
        let interrupts = [
            Interrupt::VBlank,
            Interrupt::LCDStat,
            Interrupt::Timer,
            Interrupt::Serial,
            Interrupt::Joypad,
        ];
        for interrupt in interrupts.iter() {
            if interrupt.is_requested_and_enabled(memory) {
                // println!("\n\n\nFound pending interrupt! {:?}\n\n\n", interrupt);
                return Some(*interrupt);
            }
        }
        None
    }

}
