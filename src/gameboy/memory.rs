
use super::cartridge::Cartridge;


// These are private because the Cartridge won't own its own external
// memory banks. It just provides the information so that the memory
// unit can construct the correct number and type of banks.

enum MemoryBankType {
    Rom,
    Ram,
}


struct MemoryBank {
    bank_type: MemoryBankType,
    contents: Box<[u8]>,
}

impl MemoryBank {
    fn new(size: usize, bank_type: MemoryBankType) -> MemoryBank {
        MemoryBank {
            bank_type: bank_type,
            contents: vec![0; size].into_boxed_slice(),
        }
    }

    fn read_byte(&self, address: u16) -> u8 {
        // TODO: What if address >= size?
        self.contents[address as usize]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        // TODO: What if address >= size?
        self.contents[address as usize] = value
    }
}



struct Rom<'a> {
    cartridge: &'a Cartridge
}

impl<'a> Rom<'a> {
    fn new(cartridge: &'a Cartridge) -> Rom<'a> {
        Rom {
            cartridge
        }
    }

    fn read_byte(&self, address: u16) -> u8 {
        // TODO: Panic if the address is too large for ROM size
        // as reported by cartridge type
        match address {
            0x0000 ... 0x3fff => self.cartridge.rom[address as usize],
            0x4000 ... 0x7fff => panic!("Switchable ROM bank NN not yet supported!"),
            _ => panic!("invalid ROM address 0x{:04x}", address),
        }
    }

    fn write_byte(&self, address: u16, value: u8) {
        // ROM shouldn't actually be writeable, but certain memory
        // controllers may use it to switch banks (I think). If not,
        // delete this.
    }
}



pub struct MemoryUnit<'a> {

    rom: Rom<'a>,
    // rom_0: MemoryBank,
    // other rom banks?


    vram: Box<[u8]>,
    // external ram
    work_ram_0: Box<[u8]>,
    work_ram_1: Box<[u8]>,
    // echoes
    oam: Box<[u8]>,
    // unused

    // TODO: IO devices register with memory unit to recieve callbacks on read/write
    // io_ports: MemoryBank,  // ???
    io: Box<[u8]>,

    high_ram: Box<[u8]>,


    /// IE register, CPU interrupt mask
    interrupt_enable_register: u8,
}

impl<'a> MemoryUnit<'a> {

    pub fn new(cartridge: &'a Cartridge) -> MemoryUnit {

        const FOUR_KB: usize = 4 * 1024;
        const EIGHT_KB: usize = 8 * 1024;

        MemoryUnit {
            // rom_0: MemoryBank::new(FOUR_KB, MemoryBankType::Rom),
            rom: Rom::new(cartridge),

            // external ram struct could also take &Cartridge reference
            // to support switching banks

            vram: vec![0; EIGHT_KB].into_boxed_slice(),

            work_ram_0: vec![0; FOUR_KB].into_boxed_slice(),
            work_ram_1: vec![0; FOUR_KB].into_boxed_slice(),

            oam: vec![0; 160].into_boxed_slice(),

            high_ram: vec![0; 127].into_boxed_slice(),

            // TODO: Is this needed? Maybe reads to registered I/O devices
            // will just bypass this?
            io: vec![0; 128].into_boxed_slice(),

            interrupt_enable_register: 0,
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000 ... 0x7fff => self.rom.read_byte(address - 0x0000),

            0x8000 ... 0x9fff => self.vram[(address - 0x8000) as usize],
            0xa000 ... 0xbfff => panic!("External RAM not yet supported!"),
            0xc000 ... 0xcfff => self.work_ram_0[(address - 0xc000) as usize],
            0xd000 ... 0xdfff => self.work_ram_1[(address - 0xd000) as usize],
            0xe000 ... 0xfdff => self.read_byte(address - 0xe000 + 0xc000),  // echoed
            0xfe00 ... 0xfe9f => self.oam[(address - 0xfe00) as usize],
            0xfea0 ... 0xfeff => 0,  // not usable

            // 0xff00 ... 0xff7f => panic!("IO Ports not yet supported!"),
            // 0xff00 ... 0xff7f => {
            //     println!("Tried to read I/O port at 0x{:04x}", address);
            //     0
            // },

            // TODO: I don't think this whole region is I/O. Investigate.
            0xff00 ... 0xff7f => self.io[(address - 0xff00) as usize],

            0xff80 ... 0xfffe => self.high_ram[(address - 0xff80) as usize],
            0xffff            => self.interrupt_enable_register,
            _ => unreachable!(),
        }
    }

    pub fn read_word(&self, address: u16) -> u16 {
        let low_byte = self.read_byte(address) as u16;
        let high_byte = self.read_byte(address + 1) as u16;
        (high_byte << 8) | low_byte
    }

    pub fn write_byte(&mut self, address: u16, value: u8) {
        match address {
            0x0000 ... 0x7fff => self.rom.write_byte(address, value),

            0x8000 ... 0x9fff => self.vram[(address - 0x8000) as usize] = value,
            0xa000 ... 0xbfff => panic!("External RAM not yet supported!"),
            0xc000 ... 0xcfff => self.work_ram_0[(address - 0xc000) as usize] = value,
            0xd000 ... 0xdfff => self.work_ram_1[(address - 0xd000) as usize] = value,
            0xe000 ... 0xfdff => self.write_byte(address - 0xe000 + 0xc000, value),  // echoed
            0xfe00 ... 0xfe9f => self.oam[(address - 0xfe00) as usize] = value,
            0xfea0 ... 0xfeff => (),  // not usable

            // 0xff00 ... 0xff7f => panic!("IO Ports not yet supported!"),
            // 0xff00 ... 0xff7f => {
            //     println!("Tried to write {} to I/O port at 0x{:04x}", value, address);
            // },
            0xff00 ... 0xff7f => self.io[(address - 0xff00) as usize] = value,

            0xff80 ... 0xfffe => self.high_ram[(address - 0xff80) as usize] = value,
            0xffff            => self.interrupt_enable_register = value,
            _ => unreachable!(),
        }
    }

    pub fn write_word(&mut self, address: u16, value: u16) {
        let low_byte = (value & 0x0f) as u8;
        let high_byte = ((value >> 8) & 0x0f) as u8;
        self.write_byte(address, low_byte);
        self.write_byte(address + 1, high_byte);
    }


    pub fn reset(&mut self) {
        // TODO: Randomize RAM contents.
    }

}
