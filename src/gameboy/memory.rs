
use super::cartridge::Cartridge;



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
            0x0000 ... 0x7fff => self.cartridge.rom[address as usize],


            // 0x0000 ... 0x3fff => self.cartridge.rom[address as usize],
            // This is only done with MBC, I think?
            // 0x4000 ... 0x7fff => panic!("Switchable ROM bank NN not yet supported!"),

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
    // io: Box<[u8]>,
    io_ports: Box<[IoPort]>,


    high_ram: Box<[u8]>,


    /// IE register, CPU interrupt mask
    interrupt_enable_register: u8,


    // UGH UGH UGH
    interrupt_flag_register: u8,



    // The GPU will lock and unlock these at times
    vram_locked: bool,
    oam_locked: bool,

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
            // io: vec![0; 128].into_boxed_slice(),
            io_ports: vec![IoPort::new(); 128].into_boxed_slice(),

            interrupt_enable_register: 0,


            interrupt_flag_register: 0,


            vram_locked: false,
            oam_locked: false,
        }
    }

    pub fn read_oam(&self, address: usize) -> u8 {
        if self.oam_locked { 0xff } else { self.oam[address] }
    }

    pub fn write_oam(&mut self, address: usize, value: u8) {
        if ! self.oam_locked {
            self.oam[address] = value;
        }
    }

    pub fn read_vram(&self, address: usize) -> u8 {
        if self.vram_locked { 0xff } else { self.vram[address] }
    }

    pub fn write_vram(&mut self, address: usize, value: u8) {
        if ! self.vram_locked {
            self.vram[address] = value;
        }
    }

    pub fn read_range(&self, address: u16, length: usize) -> Box<[u8]> {
        let mut data = Vec::with_capacity(length);
        for i in 0..length {
            let address = address + (i as u16);
            data.push(self.read_byte(address));
        }
        data.into_boxed_slice()
    }

    pub fn write_range(&mut self, address: u16, data: &[u8]) {
        for (i, byte) in data.iter().enumerate() {
            let address = address + (i as u16);
            self.write_byte(address, *byte);
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000 ... 0x7fff => self.rom.read_byte(address - 0x0000),

            0x8000 ... 0x9fff => self.read_vram((address - 0x8000) as usize),
            0xa000 ... 0xbfff => panic!("External RAM not yet supported!"),
            0xc000 ... 0xcfff => self.work_ram_0[(address - 0xc000) as usize],
            0xd000 ... 0xdfff => self.work_ram_1[(address - 0xd000) as usize],
            0xe000 ... 0xfdff => self.read_byte(address - 0xe000 + 0xc000),  // echoed
            0xfe00 ... 0xfe9f => self.read_oam((address - 0xfe00) as usize),
            0xfea0 ... 0xfeff => 0,  // not usable

            // 0xff00 ... 0xff7f => panic!("IO Ports not yet supported!"),
            // 0xff00 ... 0xff7f => {
            //     println!("Tried to read I/O port at 0x{:04x}", address);
            //     0
            // },


            // Ugh Ugh Ugh
            0xff0f => self.interrupt_flag_register,


            // TODO: I don't think this whole region is I/O. Investigate.
            0xff00 ... 0xff7f => self.io_ports[(address - 0xff00) as usize].read_value,

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

        // TODO: TETRIS PATCH. REMOVE THIS PLEASE!
        if address == 0xff80 {
            return;
        }

        match address {
            0x0000 ... 0x7fff => self.rom.write_byte(address, value),

            0x8000 ... 0x9fff => self.write_vram((address - 0x8000) as usize, value),
            0xa000 ... 0xbfff => panic!("External RAM not yet supported!"),
            0xc000 ... 0xcfff => self.work_ram_0[(address - 0xc000) as usize] = value,
            0xd000 ... 0xdfff => self.work_ram_1[(address - 0xd000) as usize] = value,
            0xe000 ... 0xfdff => self.write_byte(address - 0xe000 + 0xc000, value),  // echoed
            0xfe00 ... 0xfe9f => self.write_oam((address - 0xfe00) as usize, value),
            0xfea0 ... 0xfeff => (),  // not usable

            // 0xff00 ... 0xff7f => panic!("IO Ports not yet supported!"),
            // 0xff00 ... 0xff7f => {
            //     println!("Tried to write {} to I/O port at 0x{:04x}", value, address);
            // },


            // Ugh. What to do about this? It's not an I/O port at all!
            0xff0f => self.interrupt_flag_register = value,


            0xff00 ... 0xff7f => self.io_ports[(address - 0xff00) as usize].written_value = Some(value),

            0xff80 ... 0xfffe => self.high_ram[(address - 0xff80) as usize] = value,
            0xffff            => self.interrupt_enable_register = value,
            _ => unreachable!(),
        }
    }

    pub fn write_word(&mut self, address: u16, value: u16) {
        let low_byte = (value & 0xff) as u8;
        let high_byte = (value >> 8) as u8;
        self.write_byte(address, low_byte);
        self.write_byte(address + 1, high_byte);
    }

    /// Check to see if an IO port was written to, taking the written value
    /// if there is one.
    pub fn check_for_io_write(&mut self, port_number: u8) -> Option<u8> {
        self.io_ports[port_number as usize].written_value.take()
    }

    pub fn set_io_read_value(&mut self, port_number: u8, value: u8) {
        self.io_ports[port_number as usize].read_value = value;
    }

    pub fn get_io_read_value(&self, port_number: u8) -> u8 {
        self.io_ports[port_number as usize].read_value
    }

    pub fn reset(&mut self) {
        // TODO: Randomize RAM contents.

        // for port in self.io_ports.iter_mut() {
        //     *port = IoPort::new();
        // }

    }

}



#[derive(Debug, Clone, Copy)]
struct IoPort {

    /// Writing to an IO port puts a message in the mailbox. The owner
    /// may use it or discard it. Subsequent writes to a filled mailbox
    /// will overwrite the previous contents.
    written_value: Option<u8>,

    read_value: u8,
}

impl IoPort {

    fn new() -> IoPort {
        IoPort {
            written_value: None,
            read_value: 0
        }
    }

}


