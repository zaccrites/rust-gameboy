
use std::str;
use std::fs::File;
use std::io::prelude::*;


pub enum CartridgeType {
    RomOnly,
    // Others omitted for now
}



pub struct Cartridge {
    // cgb_flag: bool,
    // pub title: String,

    // pub rom_size: usize,

    // pub ram_size: usize,

    pub rom: Box<[u8]>,
}

impl Cartridge {
    // File path as str or path object or what?
    pub fn load(filename: &str) -> Cartridge {
        // TODO: proper error handling
        let mut f = File::open(filename).expect("file not found");

        // Since the files are all small (couple MB max), we can just
        // read the whole thing into a buffer.
        let mut contents = Vec::new();
        f.read_to_end(&mut contents).expect("error reading file");

        Cartridge {
            rom: contents.into_boxed_slice(),
        }
    }

    pub fn title(&self) -> String {
        str::from_utf8(&self.rom[0x0134..0x0144]).unwrap().to_string()
    }

}
