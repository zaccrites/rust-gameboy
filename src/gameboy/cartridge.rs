
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

        // TODO: Make this a fn like title
        // Or return a Result for unsupported cartridge/MBC types?
        let cartridge_type_code = contents[0x0147];
        if cartridge_type_code != 0x00 {
            panic!("Unsupported cartridge type {:02x}h", cartridge_type_code);
        }

        let rom_size_code = contents[0x0148];
        if rom_size_code != 0 {
            panic!("ROM SIZE CODE WAS {:02x}h", rom_size_code);
        }

        let ram_size_code = contents[0x0149];
        if ram_size_code != 0 {
            panic!("RAM SIZE CODE WAS {:02x}h", ram_size_code);
        }



        Cartridge {
            rom: contents.into_boxed_slice(),
        }
    }

    pub fn title(&self) -> String {
        str::from_utf8(&self.rom[0x0134..0x0144]).unwrap().to_string()
    }

}
