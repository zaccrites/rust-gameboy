

use std::rc::Rc;
use std::cell::RefCell;

// use std::drop::Drop;

use gameboy::memory::MemoryUnit;


const DISPLAY_RESOLUTION_X: usize = 160;
const DISPLAY_RESOLUTION_Y: usize = 144;

#[derive(Debug, Clone, Copy)]
enum Color {
    Lightest,
    Lighter,
    Darker,
    Darkest,
    // Disabled,  // Different color for when LCD is disabled?
}

impl Color {
    // TODO: Load colors from a config file for different styles, like BGB
    fn to_rgb(self) -> (u8, u8, u8) {
        match self {
            Color::Lightest => (224, 248, 208),
            Color::Lighter  => (136, 192, 112),
            Color::Darker   => (52, 104, 86),
            Color::Darkest  => (8, 24, 32),
        }
    }


}




/// LCD Controller Mode
enum Mode {
    HBlank = 0,            // CPU can access OAM and VRAM
    VBlank = 1,            // CPU can access OAM and VRAM
    OamRead = 2,           // CPU can access VRAM
    OamVramRead = 3,       // CPU can't access either
}



const LY_PORT_NUMBER: u8 = 0x44;


pub struct Gpu<'a> {

    memory: Rc<RefCell<MemoryUnit<'a>>>,

    /// Current controller mode
    mode: Mode,
    /// Number of cycles left until mode switch
    mode_switch_timer: i32,


    scanline: u8,

}

impl<'a> Gpu<'a> {

    pub fn new(memory: Rc<RefCell<MemoryUnit<'a>>>) -> Gpu<'a> {

        Gpu {
            memory,
            mode: Mode::OamRead,
            mode_switch_timer: 0,


            scanline: 0,
        }



    }

    pub fn reset(&mut self) {
        self.set_mode(Mode::OamRead);
    }

    pub fn step(&mut self, cycles: i32) {


        // self.mode_switch_timer -= cycles;
        // if self.mode_switch_timer <= 0 {
        //     let (next_mode, mode_switch_timer) = match self.mode {
        //         Mode::HBlank => ()
        //     }
        //     self.mode = next_mode;
        //     self.mode_switch_timer = mode_switch_timer;
        // }

        if let Some(_) = self.memory.borrow_mut().check_for_io_write(LY_PORT_NUMBER) {
            self.scanline = 0;
        }
        else {
            self.scanline += 1;
            if self.scanline > 153 as u8 {
                self.scanline = 0;
            }
        }
        self.memory.borrow_mut().set_io_read_value(LY_PORT_NUMBER, self.scanline);

    }


    pub fn is_in_vblank(&self) -> bool {
        self.scanline >= DISPLAY_RESOLUTION_Y as u8
    }


    fn set_mode(&mut self, mode: Mode) {
        // self.mode = mode;
        // self.mode_switch_timer = match mode {
        //     Mode::HBlank => 777,
        //     Mode::
        // }
    }


}
