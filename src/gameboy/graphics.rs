

use std::rc::Rc;
use std::cell::RefCell;

// use std::drop::Drop;

use gameboy::memory::MemoryUnit;


pub const DISPLAY_RESOLUTION_X: u32 = 160;
pub const DISPLAY_RESOLUTION_Y: u32 = 144;


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



    pub fn get_pixel_data(&self) -> Box<[u8]> {

        // The gameboy framebuffer is 256x256 pixels. Each tile is 8x8 pixels,
        // so there are 32x32 tiles in the framebuffer, or 1024 total.
        let background_tile_map1 = self.memory.borrow().read_range(0x9800, 1024);
        let background_tile_map2 = self.memory.borrow().read_range(0x9c00, 1024);

        // TODO: Other (signed tile numbers) pattern table?
        let background_tile_patterns: Vec<_> = (0..256).map(|tile_number| {
            let address = 0x8000 + 16 * tile_number;
            let tile_pattern_data = self.memory.borrow().read_range(address, 16);

            let mut pixels = Vec::with_capacity(64);
            for i in 0..8 {
                let byte1 = tile_pattern_data[2 * i];
                let byte2 = tile_pattern_data[2 * i + 1];

                for bit in (0..8).rev() {
                    let bit1 = byte1 & (1 << bit) != 0;
                    let bit2 = byte2 & (1 << bit) != 0;

                    let color = match (bit1, bit2) {
                        (false, false) => Color::Lightest,
                        (false, true) => Color::Lighter,
                        (true, false) => Color::Darker,
                        (true, true) => Color::Darkest,
                    };
                    pixels.push(color);
                }
            }
            pixels.into_boxed_slice()
        }).collect();


        // TODO: Window

        let mut pixel_data = Vec::new();
        for display_y in 0..DISPLAY_RESOLUTION_Y {
            for display_x in 0..DISPLAY_RESOLUTION_X {

                // TODO: Scrolling
                let tile_y = display_y % 8;
                let tile_x = display_x % 8;
                let tile_i = ((tile_y * 8) + tile_x) as usize;  // Pixel index into tile pattern

                // Note: This is per row of framebuffer, NOT per row
                // of actual display resolution. The gameboy's internal
                // frame buffer is 256x256; larger than the screen's
                // display. That's why you have the scroll registers.
                let tiles_per_row = 256 / 8;
                // TODO: Scrolling
                let tile_index_y = display_y / 8;
                let tile_index_x = display_x / 8;
                let tile_index = ((tile_index_y * tiles_per_row) + tile_index_x) as usize;
                // println!("({}, {})  tile_index y={}, x={}, index={}", display_x, display_y, tile_index_y, tile_index_x, tile_index);


                // TODO: This strategy re-loads the same tile data for every
                // pixel in every tile. If the same tile is reused, this waste
                // is even worse. It would probably be better to collect the
                // usage of each tile and blit them directly into the pixel
                // data on a pattern-by-pattern basis, rather than on a
                // pixel-by-pixel basis.
                //
                // Actually, it doesn't. All the patterns are loaded once
                // before this loop ever starts...

                // TODO: Alternate tile map selector
                let tile_pattern_number = background_tile_map1[tile_index] as usize;
                let tile_pattern = &background_tile_patterns[tile_pattern_number];

                let color = tile_pattern[tile_i];
                let (red, green, blue) = color.to_rgb();
                pixel_data.push(blue);
                pixel_data.push(green);
                pixel_data.push(red);
                pixel_data.push(0xff);  // Alpha
            }
        }
        pixel_data.into_boxed_slice()

    }

}
