

use std::rc::Rc;
use std::cell::RefCell;

// use std::drop::Drop;

use gameboy::memory::MemoryUnit;
use gameboy::interrupts::Interrupt;


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

        // GRAY
        // match self {
        //     Color::Lightest => (232, 232, 232),
        //     Color::Lighter  => (160, 160, 160),
        //     Color::Darker   => (88, 88, 88),
        //     Color::Darkest  => (16, 16, 16),
        // }

        // GREEN
        match self {
            Color::Lightest => (224, 248, 208),
            Color::Lighter  => (136, 192, 112),
            Color::Darker   => (52, 104, 86),
            Color::Darkest  => (8, 24, 32),
        }

        // RED
        // match self {
        //     Color::Lightest => (255, 192, 192),
        //     Color::Lighter  => (255, 96, 96),
        //     Color::Darker   => (192, 0, 0),
        //     Color::Darkest  => (96, 0 ,0),
        // }

        // YELLOW
        // match self {
        //     Color::Lightest => (248, 240, 120),
        //     Color::Lighter  => (176, 168, 72),
        //     Color::Darker   => (104, 104, 48),
        //     Color::Darkest  => (32, 32, 16),
        // }
    }



}




/// LCD Controller Mode
#[derive(Debug, Clone, Copy)]
enum Mode {
    HBlank = 0,            // CPU can access OAM and VRAM
    VBlank = 1,            // CPU can access OAM and VRAM
    OamRead = 2,           // CPU can access VRAM
    LCDTransfer = 3,       // CPU can't access either
}

impl Mode {
    fn cycles(self) -> i32 {
        // Assumes 1 MHz clock
        match self {
            Mode::HBlank => 194,       // about 48.6 us
            Mode::VBlank => 4320,      // about 1.08 ms
            Mode::OamRead => 76,       // about 19 us
            Mode::LCDTransfer => 164,  // about 41 us
        }
    }

    fn next_mode(self, scanline: u8) -> Mode {
        match self {
            Mode::OamRead => Mode::LCDTransfer,
            Mode::LCDTransfer => Mode::HBlank,
            Mode::VBlank => Mode::OamRead,
            Mode::HBlank => if scanline < 144 {
                Mode::OamRead
            }
            else {
                Mode::VBlank
            }
        }
    }
}



const LY_PORT_NUMBER: u8 = 0x44;
const LCDC_PORT_NUMBER: u8 = 0x40;
const STAT_PORT_NUMBER: u8 = 0x41;


pub struct Gpu<'a> {

    memory: Rc<RefCell<MemoryUnit<'a>>>,

    /// Current controller mode
    mode: Mode,
    /// Number of cycles left until mode switch
    mode_switch_timer: i32,


    scanline: u8,


    clocks: i64,



    lcdc: u8,

}

impl<'a> Gpu<'a> {

    pub fn new(memory: Rc<RefCell<MemoryUnit<'a>>>) -> Gpu<'a> {

        Gpu {
            memory,
            mode: Mode::OamRead,
            mode_switch_timer: 0,


            scanline: 0,
            clocks: 0,


            lcdc: 0,
        }



    }

    pub fn reset(&mut self) {
        self.lcdc = 0;
        self.set_mode(Mode::OamRead);
    }

    pub fn step(&mut self, cycles: i32) -> bool {

        let mut drawframe = false;



        // The GPU switches between three modes per scanline:
        //   - Mode 2 reads OAM and lasts 77-83 clocks
        //   - Mode 3 transfers data to the LCD driver and lasts 169-175 clocks
        //   - Mode 0 is the H-Blank and lasts 201-207 clocks.
        // The complete cycle lasts 456 clocks and occurs 144 times.
        // There is a 4560 clock V-Blank period during which the scanline
        // count continues to increase.

        let OAM_READ_CLOCKS = 80;           // about 19 us
        let LCD_TRANSFER_CLOCKS = 171;      // about 41 us
        let HBLANK_CLOCKS = 205;            // about 48.6 us

        let SCANLINE_PERIOD = OAM_READ_CLOCKS + LCD_TRANSFER_CLOCKS + HBLANK_CLOCKS;
        // assert_eq!(456, OAM_READ_CLOCKS + LCD_TRANSFER_CLOCKS + HBLANK_CLOCKS);

        // let VBLANK_CLOCKS = 4560;
        let VBLANK_CLOCKS = 10 * SCANLINE_PERIOD;



        // TODO: Find a more elegant way to do this.
        // TODO: OAM and VRAM locking
        self.clocks += cycles as i64;
        // if self.clocks <= OAM_READ_CLOCKS {
        //     self.mode = Mode::OamRead;
        // }
        // else if self.clocks <= OAM_READ_CLOCKS + LCD_TRANSFER_CLOCKS {
        //     self.mode = Mode::LCDTransfer;
        // }
        // else if self.clocks <= OAM_READ_CLOCKS + LCD_TRANSFER_CLOCKS + HBLANK_CLOCKS {
        //     self.mode
        // }

        if self.clocks >= SCANLINE_PERIOD {
            self.scanline += 1;
            self.clocks = 0;
        }

        if self.scanline >= 153 {
            self.scanline = 0;
        }
        self.memory.borrow_mut().set_io_read_value(LY_PORT_NUMBER, self.scanline);


        // println!(">>> scanline = {}", self.scanline);
        let mode = if self.scanline >= 144 {
            if self.scanline == 144 {
                drawframe = true;
                Interrupt::VBlank.request(&mut self.memory.borrow_mut());
            }
            Mode::VBlank
        }
        else if self.clocks <= OAM_READ_CLOCKS {
            Mode::OamRead
        }
        else if self.clocks <= OAM_READ_CLOCKS + LCD_TRANSFER_CLOCKS {
            Mode::LCDTransfer
        }
        else {
            Mode::HBlank
        };














        // TODO
        // let ly_port_write = self.memory.borrow_mut().check_for_io_write(LY_PORT_NUMBER);
        // if let Some(_) = ly_port_write {
        //     self.scanline = 0;
        // }

        // self.mode_switch_timer -= cycles;
        // if self.mode_switch_timer <= 0 {

        //     self.mode = self.mode.next_mode(self.scanline);
        //     self.mode_switch_timer = self.mode.cycles();

        //     match self.mode {
        //         // TODO: Scanline will not increment past 144. Is that a problem?
        //         Mode::HBlank => { self.scanline += 1; },
        //         Mode::VBlank => {
        //             Interrupt::VBlank.request(&mut self.memory.borrow_mut());
        //             vblank = true;
        //             self.scanline = 0;  // TODO: Should this wait until VBlank ends?
        //         },
        //         _ => (),
        //     }

        // }



        // TODO: Extract to function
        // TODO: Add other bit features
        // let lcd_status = 0 | (self.mode as u8);
        let lcd_status = 0 | (mode as u8);
        self.memory.borrow_mut().set_io_read_value(STAT_PORT_NUMBER, lcd_status);



        {
            let mut memory = self.memory.borrow_mut();
            if let Some(lcdc) = memory.check_for_io_write(LCDC_PORT_NUMBER) {
                self.lcdc = lcdc;
                memory.set_io_read_value(LCDC_PORT_NUMBER, self.lcdc);
            }
        }



        drawframe


    }




    // fn cycles(self) -> i32 {
    //     // Assumes 1 MHz clock
    //     match self {
    //         Mode::HBlank => 194,       // about 48.6 us
    //         Mode::VBlank => 4320,      // about 1.08 ms
    //         Mode::OamRead => 76,       // about 19 us
    //         Mode::LCDTransfer => 164,  // about 41 us
    //     }
    // }

    // fn next_mode(self, scanline: u8) -> Mode {
    //     match self {
    //         Mode::OamRead => Mode::LCDTransfer,
    //         Mode::LCDTransfer => Mode::HBlank,
    //         Mode::VBlank => Mode::OamRead,
    //         Mode::HBlank => if scanline < 144 {
    //             Mode::OamRead
    //         }
    //         else {
    //             Mode::VBlank
    //         }
    //     }
    // }









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





    // This function is WAY TOO SLOW!!! Start by replacing the pixel data
    // Vec with a plain array. If that's still too slow, maybe optimize
    // getting background tile patterns?
    //
    // Or maybe it's not too slow... wtf
    pub fn get_pixel_data(&self) -> Box<[u8]> {


        // use std::time::{Instant, Duration};
        // let start_time = Instant::now();

        // The gameboy framebuffer is 256x256 pixels. Each tile is 8x8 pixels,
        // so there are 32x32 tiles in the framebuffer, or 1024 total.
        let background_tile_map1 = self.memory.borrow().read_range(0x9800, 1024);
        let background_tile_map2 = self.memory.borrow().read_range(0x9c00, 1024);



        fn decode_pattern(pattern_data: &[u8]) -> Box<[Color]> {
            let mut pixels = Vec::with_capacity(64);
            for i in 0..8 {
                let byte1 = pattern_data[2 * i];
                let byte2 = pattern_data[2 * i + 1];

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
        }



        // Pattern Table 1 (at 0x8000-8fff, pattern numbers from 0 to 255) can be used for sprites, background, and window.
        // Pattern table 2 (at 0x8800-97ff, pattern numbers from -128 to 127) can be used for background and window only.

        let pattern_table1: Vec<_> = (0..256).map(|tile_number| {
            let address = 0x8000 + 16 * tile_number;
            let tile_pattern_data = self.memory.borrow().read_range(address, 16);
            decode_pattern(&tile_pattern_data)
        }).collect();

        // TODO: Pattern Table 2
        let pattern_table2: Vec<_> = (0..256).map(|tile_number| {
            let address = 0x8800 + 16 * tile_number;
            let tile_pattern_data = self.memory.borrow().read_range(address, 16);
            decode_pattern(&tile_pattern_data)
        }).collect();


        #[derive(Debug)]
        struct SpriteData {
            y: u8,
            x: u8,
            pattern_number: u8,
            flags: u8,  // TODO: Extract This
        }


        // TODO: This is where 160 bytes of OAM comes from. It's 40 4-byte blocks.
        // TODO: 8x16 sprite mode
        let oam: Vec<_> = (0..40).map(|sprite_number| {
            let address = 0xfe00 + 4 * sprite_number;
            let data = self.memory.borrow().read_range(address, 4);

            // println!(">>>>> read OAM data {:?}", data);
            SpriteData { y: data[0], x: data[1], pattern_number: data[2], flags: data[3] }
        }).collect();








        // I think with the sprites, this function is slow again. Must look
        // for a more performant way to process the data? Should profile it to
        // be sure.

        // TODO: Window

        let mut pixel_data = Vec::new();
        // let mut pixel_data = Vec::with_capacity((DISPLAY_RESOLUTION_Y * DISPLAY_RESOLUTION_X * 4) as usize);   // This helps, but not enough... use a plain array
        for display_y in 0..DISPLAY_RESOLUTION_Y {
            for display_x in 0..DISPLAY_RESOLUTION_X {



                // TODO: Scrolling
                let tile_y = display_y % 8;
                let tile_x = display_x % 8;
                let tile_i = ((tile_y * 8) + tile_x) as usize;  // Pixel index into tile pattern

                // FLOATING POINT DIVISION IS EXPENSIVE!!! Though I would imagine it optimizes (mod 8) to bitwise AND 0x07

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
                // let tile_pattern_number = background_tile_map1[tile_index] as usize;
                let tile_pattern_number = if self.lcdc & 0x08 == 0x08 {
                    background_tile_map2[tile_index]
                }
                else {
                    background_tile_map1[tile_index]
                };



                // TODO: Select this based on the LCDC control bit
                // let background_tile_patterns = &pattern_table1;
                let tile_pattern = if self.lcdc & 0x10 == 0x10 {
                    &pattern_table1[tile_pattern_number as usize]
                }
                else {
                    // Tile pattern numbers are stored in a signed format,
                    // so even though the table starts at 0x8800, the data
                    // corresponding to Pattern 0 is at 0x9000.
                    //
                    // Therefore we have to convert the signed 8-bit number
                    // to an unsigned 8-bit number.
                    //
                    // This looks disgusting, but works.
                    // TODO: Clean this up
                    let tile_pattern_index = (((tile_pattern_number as i8) as i16) + 128) as usize;
                    &pattern_table2[tile_pattern_index as usize]
                };





                // let tile_pattern = &background_tile_patterns[tile_pattern_number];

                let tile_color = tile_pattern[tile_i];


                // This is always true.
                let sprite_patterns = &pattern_table1;


                // Now check to see if there is a sprite on the screen here.
                // If there are multiple sprites then the one with the left-most
                // X coordinate has priority. If they have the same X coordinate,
                // then they are ordered according to the table ordering starting
                // at 0xfe00, 0xfe04, etc.
                //
                // TODO: It would probably be better to check for a sprite first,
                // and only then compute the color of the tile underneath if there
                // is no sprite or if the sprite's pixel at that spot is transparent.
                //
                // Check to see if the BG layer is turned off
                let mut color = if self.lcdc & 0x01 == 0x01 {
                    tile_color
                }
                else {
                    Color::Lightest
                };

                // TODO: Only 10 sprites can be displayed per line. I'm tempted
                // to add an option to disable this so it will show all the sprites.
                for sprite_data in &oam {
                    let x_flip = sprite_data.flags & 0x20 == 0x20;
                    let y_flip = sprite_data.flags & 0x40 == 0x40;
                    // TODO: Other flags

                    let sprite_pattern = &sprite_patterns[sprite_data.pattern_number as usize];


                    // if sprite_data.pattern_number == 0 {
                    //     continue;
                    // }
                    // println!("Sprite: {:?}", sprite_data);
                    // println!("sprite pattern number: {}", sprite_data.pattern_number);


                    // // TODO: ???
                    // let sprite_y = (display_y + sprite_data.y as u32) % 8;
                    // let sprite_x = (display_x + sprite_data.x as u32) % 8;
                    // let sprite_i = ((sprite_y * 8) + sprite_x) as usize;  // Pixel index into tile pattern

                    // Check to see if the current pixel is on this sprite
                    //
                    // TODO: What happens if the subtraction goes negative? Does it wrap around the screen?
                    let sprite_position_x = sprite_data.x.saturating_sub(8);
                    let sprite_position_y = sprite_data.y.saturating_sub(16);  // I guess the position is on the right/bottom of the sprite?

                    let in_x_window = (display_x as u8) >= sprite_position_x && (display_x as u8) < sprite_position_x + 8;
                    let in_y_window = (display_y as u8) >= sprite_position_y && (display_y as u8) < sprite_position_y + 8;  // TODO: 8x16, also this method sucks
                    if in_x_window && in_y_window {
                        let sprite_x = ((display_x as u8) - sprite_position_x) % 8;
                        let sprite_y = ((display_y as u8) - sprite_position_y) % 8;  // TODO: 8x16
                        let sprite_i = ((sprite_y * 8) + sprite_x) as usize;  // Pixel index into sprite pattern

                        let sprite_color = sprite_pattern[sprite_i];

                        // Check to see if the sprite layer is enabled
                        if self.lcdc & 0x02 == 0x02 {
                            // If the priority flag is 0 then the sprite covers
                            // the background and window no matter what. If it's
                            // 1 then it covers them only if the background/window
                            // pixel is the Lightest (00) color.
                            //
                            // TODO: Can these be combined somehow?
                            //
                            if sprite_data.flags & 0x80 == 0 {
                                color = sprite_color;
                            }
                            else if let Color::Lightest = tile_color {
                                color = sprite_color;
                            }
                        }




                    }



                }




                // Check to see if the LCD is off
                if self.lcdc & 0x80 != 0x80 {
                    color = Color::Lightest;
                }



                let (red, green, blue) = color.to_rgb();
                pixel_data.push(blue);
                pixel_data.push(green);
                pixel_data.push(red);
                pixel_data.push(0xff);  // Alpha
            }
        }






        let result = pixel_data.into_boxed_slice();
        // let end_time = Instant::now();

        // let duration = end_time - start_time;
        // println!("get_pixel_data time: {:?} ms", (duration.subsec_nanos() as f32) / 1_000_000f32);
        result


        // pixel_data.into_boxed_slice()
    }

}
