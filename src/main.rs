

mod gameboy;


extern crate sdl2;

use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::TextureAccess;
use sdl2::EventPump;


use std::time::{Instant, Duration};
use std::thread;


use gameboy::cartridge::Cartridge;

use gameboy::cpu::Cpu;
use gameboy::graphics::Gpu;
use gameboy::memory::MemoryUnit;
use gameboy::timer::Timer;
use gameboy::joypad::{Joypad, InputState, ButtonState};
use gameboy::dma::Dma;


use std::rc::Rc;
use std::cell::RefCell;

use std::env;



fn main() {

    let sdl_context = sdl2::init().unwrap();
    let video = sdl_context.video().unwrap();

    let window_width = gameboy::graphics::DISPLAY_RESOLUTION_X * 4;
    let window_height = gameboy::graphics::DISPLAY_RESOLUTION_Y * 4;

    let window = video.window("Gameboy Emulator", window_width, window_height)
        .position_centered().opengl()
        .build().unwrap();

    // let mut renderer = window.position_centered()
        // .accelerated()
        // .build().unwrap();

    // let mut renderer = window.renderer();
    let mut canvas = window.into_canvas()
        .accelerated()
        .build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();





    let texture_creator = canvas.texture_creator();
    // let format = PixelFormatEnum::BGR888;
    let format = PixelFormatEnum::ARGB8888;
    let mut texture = texture_creator.create_texture(
        format,
        TextureAccess::Streaming,
        gameboy::graphics::DISPLAY_RESOLUTION_X,
        gameboy::graphics::DISPLAY_RESOLUTION_Y,
    ).unwrap();




    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut quit = false;






    let args: Vec<_> = env::args().collect();

    println!("Loading ROM from {}...", args[1]);
    let cartridge = Cartridge::load(&args[1]);


    println!("Title: \"{}\"", cartridge.title());


    // All peripherals should be wrapped up in a GameBoy or System struct...
    let mut memory = Rc::new(RefCell::new(MemoryUnit::new(&cartridge)));
    let mut cpu = Cpu::new(memory.clone());
    let mut gpu = Gpu::new(memory.clone());
    let mut timer = Timer::new(memory.clone());
    let mut joypad = Joypad::new(memory.clone());
    let mut dma = Dma::new(memory.clone());
    // let mut audio = Gpu::new(memory.clone());  // apu?


    // TODO: Implement disassembly and debug output to show what the hell
    // this thing thinks it's doing. Why is it in external RAM? Something is
    // very wrong.

    cpu.reset();




    // let target_frame_duration = Duration::from_millis(16.667)
    let target_fps = 60;
    let target_duration = Duration::new(0, 1_000_000_000 / target_fps);
    while ! quit {
        let start_time = Instant::now();

        // Get input state
        // TODO: Make this configurable
        let input_state = {
            use sdl2::keyboard::{KeyboardState, Scancode};
            let keyboard_state = KeyboardState::new(&event_pump);
            let mut input_state = InputState::default();
            if keyboard_state.is_scancode_pressed(Scancode::A) { input_state.a = ButtonState::Pressed; }
            if keyboard_state.is_scancode_pressed(Scancode::S) { input_state.b = ButtonState::Pressed; }
            if keyboard_state.is_scancode_pressed(Scancode::Return) { input_state.start = ButtonState::Pressed; }
            if keyboard_state.is_scancode_pressed(Scancode::RShift) { input_state.select = ButtonState::Pressed; }
            if keyboard_state.is_scancode_pressed(Scancode::Left) { input_state.left = ButtonState::Pressed; }
            if keyboard_state.is_scancode_pressed(Scancode::Right) { input_state.right = ButtonState::Pressed; }
            if keyboard_state.is_scancode_pressed(Scancode::Up) { input_state.up = ButtonState::Pressed; }
            if keyboard_state.is_scancode_pressed(Scancode::Down) { input_state.down = ButtonState::Pressed; }
            input_state
        };


        for event in event_pump.poll_iter() {
            use sdl2::event::Event::*;
            use sdl2::keyboard::Keycode::*;

            match event {
                Quit { .. } => { quit = true },

                KeyDown { keycode: Some(keycode), repeat, keymod, .. } => match keycode {
                    Escape | Q => { quit = true },
                    _ => ()
                },

                _ => {},
            }
        }

        // match input_state {
        //     InputState { left: ButtonState::Pressed, .. } |
        //     InputState { right: ButtonState::Pressed, .. } |
        //     InputState { up: ButtonState::Pressed, .. } |
        //     InputState { down: ButtonState::Pressed, .. } |
        //     InputState { a: ButtonState::Pressed, .. } =>
        //         println!("{:?}", input_state),
        //     _ => (),
        // }

        // Execute instructions until the next frame draw time
        loop {
            // TODO: A program which disables the LCD to stop the scaneline
            // from incrementing and causing VBlank will cause this loop to
            // stop forever. It may be necessary to poll SDL events in this
            // loop in order to listen for Quit events.
            //
            // Or just rearchitect this function so it isn't a huge mess...
            //

            // print!("{:04}: ", i + 1);
            joypad.step(&input_state);


            let cycles = cpu.step();

            dma.step(cycles);


            timer.step(cycles);

            // Return true/false to indicate when to redraw (VBlank start)?
            let do_draw_frame = gpu.step(cycles);
            // println!(">>>>> do_draw_frame = {:?}", do_draw_frame);

            if do_draw_frame {
                break;
            }
        }


        // Todo: extract draw function
        canvas.clear();




        // TODO: Nearest neighbor interpolation for texture stretching

        // Update once per VBlank? Per scanline when debugging?
        let pixel_data = gpu.get_pixel_data();
        texture.update(None, &pixel_data, format.byte_size_of_pixels(gameboy::graphics::DISPLAY_RESOLUTION_X as usize));
        canvas.copy(&texture, None, None);

        canvas.present();


        let end_time = Instant::now();
        let frame_duration = (end_time - start_time);



        // TODO: Revisit this.
        // It should take the 1 MHz processor speed into account, but
        // stopping and starting the thread after each instruction is
        // too slow. Perhaps break it up into 100 cycle chunks or something.
        //
        // I would have *thought* that stopping after each VBlank until the next
        // frame was supposed to start would be enough, but it sleeps too long apparently.
        //
        //
        if (frame_duration < target_duration) {
            let sleep_duration = target_duration - frame_duration;
            // thread::sleep(sleep_duration);
        }

        let actual_end_time = Instant::now();
        let total_frame_duration = (actual_end_time - start_time);
        // println!("ACTUAL FRAME DURATION: {:?}", (total_frame_duration.subsec_nanos() as f32) / 1_000_000f32);



    }



}
