

mod gameboy;


// extern crate sdl2;

// use sdl2::pixels::Color;
// use sdl2::EventPump;


use std::time::{Instant, Duration};
use std::thread;


use gameboy::cartridge::Cartridge;

use gameboy::cpu::Cpu;
use gameboy::memory::MemoryUnit;


use std::rc::Rc;
use std::cell::RefCell;

fn main() {

    // TODO: Use command line argument
    let cartridge = Cartridge::load("roms/tetris.gb");

    println!("Title: \"{}\"", cartridge.title());


    let mut memory = Rc::new(RefCell::new(MemoryUnit::new(&cartridge)));
    // let mut memory = MemoryUnit::new(&cartridge);
    // let mut cpu = Cpu::new(memory.clone());
    let mut cpu = Cpu::new(memory.clone());
    // let mut gpu = Gpu::new(memory.clone());    // ppu?
    // let mut audio = Gpu::new(memory.clone());  // apu?


    // TODO: Implement disassembly and debug output to show what the hell
    // this thing thinks it's doing. Why is it in external RAM? Something is
    // very wrong.

    cpu.reset();

    for i in 0..2000 {
        // TODO: Print instructions as they execute? Yet another reason
        // to have a struct for each instruction type that can give
        //  - Implementation
        //  - Cycle timing
        //  - Disassembly
        //  - etc.
        print!("{}: ", i + 1);
        cpu.fetch_and_execute();


        let delay = Duration::from_millis(25);
        thread::sleep(delay);

    }

    return;


    // let sdl_context = sdl2::init().unwrap();
    // let video = sdl_context.video().unwrap();

    // let window = video.window("Hello World", 800, 600)
    //     .position_centered().opengl()
    //     .build().unwrap();

    // // let mut renderer = window.position_centered()
    //     // .accelerated()
    //     // .build().unwrap();

    // // let mut renderer = window.renderer();
    // let mut canvas = window.into_canvas()
    //     .accelerated()
    //     .build().unwrap();

    // canvas.set_draw_color(Color::RGB(0, 0, 0));


    // let mut event_pump = sdl_context.event_pump().unwrap();

    // let mut quit = false;

    // // let target_frame_duration = Duration::from_millis(16.667)
    // let target_fps = 60;
    // let target_duration = Duration::new(0, 1_000_000_000 / target_fps);
    // while ! quit {
    //     let start_time = Instant::now();

    //     canvas.clear();


    //     for event in event_pump.poll_iter() {
    //         use sdl2::event::Event::*;
    //         use sdl2::keyboard::Keycode::*;

    //         match event {
    //             Quit { .. } => { quit = true },

    //             KeyDown { keycode: Some(keycode), repeat, keymod, .. } => match keycode {
    //                 Escape | P if (keymod.contains(sdl2::keyboard::LSHIFTMOD)) => { quit = true },
    //                 Q if repeat => quit = true,
    //                 X if keymod.contains(sdl2::keyboard::LALTMOD) => quit = true,
    //                 Y => println!("keymod: {:?}", keymod),
    //                 _ => {}
    //             },

    //             _ => {},
    //         }
    //     }


    //     canvas.present();

    //     let end_time = Instant::now();
    //     let frame_duration = (end_time - start_time);

    //     if (frame_duration < target_duration) {
    //         let sleep_duration = target_duration - frame_duration;
    //         thread::sleep(sleep_duration);
    //     }
    // }



}


// Run the frame, returning whether or not the game is lagging behind
// the target frame duration
// fn run_frame(target_duration: Duration) -> bool {
//
// }
