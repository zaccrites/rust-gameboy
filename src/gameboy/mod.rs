
pub mod cartridge;


// These won't be public in the end, probably, but they can be for now.
pub mod cpu;
pub mod memory;
pub mod graphics;
pub mod joypad;

// Should this just be a child of the CPU or something?
// Honestly most of this could just be wrapped up in a System or GameBoy struct...
pub mod timer;
pub mod dma;


mod interrupts;
