
use std::rc::Rc;
use std::cell::RefCell;

use gameboy::memory::MemoryUnit;


const JOYPAD_PORT_NUMBER: u8 = 0x00;


#[derive(Debug, Clone, Copy)]
pub enum ButtonState {
    Released,
    Pressed,
}


pub struct InputState {
    pub up: ButtonState,
    pub down: ButtonState,
    pub left: ButtonState,
    pub right: ButtonState,
    pub a: ButtonState,
    pub b: ButtonState,
    pub start: ButtonState,
    pub select: ButtonState,
}

impl Default for InputState {
    fn default() -> InputState {
        InputState {
            up: ButtonState::Released,
            down: ButtonState::Released,
            left: ButtonState::Released,
            right: ButtonState::Released,
            a: ButtonState::Released,
            b: ButtonState::Released,
            start: ButtonState::Released,
            select: ButtonState::Released,
        }
    }
}

impl InputState {
    fn read_user_input() -> InputState {
        // TODO: Read keyboard, controller, HTTP, whatever.
        InputState::default()
    }

    // // TODO: Better way?
    // fn to_byte(self, channel: JoypadChannelSelect) -> u8 {
    //     // TODO: Better way?
    //     macro_rules! button_pressed_bit {
    //         ($button:expr, $bit:expr) => {
    //             (if let ButtonState::Pressed = $button { 1 } else { 0 }) << $bit
    //         }
    //     }

    //     match channel {
    //         JoypadChannelSelect::DPad => {
    //             button_pressed_bit!(self.right, 0) |
    //             button_pressed_bit!(self.left, 1) |
    //             button_pressed_bit!(self.up, 2) |
    //             button_pressed_bit!(self.down, 3)
    //         },
    //         JoypadChannelSelect::Buttons => {
    //             button_pressed_bit!(self.a, 0) |
    //             button_pressed_bit!(self.b, 1) |
    //             button_pressed_bit!(self.select, 2) |
    //             button_pressed_bit!(self.start, 3)
    //         },
    //     }
    // }
}


// #[derive(Debug, Clone, Copy)]
// enum JoypadChannelSelect {
//     DPad,
//     Buttons,
// }

// impl JoypadChannelSelect {
//     // TODO: Better way?
//     fn from_byte(value: u8) -> Option<JoypadChannelSelect> {
//         match value & 0x30 {
//             0x10 => Some(JoypadChannelSelect::DPad),
//             0x20 => Some(JoypadChannelSelect::Buttons),
//             _ => None,
//         }
//     }

//     // TODO: Better way?
//     fn to_byte(self) -> u8 {
//         match self {
//             JoypadChannelSelect::DPad => 0x10,
//             JoypadChannelSelect::Buttons => 0x20,
//         }
//     }
// }



pub struct Joypad<'a> {
    memory: Rc<RefCell<MemoryUnit<'a>>>,
    // selected_channel: Option<JoypadChannelSelect>,

    // TODO: BETTER WAY!!
    selected_channel_bits: u8,
}


impl<'a> Joypad<'a> {

    pub fn new(memory: Rc<RefCell<MemoryUnit<'a>>>) -> Joypad {
        Joypad {
            memory,
            // selected_channel: None,
            selected_channel_bits: 0,
        }
    }

    pub fn step(&mut self, cycles: i32, input_state: &InputState) {

        // let input_state = InputState::read_user_input();

        if let Some(value) = self.memory.borrow_mut().check_for_io_write(JOYPAD_PORT_NUMBER) {
            // self.selected_channel = JoypadChannelSelect::from_byte(value);
            self.selected_channel_bits = value & 0x30;
        }

        // let new_io_value = if let Some(channel) = self.selected_channel {
        //     channel.to_byte() | input_state.to_byte(channel)
        // }
        // else {

        // };


        // TODO: BETTER WAY
        let mut button_value_bits = 0;

        if self.selected_channel_bits & 0x10 == 0 {
            if let ButtonState::Released = input_state.right { button_value_bits |= (1 << 0); }
            if let ButtonState::Released = input_state.left { button_value_bits |= (1 << 1); }
            if let ButtonState::Released = input_state.up { button_value_bits |= (1 << 2); }
            if let ButtonState::Released = input_state.down { button_value_bits |= (1 << 3); }
        }
        else if self.selected_channel_bits & 0x20 == 0 {
            if let ButtonState::Released = input_state.a { button_value_bits |= (1 << 0); }
            if let ButtonState::Released = input_state.b { button_value_bits |= (1 << 1); }
            if let ButtonState::Released = input_state.select { button_value_bits |= (1 << 2); }
            if let ButtonState::Released = input_state.start { button_value_bits |= (1 << 3); }
        }
        else {
            button_value_bits |= 0x0f;
        }

        println!(">> {:02x}", button_value_bits);

        self.memory.borrow_mut().set_io_read_value(JOYPAD_PORT_NUMBER, self.selected_channel_bits | button_value_bits);



    }

}




