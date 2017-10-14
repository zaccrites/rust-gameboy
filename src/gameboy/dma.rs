
use std::rc::Rc;
use std::cell::RefCell;
// use std::cmp::max;

use gameboy::memory::MemoryUnit;


const DMA_PORT_NUMBER: u8 = 0x46;


#[derive(Debug, Clone, Copy)]
struct DmaJob {
    cycles_remaining: i32,
    start_address: u16,
}



pub struct Dma<'a> {
    memory: Rc<RefCell<MemoryUnit<'a>>>,
    job_in_progress: Option<DmaJob>,
}

impl<'a> Dma<'a> {
    pub fn new(memory: Rc<RefCell<MemoryUnit<'a>>>) -> Dma {
        Dma {
            memory,
            job_in_progress: None,
        }
    }

    pub fn step(&mut self, cycles: i32) {
        let mut memory = self.memory.borrow_mut();

        if let Some(multiple) = memory.check_for_io_write(DMA_PORT_NUMBER) {
            // The DMA start address is specified in multiples of 0x100 bytes,
            // starting from 0x0000 and up to 0xf100.
            let start_address = (0x0100 * multiple as u16) % 0xf100;

            // The operation takes 160 microseconds to complete (160 cycles).
            self.job_in_progress = Some(DmaJob { start_address, cycles_remaining: 160 });

            // TODO: What happens if a second DMA request is scheduled
            // while the first is being processed?

            // println!("Starting DMA transfer from 0x{:04x}", start_address);


        }
        else {
            // TODO: Apparently DMA locks all of memory except High RAM until the job is done

            // TODO: This seems like a hack.
            let do_transfer = if let Some(ref mut job) = self.job_in_progress {
                // println!("DMA transfer in progress, {} cycles remaining", job.cycles_remaining);
                job.cycles_remaining -= cycles;
                job.cycles_remaining <= 0
            }
            else {
                false
            };

            // TODO: This REALLY seems like a hack...
            if do_transfer {
                // let source_address = self.job_in_progress.unwrap().start_address;
                let job = self.job_in_progress.unwrap();
                let source_address = job.start_address;
                let dest_address = 0xfe00;
                self.job_in_progress = None;

                // Write each byte from RAM or ROM to OAM
                let source_bytes = memory.read_range(source_address, 160);


                memory.write_range(dest_address, &source_bytes);


                // println!("Performing DMA transfer from 0x{:04x}", source_address);
                // println!("   source_bytes = {:?}", source_bytes);
                // let dest_bytes = memory.read_range(dest_address, 160);
                // println!("   dest_bytes   = {:?}", dest_bytes);

                // DMA IS STILL NOT WORKING!!!!!!!




                // for offset in 0..160 {
                //     let address = start_address + offset;
                //     let value = self.memory.borrow().read_byte()
                // }

            }
        }


        // let job_is_done = if let Some(ref mut job) = self.job_in_progress {
        //     job.cycles_remaining -= cycles;
        //     if job.cycles_remaining <= 0 {
        //         // TODO: Do DMA transfer
        //         self.job_in_progress = None;
        //     }
        // }



    }
}
