use bitflags::bitflags;

use crate::bus::Bus;

use self::opcode::{OpCode, OpCodeFn, OPCODE_TABLE};

mod opcode;

bitflags! {
    pub struct Status: u8 {
        const Carry             = 0b0000_0001;
        const Zero              = 0b0000_0010;
        const IRQDisable        = 0b0000_0100;
        const Decimal           = 0b0000_1000;
        const IndexSize         = 0b0001_0000;
        const AccumulatorSize   = 0b0010_0000;
        const Overflow          = 0b0100_0000;
        const Negative          = 0b1000_0000;
    }
}

#[derive(Clone, Copy)]
pub enum AddressingMode {
    Implied,
    Accumulator,
    Immidiate,

    Absolute,
    AbsoluteLong,
    AbsoluteIndirect,
    AbsoluteIndirectLong,
    AbsoluteIndirectX,

    Direct,
    DirectIndirect,
    DirectIndirectLong,

    AbsoluteX,
    DirectX,
    AbsoluteLongX,
    DirectIndirectX,

    AbsoluteY,
    DirectY,
    DirectIndirectY,
    DirectIndirectLongY,

    Stack,
    StackIndirectY,
    Relative8,
    Relative16,
}

impl AddressingMode {
    pub fn fetch_addr(&self, cpu: &Cpu, memory: &dyn Bus) -> Result<(u32, bool), &'static str> {
        match self {
            AddressingMode::Implied => Err("Imply"),
            AddressingMode::Accumulator => Err("Accumulator"),
            AddressingMode::Immidiate => Ok((cpu.program_counter() + 1, false)),
            AddressingMode::Absolute => {
                let pc = cpu.program_counter();
                let addr = memory.read_word(pc + 1);
                Ok((address(cpu.db, addr), false))
            }
            AddressingMode::AbsoluteLong => {
                let pc = cpu.program_counter();
                let addr = memory.read_word(pc + 1);
                let bank = memory.read_byte(pc + 3);
                Ok((address(bank, addr), false))
            }
            AddressingMode::AbsoluteIndirect => {
                let pc = cpu.program_counter();
                let pointer = memory.read_word(pc + 1);
                let low = memory.read_byte(address(cpu.pb, pointer)) as u16;
                let high = memory.read_byte(address(cpu.pb, pointer.wrapping_add(1))) as u16;
                Ok((address(cpu.pb, low | (high << 8)), pointer >= 0xFFFF))
            }
            AddressingMode::AbsoluteIndirectLong => {
                let pc = cpu.program_counter();
                let pointer = memory.read_word(pc + 1);
                let low = memory.read_byte(address(cpu.pb, pointer)) as u16;
                let high = memory.read_byte(address(cpu.pb, pointer.wrapping_add(1))) as u16;
                let bank = memory.read_byte(address(cpu.pb, pointer.wrapping_add(2)));
                Ok((address(bank, low | (high << 8)), pointer >= 0xFFFF))
            }
            AddressingMode::AbsoluteIndirectX => {
                let pc = cpu.program_counter();
                let pointer = memory.read_word(pc + 1);
                let pointer = pointer.wrapping_add(cpu.x);
                let low = memory.read_byte(address(cpu.pb, pointer)) as u16;
                let high = memory.read_byte(address(cpu.pb, pointer.wrapping_add(1))) as u16;
                Ok((address(cpu.pb, low | (high << 8)), pointer >= 0xFFFF))
            }
            AddressingMode::Direct => {
                let pc = cpu.program_counter();
                let offset = memory.read_byte(pc + 1);
                let (addr, page_crossed) = cpu.d.overflowing_add(offset as u16);
                Ok((address(0, addr), page_crossed))
            }
            AddressingMode::DirectIndirect => {
                let pc = cpu.program_counter();
                let offset = memory.read_byte(pc + 1);
                let addr = cpu.d.wrapping_add(offset as u16) as u32;
                let low = memory.read_byte(addr) as u16;
                let high = memory.read_byte(addr + 1) as u16;
                Ok((address(cpu.db, low | high << 8), addr >= 0xFFFF))
            }
            AddressingMode::DirectIndirectLong => {
                let pc = cpu.program_counter();
                let offset = memory.read_byte(pc + 1) as u16;
                let addr = cpu.d.wrapping_add(offset) as u32;
                let low = memory.read_byte(addr) as u16;
                let mid = memory.read_byte(addr.wrapping_add(1)) as u16;
                let bank = memory.read_byte(addr.wrapping_add(2));
                Ok((address(bank, low | mid << 8), addr >= 0xFFFE))
            }
            AddressingMode::AbsoluteX => {
                let pc = cpu.program_counter();
                let addr = memory.read_word(pc + 1);
                let addr = address(cpu.db, addr);
                Ok((addr + cpu.x as u32, false))
            }
            AddressingMode::DirectX => {
                let pc = cpu.program_counter();
                let offset = memory.read_byte(pc + 1) as u16;
                let addr = cpu.d.wrapping_add(offset as u16).wrapping_add(cpu.x);
                Ok((address(0, addr), addr == 0xFFFF))
            }
            AddressingMode::AbsoluteLongX => {
                let pc = cpu.program_counter();
                let addr = memory.read_word(pc + 1);
                let bank = memory.read_byte(pc + 3);
                let addr = address(bank, addr);
                Ok((addr + cpu.x as u32, false))
            }
            AddressingMode::DirectIndirectX => {
                let pc = cpu.program_counter();
                let low = memory.read_byte(pc + 1) as u16;
                let high = memory.read_byte(pc + 2) as u16;
                let offset = cpu.x.wrapping_add(low | high << 8);
                let addr = address(0, offset);

                let low = memory.read_byte(addr) as u16;
                let high = memory.read_byte(addr.wrapping_add(1)) as u16;
                Ok((address(cpu.db, low | high << 8), addr >= 0xFFFF))
            }
            AddressingMode::AbsoluteY => {
                let pc = cpu.program_counter();
                let addr = memory.read_word(pc + 1);
                let addr = address(cpu.db, addr);
                Ok((addr + cpu.y as u32, false))
            }
            AddressingMode::DirectY => {
                let pc = cpu.program_counter();
                let offset = memory.read_byte(pc + 1) as u16;
                let addr = cpu.d.wrapping_add(offset as u16).wrapping_add(cpu.y);
                Ok((address(0, addr), addr >= 0xFFFF))
            }
            AddressingMode::DirectIndirectY => {
                let pc = cpu.program_counter();
                let low = memory.read_byte(pc + 1) as u16;
                let high = memory.read_byte(pc + 2) as u16;
                let addr = address(0, low | high << 8);

                let low = memory.read_byte(addr) as u16;
                let high = memory.read_byte(addr.wrapping_add(1)) as u16;
                let addr2 = address(cpu.db, low | high << 8);
                Ok((addr2 + cpu.y as u32, addr >= 0xFFFF))
            }
            AddressingMode::DirectIndirectLongY => {
                let pc = cpu.program_counter();
                let low = memory.read_byte(pc + 1) as u16;
                let high = memory.read_byte(pc + 2) as u16;
                let addr = address(0, low | high << 8);

                let low = memory.read_byte(addr) as u16;
                let mid = memory.read_byte(addr.wrapping_add(1)) as u16;
                let bank = memory.read_byte(addr.wrapping_add(2));
                let addr2 = address(bank, low | mid << 8);
                Ok((addr2 + cpu.y as u32, addr >= 0xFFFE))
            }
            AddressingMode::Stack => {
                let pc = cpu.program_counter();
                let low = memory.read_byte(pc + 1) as u16;
                let offset = cpu.s.wrapping_add(low);
                Ok((address(0, offset), offset >= 0xFFFF))
            }
            AddressingMode::StackIndirectY => {
                let pc = cpu.program_counter();
                let low = memory.read_byte(pc + 1) as u16;
                let offset = cpu.s.wrapping_add(low);
                Ok((address(0, offset) + cpu.y as u32, offset >= 0xFFFF))
            }
            AddressingMode::Relative8 => Ok((cpu.program_counter() + 1, false)),
            AddressingMode::Relative16 => Ok((cpu.program_counter() + 1, false)),
        }
    }
}

pub struct Cpu {
    pub a: u16,
    pub x: u16,
    pub y: u16,
    pub s: u16,
    pub db: u8,
    pub d: u16,
    pub pb: u8,
    pub pc: u16,
    pub p: Status,
    pub(crate) wait: bool,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            s: 0,
            db: 0,
            d: 0,
            pb: 0,
            pc: 0,
            p: Status::from_bits_truncate(0),
            wait: false,
        }
    }

    pub fn program_counter(&self) -> u32 {
        address(self.pb, self.pc)
    }

    fn push_byte(&mut self, bus: &mut dyn Bus, value: u8) {
        bus.write_byte(address(0, self.s), value);
        self.s = self.s.wrapping_sub(1);
    }

    fn push_word(&mut self, bus: &mut dyn Bus, value: u16) {
        self.push_byte(bus, (value >> 8) as u8);
        self.push_byte(bus, value as u8);
    }

    fn pull_byte(&mut self, bus: &mut dyn Bus) -> u8 {
        self.s = self.s.wrapping_add(1);
        bus.read_byte(address(0, self.s))
    }

    fn pull_word(&mut self, bus: &mut dyn Bus) -> u16 {
        let low = self.pull_byte(bus) as u16;
        let high = self.pull_byte(bus) as u16;
        high << 8 | low
    }

    pub fn fetch(&self, bus: &dyn Bus) -> u8 {
        bus.read_byte(self.program_counter())
    }

    pub fn decode(&self, opcode: u8) -> &'static OpCode {
        OPCODE_TABLE.get(&opcode).expect("Illegal Instruction")
    }

    pub fn execute(&mut self, bus: &mut dyn Bus, opcode: &OpCode) -> usize {
        let (resize, elapsed_cycle, is_branched) = (opcode.operation)(self, bus, opcode.mode);
        if !is_branched {
            let next_pc = self.program_counter() + (opcode.size - resize) as u32;
            self.pc = next_pc as u16;
            self.pb = (next_pc >> 8) as u8;
        }

        elapsed_cycle
    }
}

fn address(bank: u8, offset: u16) -> u32 {
    offset as u32 | ((bank as u32) << 16)
}
