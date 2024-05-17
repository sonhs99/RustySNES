use bitflags::Flags;
use hashbrown::HashMap;
use spin::Lazy;

use super::{address, AddressingMode, Cpu, Status};
use crate::bus::Bus;

pub type OpCodeFn = fn(&mut Cpu, &mut dyn Bus, AddressingMode) -> (usize, usize, bool);

pub struct OpCode {
    pub mnemonic: &'static str,
    pub size: usize,
    pub operation: OpCodeFn,
    pub mode: AddressingMode,
}

impl OpCode {
    pub fn new(mnemonic: &'static str, operation: OpCodeFn, size: usize, mode: AddressingMode) -> Self {
        Self {
            mnemonic,
            operation,
            mode,
            size,
        }
    }
}

pub static OPCODE_TABLE: Lazy<HashMap<u8, OpCode>> = Lazy::new(|| {
    let mut table = HashMap::new();

    // 0. No Operation
    // NOP, WDM
    table.insert(0xEA, OpCode::new("NOP", nop, 1, AddressingMode::Implied));
    table.insert(0x42, OpCode::new("WDM", nop, 2, AddressingMode::Immidiate));

    // 1. Addition + Subtraction
    // ADC
    table.insert(0x61, OpCode::new("ADC", adc, 2, AddressingMode::DirectIndirectX));
    table.insert(0x63, OpCode::new("ADC", adc, 2, AddressingMode::Stack));
    table.insert(0x65, OpCode::new("ADC", adc, 2, AddressingMode::Direct));
    table.insert(0x67, OpCode::new("ADC", adc, 2, AddressingMode::DirectIndirectLong));
    table.insert(0x69, OpCode::new("ADC", adc, 3, AddressingMode::Immidiate));
    table.insert(0x6D, OpCode::new("ADC", adc, 3, AddressingMode::Absolute));
    table.insert(0x6F, OpCode::new("ADC", adc, 4, AddressingMode::AbsoluteLong));
    table.insert(0x71, OpCode::new("ADC", adc, 2, AddressingMode::DirectIndirectY));
    table.insert(0x72, OpCode::new("ADC", adc, 2, AddressingMode::DirectIndirect));
    table.insert(0x73, OpCode::new("ADC", adc, 2, AddressingMode::StackIndirectY));
    table.insert(0x75, OpCode::new("ADC", adc, 2, AddressingMode::DirectX));
    table.insert(0x77, OpCode::new("ADC", adc, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0x79, OpCode::new("ADC", adc, 3, AddressingMode::AbsoluteY));
    table.insert(0x7D, OpCode::new("ADC", adc, 3, AddressingMode::AbsoluteX));
    table.insert(0x7F, OpCode::new("ADC", adc, 4, AddressingMode::AbsoluteLongX));

    // SBC
    table.insert(0xE1, OpCode::new("SBC", sbc, 2, AddressingMode::DirectIndirectX));
    table.insert(0xE3, OpCode::new("SBC", sbc, 2, AddressingMode::Stack));
    table.insert(0xE5, OpCode::new("SBC", sbc, 2, AddressingMode::Direct));
    table.insert(0xE7, OpCode::new("SBC", sbc, 2, AddressingMode::DirectIndirectLong));
    table.insert(0xE9, OpCode::new("SBC", sbc, 3, AddressingMode::Immidiate));
    table.insert(0xED, OpCode::new("SBC", sbc, 3, AddressingMode::Absolute));
    table.insert(0xEF, OpCode::new("SBC", sbc, 4, AddressingMode::AbsoluteLong));
    table.insert(0xF1, OpCode::new("SBC", sbc, 2, AddressingMode::DirectIndirectY));
    table.insert(0xF2, OpCode::new("SBC", sbc, 2, AddressingMode::DirectIndirect));
    table.insert(0xF3, OpCode::new("SBC", sbc, 2, AddressingMode::StackIndirectY));
    table.insert(0xF5, OpCode::new("SBC", sbc, 2, AddressingMode::DirectX));
    table.insert(0xF7, OpCode::new("SBC", sbc, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0xF9, OpCode::new("SBC", sbc, 3, AddressingMode::AbsoluteY));
    table.insert(0xFD, OpCode::new("SBC", sbc, 3, AddressingMode::AbsoluteX));
    table.insert(0xFF, OpCode::new("SBC", sbc, 4, AddressingMode::AbsoluteLongX));

    // 2. Compare
    // CMP
    table.insert(0xC1, OpCode::new("CMP", cmp, 2, AddressingMode::DirectIndirectX));
    table.insert(0xC3, OpCode::new("CMP", cmp, 2, AddressingMode::Stack));
    table.insert(0xC5, OpCode::new("CMP", cmp, 2, AddressingMode::Direct));
    table.insert(0xC7, OpCode::new("CMP", cmp, 2, AddressingMode::DirectIndirectLong));
    table.insert(0xC9, OpCode::new("CMP", cmp, 3, AddressingMode::Immidiate));
    table.insert(0xCD, OpCode::new("CMP", cmp, 3, AddressingMode::Absolute));
    table.insert(0xCF, OpCode::new("CMP", cmp, 4, AddressingMode::AbsoluteLong));
    table.insert(0xD1, OpCode::new("CMP", cmp, 2, AddressingMode::DirectIndirectY));
    table.insert(0xD2, OpCode::new("CMP", cmp, 2, AddressingMode::DirectIndirect));
    table.insert(0xD3, OpCode::new("CMP", cmp, 2, AddressingMode::StackIndirectY));
    table.insert(0xD5, OpCode::new("CMP", cmp, 2, AddressingMode::DirectX));
    table.insert(0xD7, OpCode::new("CMP", cmp, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0xD9, OpCode::new("CMP", cmp, 3, AddressingMode::AbsoluteY));
    table.insert(0xDD, OpCode::new("CMP", cmp, 3, AddressingMode::AbsoluteX));
    table.insert(0xDF, OpCode::new("CMP", cpx, 4, AddressingMode::AbsoluteLongX));

    // CPX
    table.insert(0xE0, OpCode::new("CPX", cpx, 4, AddressingMode::Immidiate));
    table.insert(0xE4, OpCode::new("CPX", cpx, 4, AddressingMode::Direct));
    table.insert(0xEC, OpCode::new("CPX", cpx, 4, AddressingMode::Absolute));

    // CPY
    table.insert(0xC0, OpCode::new("CPY", cpy, 4, AddressingMode::Immidiate));
    table.insert(0xC4, OpCode::new("CPY", cpy, 4, AddressingMode::Direct));
    table.insert(0xCC, OpCode::new("CPY", cpy, 4, AddressingMode::Absolute));

    // 3. Increase, Decrease
    // DEC, DEX, DEY
    table.insert(0x3A, OpCode::new("DEC", dea, 1, AddressingMode::Accumulator));
    table.insert(0xC6, OpCode::new("DEC", dec, 2, AddressingMode::Direct));
    table.insert(0xCE, OpCode::new("DEC", dec, 3, AddressingMode::Absolute));
    table.insert(0xD6, OpCode::new("DEC", dec, 2, AddressingMode::DirectX));
    table.insert(0xDE, OpCode::new("DEC", dec, 3, AddressingMode::AbsoluteX));
    table.insert(0xCA, OpCode::new("DEX", dex, 1, AddressingMode::Implied));
    table.insert(0x88, OpCode::new("DEY", dey, 1, AddressingMode::Implied));

    // INC, INX, INY
    table.insert(0x1A, OpCode::new("INC", ina, 1, AddressingMode::Accumulator));
    table.insert(0xE6, OpCode::new("INC", inc, 2, AddressingMode::Direct));
    table.insert(0xEE, OpCode::new("INC", inc, 3, AddressingMode::Absolute));
    table.insert(0xF6, OpCode::new("INC", inc, 2, AddressingMode::DirectX));
    table.insert(0xFE, OpCode::new("INC", inc, 3, AddressingMode::AbsoluteX));
    table.insert(0xE8, OpCode::new("INX", inx, 1, AddressingMode::Implied));
    table.insert(0xC8, OpCode::new("INY", iny, 1, AddressingMode::Implied));

    // 4. Logical Operation
    // AND
    table.insert(0x21, OpCode::new("AND", and, 2, AddressingMode::DirectIndirectX));
    table.insert(0x23, OpCode::new("AND", and, 2, AddressingMode::Stack));
    table.insert(0x25, OpCode::new("AND", and, 2, AddressingMode::Direct));
    table.insert(0x27, OpCode::new("AND", and, 2, AddressingMode::DirectIndirectLong));
    table.insert(0x29, OpCode::new("AND", and, 3, AddressingMode::Immidiate));
    table.insert(0x2D, OpCode::new("AND", and, 3, AddressingMode::Absolute));
    table.insert(0x2F, OpCode::new("AND", and, 4, AddressingMode::AbsoluteLong));
    table.insert(0x31, OpCode::new("AND", and, 2, AddressingMode::DirectIndirectY));
    table.insert(0x32, OpCode::new("AND", and, 2, AddressingMode::DirectIndirect));
    table.insert(0x33, OpCode::new("AND", and, 2, AddressingMode::StackIndirectY));
    table.insert(0x35, OpCode::new("AND", and, 2, AddressingMode::DirectX));
    table.insert(0x37, OpCode::new("AND", and, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0x39, OpCode::new("AND", and, 3, AddressingMode::AbsoluteY));
    table.insert(0x3D, OpCode::new("AND", and, 3, AddressingMode::AbsoluteX));
    table.insert(0x3F, OpCode::new("AND", and, 4, AddressingMode::AbsoluteLongX));

    // EOR
    table.insert(0x41, OpCode::new("EOR", eor, 2, AddressingMode::DirectIndirectX));
    table.insert(0x43, OpCode::new("EOR", eor, 2, AddressingMode::Stack));
    table.insert(0x45, OpCode::new("EOR", eor, 2, AddressingMode::Direct));
    table.insert(0x47, OpCode::new("EOR", eor, 2, AddressingMode::DirectIndirectLong));
    table.insert(0x49, OpCode::new("EOR", eor, 3, AddressingMode::Immidiate));
    table.insert(0x4D, OpCode::new("EOR", eor, 3, AddressingMode::Absolute));
    table.insert(0x4F, OpCode::new("EOR", eor, 4, AddressingMode::AbsoluteLong));
    table.insert(0x51, OpCode::new("EOR", eor, 2, AddressingMode::DirectIndirectY));
    table.insert(0x52, OpCode::new("EOR", eor, 2, AddressingMode::DirectIndirect));
    table.insert(0x53, OpCode::new("EOR", eor, 2, AddressingMode::StackIndirectY));
    table.insert(0x55, OpCode::new("EOR", eor, 2, AddressingMode::DirectX));
    table.insert(0x57, OpCode::new("EOR", eor, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0x59, OpCode::new("EOR", eor, 3, AddressingMode::AbsoluteY));
    table.insert(0x5D, OpCode::new("EOR", eor, 3, AddressingMode::AbsoluteX));
    table.insert(0x5F, OpCode::new("EOR", eor, 4, AddressingMode::AbsoluteLongX));

    // ORA
    table.insert(0x01, OpCode::new("ORA", ora, 2, AddressingMode::DirectIndirectX));
    table.insert(0x03, OpCode::new("ORA", ora, 2, AddressingMode::Stack));
    table.insert(0x05, OpCode::new("ORA", ora, 2, AddressingMode::Direct));
    table.insert(0x07, OpCode::new("ORA", ora, 2, AddressingMode::DirectIndirectLong));
    table.insert(0x09, OpCode::new("ORA", ora, 3, AddressingMode::Immidiate));
    table.insert(0x0D, OpCode::new("ORA", ora, 3, AddressingMode::Absolute));
    table.insert(0x0F, OpCode::new("ORA", ora, 4, AddressingMode::AbsoluteLong));
    table.insert(0x11, OpCode::new("ORA", ora, 2, AddressingMode::DirectIndirectY));
    table.insert(0x12, OpCode::new("ORA", ora, 2, AddressingMode::DirectIndirect));
    table.insert(0x13, OpCode::new("ORA", ora, 2, AddressingMode::StackIndirectY));
    table.insert(0x15, OpCode::new("ORA", ora, 2, AddressingMode::DirectX));
    table.insert(0x17, OpCode::new("ORA", ora, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0x19, OpCode::new("ORA", ora, 3, AddressingMode::AbsoluteY));
    table.insert(0x1D, OpCode::new("ORA", ora, 3, AddressingMode::AbsoluteX));
    table.insert(0x1F, OpCode::new("ORA", ora, 4, AddressingMode::AbsoluteLongX));

    // 5. BIT
    table.insert(0x24, OpCode::new("BIT", bit, 2, AddressingMode::Direct));
    table.insert(0x2C, OpCode::new("BIT", bit, 3, AddressingMode::Absolute));
    table.insert(0x34, OpCode::new("BIT", bit, 2, AddressingMode::DirectX));
    table.insert(0x3C, OpCode::new("BIT", bit, 3, AddressingMode::AbsoluteX));
    table.insert(0x89, OpCode::new("BIT", bit, 3, AddressingMode::Immidiate));

    // 6. Test and Set-Reset Bit
    // TRB
    table.insert(0x14, OpCode::new("TRB", trb, 2, AddressingMode::Direct));
    table.insert(0x1C, OpCode::new("TRB", trb, 3, AddressingMode::Absolute));

    // TSB
    table.insert(0x04, OpCode::new("TSB", tsb, 2, AddressingMode::Direct));
    table.insert(0x0C, OpCode::new("TSB", tsb, 3, AddressingMode::Absolute));

    // 7. Rotate
    // ASL
    table.insert(0x06, OpCode::new("ASL", asl, 2, AddressingMode::Direct));
    table.insert(0x0A, OpCode::new("ASL", asla, 1, AddressingMode::Accumulator));
    table.insert(0x0E, OpCode::new("ASL", asl, 3, AddressingMode::Absolute));
    table.insert(0x16, OpCode::new("ASL", asl, 2, AddressingMode::DirectX));
    table.insert(0x1E, OpCode::new("ASL", asl, 3, AddressingMode::AbsoluteX));

    //LSR
    table.insert(0x46, OpCode::new("LSR", lsr, 2, AddressingMode::Direct));
    table.insert(0x4A, OpCode::new("LSR", lsra, 1, AddressingMode::Accumulator));
    table.insert(0x4E, OpCode::new("LSR", lsr, 3, AddressingMode::Absolute));
    table.insert(0x56, OpCode::new("LSR", lsr, 2, AddressingMode::DirectX));
    table.insert(0x5E, OpCode::new("LSR", lsr, 3, AddressingMode::AbsoluteX));

    // ROL
    table.insert(0x26, OpCode::new("ROL", rol, 2, AddressingMode::Direct));
    table.insert(0x2A, OpCode::new("ROL", rola, 1, AddressingMode::Accumulator));
    table.insert(0x2E, OpCode::new("ROL", rol, 3, AddressingMode::Absolute));
    table.insert(0x36, OpCode::new("ROL", rol, 2, AddressingMode::DirectX));
    table.insert(0x3E, OpCode::new("ROL", rol, 3, AddressingMode::AbsoluteX));

    // ROR
    table.insert(0x66, OpCode::new("ROR", ror, 2, AddressingMode::Direct));
    table.insert(0x6A, OpCode::new("ROR", rora, 1, AddressingMode::Accumulator));
    table.insert(0x6E, OpCode::new("ROR", ror, 3, AddressingMode::Absolute));
    table.insert(0x76, OpCode::new("ROR", ror, 2, AddressingMode::DirectX));
    table.insert(0x7E, OpCode::new("ROR", ror, 3, AddressingMode::AbsoluteX));

    // 8. Branch, Jump
    // Short Branch
    table.insert(0x90, OpCode::new("BCC", bcc, 2, AddressingMode::Relative8));
    table.insert(0xB0, OpCode::new("BCS", bcs, 2, AddressingMode::Relative8));
    table.insert(0xF0, OpCode::new("BEQ", beq, 2, AddressingMode::Relative8));
    table.insert(0x30, OpCode::new("BMI", bmi, 2, AddressingMode::Relative8));
    table.insert(0xD0, OpCode::new("BNE", bne, 2, AddressingMode::Relative8));
    table.insert(0x10, OpCode::new("BPL", bpl, 2, AddressingMode::Relative8));
    table.insert(0x80, OpCode::new("BRA", bra, 2, AddressingMode::Relative8));
    table.insert(0x50, OpCode::new("BVC", bvc, 2, AddressingMode::Relative8));
    table.insert(0x70, OpCode::new("BVS", bvs, 2, AddressingMode::Relative8));

    // BRL
    table.insert(0x82, OpCode::new("BRL", brl, 3, AddressingMode::Relative16));

    // JMP, JSL, JSR
    table.insert(0x4C, OpCode::new("JMP", jmp, 3, AddressingMode::Absolute));
    table.insert(0x5C, OpCode::new("JMP", jmp, 4, AddressingMode::AbsoluteLong));
    table.insert(0x6C, OpCode::new("JMP", jmp, 3, AddressingMode::AbsoluteIndirect));
    table.insert(0x7C, OpCode::new("JMP", jmp, 3, AddressingMode::AbsoluteIndirectX));
    table.insert(0xDC, OpCode::new("JMP", jmp, 3, AddressingMode::AbsoluteIndirectLong));
    table.insert(0x22, OpCode::new("JSL", jmp, 4, AddressingMode::AbsoluteLong));
    table.insert(0x20, OpCode::new("JSR", jmp, 3, AddressingMode::Absolute));
    table.insert(0xFC, OpCode::new("JSR", jmp, 3, AddressingMode::AbsoluteIndirectX));

    // RTL, RTS
    table.insert(0x6B, OpCode::new("RTL", rtl, 1, AddressingMode::Implied));
    table.insert(0x60, OpCode::new("RTS", rts, 1, AddressingMode::Implied));

    // 9. Interrupt
    // BRK, COP
    table.insert(0x00, OpCode::new("BRK", brk, 1, AddressingMode::Implied));
    table.insert(0x02, OpCode::new("COP", cop, 2, AddressingMode::Immidiate));

    // RTI
    table.insert(0x40, OpCode::new("RTI", rti, 1, AddressingMode::Implied));

    // 10. Status
    // CLC, CLD, CLI, CLV, SEC, SED, SEI
    table.insert(0x18, OpCode::new("CLC", clc, 1, AddressingMode::Implied));
    table.insert(0xD8, OpCode::new("CLD", cld, 1, AddressingMode::Implied));
    table.insert(0x58, OpCode::new("CLI", cli, 1, AddressingMode::Implied));
    table.insert(0xB8, OpCode::new("CLV", clv, 1, AddressingMode::Implied));
    table.insert(0x38, OpCode::new("SEC", sec, 1, AddressingMode::Implied));
    table.insert(0xF8, OpCode::new("SED", sed, 1, AddressingMode::Implied));
    table.insert(0x78, OpCode::new("SEI", sei, 1, AddressingMode::Implied));

    // REP, SEP
    table.insert(0xC2, OpCode::new("REP", rep, 2, AddressingMode::Implied));
    table.insert(0xE2, OpCode::new("SEP", sep, 2, AddressingMode::Implied));

    // 11. Load, Store
    // LDA
    table.insert(0xA1, OpCode::new("LDA", lda, 2, AddressingMode::DirectIndirectX));
    table.insert(0xA3, OpCode::new("LDA", lda, 2, AddressingMode::Stack));
    table.insert(0xA5, OpCode::new("LDA", lda, 2, AddressingMode::Direct));
    table.insert(0xA7, OpCode::new("LDA", lda, 2, AddressingMode::DirectIndirectLong));
    table.insert(0xA9, OpCode::new("LDA", lda, 3, AddressingMode::Immidiate));
    table.insert(0xAD, OpCode::new("LDA", lda, 3, AddressingMode::Absolute));
    table.insert(0xAF, OpCode::new("LDA", lda, 4, AddressingMode::AbsoluteLong));
    table.insert(0xB1, OpCode::new("LDA", lda, 2, AddressingMode::DirectIndirectY));
    table.insert(0xB2, OpCode::new("LDA", lda, 2, AddressingMode::DirectIndirect));
    table.insert(0xB3, OpCode::new("LDA", lda, 2, AddressingMode::StackIndirectY));
    table.insert(0xB5, OpCode::new("LDA", lda, 2, AddressingMode::DirectX));
    table.insert(0xB7, OpCode::new("LDA", lda, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0xB9, OpCode::new("LDA", lda, 3, AddressingMode::AbsoluteY));
    table.insert(0xBD, OpCode::new("LDA", lda, 3, AddressingMode::AbsoluteX));
    table.insert(0xBF, OpCode::new("LDA", lda, 4, AddressingMode::AbsoluteLongX));

    // LDX
    table.insert(0xA2, OpCode::new("LDX", ldx, 3, AddressingMode::Immidiate));
    table.insert(0xA6, OpCode::new("LDX", ldx, 2, AddressingMode::Direct));
    table.insert(0xAE, OpCode::new("LDX", ldx, 3, AddressingMode::Absolute));
    table.insert(0xB6, OpCode::new("LDX", ldx, 2, AddressingMode::DirectY));
    table.insert(0xBE, OpCode::new("LDX", ldx, 3, AddressingMode::AbsoluteX));

    // LDY
    table.insert(0xA0, OpCode::new("LDY", ldy, 3, AddressingMode::Immidiate));
    table.insert(0xA4, OpCode::new("LDY", ldy, 2, AddressingMode::Direct));
    table.insert(0xAC, OpCode::new("LDY", ldy, 3, AddressingMode::Absolute));
    table.insert(0xB4, OpCode::new("LDY", ldy, 2, AddressingMode::DirectX));
    table.insert(0xBC, OpCode::new("LDY", ldy, 3, AddressingMode::AbsoluteX));

    // STA
    table.insert(0x81, OpCode::new("STA", sta, 2, AddressingMode::DirectIndirectX));
    table.insert(0x83, OpCode::new("STA", sta, 2, AddressingMode::Stack));
    table.insert(0x85, OpCode::new("STA", sta, 2, AddressingMode::Direct));
    table.insert(0x87, OpCode::new("STA", sta, 2, AddressingMode::DirectIndirectLong));
    table.insert(0x8D, OpCode::new("STA", sta, 3, AddressingMode::Absolute));
    table.insert(0x8F, OpCode::new("STA", sta, 4, AddressingMode::AbsoluteLong));
    table.insert(0x91, OpCode::new("STA", sta, 2, AddressingMode::DirectIndirectY));
    table.insert(0x92, OpCode::new("STA", sta, 2, AddressingMode::DirectIndirect));
    table.insert(0x93, OpCode::new("STA", sta, 2, AddressingMode::StackIndirectY));
    table.insert(0x95, OpCode::new("STA", sta, 2, AddressingMode::DirectX));
    table.insert(0x97, OpCode::new("STA", sta, 2, AddressingMode::DirectIndirectLongY));
    table.insert(0x99, OpCode::new("STA", sta, 3, AddressingMode::AbsoluteY));
    table.insert(0x9D, OpCode::new("STA", sta, 3, AddressingMode::AbsoluteX));
    table.insert(0x9F, OpCode::new("STA", sta, 4, AddressingMode::AbsoluteLongX));

    // STX
    table.insert(0x86, OpCode::new("STX", stx, 2, AddressingMode::Direct));
    table.insert(0x8E, OpCode::new("STX", stx, 3, AddressingMode::Absolute));
    table.insert(0x96, OpCode::new("STX", stx, 2, AddressingMode::DirectY));

    // STY
    table.insert(0x84, OpCode::new("STY", sty, 2, AddressingMode::Direct));
    table.insert(0x8C, OpCode::new("STY", sty, 3, AddressingMode::Absolute));
    table.insert(0x94, OpCode::new("STY", sty, 2, AddressingMode::DirectX));

    // STZ
    table.insert(0x64, OpCode::new("STZ", stz, 2, AddressingMode::Direct));
    table.insert(0x74, OpCode::new("STZ", stz, 2, AddressingMode::DirectX));
    table.insert(0x9C, OpCode::new("STZ", stz, 3, AddressingMode::Absolute));
    table.insert(0x9E, OpCode::new("STZ", stz, 3, AddressingMode::AbsoluteX));

    // 12. Move, Stack
    // MVN, MVP
    table.insert(0x54, OpCode::new("MVN", mvn, 3, AddressingMode::Immidiate));
    table.insert(0x44, OpCode::new("MVP", mvp, 3, AddressingMode::Immidiate));

    // PEA, PEI, PER
    table.insert(0xF4, OpCode::new("PEA", pea, 3, AddressingMode::Immidiate));
    table.insert(0xD4, OpCode::new("PEI", pea, 2, AddressingMode::Direct));
    table.insert(0x62, OpCode::new("PER", per, 3, AddressingMode::Immidiate));

    // PHA, PHX, PHY, PLA, PLX, PLY
    table.insert(0x48, OpCode::new("PHA", pha, 1, AddressingMode::Implied));
    table.insert(0xDA, OpCode::new("PHX", phx, 1, AddressingMode::Implied));
    table.insert(0x5A, OpCode::new("PHY", phy, 1, AddressingMode::Implied));
    table.insert(0x68, OpCode::new("PLA", pla, 1, AddressingMode::Implied));
    table.insert(0xFA, OpCode::new("PLX", plx, 1, AddressingMode::Implied));
    table.insert(0x7A, OpCode::new("PLY", ply, 1, AddressingMode::Implied));

    // PHB, PHD, PHK, PHP, PLB, PLD, PLP
    table.insert(0x8B, OpCode::new("PHB", phb, 1, AddressingMode::Implied));
    table.insert(0x0B, OpCode::new("PHD", phd, 1, AddressingMode::Implied));
    table.insert(0x4B, OpCode::new("PHK", phk, 1, AddressingMode::Implied));
    table.insert(0x08, OpCode::new("PHP", php, 1, AddressingMode::Implied));
    table.insert(0xAB, OpCode::new("PLB", plb, 1, AddressingMode::Implied));
    table.insert(0x2B, OpCode::new("PLD", pld, 1, AddressingMode::Implied));
    table.insert(0x28, OpCode::new("PLP", plp, 1, AddressingMode::Implied));

    // 13. Transfer
    // TAX, TAY, TSX, TXA, TXS, TXY, TYA, TYX
    table.insert(0xAA, OpCode::new("TAX", tax, 1, AddressingMode::Implied));
    table.insert(0xA8, OpCode::new("TAY", tay, 1, AddressingMode::Implied));
    table.insert(0xBA, OpCode::new("TSX", tsx, 1, AddressingMode::Implied));
    table.insert(0x8A, OpCode::new("TXA", txa, 1, AddressingMode::Implied));
    table.insert(0x9A, OpCode::new("TXS", txs, 1, AddressingMode::Implied));
    table.insert(0x9B, OpCode::new("TXY", txy, 1, AddressingMode::Implied));
    table.insert(0x98, OpCode::new("TYA", tya, 1, AddressingMode::Implied));
    table.insert(0xBB, OpCode::new("TYX", tyx, 1, AddressingMode::Implied));

    // TCD, TCS, TDC, TSC
    table.insert(0x5B, OpCode::new("TCD", tcd, 1, AddressingMode::Implied));
    table.insert(0x1B, OpCode::new("TCS", tcs, 1, AddressingMode::Implied));
    table.insert(0x7B, OpCode::new("TDC", tdc, 1, AddressingMode::Implied));
    table.insert(0x3B, OpCode::new("TSC", tsc, 1, AddressingMode::Implied));

    // 14. Halt
    // STP WAI
    table.insert(0xDB, OpCode::new("STP", stp, 1, AddressingMode::Implied));
    table.insert(0xCB, OpCode::new("WAI", wai, 1, AddressingMode::Implied));

    // 15. Exchange
    table.insert(0xEB, OpCode::new("XBA", xba, 1, AddressingMode::Implied));
    table.insert(0xFB, OpCode::new("XCE", xce, 1, AddressingMode::Implied));

    table
});

fn bcd(a: u16, b: u16, mut res: u16, sub: bool, size: usize) -> (u16, bool) {
    let mut adder = 6;
    let mut carry = false;
    for i in 0..size * 2 {
        let nibble_a = a >> (i * 4) & 0x000F;
        let nibble_b = b >> (i * 4) & 0x000F;
        let nibble_res = res >> (i * 4) & 0x000F;
        carry = nibble_a + nibble_b > 0x000F;
        carry = carry || nibble_res > 9;
        if carry {
            if !sub {
                res = res.wrapping_add(adder);
            } else {
                res = res.wrapping_sub(adder);
            }
        }
        adder <<= 4;
    }
    (res, carry)
}

fn elementary(m: usize, x: usize, w: usize, p: usize, s: usize, mode: AddressingMode) -> (usize, usize, bool) {
    match mode {
        AddressingMode::DirectIndirectX => (0, 7 - m + w, false),
        AddressingMode::Stack => (0, 5 - m, false),
        AddressingMode::Direct => (0, 4 - m + w, false),
        AddressingMode::DirectIndirectLong => (0, 7 - m + w, false),
        AddressingMode::Immidiate => (s, 3 - m, false),
        AddressingMode::Absolute => (0, 5 - m, false),
        AddressingMode::AbsoluteLong => (0, 6 - m, false),
        AddressingMode::DirectIndirectY => (0, 7 - m + w - x + x * p, false),
        AddressingMode::DirectIndirect => (0, 6 - m + w, false),
        AddressingMode::StackIndirectY => (0, 8 - m, false),
        AddressingMode::DirectX => (0, 5 - m + w, false),
        AddressingMode::DirectY => (0, 5 - m + w, false),
        AddressingMode::DirectIndirectLongY => (0, 7 - m + w, false),
        AddressingMode::AbsoluteY => (0, 6 - m + w + x * p, false),
        AddressingMode::AbsoluteX => (0, 6 - m + w + x * p, false),
        AddressingMode::AbsoluteLongX => (0, 6 - m, false),
        _ => panic!("Unrecognized Addressing Mode Detected"),
    }
}

fn binary(m: usize, w: usize, mode: AddressingMode) -> (usize, usize, bool) {
    match mode {
        AddressingMode::Direct => (0, 7 - 2 * m + w, false),
        AddressingMode::Absolute => (0, 8 - 2 * m, false),
        AddressingMode::DirectX => (0, 8 - 2 * m + w, false),
        AddressingMode::AbsoluteX => (0, 9 - 2 * m, false),
        _ => panic!("Unrecognized Addressing Mode Detected"),
    }
}

fn adc(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let carry = cpu.p.contains(Status::Carry);
    let (result, carry, mask) = if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let (result, carry) = accumulator.carrying_add(operand, carry);
        let (result, carry) = if cpu.p.contains(Status::Decimal) {
            let result = bcd(accumulator as u16, operand as u16, result as u16, false, 1);
            (result.0 as u8, result.1)
        } else {
            (result, carry)
        };
        cpu.p.set(Status::Overflow, (!result ^ accumulator) & (!result ^ operand) & 0x80 != 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        (result as u16, carry, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        let (result, carry) = cpu.a.carrying_add(operand, carry);
        let (result, carry) = if cpu.p.contains(Status::Decimal) {
            bcd(cpu.a, operand, result, false, 2)
        } else {
            (result, carry)
        };
        cpu.p.set(Status::Overflow, (!result ^ cpu.a) & (!result ^ operand) & 0x8000 != 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        (result as u16, carry, 0x0000u16)
    };

    cpu.p.set(Status::Carry, carry);
    cpu.p.set(Status::Zero, result == 0);

    cpu.a = (cpu.a & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn sbc(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let carry = cpu.p.contains(Status::Carry);
    let (result, carry, mask) = if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let (result, carry) = accumulator.carrying_add(!operand, carry);
        let (result, carry) = if cpu.p.contains(Status::Decimal) {
            let result = bcd(accumulator as u16, (!operand + 1) as u16, result as u16, true, 1);
            (result.0 as u8, result.1)
        } else {
            (result, accumulator >= operand)
        };
        cpu.p.set(Status::Overflow, (!result ^ accumulator) & (!result ^ operand) & 0x80 != 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        (result as u16, accumulator >= operand, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        let (result, carry) = cpu.a.carrying_add(!operand, carry);
        let (result, carry) = if cpu.p.contains(Status::Decimal) {
            bcd(cpu.a, !operand + 1, result, true, 2)
        } else {
            (result, cpu.a >= operand)
        };
        cpu.p.set(Status::Overflow, (!result ^ cpu.a) & (!result ^ operand) & 0x8000 != 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        (result as u16, cpu.a >= operand, 0x0000u16)
    };

    cpu.p.set(Status::Carry, carry);
    cpu.p.set(Status::Zero, result == 0);

    cpu.a = (cpu.a & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn cmp(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let (operand, accumulator) = if cpu.p.contains(Status::AccumulatorSize) {
        (bus.read_byte(operand_addr) as u16, cpu.a & 0x00FF)
    } else {
        (bus.read_word(operand_addr), cpu.a)
    };
    let result = accumulator.wrapping_sub(operand);
    let msb = if cpu.p.contains(Status::AccumulatorSize) {
        result & 0x80 != 0
    } else {
        result & 0x8000 != 0
    };

    cpu.p.set(Status::Carry, accumulator >= result);
    cpu.p.set(Status::Zero, result == 0);
    cpu.p.set(Status::Negative, msb);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn cpx(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let (operand, index_x) = if cpu.p.contains(Status::IndexSize) {
        (bus.read_byte(operand_addr) as u16, cpu.x & 0x00FF)
    } else {
        (bus.read_word(operand_addr), cpu.x)
    };
    let result = index_x.wrapping_sub(operand);
    let msb = if cpu.p.contains(Status::AccumulatorSize) {
        result & 0x80 != 0
    } else {
        result & 0x8000 != 0
    };

    cpu.p.set(Status::Carry, index_x >= result);
    cpu.p.set(Status::Zero, result == 0);
    cpu.p.set(Status::Negative, msb);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, x, mode)
}

fn cpy(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let (operand, index_y) = if cpu.p.contains(Status::IndexSize) {
        (bus.read_byte(operand_addr) as u16, cpu.y & 0x00FF)
    } else {
        (bus.read_word(operand_addr), cpu.y)
    };
    let result = index_y.wrapping_sub(operand);
    let msb = if cpu.p.contains(Status::AccumulatorSize) {
        result & 0x80 != 0
    } else {
        result & 0x8000 != 0
    };

    cpu.p.set(Status::Carry, index_y >= result);
    cpu.p.set(Status::Zero, result == 0);
    cpu.p.set(Status::Negative, msb);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, x, mode)
}

fn inc(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::IndexSize) {
        let operand = bus.read_byte(operand_addr);
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        bus.write_word(operand_addr, result);
    }

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn ina(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let operand = cpu.a as u8;
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.a = cpu.a & 0xFF00 | result as u16;
    } else {
        let operand = cpu.a;
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.a = result;
    }

    (0, 2, false)
}

fn inx(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let operand = cpu.x as u8;
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.x = cpu.x & 0xFF00 | result as u16;
    } else {
        let operand = cpu.x;
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.x = result;
    }

    (0, 2, false)
}

fn iny(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let operand = cpu.y as u8;
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.y = cpu.y & 0xFF00 | result as u16;
    } else {
        let operand = cpu.y;
        let result = operand.wrapping_add(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.y = result;
    }

    (0, 2, false)
}

fn dec(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::IndexSize) {
        let operand = bus.read_byte(operand_addr);
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        bus.write_word(operand_addr, result);
    }

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn dea(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let operand = cpu.a as u8;
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.a = cpu.a & 0xFF00 | result as u16;
    } else {
        let operand = cpu.a;
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.a = result;
    }

    (0, 2, false)
}

fn dex(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let operand = cpu.x as u8;
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.x = cpu.x & 0xFF00 | result as u16;
    } else {
        let operand = cpu.x;
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.x = result;
    }

    (0, 2, false)
}

fn dey(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let operand = cpu.y as u8;
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.y = cpu.y & 0xFF00 | result as u16;
    } else {
        let operand = cpu.y;
        let result = operand.wrapping_sub(1);
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.y = result;
    }

    (0, 2, false)
}

fn and(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let carry = cpu.p.contains(Status::Carry);
    let (result, carry, mask) = if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let result = accumulator & operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        (result as u16, carry, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        let result = cpu.a & operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        (result as u16, carry, 0x0000u16)
    };

    cpu.a = (cpu.a & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn eor(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let carry = cpu.p.contains(Status::Carry);
    let (result, carry, mask) = if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let result = accumulator ^ operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        (result as u16, carry, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        let result = cpu.a ^ operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        (result as u16, carry, 0x0000u16)
    };

    cpu.a = (cpu.a & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn ora(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let carry = cpu.p.contains(Status::Carry);
    let (result, carry, mask) = if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let result = accumulator | operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        (result as u16, carry, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        let result = cpu.a | operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        (result as u16, carry, 0x0000u16)
    };

    cpu.a = (cpu.a & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn bit(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let result = accumulator & operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Overflow, result & 0x40 != 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
    } else {
        let operand = bus.read_word(operand_addr);
        let result = cpu.a & operand;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Overflow, result & 0x4000 != 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
    };

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn trb(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let result = accumulator & operand;
        cpu.p.set(Status::Zero, result == 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let result = cpu.a & operand;
        cpu.p.set(Status::Zero, result == 0);
        bus.write_word(operand_addr, result);
    };

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn tsb(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let accumulator = cpu.a as u8;
        let result = accumulator | operand;
        cpu.p.set(Status::Zero, result == 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let result = cpu.a | operand;
        cpu.p.set(Status::Zero, result == 0);
        bus.write_word(operand_addr, result);
    };

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn asl(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let result = operand << 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x80 != 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let result = operand << 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x8000 != 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        bus.write_word(operand_addr, result);
    }

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn asla(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = cpu.a as u8;
        let result = operand << 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x80 != 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.a = cpu.a & 0xFF00 | result as u16;
    } else {
        let operand = cpu.a;
        let result = operand << 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x8000 != 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.a = result;
    }

    (0, 2, false)
}

fn lsr(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let result = operand >> 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x01 != 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let result = operand >> 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x01 != 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        bus.write_word(operand_addr, result);
    }

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn lsra(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = cpu.a as u8;
        let result = operand >> 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x01 != 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.a = cpu.a & 0xFF00 | result as u16;
    } else {
        let operand = cpu.a;
        let result = operand >> 1;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Carry, operand & 0x01 != 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.a = result;
    }

    (0, 2, false)
}

fn rol(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let carry = cpu.p.contains(Status::Carry) as u8;
        let result = operand << 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let carry = cpu.p.contains(Status::Carry) as u16;
        let result = operand << 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        bus.write_word(operand_addr, result);
    }

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn rola(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = cpu.a as u8;
        let carry = cpu.p.contains(Status::Carry) as u8;
        let result = operand << 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.a = cpu.a & 0xFF00 | result as u16;
    } else {
        let operand = cpu.a;
        let carry = cpu.p.contains(Status::Carry) as u16;
        let result = operand << 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.a = result;
    }

    (0, 2, false)
}

fn ror(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        let carry = (cpu.p.contains(Status::Carry) as u8) << 7;
        let result = operand >> 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        bus.write_byte(operand_addr, result);
    } else {
        let operand = bus.read_word(operand_addr);
        let carry = (cpu.p.contains(Status::Carry) as u16) << 15;
        let result = operand >> 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        bus.write_word(operand_addr, result);
    }

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;

    binary(m, w, mode)
}

fn rora(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        let operand = cpu.a as u8;
        let carry = (cpu.p.contains(Status::Carry) as u8) << 7;
        let result = operand >> 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x80 != 0);
        cpu.a = cpu.a & 0xFF00 | result as u16;
    } else {
        let operand = cpu.a;
        let carry = (cpu.p.contains(Status::Carry) as u16) << 15;
        let result = operand >> 1 | carry;
        cpu.p.set(Status::Zero, result == 0);
        cpu.p.set(Status::Negative, result & 0x8000 != 0);
        cpu.a = result;
    }

    (0, 2, false)
}

fn bcc(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = !cpu.p.contains(Status::Carry);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn bcs(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = cpu.p.contains(Status::Carry);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn beq(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = cpu.p.contains(Status::Zero);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn bmi(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = cpu.p.contains(Status::Negative);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn bne(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = !cpu.p.contains(Status::Zero);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn bpl(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = !cpu.p.contains(Status::Negative);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn bra(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = true;
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn bvc(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = !cpu.p.contains(Status::Overflow);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn bvs(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_byte(operand_addr);
    let is_branched = cpu.p.contains(Status::Overflow);
    let page_crossed = if is_branched {
        let offset = (offset as i8 as u32) & 0x00FFFFFF;
        let next_pc = cpu.program_counter().wrapping_add(offset).wrapping_add(2);
        let next_pb = (next_pc >> 16) as u8;
        let next_pc = next_pc as u16;
        let page_crossed = cpu.pb != next_pb;
        cpu.pb = next_pb;
        cpu.pc = next_pc;
        page_crossed
    } else {
        false
    };
    let p = page_crossed as usize;
    let t = is_branched as usize;
    let e = 0;
    (0, 2 + t + t * e * p, is_branched)
}

fn brl(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let offset = bus.read_word(operand_addr);
    cpu.pc = cpu.pc.wrapping_add(offset).wrapping_add(3);

    (0, 4, true)
}

fn jmp(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let jmp_addr = match mode {
        AddressingMode::Absolute => {
            let offset = bus.read_word(cpu.program_counter() + 1);
            address(cpu.pb, offset)
        }
        AddressingMode::AbsoluteLong => {
            let pc = cpu.program_counter();
            let offset = bus.read_word(pc + 1);
            let bank = bus.read_byte(pc + 3);
            address(bank, offset)
        }
        _ => match mode.fetch_addr(cpu, bus) {
            Ok(addr) => addr.0,
            Err(msg) => panic!("{}", msg),
        },
    };
    cpu.pc = jmp_addr as u16;
    cpu.pb = (jmp_addr >> 16) as u8;

    match mode {
        AddressingMode::Absolute => (0, 3, true),
        AddressingMode::AbsoluteLong => (0, 4, true),
        AddressingMode::AbsoluteIndirect => (0, 5, true),
        AddressingMode::AbsoluteIndirectX => (0, 6, true),
        AddressingMode::AbsoluteIndirectLong => (0, 6, true),
        _ => panic!("Unrecognized Addressing Mode Detected"),
    }
}

fn jsl(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let pc = cpu.program_counter();

    cpu.push_byte(bus, cpu.pb);
    cpu.push_word(bus, cpu.pc + 3);

    cpu.pc = bus.read_word(pc + 1);
    cpu.pb = bus.read_byte(pc + 3);

    (0, 8, true)
}

fn jsr(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let jmp_addr = match mode {
        AddressingMode::Absolute => {
            let offset = bus.read_word(cpu.program_counter() + 1);
            address(cpu.pb, offset)
        }
        _ => match mode.fetch_addr(cpu, bus) {
            Ok(addr) => addr.0,
            Err(msg) => panic!("{}", msg),
        },
    };

    cpu.push_word(bus, cpu.pc + 2);
    cpu.pc = jmp_addr as u16;

    match mode {
        AddressingMode::Absolute => (0, 6, true),
        AddressingMode::AbsoluteIndirectX => (0, 8, true),
        _ => panic!("Unrecognized Addressing Mode Detected"),
    }
}

fn rtl(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.pc = cpu.pull_word(bus);
    cpu.pb = cpu.pull_byte(bus);

    (0, 6, true)
}

fn rts(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.pc = cpu.pull_word(bus);

    (0, 6, true)
}

fn brk(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.push_byte(bus, cpu.pb);
    cpu.push_word(bus, cpu.pc.wrapping_add(2));
    cpu.push_byte(bus, cpu.p.bits());

    cpu.pb = 0x00;
    cpu.pc = bus.read_word(0x00FFE6);

    cpu.p.remove(Status::Decimal);
    cpu.p.insert(Status::IRQDisable);

    let e = 0;
    (0, 8 - e, true)
}

fn cop(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.push_byte(bus, cpu.pb);
    cpu.push_word(bus, cpu.pc.wrapping_add(2));
    cpu.push_byte(bus, cpu.p.bits());

    cpu.pb = 0x00;
    cpu.pc = bus.read_word(0x00FFE4);

    cpu.p.remove(Status::Decimal);
    cpu.p.insert(Status::IRQDisable);

    let e = 0;
    (0, 8 - e, true)
}

fn rti(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p = Status::from_bits_truncate(cpu.pull_byte(bus));
    cpu.pc = cpu.pull_word(bus);
    cpu.pb = cpu.pull_byte(bus);

    let e = 0;
    (0, 7 - e, true)
}

fn clc(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p.remove(Status::Carry);
    (0, 2, false)
}

fn cld(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p.remove(Status::Decimal);
    (0, 2, false)
}

fn cli(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p.remove(Status::IRQDisable);
    (0, 2, false)
}

fn clv(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p.remove(Status::Overflow);
    (0, 2, false)
}

fn sec(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p.insert(Status::Carry);
    (0, 2, false)
}

fn sed(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p.insert(Status::Decimal);
    (0, 2, false)
}

fn sei(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p.insert(Status::IRQDisable);
    (0, 2, false)
}

fn rep(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    let operand = bus.read_byte(cpu.program_counter() + 1);
    let result = cpu.p.bits() & !operand;
    cpu.p = Status::from_bits_truncate(result);

    (0, 3, false)
}

fn sep(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    let operand = bus.read_byte(cpu.program_counter() + 1);
    let result = cpu.p.bits() | operand;
    cpu.p = Status::from_bits_truncate(result);

    (0, 3, false)
}

fn lda(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let (result, mask) = if cpu.p.contains(Status::AccumulatorSize) {
        let operand = bus.read_byte(operand_addr);
        cpu.p.set(Status::Negative, operand & 0x80 != 0);
        (operand as u16, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        cpu.p.set(Status::Negative, operand & 0x8000 != 0);
        (operand, 0x0000u16)
    };

    cpu.p.set(Status::Zero, result == 0);

    cpu.a = (cpu.a & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn ldx(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let (result, mask) = if cpu.p.contains(Status::IndexSize) {
        let operand = bus.read_byte(operand_addr);
        cpu.p.set(Status::Negative, operand & 0x80 != 0);
        (operand as u16, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        cpu.p.set(Status::Negative, operand & 0x8000 != 0);
        (operand, 0x0000u16)
    };

    cpu.p.set(Status::Zero, result == 0);

    cpu.x = (cpu.x & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, x, mode)
}

fn ldy(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let (result, mask) = if cpu.p.contains(Status::IndexSize) {
        let operand = bus.read_byte(operand_addr);
        cpu.p.set(Status::Negative, operand & 0x80 != 0);
        (operand as u16, 0xFF00u16)
    } else {
        let operand = bus.read_word(operand_addr);
        cpu.p.set(Status::Negative, operand & 0x8000 != 0);
        (operand, 0x0000u16)
    };

    cpu.p.set(Status::Zero, result == 0);

    cpu.y = (cpu.y & mask) | (result & !mask);

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, x, mode)
}

fn sta(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        let register = cpu.a as u8;
        bus.write_byte(operand_addr, register);
    } else {
        let register = cpu.a;
        bus.write_word(operand_addr, register);
    };

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, m, mode)
}

fn stx(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::IndexSize) {
        let register = cpu.x as u8;
        bus.write_byte(operand_addr, register);
    } else {
        let register = cpu.x;
        bus.write_word(operand_addr, register);
    };

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, x, mode)
}

fn sty(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::IndexSize) {
        let register = cpu.y as u8;
        bus.write_byte(operand_addr, register);
    } else {
        let register = cpu.y;
        bus.write_word(operand_addr, register);
    };

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, x, mode)
}

fn stz(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, page_crossed) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    if cpu.p.contains(Status::AccumulatorSize) {
        bus.write_byte(operand_addr, 0);
    } else {
        bus.write_word(operand_addr, 0);
    };

    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    let x = cpu.p.contains(Status::IndexSize) as usize;
    let w = (cpu.d & 0x00FF != 0) as usize;
    let p = page_crossed as usize;

    elementary(m, x, w, p, x, mode)
}

fn mvn(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    let src_bank = bus.read_byte(cpu.program_counter() + 1);
    let dst_bank = bus.read_byte(cpu.program_counter() + 2);
    let data = bus.read_byte(address(src_bank, cpu.x));
    bus.write_byte(address(dst_bank, cpu.y), data);
    cpu.a = cpu.a.wrapping_sub(1);
    cpu.x = cpu.x.wrapping_sub(1);
    cpu.y = cpu.y.wrapping_sub(1);
    (0, 7, cpu.a != 0xFFFF)
}

fn mvp(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    let src_bank = bus.read_byte(cpu.program_counter() + 1);
    let dst_bank = bus.read_byte(cpu.program_counter() + 2);
    let data = bus.read_byte(address(src_bank, cpu.x));
    bus.write_byte(address(dst_bank, cpu.y), data);
    cpu.a = cpu.a.wrapping_sub(1);
    cpu.x = cpu.x.wrapping_add(1);
    cpu.y = cpu.y.wrapping_add(1);
    (0, 7, cpu.a != 0xFFFF)
}

fn nop(cpu: &mut Cpu, _bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    (0, 2, false)
}

fn pea(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let operand = bus.read_word(operand_addr);
    cpu.push_word(bus, operand);

    let w = (cpu.d & 0x00FF != 0) as usize;

    match mode {
        AddressingMode::Absolute => (0, 5, false),
        AddressingMode::AbsoluteIndirectX => (0, 6 + w, false),
        _ => panic!("Unrecognized Addressing Mode Detected"),
    }
}

fn per(cpu: &mut Cpu, bus: &mut dyn Bus, mode: AddressingMode) -> (usize, usize, bool) {
    let (operand_addr, _) = match mode.fetch_addr(cpu, bus) {
        Ok(addr) => addr,
        Err(msg) => panic!("{}", msg),
    };
    let operand = bus.read_word(operand_addr);
    cpu.push_word(bus, cpu.pc.wrapping_add(operand));

    (0, 6, false)
}

fn pha(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        cpu.push_byte(bus, cpu.a as u8);
    } else {
        cpu.push_word(bus, cpu.a);
    }
    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    (0, 4 - m, false)
}

fn phx(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        cpu.push_byte(bus, cpu.x as u8);
    } else {
        cpu.push_word(bus, cpu.x);
    }
    let x = cpu.p.contains(Status::IndexSize) as usize;
    (0, 4 - x, false)
}

fn phy(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        cpu.push_byte(bus, cpu.y as u8);
    } else {
        cpu.push_word(bus, cpu.y);
    }
    let x = cpu.p.contains(Status::IndexSize) as usize;
    (0, 4 - x, false)
}

fn pla(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        let value = cpu.pull_byte(bus) as u16;
        cpu.a = cpu.a & 0xFF00 | value;
        cpu.p.set(Status::Zero, cpu.a == 0);
        cpu.p.set(Status::Negative, value & 0x80 != 0);
    } else {
        cpu.a = cpu.pull_word(bus);
        cpu.p.set(Status::Zero, cpu.a == 0);
        cpu.p.set(Status::Negative, cpu.a & 0x8000 != 0);
    }
    let m = cpu.p.contains(Status::AccumulatorSize) as usize;
    (0, 5 - m, false)
}

fn plx(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let value = cpu.pull_byte(bus) as u16;
        cpu.x = cpu.x & 0xFF00 | value;
        cpu.p.set(Status::Zero, cpu.x == 0);
        cpu.p.set(Status::Negative, value & 0x80 != 0);
    } else {
        cpu.x = cpu.pull_word(bus);
        cpu.p.set(Status::Zero, cpu.x == 0);
        cpu.p.set(Status::Negative, cpu.x & 0x8000 != 0);
    }
    let x = cpu.p.contains(Status::IndexSize) as usize;
    (0, 5 - x, false)
}

fn ply(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        let value = cpu.pull_byte(bus) as u16;
        cpu.y = cpu.y & 0xFF00 | value;
        cpu.p.set(Status::Zero, cpu.y == 0);
        cpu.p.set(Status::Negative, value & 0x80 != 0);
    } else {
        cpu.x = cpu.pull_word(bus);
        cpu.p.set(Status::Zero, cpu.y == 0);
        cpu.p.set(Status::Negative, cpu.y & 0x8000 != 0);
    }
    let x = cpu.p.contains(Status::IndexSize) as usize;
    (0, 5 - x, false)
}

fn phb(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.push_byte(bus, cpu.db);
    (0, 3, false)
}

fn phd(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.push_word(bus, cpu.d);
    (0, 4, false)
}

fn phk(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.push_byte(bus, cpu.pb);
    (0, 3, false)
}

fn php(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.push_byte(bus, cpu.p.bits());
    (0, 3, false)
}

fn plb(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.db = cpu.pull_byte(bus);
    cpu.p.set(Status::Negative, cpu.db & 0x80 != 0);
    cpu.p.set(Status::Zero, cpu.db == 0);
    (0, 4, false)
}

fn pld(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.d = cpu.pull_word(bus);
    cpu.p.set(Status::Negative, cpu.d & 0x8000 != 0);
    cpu.p.set(Status::Zero, cpu.d == 0);
    (0, 5, false)
}

fn plp(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.p = Status::from_bits_truncate(cpu.pull_byte(bus));
    (0, 4, false)
}

fn stp(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    loop {}
    (0, 3, false)
}

fn wai(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.wait = true;
    (0, 3, false)
}

fn tax(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        cpu.x = cpu.x & 0xFF00 | cpu.a & 0x00FF;
        cpu.p.set(Status::Negative, cpu.x & 0x0080 != 0);
        cpu.p.set(Status::Zero, cpu.x & 0x00FF == 0);
    } else {
        cpu.x = cpu.a;
        cpu.p.set(Status::Negative, cpu.x & 0x8000 != 0);
        cpu.p.set(Status::Zero, cpu.x == 0);
    }
    (0, 2, false)
}

fn tay(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        cpu.y = cpu.y & 0xFF00 | cpu.a & 0x00FF;
        cpu.p.set(Status::Negative, cpu.y & 0x0080 != 0);
        cpu.p.set(Status::Zero, cpu.y & 0x00FF == 0);
    } else {
        cpu.y = cpu.a;
        cpu.p.set(Status::Negative, cpu.y & 0x8000 != 0);
        cpu.p.set(Status::Zero, cpu.y == 0);
    }
    (0, 2, false)
}

fn tsx(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.x = cpu.s;
    cpu.p.set(Status::Negative, cpu.x & 0x8000 != 0);
    cpu.p.set(Status::Zero, cpu.x == 0);

    (0, 2, false)
}

fn txa(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        cpu.a = cpu.a & 0xFF00 | cpu.x & 0x00FF;
        cpu.p.set(Status::Negative, cpu.x & 0x0080 != 0);
        cpu.p.set(Status::Zero, cpu.x & 0x00FF == 0);
    } else {
        cpu.a = cpu.x;
        cpu.p.set(Status::Negative, cpu.y & 0x8000 != 0);
        cpu.p.set(Status::Zero, cpu.y == 0);
    }
    (0, 2, false)
}

fn txs(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.s = cpu.x;

    (0, 2, false)
}

fn txy(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        cpu.y = cpu.y & 0xFF00 | cpu.x & 0x00FF;
        cpu.p.set(Status::Negative, cpu.y & 0x0080 != 0);
        cpu.p.set(Status::Zero, cpu.y & 0x00FF == 0);
    } else {
        cpu.y = cpu.x;
        cpu.p.set(Status::Negative, cpu.y & 0x8000 != 0);
        cpu.p.set(Status::Zero, cpu.y == 0);
    }
    (0, 2, false)
}

fn tya(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::AccumulatorSize) {
        cpu.a = cpu.a & 0xFF00 | cpu.y & 0x00FF;
        cpu.p.set(Status::Negative, cpu.a & 0x0080 != 0);
        cpu.p.set(Status::Zero, cpu.a & 0x00FF == 0);
    } else {
        cpu.a = cpu.y;
        cpu.p.set(Status::Negative, cpu.a & 0x8000 != 0);
        cpu.p.set(Status::Zero, cpu.a == 0);
    }
    (0, 2, false)
}

fn tyx(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    if cpu.p.contains(Status::IndexSize) {
        cpu.x = cpu.x & 0xFF00 | cpu.y & 0x00FF;
        cpu.p.set(Status::Negative, cpu.y & 0x0080 != 0);
        cpu.p.set(Status::Zero, cpu.y & 0x00FF == 0);
    } else {
        cpu.x = cpu.y;
        cpu.p.set(Status::Negative, cpu.y & 0x8000 != 0);
        cpu.p.set(Status::Zero, cpu.y == 0);
    }
    (0, 2, false)
}

fn tcd(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.d = cpu.a;
    cpu.p.set(Status::Negative, cpu.d & 0x8000 != 0);
    cpu.p.set(Status::Zero, cpu.d == 0);

    (0, 2, false)
}

fn tcs(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.s = cpu.a;
    cpu.p.set(Status::Negative, cpu.s & 0x8000 != 0);
    cpu.p.set(Status::Zero, cpu.s == 0);

    (0, 2, false)
}

fn tdc(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.a = cpu.d;
    cpu.p.set(Status::Negative, cpu.a & 0x8000 != 0);
    cpu.p.set(Status::Zero, cpu.a == 0);

    (0, 2, false)
}

fn tsc(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.a = cpu.s;
    cpu.p.set(Status::Negative, cpu.a & 0x8000 != 0);
    cpu.p.set(Status::Zero, cpu.a == 0);

    (0, 2, false)
}

fn xba(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    cpu.a = (cpu.a >> 8) | (cpu.a << 8);
    cpu.p.set(Status::Negative, cpu.a & 0x8000 != 0);
    cpu.p.set(Status::Zero, cpu.a == 0);

    (0, 3, false)
}

fn xce(cpu: &mut Cpu, bus: &mut dyn Bus, _mode: AddressingMode) -> (usize, usize, bool) {
    let e = false;
    cpu.p.set(Status::Carry, e);

    (0, 2, false)
}

#[cfg(test)]
mod test {
    use super::*;
    use hashbrown::HashMap;

    const ZERO: u8 = 0x00;
    const DATA: u8 = 0x40;
    const CODE: u8 = 0x80;

    struct TestBus {
        memory: HashMap<u32, u8>,
    }

    impl TestBus {
        pub fn empty() -> Self {
            Self { memory: HashMap::new() }
        }

        pub fn insert(&mut self, base: u32, code: &[u8]) {
            for (idx, &byte) in code.iter().enumerate() {
                self.write_byte(base + idx as u32, byte);
            }
        }
    }

    impl Bus for TestBus {
        fn read_byte(&self, address: u32) -> u8 {
            *self.memory.get(&address).unwrap_or(&0)
        }

        fn write_byte(&mut self, address: u32, value: u8) {
            self.memory.insert(address, value);
        }
    }

    #[test]
    fn all_instruction_implement_test() {
        for opcode in 0..=0xFF {
            assert!(!OPCODE_TABLE.get(&opcode).is_none(), "Opcode 0x{opcode:02X} is not implemented");
        }
    }

    #[test]
    fn test_sbc_binary() {
        let mut cpu = Cpu::new();
        let mut bus = TestBus::empty();

        cpu.a = 0x0001;
        cpu.pb = CODE;
        cpu.db = DATA;
        bus.insert(CODE as u32 * 0x10000, &[0xE9, 0x03, 0x20]);

        cpu.p = Status::Carry;
        let opcode = cpu.fetch(&mut bus);
        let inst = cpu.decode(opcode);
        cpu.execute(&mut bus, inst);

        assert_eq!(cpu.a, 0xDFFE);
        assert!(cpu.p.contains(Status::Negative));
        assert!(!cpu.p.contains(Status::Overflow));
        assert!(!cpu.p.contains(Status::Zero));
        assert!(!cpu.p.contains(Status::Carry));
    }

    #[test]
    fn test_sbc_bcd() {
        let mut cpu = Cpu::new();
        let mut bus = TestBus::empty();

        cpu.a = 0x0001;
        cpu.pb = CODE;
        cpu.db = DATA;
        bus.insert(CODE as u32 * 0x10000, &[0xE9, 0x03, 0x20]);

        cpu.p = Status::Carry | Status::Decimal;
        let opcode = cpu.fetch(&mut bus);
        let inst = cpu.decode(opcode);
        cpu.execute(&mut bus, inst);

        assert_eq!(cpu.a, 0x7998);
        assert!(!cpu.p.contains(Status::Negative));
        assert!(!cpu.p.contains(Status::Zero));
        assert!(!cpu.p.contains(Status::Carry));
    }
}
