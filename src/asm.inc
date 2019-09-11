ASM_DEF(ADC, 
        0   , 0, // relative
        0x69, 2, // immediate
        0x65, 3, // zeropage
        0x75, 4, // zeropage, x
        0   , 0, // zeropage, y
        0x6D, 4, // absolute
        0x7D, 4, // absolute, x
        0x79, 4, // absolute, y
        0   , 0, // indirect
        0x61, 6, // (indirect, x)
        0x71, 5, // (indirect, y)
        )

ASM_DEF(AND, 
        0   , 0, // relative
        0x29, 2, // immediate
        0x25, 3, // zeropage
        0x35, 4, // zeropage, x
        0   , 0, // zeropage, y
        0x2D, 4, // absolute
        0x3D, 4, // absolute, x
        0x39, 4, // absolute, y
        0   , 0, // indirect
        0x21, 6, // (indirect, x)
        0x31, 5, // (indirect, y)
        )




extern opcode_table_t op_relative_table;
extern opcode_table_t op_immediate_table;
extern opcode_table_t op_immediate_x_table;
extern opcode_table_t op_immediate_y_table;
extern opcode_table_t op_absolute_table;
extern opcode_table_t op_absolute_x_table;
extern opcode_table_t op_absolute_y_table;
extern opcode_table_t op_indirect_table;
extern opcode_table_t op_indirect_x_table;
extern opcode_table_t op_indirect_y_table;

ASM_DEF(AND
ASM_DEF(ASL
ASM_DEF(BCC
ASM_DEF(BCS
ASM_DEF(BEQ
ASM_DEF(BIT
ASM_DEF(BMI
ASM_DEF(BNE
ASM_DEF(BPL
ASM_DEF(BRK
ASM_DEF(BVC
ASM_DEF(BVS
ASM_DEF(CLC
ASM_DEF(CLD
ASM_DEF(CLI
ASM_DEF(CLV
ASM_DEF(CMP
ASM_DEF(CPX
ASM_DEF(CPY
ASM_DEF(DEC
ASM_DEF(DEX
ASM_DEF(DEY
ASM_DEF(EOR
ASM_DEF(INC
ASM_DEF(INX
ASM_DEF(INY
ASM_DEF(JMP
ASM_DEF(JSR
ASM_DEF(LDA
ASM_DEF(LDX
ASM_DEF(LDY
ASM_DEF(LSR
ASM_DEF(NOP
ASM_DEF(ORA
ASM_DEF(PHA
ASM_DEF(PHP
ASM_DEF(PLA
ASM_DEF(PLP
ASM_DEF(ROL
ASM_DEF(ROR
ASM_DEF(RTI
ASM_DEF(RTS
ASM_DEF(SBC
ASM_DEF(SEC
ASM_DEF(SED
ASM_DEF(SEI
ASM_DEF(STA
ASM_DEF(STX
ASM_DEF(STY
ASM_DEF(TAX
ASM_DEF(TAY
ASM_DEF(TSX
ASM_DEF(TXA
ASM_DEF(TXS
ASM_DEF(TYA

