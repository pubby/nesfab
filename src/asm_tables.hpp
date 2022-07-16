#define OP(name, mode) .op=name##_##mode, .op_name=name, .addr_mode=MODE_##mode

constexpr op_def_t op_defs_table[NUM_OPS] =
{
    { .op = BAD_OP },
    { .op = ASM_LABEL },
    { .op = ASM_PRUNED },
    { 
        .op = ASM_DELAY,
        .cycles = 255,
    },
    { 
        .op = MAYBE_STA,
        .size = 3,
        .cycles = 2, // Arbitrary
        .input_regs = REGF_A,
        .output_regs = REGF_M,
        .flags = ASMF_MAYBE | ASMF_MAYBE_STORE,
    },
    { 
        .op = MAYBE_STX,
        .size = 3,
        .cycles = 2, // Arbitrary
        .input_regs = REGF_X,
        .output_regs = REGF_M,
        .flags = ASMF_MAYBE | ASMF_MAYBE_STORE,
    },
    { 
        .op = MAYBE_STY,
        .size = 3,
        .cycles = 2, // Arbitrary
        .input_regs = REGF_Y,
        .output_regs = REGF_M,
        .flags = ASMF_MAYBE | ASMF_MAYBE_STORE,
    },
    { 
        .op = MAYBE_SAX,
        .size = 3,
        .cycles = 2, // Arbitrary
        .input_regs = REGF_AX,
        .output_regs = REGF_M,
        .flags = ASMF_MAYBE | ASMF_MAYBE_STORE,
    },
    { 
        .op = MAYBE_STORE_C,
        .size = 9,
        .cycles = 13, // Arbitrary
        .input_regs = REGF_C,
        .output_regs = REGF_M,
        .flags = ASMF_MAYBE | ASMF_MAYBE_STORE,
    },

    // ADC
    {
        OP(ADC, IMMEDIATE),
        .op_code = 105,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A | REGF_C,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ADC, ZERO_PAGE),
        .op_code = 101,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ADC, ZERO_PAGE_X),
        .op_code = 117,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ADC, ABSOLUTE),
        .op_code = 109,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ADC, ABSOLUTE_X),
        .op_code = 125,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ADC, ABSOLUTE_Y),
        .op_code = 121,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ADC, INDIRECT_X),
        .op_code = 97,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ADC, INDIRECT_Y),
        .op_code = 113,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },

    // AND
    {
        OP(AND, IMMEDIATE),
        .op_code = 41,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(AND, ZERO_PAGE),
        .op_code = 37,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(AND, ZERO_PAGE_X),
        .op_code = 53,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(AND, ABSOLUTE),
        .op_code = 45,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(AND, ABSOLUTE_X),
        .op_code = 61,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(AND, ABSOLUTE_Y),
        .op_code = 57,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(AND, INDIRECT_X),
        .op_code = 33,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(AND, INDIRECT_Y),
        .op_code = 49,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },

    // ASL
    {
        OP(ASL, IMPLIED),
        .op_code = 10,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ASL, ZERO_PAGE),
        .op_code = 6,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ASL, ZERO_PAGE_X),
        .op_code = 22,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ASL, ABSOLUTE),
        .op_code = 14,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ASL, ABSOLUTE_X),
        .op_code = 30,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },

    // BCC
    {
        OP(BCC, RELATIVE),
        .op_code = 144,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_C,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BCC, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_C,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // BCS
    {
        OP(BCS, RELATIVE),
        .op_code = 176,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_C,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BCS, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_C,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // BEQ
    {
        OP(BEQ, RELATIVE),
        .op_code = 240,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_NZ,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BEQ, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_NZ,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // BIT
    {
        OP(BIT, ZERO_PAGE),
        .op_code = 36,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ,
    },
    {
        OP(BIT, ABSOLUTE),
        .op_code = 44,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ,
    },

    // BMI
    {
        OP(BMI, RELATIVE),
        .op_code = 48,
        .size = 2,
        .cycles = 3,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BMI, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // BNE
    {
        OP(BNE, RELATIVE),
        .op_code = 208,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_NZ,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BNE, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_NZ,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // BPL
    {
        OP(BPL, RELATIVE),
        .op_code = 16,
        .size = 2,
        .cycles = 3,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BPL, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // BRK
    {
        OP(BRK, IMPLIED),
        .op_code = 0,
        .size = 1,
        .cycles = 7,
        .input_regs = 0,
        .output_regs = 0,
    },

    // BVC
    {
        OP(BVC, RELATIVE),
        .op_code = 80,
        .size = 2,
        .cycles = 3,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BVC, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // BVS
    {
        OP(BVS, RELATIVE),
        .op_code = 112,
        .size = 2,
        .cycles = 3,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },
    {
        OP(BVS, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH | ASMF_JUMP,
    },

    // CLC
    {
        OP(CLC, IMPLIED),
        .op_code = 24,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_C,
    },

    // CLD
    {
        OP(CLD, IMPLIED),
        .op_code = 216,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
    },

    // CLI
    {
        OP(CLI, IMPLIED),
        .op_code = 88,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
    },

    // CLV
    {
        OP(CLV, IMPLIED),
        .op_code = 184,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
    },

    // CMP
    {
        OP(CMP, IMMEDIATE),
        .op_code = 201,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CMP, ZERO_PAGE),
        .op_code = 197,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CMP, ZERO_PAGE_X),
        .op_code = 213,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CMP, ABSOLUTE),
        .op_code = 205,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CMP, ABSOLUTE_X),
        .op_code = 221,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CMP, ABSOLUTE_Y),
        .op_code = 217,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CMP, INDIRECT_X),
        .op_code = 193,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CMP, INDIRECT_Y),
        .op_code = 209,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },

    // CPX
    {
        OP(CPX, IMMEDIATE),
        .op_code = 224,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_X,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CPX, ZERO_PAGE),
        .op_code = 228,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CPX, ABSOLUTE),
        .op_code = 236,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },

    // CPY
    {
        OP(CPY, IMMEDIATE),
        .op_code = 192,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CPY, ZERO_PAGE),
        .op_code = 196,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },
    {
        OP(CPY, ABSOLUTE),
        .op_code = 204,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_C,
    },

    // DEC
    {
        OP(DEC, ZERO_PAGE),
        .op_code = 198,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },
    {
        OP(DEC, ZERO_PAGE_X),
        .op_code = 214,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },
    {
        OP(DEC, ABSOLUTE),
        .op_code = 206,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },
    {
        OP(DEC, ABSOLUTE_X),
        .op_code = 222,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },

    // DEX
    {
        OP(DEX, IMPLIED),
        .op_code = 202,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_X,
        .output_regs = REGF_NZ | REGF_X,
    },

    // DEY
    {
        OP(DEY, IMPLIED),
        .op_code = 136,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_Y,
        .output_regs = REGF_NZ | REGF_Y,
    },

    // EOR
    {
        OP(EOR, IMMEDIATE),
        .op_code = 73,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(EOR, ZERO_PAGE),
        .op_code = 69,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(EOR, ZERO_PAGE_X),
        .op_code = 85,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(EOR, ABSOLUTE),
        .op_code = 77,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(EOR, ABSOLUTE_X),
        .op_code = 93,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(EOR, ABSOLUTE_Y),
        .op_code = 89,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(EOR, INDIRECT_X),
        .op_code = 65,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(EOR, INDIRECT_Y),
        .op_code = 81,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },

    // INC
    {
        OP(INC, ZERO_PAGE),
        .op_code = 230,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },
    {
        OP(INC, ZERO_PAGE_X),
        .op_code = 246,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },
    {
        OP(INC, ABSOLUTE),
        .op_code = 238,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },
    {
        OP(INC, ABSOLUTE_X),
        .op_code = 254,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_M,
    },

    // INX
    {
        OP(INX, IMPLIED),
        .op_code = 232,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_X,
        .output_regs = REGF_NZ | REGF_X,
    },

    // INY
    {
        OP(INY, IMPLIED),
        .op_code = 200,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_Y,
        .output_regs = REGF_NZ | REGF_Y,
    },

    // JMP
    {
        OP(JMP, ABSOLUTE),
        .op_code = 76,
        .size = 3,
        .cycles = 3,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_JUMP,
    },
    {
        OP(JMP, INDIRECT),
        .op_code = 108,
        .size = 3,
        .cycles = 5,
        .input_regs = REGF_M,
        .output_regs = 0,
        .flags = ASMF_JUMP,
    },

    // JSR
    {
        OP(JSR, ABSOLUTE),
        .op_code = 32,
        .size = 3,
        .cycles = 6,
        .input_regs = 0,
        .output_regs = 0,
    },

    // LDA
    {
        OP(LDA, IMMEDIATE),
        .op_code = 169,
        .size = 2,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(LDA, ZERO_PAGE),
        .op_code = 165,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(LDA, ZERO_PAGE_X),
        .op_code = 181,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(LDA, ABSOLUTE),
        .op_code = 173,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(LDA, ABSOLUTE_X),
        .op_code = 189,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(LDA, ABSOLUTE_Y),
        .op_code = 185,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(LDA, INDIRECT_X),
        .op_code = 161,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(LDA, INDIRECT_Y),
        .op_code = 177,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },

    // LDX
    {
        OP(LDX, IMMEDIATE),
        .op_code = 162,
        .size = 2,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_NZ | REGF_X,
    },
    {
        OP(LDX, ZERO_PAGE),
        .op_code = 166,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_X,
    },
    {
        OP(LDX, ZERO_PAGE_Y),
        .op_code = 182,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_X,
    },
    {
        OP(LDX, ABSOLUTE),
        .op_code = 174,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_X,
    },
    {
        OP(LDX, ABSOLUTE_Y),
        .op_code = 190,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_X,
    },

    // LDY
    {
        OP(LDY, IMMEDIATE),
        .op_code = 160,
        .size = 2,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_NZ | REGF_Y,
    },
    {
        OP(LDY, ZERO_PAGE),
        .op_code = 164,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_Y,
    },
    {
        OP(LDY, ZERO_PAGE_X),
        .op_code = 180,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_Y,
    },
    {
        OP(LDY, ABSOLUTE),
        .op_code = 172,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_Y,
    },
    {
        OP(LDY, ABSOLUTE_X),
        .op_code = 188,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_Y,
    },

    // LSR
    {
        OP(LSR, IMPLIED),
        .op_code = 74,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(LSR, ZERO_PAGE),
        .op_code = 70,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(LSR, ZERO_PAGE_X),
        .op_code = 86,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(LSR, ABSOLUTE),
        .op_code = 78,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(LSR, ABSOLUTE_X),
        .op_code = 94,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },

    // NOP
    {
        OP(NOP, IMPLIED),
        .op_code = 234,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
    },

    // ORA
    {
        OP(ORA, IMMEDIATE),
        .op_code = 9,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(ORA, ZERO_PAGE),
        .op_code = 5,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(ORA, ZERO_PAGE_X),
        .op_code = 21,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(ORA, ABSOLUTE),
        .op_code = 13,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(ORA, ABSOLUTE_X),
        .op_code = 29,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(ORA, ABSOLUTE_Y),
        .op_code = 25,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(ORA, INDIRECT_X),
        .op_code = 1,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },
    {
        OP(ORA, INDIRECT_Y),
        .op_code = 17,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },

    // PHA
    {
        OP(PHA, IMPLIED),
        .op_code = 72,
        .size = 1,
        .cycles = 3,
        .input_regs = REGF_A,
        .output_regs = 0,
    },

    // PHP
    {
        OP(PHP, IMPLIED),
        .op_code = 8,
        .size = 1,
        .cycles = 3,
        .input_regs = REGF_C,
        .output_regs = 0,
    },

    // PLA
    {
        OP(PLA, IMPLIED),
        .op_code = 104,
        .size = 1,
        .cycles = 4,
        .input_regs = 0,
        .output_regs = REGF_NZ | REGF_A,
    },

    // PLP
    {
        OP(PLP, IMPLIED),
        .op_code = 40,
        .size = 1,
        .cycles = 4,
        .input_regs = 0,
        .output_regs = REGF_NZ | REGF_C,
    },

    // ROL
    {
        OP(ROL, IMPLIED),
        .op_code = 42,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_A | REGF_C,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ROL, ZERO_PAGE),
        .op_code = 38,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ROL, ZERO_PAGE_X),
        .op_code = 54,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ROL, ABSOLUTE),
        .op_code = 46,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ROL, ABSOLUTE_X),
        .op_code = 62,
        .size = 3,
        .cycles = 3,
        .input_regs = REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },

    // ROR
    {
        OP(ROR, IMPLIED),
        .op_code = 106,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_A | REGF_C,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ROR, ZERO_PAGE),
        .op_code = 102,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ROR, ZERO_PAGE_X),
        .op_code = 118,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ROR, ABSOLUTE),
        .op_code = 110,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },
    {
        OP(ROR, ABSOLUTE_X),
        .op_code = 126,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_C | REGF_M,
    },

    // RTI
    {
        OP(RTI, IMPLIED),
        .op_code = 64,
        .size = 1,
        .cycles = 6,
        .input_regs = 0,
        .output_regs = REGF_C | REGF_NZ,
    },

    // RTS
    {
        OP(RTS, IMPLIED),
        .op_code = 96,
        .size = 1,
        .cycles = 6,
        .input_regs = 0,
        .output_regs = 0,
    },

    // SBC
    {
        OP(SBC, IMMEDIATE),
        .op_code = 233,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A | REGF_C,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SBC, ZERO_PAGE),
        .op_code = 229,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SBC, ZERO_PAGE_X),
        .op_code = 245,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SBC, ABSOLUTE),
        .op_code = 237,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SBC, ABSOLUTE_X),
        .op_code = 253,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SBC, ABSOLUTE_Y),
        .op_code = 249,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SBC, INDIRECT_X),
        .op_code = 225,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SBC, INDIRECT_Y),
        .op_code = 241,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },

    // SEC
    {
        OP(SEC, IMPLIED),
        .op_code = 56,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_C,
    },

    // SED
    {
        OP(SED, IMPLIED),
        .op_code = 248,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
    },

    // SEI
    {
        OP(SEI, IMPLIED),
        .op_code = 120,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
    },

    // STA
    {
        OP(STA, ZERO_PAGE),
        .op_code = 133,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A,
        .output_regs = REGF_M,
    },
    {
        OP(STA, ZERO_PAGE_X),
        .op_code = 149,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
    },
    {
        OP(STA, ABSOLUTE),
        .op_code = 141,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A,
        .output_regs = REGF_M,
    },
    {
        OP(STA, ABSOLUTE_X),
        .op_code = 157,
        .size = 3,
        .cycles = 5,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
    },
    {
        OP(STA, ABSOLUTE_Y),
        .op_code = 153,
        .size = 3,
        .cycles = 5,
        .input_regs = REGF_A | REGF_Y,
        .output_regs = REGF_M,
    },
    {
        OP(STA, INDIRECT_X),
        .op_code = 129,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
    },
    {
        OP(STA, INDIRECT_Y),
        .op_code = 145,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_Y,
        .output_regs = REGF_M,
    },

    // STX
    {
        OP(STX, ZERO_PAGE),
        .op_code = 134,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_X,
        .output_regs = REGF_M,
    },
    {
        OP(STX, ZERO_PAGE_Y),
        .op_code = 150,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_X | REGF_Y,
        .output_regs = REGF_M,
    },
    {
        OP(STX, ABSOLUTE),
        .op_code = 142,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_X,
        .output_regs = REGF_M,
    },

    // STY
    {
        OP(STY, ZERO_PAGE),
        .op_code = 132,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_Y,
        .output_regs = REGF_M,
    },
    {
        OP(STY, ZERO_PAGE_X),
        .op_code = 148,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_Y | REGF_X,
        .output_regs = REGF_M,
    },
    {
        OP(STY, ABSOLUTE),
        .op_code = 140,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_Y,
        .output_regs = REGF_M,
    },

    // TAX
    {
        OP(TAX, IMPLIED),
        .op_code = 170,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_X,
    },

    // TAY
    {
        OP(TAY, IMPLIED),
        .op_code = 168,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_Y,
    },

    // TSX
    {
        OP(TSX, IMPLIED),
        .op_code = 186,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_NZ | REGF_X,
    },

    // TXA
    {
        OP(TXA, IMPLIED),
        .op_code = 138,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_X,
        .output_regs = REGF_NZ | REGF_A,
    },

    // TXS
    {
        OP(TXS, IMPLIED),
        .op_code = 154,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_X,
        .output_regs = 0,
    },

    // TYA
    {
        OP(TYA, IMPLIED),
        .op_code = 152,
        .size = 1,
        .cycles = 2,
        .input_regs = REGF_Y,
        .output_regs = REGF_NZ | REGF_A,
    },

    /////////////
    // ILLEGAL //
    /////////////

    // LAX
    {
        OP(LAX, ZERO_PAGE),
        .op_code = 0xA7,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_X,
    },
    {
        OP(LAX, ZERO_PAGE_Y),
        .op_code = 0xB7,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_X,
    },
    {
        OP(LAX, ABSOLUTE),
        .op_code = 0xAF,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_X,
    },
    {
        OP(LAX, ABSOLUTE_Y),
        .op_code = 0xBF,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_X,
    },
    {
        OP(LAX, INDIRECT_X),
        .op_code = 0xA3,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_X,
    },
    {
        OP(LAX, INDIRECT_Y),
        .op_code = 0xB3,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_X,
    },

    // AXS
    {
        OP(AXS, IMMEDIATE),
        .op_code = 0xCB,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_NZ | REGF_X | REGF_C,
    },

    // ANC
    {
        OP(ANC, IMMEDIATE),
        .op_code = 0x0B,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },

    // ALR
    {
        OP(ALR, IMMEDIATE),
        .op_code = 0x4B,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },

    // ARR
    {
        OP(ARR, IMMEDIATE),
        .op_code = 0x6B,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A,
        .output_regs = REGF_NZ | REGF_A | REGF_C,
    },

    // SAX
    {
        OP(SAX, ZERO_PAGE),
        .op_code = 0x87,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
    },
    {
        OP(SAX, ZERO_PAGE_Y),
        .op_code = 0x97,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X | REG_Y,
        .output_regs = REGF_M,
    },
    {
        OP(SAX, ABSOLUTE),
        .op_code = 0x8F,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
    },
    {
        OP(SAX, INDIRECT_X),
        .op_code = 0x83,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
    },
    // SKB
    { 
        OP(SKB, IMPLIED), // A 1-byte jmp
        .op_code = 0x80,
        .size = 1,
        .cycles = 2,
        .flags = ASMF_JUMP,
    },
    // IGN
    { 
        OP(IGN, IMPLIED), // A 2-byte jmp
        .op_code = 0x0C,
        .size = 1,
        .cycles = 4,
        .flags = ASMF_JUMP,
    },
};

#undef OP
