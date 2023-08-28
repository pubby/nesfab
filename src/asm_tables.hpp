#define OP(name, mode) .op=name##_##mode, .op_name=name, .addr_mode=MODE_##mode

constexpr unsigned MAYBE_SIZE = 3;
constexpr unsigned MAYBE_CYCLES = 1;

constexpr unsigned LIKELY_SIZE = 3;
constexpr unsigned LIKELY_CYCLES = 4;

constexpr op_def_t op_defs_table[NUM_NORMAL_OPS] =
{
    { .op = BAD_OP, .flags = ASMF_FAKE },
    { .op = ASM_LABEL, .flags = ASMF_FAKE },
    { .op = ASM_PRUNED, .flags = ASMF_FAKE },
    { .op = ASM_LOOP_DEPTH, .flags = ASMF_FAKE },
    { .op = ASM_FENCE, .flags = ASMF_FAKE | ASMF_FENCE, },
    { .op = ASM_DATA, .size = 1, .flags = ASMF_FAKE },
    { 
        .op = ASM_DELAY,
        .cycles = 255,
        .flags = ASMF_FAKE,
    },
    { 
        .op = MAYBE_STORE_C,
        .addr_mode = MODE_ABSOLUTE,
        .size = 10, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_C,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    { 
        .op = STORE_C_ABSOLUTE,
        .addr_mode = MODE_ABSOLUTE,
        .size = 10, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_C,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE,
    },
    { 
        .op = MAYBE_STORE_C_FAST,
        .addr_mode = MODE_ABSOLUTE,
        .size = 6, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_C,
        .output_regs = REGF_M | REGF_A | REGF_N | REGF_Z | REGF_C,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    { 
        .op = STORE_C_ABSOLUTE_FAST,
        .addr_mode = MODE_ABSOLUTE,
        .size = 6, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_C,
        .output_regs = REGF_M | REGF_A | REGF_N | REGF_Z | REGF_C,
        .flags = ASMF_FAKE,
    },
    { 
        .op = MAYBE_STORE_Z,
        .addr_mode = MODE_ABSOLUTE,
        .size = 11, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_Z,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    { 
        .op = STORE_Z_ABSOLUTE,
        .addr_mode = MODE_ABSOLUTE,
        .size = 11, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_Z,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE,
    },
    { 
        .op = MAYBE_STORE_Z_FAST,
        .addr_mode = MODE_ABSOLUTE,
        .size = 7, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_Z,
        .output_regs = REGF_M | REGF_A | REGF_N | REGF_Z | REGF_C,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    { 
        .op = STORE_Z_ABSOLUTE_FAST,
        .addr_mode = MODE_ABSOLUTE,
        .size = 7, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_Z,
        .output_regs = REGF_M | REGF_A | REGF_N | REGF_Z | REGF_C,
        .flags = ASMF_FAKE,
    },
    { 
        .op = MAYBE_STORE_N,
        .addr_mode = MODE_ABSOLUTE,
        .size = 12, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_N,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    { 
        .op = STORE_N_ABSOLUTE,
        .addr_mode = MODE_ABSOLUTE,
        .size = 12, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_N,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE,
    },
    { 
        .op = MAYBE_STORE_N_FAST,
        .addr_mode = MODE_ABSOLUTE,
        .size = 8, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_N,
        .output_regs = REGF_M | REGF_A | REGF_N | REGF_Z | REGF_C,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    { 
        .op = STORE_N_ABSOLUTE_FAST,
        .addr_mode = MODE_ABSOLUTE,
        .size = 8, // Keep in sync with 'asm_proc.cpp'
        .cycles = MAYBE_CYCLES, // Arbitrary
        .input_regs = REGF_N,
        .output_regs = REGF_M | REGF_A | REGF_N | REGF_Z | REGF_C,
        .flags = ASMF_FAKE,
    },
    {
        .op = BANKED_X_JSR,
        .addr_mode = MODE_ABSOLUTE,
        .size = 3 + (2 * 2),
        .cycles = 6 + (2 * 2),
        .input_regs = REGF_X,
        .output_regs = REGF_Y | REGF_A, // Always clobbers these.
        .flags = ASMF_FAKE | ASMF_CALL,
    },
    {
        .op = BANKED_Y_JSR,
        .addr_mode = MODE_ABSOLUTE,
        .size = 3 + (2 * 2),
        .cycles = 6 + (2 * 2),
        .input_regs = REGF_Y,
        .output_regs = REGF_X | REGF_A, // Always clobbers these.
        .flags = ASMF_FAKE | ASMF_CALL,
    },
    {
        .op = BANKED_JSR,
        .addr_mode = MODE_ABSOLUTE,
        .size = 3 + (2 * 2),
        .cycles = 6 + (2 * 2),
        .input_regs = 0,
        .output_regs = REGF_X | REGF_Y | REGF_A, // Always clobbers these.
        .flags = ASMF_FAKE | ASMF_CALL,
    },
    {
        .op = BANKED_X_JMP,
        .addr_mode = MODE_ABSOLUTE,
        .size = 3 + (2 * 2),
        .cycles = 3 + (2 * 2),
        .input_regs = REGF_X,
        .output_regs = REGF_Y | REGF_A, // Always clobbers these.
        .flags = ASMF_FAKE | ASMF_JUMP,
    },
    {
        .op = BANKED_Y_JMP,
        .addr_mode = MODE_ABSOLUTE,
        .size = 3 + (2 * 2),
        .cycles = 3 + (2 * 2),
        .input_regs = REGF_Y,
        .output_regs = REGF_X | REGF_A, // Always clobbers these.
        .flags = ASMF_FAKE | ASMF_JUMP,
    },
    {
        .op = BANKED_JMP,
        .addr_mode = MODE_ABSOLUTE,
        .size = 3 + (2 * 2),
        .cycles = 3 + (2 * 2),
        .input_regs = REGF_Y,
        .output_regs = REGF_X | REGF_Y | REGF_A, // Always clobbers these.
        .flags = ASMF_FAKE | ASMF_JUMP,
    },
    {
        .op = ASM_X_SWITCH,
        .size = 3+1+3+1+1,
        .cycles = 4+3+4+3+6,
        .input_regs = REGF_X,
        .output_regs = REGF_A | REGF_N | REGF_Z, // clobbers
        .flags = ASMF_FAKE | ASMF_JUMP | ASMF_SWITCH,
    },
    {
        .op = ASM_Y_SWITCH,
        .size = 3+1+3+1+1,
        .cycles = 4+3+4+3+6,
        .input_regs = REGF_Y,
        .output_regs = REGF_A | REGF_N | REGF_Z, // clobbers
        .flags = ASMF_FAKE | ASMF_JUMP | ASMF_SWITCH,
    },

    // ADC
    {
        OP(ADC, IMMEDIATE),
        .op_code = 105,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A | REGF_C,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(ADC, ZERO_PAGE),
        .op_code = 101,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(ADC, ZERO_PAGE_X),
        .op_code = 117,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(ADC, ABSOLUTE),
        .op_code = 109,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(ADC, ABSOLUTE_X),
        .op_code = 125,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(ADC, ABSOLUTE_Y),
        .op_code = 121,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(ADC, INDIRECT_X),
        .op_code = 97,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(ADC, INDIRECT_Y),
        .op_code = 113,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
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
        .flags = ASMF_BRANCH
    },
    {
        OP(BCC, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_C,
        .output_regs = 0,
        .flags = ASMF_BRANCH
    },

    // BCS
    {
        OP(BCS, RELATIVE),
        .op_code = 176,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_C,
        .output_regs = 0,
        .flags = ASMF_BRANCH
    },
    {
        OP(BCS, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_C,
        .output_regs = 0,
        .flags = ASMF_BRANCH
    },

    // BEQ
    {
        OP(BEQ, RELATIVE),
        .op_code = 240,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_Z,
        .output_regs = 0,
        .flags = ASMF_BRANCH
    },
    {
        OP(BEQ, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_Z,
        .output_regs = 0,
        .flags = ASMF_BRANCH
    },

    // BIT
    {
        OP(BIT, ZERO_PAGE),
        .op_code = 36,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_V,
    },
    {
        OP(BIT, ABSOLUTE),
        .op_code = 44,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_V,
    },

    // BMI
    {
        OP(BMI, RELATIVE),
        .op_code = 48,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_N,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },
    {
        OP(BMI, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_N,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },

    // BNE
    {
        OP(BNE, RELATIVE),
        .op_code = 208,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_Z,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },
    {
        OP(BNE, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_Z,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },

    // BPL
    {
        OP(BPL, RELATIVE),
        .op_code = 16,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_N,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },
    {
        OP(BPL, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_N,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
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
        .flags = ASMF_BRANCH,
    },
    {
        OP(BVC, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },

    // BVS
    {
        OP(BVS, RELATIVE),
        .op_code = 112,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_V,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },
    {
        OP(BVS, LONG),
        .size = 5,
        .cycles = 5,
        .input_regs = REGF_V,
        .output_regs = 0,
        .flags = ASMF_BRANCH,
    },

    // CLC
    {
        OP(CLC, IMPLIED),
        .op_code = 24,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_C,
        .flags = ASMF_IMPURE,
    },

    // CLD
    {
        OP(CLD, IMPLIED),
        .op_code = 216,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_IMPURE,
    },

    // CLI
    {
        OP(CLI, IMPLIED),
        .op_code = 88,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_IMPURE | ASMF_FENCE,
    },

    // CLV
    {
        OP(CLV, IMPLIED),
        .op_code = 184,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_V,
        .flags = ASMF_IMPURE,
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
        .input_regs = REGF_Y,
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
        .flags = ASMF_CALL,
    },
    {
        OP(JSR, INDIRECT), // Fake! Get converted to JMP_INDIRECT + JSR_ABSOLUTE
        .size = 3,
        .cycles = 11,
        .input_regs = REGF_M,
        .output_regs = 0,
        .flags = ASMF_CALL,
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
        .output_regs = REGF_M,
    },

    // PHP
    {
        OP(PHP, IMPLIED),
        .op_code = 8,
        .size = 1,
        .cycles = 3,
        .input_regs = REGF_6502,
        .output_regs = REGF_M,
    },

    // PLA
    {
        OP(PLA, IMPLIED),
        .op_code = 104,
        .size = 1,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_NZ | REGF_A,
    },

    // PLP
    {
        OP(PLP, IMPLIED),
        .op_code = 40,
        .size = 1,
        .cycles = 4,
        .input_regs = REGF_M,
        .output_regs = REGF_6502,
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
        .input_regs = REGF_M,
        .output_regs = REGF_6502,
        .flags = ASMF_RETURN,
    },

    // RTS
    {
        OP(RTS, IMPLIED),
        .op_code = 96,
        .size = 1,
        .cycles = 6,
        .input_regs = REGF_M,
        .output_regs = 0,
        .flags = ASMF_RETURN,
    },

    // SBC
    {
        OP(SBC, IMMEDIATE),
        .op_code = 233,
        .size = 2,
        .cycles = 2,
        .input_regs = REGF_A | REGF_C,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(SBC, ZERO_PAGE),
        .op_code = 229,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(SBC, ZERO_PAGE_X),
        .op_code = 245,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(SBC, ABSOLUTE),
        .op_code = 237,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(SBC, ABSOLUTE_X),
        .op_code = 253,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(SBC, ABSOLUTE_Y),
        .op_code = 249,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(SBC, INDIRECT_X),
        .op_code = 225,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_C | REGF_X | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },
    {
        OP(SBC, INDIRECT_Y),
        .op_code = 241,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_A | REGF_C | REGF_Y | REGF_M,
        .output_regs = REGF_NZ | REGF_A | REGF_C | REGF_V,
    },

    // SEC
    {
        OP(SEC, IMPLIED),
        .op_code = 56,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = REGF_C,
        .flags = ASMF_IMPURE,
    },

    // SED
    {
        OP(SED, IMPLIED),
        .op_code = 248,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_IMPURE,
    },

    // SEI
    {
        OP(SEI, IMPLIED),
        .op_code = 120,
        .size = 1,
        .cycles = 2,
        .input_regs = 0,
        .output_regs = 0,
        .flags = ASMF_IMPURE | ASMF_FENCE,
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
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_M,
    },
    {
        OP(STA, INDIRECT_Y),
        .op_code = 145,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_A | REGF_Y | REGF_M,
        .output_regs = REGF_M,
    },
    {
        OP(STA, MAYBE),
        .size = MAYBE_SIZE,
        .cycles = MAYBE_CYCLES,
        .input_regs = REGF_A,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    {
        OP(STA, LIKELY),
        .size = LIKELY_SIZE,
        .cycles = LIKELY_CYCLES,
        .input_regs = REGF_A,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
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
    {
        OP(STX, MAYBE),
        .size = MAYBE_SIZE,
        .cycles = MAYBE_CYCLES,
        .input_regs = REGF_X,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    {
        OP(STX, LIKELY),
        .size = LIKELY_SIZE,
        .cycles = LIKELY_CYCLES,
        .input_regs = REGF_X,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
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
    {
        OP(STY, MAYBE),
        .size = MAYBE_SIZE,
        .cycles = MAYBE_CYCLES,
        .input_regs = REGF_Y,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    {
        OP(STY, LIKELY),
        .size = LIKELY_SIZE,
        .cycles = LIKELY_CYCLES,
        .input_regs = REGF_Y,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
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
        .input_regs = REGF_M,
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
        .output_regs = REGF_M,
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
        .input_regs = REGF_A | REGF_X | REGF_M,
        .output_regs = REGF_M,
    },
    {
        OP(SAX, MAYBE),
        .size = MAYBE_SIZE,
        .cycles = MAYBE_CYCLES,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },
    {
        OP(SAX, LIKELY),
        .size = LIKELY_SIZE,
        .cycles = LIKELY_CYCLES,
        .input_regs = REGF_A | REGF_X,
        .output_regs = REGF_M,
        .flags = ASMF_FAKE | ASMF_MAYBE_STORE,
    },

    // SKB
    { 
        OP(SKB, IMPLIED), // A 1-byte jmp
        .op_code = 0x80,
        .size = 1,
        .cycles = 2,
        .flags = ASMF_JUMP,
    },
    { 
        OP(SKB, IMMEDIATE),
        .op_code = 0x80,
        .size = 2,
        .cycles = 2,
    },

    // IGN
    { 
        OP(IGN, IMPLIED), // A 2-byte jmp
        .op_code = 0x0C,
        .size = 1,
        .cycles = 4,
        .flags = ASMF_JUMP,
    },
    { 
        OP(IGN, ZERO_PAGE),
        .op_code = 0x04,
        .size = 2,
        .cycles = 3,
        .input_regs = REGF_M,
    },
    { 
        OP(IGN, ZERO_PAGE_X),
        .op_code = 0x14,
        .size = 2,
        .cycles = 4,
        .input_regs = REGF_M | REGF_X,
    },
    { 
        OP(IGN, ABSOLUTE),
        .op_code = 0x0C,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M,
    },
    { 
        OP(IGN, ABSOLUTE_X),
        .op_code = 0x1C,
        .size = 3,
        .cycles = 4,
        .input_regs = REGF_M | REGF_X,
    },

    // DCP
    {
        OP(DCP, ZERO_PAGE),
        .op_code = 0xC7,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M | REGF_A,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(DCP, ZERO_PAGE_X),
        .op_code = 0xD7,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(DCP, ABSOLUTE),
        .op_code = 0xCF,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(DCP, ABSOLUTE_X),
        .op_code = 0xDF,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(DCP, ABSOLUTE_Y),
        .op_code = 0xDB,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_Y,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(DCP, INDIRECT_X),
        .op_code = 0xC3,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(DCP, INDIRECT_Y),
        .op_code = 0xD3,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_Y,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },

    // ISC
    {
        OP(ISC, ZERO_PAGE),
        .op_code = 0xE7,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M | REGF_A | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ISC, ZERO_PAGE_X),
        .op_code = 0xF7,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ISC, ABSOLUTE),
        .op_code = 0xEF,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ISC, ABSOLUTE_X),
        .op_code = 0xFF,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ISC, ABSOLUTE_Y),
        .op_code = 0xFB,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_Y | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ISC, INDIRECT_X),
        .op_code = 0xE3,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(ISC, INDIRECT_Y),
        .op_code = 0xF3,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_Y | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },

    // RLA
    {
        OP(RLA, ZERO_PAGE),
        .op_code = 0x27,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M | REGF_A | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RLA, ZERO_PAGE_X),
        .op_code = 0x37,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RLA, ABSOLUTE),
        .op_code = 0x2F,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RLA, ABSOLUTE_X),
        .op_code = 0x3F,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RLA, ABSOLUTE_Y),
        .op_code = 0x3B,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_Y | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RLA, INDIRECT_X),
        .op_code = 0x23,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RLA, INDIRECT_Y),
        .op_code = 0x33,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_Y | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },

    // RRA
    {
        OP(RRA, ZERO_PAGE),
        .op_code = 0x67,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M | REGF_A | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RRA, ZERO_PAGE_X),
        .op_code = 0x77,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RRA, ABSOLUTE),
        .op_code = 0x6F,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RRA, ABSOLUTE_X),
        .op_code = 0x7F,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RRA, ABSOLUTE_Y),
        .op_code = 0x7B,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_Y | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RRA, INDIRECT_X),
        .op_code = 0x63,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_X | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(RRA, INDIRECT_Y),
        .op_code = 0x73,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_Y | REGF_C,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },

    // SLO
    {
        OP(SLO, ZERO_PAGE),
        .op_code = 0x07,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M | REGF_A,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SLO, ZERO_PAGE_X),
        .op_code = 0x17,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SLO, ABSOLUTE),
        .op_code = 0x0F,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SLO, ABSOLUTE_X),
        .op_code = 0x1F,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SLO, ABSOLUTE_Y),
        .op_code = 0x1B,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_Y,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SLO, INDIRECT_X),
        .op_code = 0x03,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SLO, INDIRECT_Y),
        .op_code = 0x13,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_Y,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },

    // SRE
    {
        OP(SRE, ZERO_PAGE),
        .op_code = 0x47,
        .size = 2,
        .cycles = 5,
        .input_regs = REGF_M | REGF_A,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SRE, ZERO_PAGE_X),
        .op_code = 0x57,
        .size = 2,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SRE, ABSOLUTE),
        .op_code = 0x4F,
        .size = 3,
        .cycles = 6,
        .input_regs = REGF_M | REGF_A,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SRE, ABSOLUTE_X),
        .op_code = 0x5F,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SRE, ABSOLUTE_Y),
        .op_code = 0x5B,
        .size = 3,
        .cycles = 7,
        .input_regs = REGF_M | REGF_A | REGF_Y,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SRE, INDIRECT_X),
        .op_code = 0x43,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_X,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },
    {
        OP(SRE, INDIRECT_Y),
        .op_code = 0x53,
        .size = 2,
        .cycles = 8,
        .input_regs = REGF_M | REGF_A | REGF_Y,
        .output_regs = REGF_M | REGF_NZ | REGF_A | REGF_C,
    },

};

static_assert([]
{
    for(unsigned i = 0; i < NUM_NORMAL_OPS; ++i)
        if(op_defs_table[i].op != op_t(i))
            return false;
    return true;
}());

#undef OP
