#ifndef ASM_DEF_HPP
#define ASM_DEF_HPP

#include <boost/preprocessor/stringize.hpp>

enum addr_mode_t
{
#define ADDR_MODE(name) MODE_##name,
#include "addr_mode.inc"
#undef ADDR_MODE
};

inline char const* addr_mode_name(addr_mode_t addr_mode)
{
    switch(addr_mode)
    {
#define ADDR_MODE(name) case MODE_##name: return BOOST_PP_STRINGIZE(name);
#include "addr_mode.inc"
#undef ADDR_MODE
    }
    return nullptr;
}

using regs_t = std::uint8_t;
regs_t REG_A = 1 << 0;
regs_t REG_X = 1 << 1;
regs_t REG_Y = 1 << 2;
regs_t REG_C = 1 << 3;

struct instr_def_t
{
    char const* name;
    char const* opname;
    unsigned opcode;
    char const* addr_mode;
    unsigned size;
    unsigned cycles;
    regs_t implicit_regs;
    regs_t arg_regs;
    regs_t out_regs;
};

#endif
