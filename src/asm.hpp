#ifndef ASM_HPP
#define ASM_HPP

#include <cstdint>
#include <vector>

#include "flat/vector_map.hpp"

#include "asm_tables.hpp"

namespace bc = boost::container;

enum addr_mode_t
{
#define ADDR_MODE(name) MODE_##name,
#include "addr_mode.inc"
#undef ADDR_MODE
};

struct mode_pair_t
{
    addr_mode_t mode;
    std::uint32_t value;
};

struct instruction_t
{
    asm_op_t op;
    std::uint32_t value;

    constexpr unsigned cycles() const
    {
        return asm_cycles_table[(unsigned)op];
    }

    constexpr unsigned code_size() const
    {
        return asm_size_table[(unsigned)op];
    }
};

instruction_t to_instruction(mode_pair_t mp, asm_op_name_t op_name)
{
    return TODO;
}

class asm_block_t // A basic block
{
public:
    unsigned cycles() const;
    unsigned code_size() const;

    std::vector<instruction_t> code;
};

class asm_fn_t
{
public:
    void to_file(FILE* fp) const;

    std::vector<asm_block_t> blocks;
    fc::vector_map<unsigned, unsigned> block_map;
};

#endif
