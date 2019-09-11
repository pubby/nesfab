#ifndef ASM_HPP
#define ASM_HPP

#include <vector>

#include <boost/container/small_vector.hpp>

#include "asm_tables.hpp"

namespace bc = boost::container;

enum addr_mode_t
{
#define ADDR_MODE(name) MODE_##name,
#include "addr_mode.inc"
#undef ADDR_MODE
};

struct instruction_t
{
    asm_op_t op;
    unsigned value;

    constexpr unsigned cycles()
    {
        return asm_cycles_table[(unsigned)op];
    }

    constexpr unsigned code_size()
    {
        return asm_size_table[(unsigned)op];
    }
};

class asm_block_t // A basic block
{
public:
    unsigned cycles() const;
    unsigned code_size() const;

    std::vector<instruction_t> instructions;
};

class asm_fn_t
{
public:
    void to_file(FILE* fp) const;

    std::vector<asm_block_t> blocks;
    bc::flat_map<unsigned, unsigned> block_map;
};

#endif
