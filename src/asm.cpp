#include "asm.hpp"

unsigned asm_block_t::cycles() const
{
    unsigned sum = 0;
    for(instruction_t const& i : instructions)
        sum += i.cycles();
    return sum;
}

unsigned asm_block_t::code_size() const
{
    unsigned sum = 0;
    for(instruction_t const& i : instructions)
        sum += i.code_size();
    return sum;
}
