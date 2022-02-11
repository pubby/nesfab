#ifndef ASM_CODE_HPP
#define ASM_CODE_HPP

#include <list>
#include <vector>

#include "robin/map.hpp"

#include "locator.hpp"
#include "ssa_op.hpp"

struct asm_inst_t
{
    op_t op;
    ssa_op_t ssa_op; // Which op generated this instruction. (Useful for debugging)
    locator_t arg;
};

// A relocatable sequence of assembly instructions, 
// used after code generation but still amenable to code optimizations.
struct asm_proc_t
{
    std::vector<asm_inst_t> code;
    rh::robin_map<locator_t, unsigned> labels;
    std::list<unsigned> relative_branches;
    std::list<unsigned> absolute_jmps;

    // Adds 'inst' to 'code':
    void push_inst(asm_inst_t inst);

    // Converts invalid relative branches into long branches.
    void expand_branch_ops();

    // Converts very short jumps to SKB or IGN ops.
    void nopify_short_jumps();

    // Number of bytes between two instruction indexes.
    int bytes_between(unsigned ai, unsigned bi) const;
};

#endif
