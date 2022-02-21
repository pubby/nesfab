#ifndef ASM_PROC_HPP
#define ASM_PROC_HPP

#include <ostream>
#include <list>
#include <vector>

#include "robin/map.hpp"

#include "asm.hpp"
#include "decl.hpp"
#include "locator.hpp"
#include "ssa_op.hpp"

// A single assembly instruction.
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
    rh::robin_map<locator_t, unsigned> mem_usage; // Counts how many times locators are mentioned.
    rh::robin_map<locator_t, unsigned> labels;

    // Adds 'inst' to 'code':
    void push_inst(asm_inst_t inst);

    // Converts invalid relative branches into long branches.
    void expand_branch_ops();

    // Converts very short jumps to SKB or IGN ops.
    void nopify_short_jumps();

    // Converts identifier-based labels to relocatable ones.
    void make_relocatable();

    // Number of bytes between two instruction indexes.
    int bytes_between(unsigned ai, unsigned bi) const;

    std::size_t size_in_bytes() const { return bytes_between(0, code.size()); }

    void write_assembly(std::ostream& os, fn_t const& fn) const;
    void write_binary(std::uint8_t* rom, addr16_t start_addr) const;
};

std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst);

#endif
