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

struct span_t;

// A single assembly instruction.
struct asm_inst_t
{
    op_t op;
    ssa_op_t ssa_op; // Which op generated this instruction. (Useful for debugging)
    locator_t arg;
    locator_t ptr_hi; // Always used for indirect addressing modes. Absolute X/Y modes can use if desired.
    int cost; // another debugging tool
};

// A relocatable sequence of assembly instructions, 
// used after code generation but still amenable to code optimizations.
struct asm_proc_t
{
    fn_ht fn = {};
    std::vector<asm_inst_t> code;
    //rh::robin_map<locator_t, unsigned> mem_usage; // Counts how many times locators are mentioned.
    rh::batman_map<locator_t, unsigned> labels; // Maps from locators to code indices

    // Adds 'inst' to 'code':
    void push_inst(asm_inst_t inst);
    void push_inst(op_t op, locator_t arg = {}) { assert(op); push_inst({ .op = op, .arg = arg }); }
    void push_inst(op_name_t op_name) { push_inst(get_op(op_name, MODE_IMPLIED)); }
    void push_inst(op_name_t op_name, std::uint8_t i) { push_inst(get_op(op_name, MODE_IMMEDIATE), locator_t::const_byte(i)); }
    locator_t push_label(unsigned id) { auto l = make_label(id); push_inst(ASM_LABEL, l); return l; }
    locator_t make_label(unsigned id) const { return locator_t::minor_label(id); }

    void initial_optimize();

    // Converts identifier-based labels to relocatable ones.
    //void make_relocatable();

    // Number of bytes between two instruction indexes.
    int bytes_between(unsigned ai, unsigned bi) const;

    std::size_t size() const { return bytes_between(0, code.size()); }

    void write_assembly(std::ostream& os, fn_ht fn = {}) const;
    void write_bytes(std::uint8_t* const start, int bank) const;

    // Replaces some locators with linked ones, then optimizes.
    void link(int bank = -1);

    // Replaces labels with constant addresses.
    void relocate(std::uint16_t addr);
private:
    void optimize(bool initial);

    // Converts absolute instructions to zp, when appropriate
    void absolute_to_zp();

    // Converts very short jumps to SKB or IGN ops.
    void optimize_short_jumps(bool initial);

    // Converts invalid relative branches into long branches.
    void convert_long_branch_ops();

    // Removes NOP instructions
    void prune_nops();
};

std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst);

#endif
