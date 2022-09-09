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
    int iasm_stmt = -1;
    locator_t arg;

    // 'alt' typically holds the hi part of a pointer.
    // It's always used for indirect addressing modes. 
    // Absolute X/Y modes can use if desired.
    //
    // Alternatively, 'alt' can be used to temporarily track additional information.
    locator_t alt;

    //int cost; // another debugging tool. TODO: remove this

    bool operator==(asm_inst_t const& o) const
        { return op == o.op && arg == o.arg && alt == o.alt; }
    bool operator!=(asm_inst_t const& o) const
        { return !operator==(o); }
};

bool is_return(asm_inst_t const& inst);
bool mem_inst(asm_inst_t const& inst);

template<typename It>
unsigned size_in_bytes(It begin, It end)
{
    unsigned size = 0;
    for(It it = begin; it != end; ++it)
        size += op_size(it->op);
    return size;
}

// A relocatable sequence of assembly instructions, 
// used after code generation but still amenable to code optimizations.
struct asm_proc_t
{
    struct label_info_t
    {
        unsigned index; // Position in 'code'
        int offset; // Bytes from start
    };

    asm_proc_t() = default;
    asm_proc_t(fn_ht fn, std::vector<asm_inst_t> code, locator_t entry_label);

    fn_ht fn = {};
    locator_t entry_label = {};
    std::vector<asm_inst_t> code;
    rh::batman_map<locator_t, label_info_t> labels;

    // Adds 'inst' to 'code':
    void push_inst(asm_inst_t inst);
    void push_inst(op_t op, locator_t arg = {}) { assert(op); push_inst({ .op = op, .arg = arg }); }
    void push_inst(op_name_t op_name) { push_inst(get_op(op_name, MODE_IMPLIED)); }
    void push_inst(op_name_t op_name, std::uint8_t i) { push_inst(get_op(op_name, MODE_IMMEDIATE), locator_t::const_byte(i)); }
    locator_t push_label(unsigned id) { auto l = make_label(id); push_inst(ASM_LABEL, l); return l; }
    locator_t make_label(unsigned id) const { return locator_t::minor_label(id); }

    asm_inst_t* prev_inst(int i);
    asm_inst_t* next_inst(int i);

    void rebuild_label_map();   // Sets 'index' of each label_info, not 'offset'.
    void build_label_offsets(); // Sets 'offset'.

    void initial_optimize();

    // Converts identifier-based labels to relocatable ones.
    //void make_relocatable();

    // Number of bytes between two instruction indexes.
    int bytes_between(unsigned ai, unsigned bi) const;

    std::size_t size() const { return bytes_between(0, code.size()); }

    void write_assembly(std::ostream& os, romv_t romv) const;
    void write_bytes(std::uint8_t* const start, romv_t romv, int bank) const;

    // Replaces some locators with linked ones, then optimizes.
    void link(romv_t romv, int bank = -1);

    // Replaces labels with constant addresses.
    void relocate(std::uint16_t addr);

    label_info_t const* lookup_label(locator_t loc) const { return labels.mapped(loc.mem_head()); }
    label_info_t* lookup_label(locator_t loc) { return labels.mapped(loc.mem_head()); }
    label_info_t& get_label(locator_t loc) { return labels[loc.mem_head()]; }
private:

    void process_inst(asm_inst_t const& inst);

    void optimize(bool initial);

    // Converts absolute instructions to zp, when appropriate
    void absolute_to_zp();

    // Converts very short jumps to SKB or IGN ops.
    void optimize_short_jumps(bool use_nops);

    // Converts invalid relative branches into long branches.
    void convert_long_branch_ops();

    // Removes NOP instructions
    void prune_nops();
};

std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst);

#endif
