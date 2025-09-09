#ifndef ASM_PROC_HPP
#define ASM_PROC_HPP

#include <ostream>
#include <list>
#include <vector>
#include <exception>

#include "robin/map.hpp"

#include "asm.hpp"
#include "decl.hpp"
#include "rom_decl.hpp"
#include "locator.hpp"
#include "ssa_op.hpp"
#include "span.hpp"

// A single assembly instruction.
struct asm_inst_t
{
    op_t op;
    ssa_op_t ssa_op; // Which op generated this instruction. (Useful for debugging)
    int iasm_child = -1;
    locator_t arg;

    // 'alt' typically holds the hi part of a pointer.
    // It's always used for indirect addressing modes. 
    // Absolute X/Y modes can use if desired.
    //
    // Alternatively, 'alt' can be used to temporarily track additional information.
    locator_t alt;

#ifndef NDEBUG
    int cost; // another debugging tool. TODO: remove this
#endif

    bool operator==(asm_inst_t const& o) const
        { return op == o.op && arg == o.arg && alt == o.alt; }
    bool operator!=(asm_inst_t const& o) const
        { return !operator==(o); }

    void prune(op_t replacement = ASM_PRUNED)
    {
        op = replacement;
        arg = alt = {};
    }

    bool has_alt() const { return !!alt; }
};

inline void push_byte(std::vector<asm_inst_t>& vec, std::uint8_t data)
{
    vec.push_back({ .op = ASM_DATA, .arg = locator_t::const_byte(data) });
}

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

inline asm_inst_t* prev_inst(asm_inst_t* begin, asm_inst_t* end, asm_inst_t* inst)
{
    for(--inst; inst >= begin; --inst)
        if(inst->op != ASM_PRUNED)
            return inst;
    return nullptr;
}

inline asm_inst_t* next_inst(asm_inst_t* code, asm_inst_t* end, asm_inst_t* inst) 
{
    for(++inst; inst < end; ++inst)
        if(inst->op != ASM_PRUNED)
            return inst;
    return nullptr;
}

template<typename Fn>
void for_each_peephole(asm_inst_t* begin, asm_inst_t* end, Fn const& fn)
{
    if(begin == end)
        return;

    asm_inst_t* a, *b, *c;

    auto const next_inst = [&](asm_inst_t* inst) { return ::next_inst(begin, end, inst); };

    a = begin;
    if(op_size(a->op) == 0)
        if(!(a = next_inst(a)))
            return;

    b = next_inst(a);

    while(b)
    {
        c = next_inst(b);

        fn(*a, *b, c);

        a = b;
        b = c;
    }
}

bool o_redundant_loads(asm_inst_t* begin, asm_inst_t* end);
bool o_peephole(asm_inst_t* begin, asm_inst_t* end);

// A relocatable sequence of assembly instructions, 
// used after code generation but still amenable to code optimizations.
class asm_proc_t
{
public:
    struct label_info_t
    {
        std::uint32_t index; // Position in 'code'
        std::int64_t offset; // Bytes from start
    };

    asm_proc_t() = default;
    asm_proc_t(fn_ht fn, std::vector<asm_inst_t> code, locator_t entry_label);

    fn_ht fn = {};
    unsigned cached_size = 0;
    locator_t entry_label = {};
    std::vector<asm_inst_t> code;
    std::vector<pstring_t> pstrings;
    rh::batman_map<locator_t, label_info_t> labels;

    // Adds 'inst' to 'code':
    void push_inst(asm_inst_t inst);
    void push_inst(op_t op, locator_t arg = {}) { assert(op); push_inst({ .op = op, .arg = arg }); }
    void push_inst(op_name_t op_name) { push_inst(get_op(op_name, MODE_IMPLIED)); }
    void push_inst(op_name_t op_name, std::uint8_t i) { push_inst(get_op(op_name, MODE_IMMEDIATE), locator_t::const_byte(i)); }
    locator_t push_label(unsigned id) { auto l = make_label(id); push_inst(ASM_LABEL, l); return l; }
    locator_t make_label(unsigned id) const { return locator_t::minor_label(id); }
    unsigned next_label_id() const;

    asm_inst_t* prev_inst(int i);
    asm_inst_t* next_inst(int i);

    void rebuild_label_map();   // Sets 'index' of each label_info, not 'offset'.
    void build_label_offsets(); // Sets 'offset'.

    void initial_optimize() { optimize(true); }
    void late_optimize() { optimize(false); }

    // Converts absolute instructions to zp, when appropriate
    void absolute_to_zp();

    // Replaces banked JSR with regular JSR
    bool remove_banked_jsr(romv_t romv, int bank);

    // Number of bytes between two instruction indexes.
    int bytes_between(unsigned ai, unsigned bi) const;

    std::size_t size() const { return bytes_between(0, code.size()); }

    loc_vec_t loc_vec() const;
    void write_assembly(std::ostream& os, romv_t romv) const;
    void write_bytes(std::uint8_t* const start, romv_t romv, int bank) const;

    // Replaces some locators with linked ones.
    void link(romv_t romv, int bank = -1);
    void link_variables(romv_t romv = ROMV_MODE);

    // Replaces labels with constant addresses.
    void relocate(locator_t from);

    // Checks if ZP address modes are valid.
    void verify_addr_modes();

    // Warn if illegal instructions are used.
    void verify_legal();

    label_info_t const* lookup_label(locator_t loc) const { return labels.mapped(loc.mem_head()); }
    label_info_t* lookup_label(locator_t loc) { return labels.mapped(loc.mem_head()); }
    label_info_t& get_label(locator_t loc) { return labels[loc.mem_head()]; }

    unsigned add_pstring(pstring_t pstring);
    void append(asm_proc_t const& proc);

    void cache_size() { cached_size = size(); }
private:
    template<typename Fn>
    void for_each_inst(Fn const& fn) const;

    template<typename Fn>
    void for_each_locator(Fn const& fn) const;

    void process_inst(asm_inst_t const& inst);

    void optimize(bool initial);

    // Converts very short jumps to SKB or IGN ops.
    void optimize_short_jumps(bool use_nops);

    // Converts invalid relative branches into long branches.
    void convert_long_branch_ops();

    // Converts JSR_INDIRECT to JMP_INDIRECT plus JSR_ABSOLUTE
    void convert_indirect_jsr();

    // Removes NOP instructions
    void prune_nops();

    pstring_t get_pstring(asm_inst_t const& inst);
};

std::ostream& operator<<(std::ostream& o, asm_inst_t const& inst);

struct relocate_error_t : public std::exception
{
    explicit relocate_error_t(std::string const& msg)
    : msg(msg) {}

    virtual const char* what() const noexcept { return msg.c_str(); }
    std::string msg;
};

bool live_peephole(regs_t live_out, asm_inst_t* code, std::size_t size, log_t* log = nullptr);

#endif
