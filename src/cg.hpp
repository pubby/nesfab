#ifndef CG_HPP
#define CG_HPP

// Code-gen related code.

#include <vector>

#include "flat/small_set.hpp"

#include "asm.hpp"
#include "asm_proc.hpp"
#include "bitset.hpp"
#include "ir.hpp"
#include "debug_print.hpp"

//////////
// data //
//////////

struct cfg_liveness_d
{
    bitset_uint_t* in;
    bitset_uint_t* out; // Also used to hold the 'KILL' set temporarily.
};

struct cfg_order_d
{
    std::vector<unsigned> pheramones;
    std::uint16_t bytes = 0;
    std::uint16_t offset = 0;
};

struct ssa_schedule_d
{
    bitset_uint_t* deps = nullptr;
    ssa_ht carry_user = {};
    int exit_distance = 0;

    unsigned index = 0;
};

struct ssa_isel_d
{
    std::uint64_t store_mask = 0;
    std::uint64_t last_use = 0;
    bool likely_store = false;
};

//

struct cfg_cg_d
{
    cfg_liveness_d live;
    cfg_order_d order;

    std::vector<ssa_ht> schedule;
    std::vector<asm_inst_t> code;
};

struct ssa_cg_d
{
    ssa_value_t cset_head = {}; // Basically a union-find pointer.
    ssa_ht cset_next = {}; // A linked-list to the next node

    // These are used to implement/coalesce indirect pointers.
    // 'ptr_pair' points to the other byte in the zero-page address of a pointer.
    // 'is_ptr_lo' determines if this is the lo or hi byte of said pointer.
    // (These values are defined for cset heads)
    ssa_ht ptr_alt = {}; 
    bool is_ptr_hi = false;
    bool has_ptr(bool is_ptr_hi) const { return ptr_alt && this->is_ptr_hi == is_ptr_hi; }

    ssa_schedule_d schedule;
    ssa_isel_d isel;
};

//

inline cfg_cg_d& cg_data(cfg_ht h) { return h.data<cfg_cg_d>(); }
inline ssa_cg_d& cg_data(ssa_ht h) { return h.data<ssa_cg_d>(); }

inline void cg_data_resize()
{
    cfg_data_pool::resize<cfg_cg_d>(cfg_pool::array_size());
    ssa_data_pool::resize<ssa_cg_d>(ssa_pool::array_size());
}

void code_gen(log_t* log, ir_t& ir, fn_t& fn);

#endif
