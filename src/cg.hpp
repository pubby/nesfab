#ifndef CG_HPP
#define CG_HPP

// Code-gen related code.

#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_set.hpp"

#include "asm.hpp"
#include "ir.hpp"

namespace bc = ::boost::container;

///////////
// aop_t //
///////////

// An "abstract instruction"; a convenient form for the code gen to use.
struct ainst_t
{
    op_t op;
    ssa_value_t arg;
};

std::ostream& operator<<(std::ostream& o, ainst_t const& inst);

//////////
// data //
//////////

struct cfg_liveness_d
{
    bitset_uint_t* in;
    bitset_uint_t* out; // Also used to hold the 'KILL' set temporarily.
};

struct ssa_schedule_d
{
    ssa_ht carry_user = {};
    bitset_uint_t* deps = nullptr;

    unsigned index = 0;
    unsigned rank = 0;
};

struct ssa_isel_d
{
    std::uint64_t store_mask = 0;
    std::uint64_t last_use = 0;
};

//

struct cfg_cg_d
{
    cfg_liveness_d live;

    std::vector<ssa_ht> schedule;
    std::vector<ainst_t> code;
};

struct ssa_cg_d
{
    ssa_value_t cset_head = {}; // Basically a union-find pointer.
    ssa_ht cset_next = {}; // A linked-list to the next node

    // The node will also be stored in these locators:
    fc::small_set<locator_t, 1> store_in_locs;

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

void code_gen(ir_t& ir);

// cset functions: (declare as needed)
ssa_ht cset_head(ssa_ht h);
locator_t cset_locator(ssa_ht h);

#endif
