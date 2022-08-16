#ifndef CG_ISEL_HPP
#define CG_ISEL_HPP

// Instruction selection

#include <array>
#include <exception>

#include "robin/map.hpp"
#include "robin/set.hpp"

#include "asm.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "pbqp.hpp"

struct isel_no_progress_error_t : public std::exception
{
    virtual char const* what() const noexcept 
    { 
        return ("Instruction selector unable to make progress. " 
                "File a bug report with reproducible code!");
    }
};

using isel_cost_t = pbqp_cost_t;

struct sel_t
{
    sel_t(sel_t const* prev, unsigned cost, asm_inst_t inst)
    : prev(prev)
    , cost(cost)
    , inst(inst)
    {}

    sel_t const* prev = nullptr;
    isel_cost_t cost = 0;
    asm_inst_t inst = {};
};

namespace isel
{
    struct cross_cpu_t
    {
        std::array<locator_t, NUM_CROSS_REGS> defs = {};
        auto operator<=>(cross_cpu_t const&) const = default;
    };

    struct result_t
    {
        isel_cost_t cost = 0;
        cross_cpu_t in_state;
        std::vector<asm_inst_t> code;
    };

    struct cfg_d : public pbqp_node_t
    {
        std::vector<unsigned> to_compute;
        rh::batman_set<cross_cpu_t> in_states;
        rh::batman_map<cross_cpu_t, result_t> sels;

        std::vector<asm_inst_t> const& final_code() const { return sels.begin()[sel].second.code; }
        std::vector<asm_inst_t>& final_code() { return sels.begin()[sel].second.code; }
        cross_cpu_t const& final_in_state() const { return sels.begin()[sel].second.in_state; }
        cross_cpu_t const& final_out_state() const { return sels.begin()[sel].first; }
    };

    inline thread_local std::vector<cfg_d> _data_vec;
    inline cfg_d& data(cfg_ht h) { assert(h.id < _data_vec.size()); return _data_vec[h.id]; }
} // end namespace isel

template<>
struct std::hash<isel::cross_cpu_t>
{
    std::size_t operator()(isel::cross_cpu_t const& cross) const noexcept
    {
        std::size_t h = 0xDEADBEEF;
        for(locator_t const& v : cross.defs)
            h = rh::hash_combine(h, v.to_uint());
        return h;
    }
};


void select_instructions(fn_t const& fn, ir_t& ir);

#endif
