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
#include "cg_isel_cpu.hpp"

struct isel_no_progress_error_t : public std::exception
{
    virtual char const* what() const noexcept 
    { 
        return ("Instruction selector unable to make progress. " 
                "File a bug report with reproducible code!");
    }
};

using isel_cost_t = pbqp_cost_t;

namespace isel
{
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

    struct result_t
    {
        isel_cost_t cost = 0;
        std::shared_ptr<std::vector<asm_inst_t>> code;
    };

    using preprep_bitset_t = static_bitset_t<256>;

    struct cfg_d : public pbqp_node_t
    {
        std::vector<preprep_bitset_t> preprep;
        std::vector<unsigned> to_compute;
        rh::batman_set<cross_cpu_t> in_states;
        rh::batman_map<cross_transition_t, result_t> sels;
        std::vector<isel_cost_t> min_in_costs;
        isel_cost_t min_sel_cost = isel_cost_t(~0ull) / 2;

        std::vector<asm_inst_t> const& final_code() const { return *sels.begin()[sel].second.code; }
        std::vector<asm_inst_t>& final_code() { return *sels.begin()[sel].second.code; }

        cross_cpu_t const& final_in_state() const { return sels.begin()[sel].first.in_state; }
        cross_cpu_t const& final_out_state() const { return sels.begin()[sel].first.out_state; }
        isel_cost_t final_cost() const { return sels.begin()[sel].second.cost; }
    };

    inline thread_local std::vector<cfg_d> _data_vec;
    inline cfg_d& data(cfg_ht h) { assert(h.id < _data_vec.size()); return _data_vec[h.id]; }
} // end namespace isel

void select_instructions(log_t* log, fn_t& fn, ir_t& ir);

#endif
