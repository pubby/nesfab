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
#include "thread.hpp"

struct isel_no_progress_error_t : public std::exception
{
    virtual char const* what() const noexcept 
    { 
        return ("Instruction selector unable to make progress. " 
                "File a bug report with reproducible code!");
    }
};

struct isel_no_progress_error_pretty_t : public std::exception
{
    isel_no_progress_error_pretty_t(fn_ht fn, ssa_ht node);

    fn_ht fn;
    ssa_ht node;
    std::string message;

    virtual char const* what() const noexcept { return message.c_str(); }
};

using isel_cost_t = pbqp_cost_t;

namespace isel
{
    struct sel_t
    {
        sel_t(sel_t const* prev, asm_inst_t const& inst)
        : prev(prev)
        , inst(inst)
        {}

        sel_t const* prev = nullptr;
        asm_inst_t inst = {};
    };

    struct sel_pair_t
    {
        sel_t const* sel = nullptr;
        isel_cost_t cost = 0;
    };

    struct result_t
    {
        isel_cost_t cost = 0;
        std::shared_ptr<std::vector<asm_inst_t>> code;
    };

    using prep_flags_t = std::uint8_t;
    constexpr prep_flags_t PREPREP_A_0 = 1 << 0;
    constexpr prep_flags_t PREPREP_X_0 = 1 << 1;
    constexpr prep_flags_t PREPREP_Y_0 = 1 << 2;
    constexpr prep_flags_t POSTPREP_TAX = 1 << 3;
    constexpr prep_flags_t POSTPREP_TAY = 1 << 4;
    constexpr prep_flags_t POSTPREP_TXA = 1 << 5;
    constexpr prep_flags_t POSTPREP_TYA = 1 << 6;

    constexpr prep_flags_t PREPREP_FLAGS = PREPREP_A_0 | PREPREP_X_0 | PREPREP_Y_0;
    constexpr prep_flags_t POSTPREP_FLAGS = POSTPREP_TAX | POSTPREP_TAY | POSTPREP_TXA | POSTPREP_TYA;

    struct memoized_input_t
    {
        locator_t main;
        locator_t phi;
    };

    struct cfg_d : public pbqp_node_t
    {
        unsigned iter = 0;

        std::vector<prep_flags_t> prep;
        std::vector<unsigned> to_compute;
        rh::batman_set<cross_cpu_t> in_states;
        rh::batman_map<cross_transition_t, result_t> sels;
        isel_cost_t min_sel_cost = isel_cost_t(~0ull) / 2;

        std::vector<rh::robin_map<locator_t, memoized_input_t>> memoized_input_maps;

        std::vector<asm_inst_t> const& final_code() const { return *sels.begin()[sel].second.code; }
        std::vector<asm_inst_t>& final_code() { return *sels.begin()[sel].second.code; }

        cross_cpu_t const& final_in_state() const { return sels.begin()[sel].first.in_state; }
        cross_cpu_t const& final_out_state() const { return sels.begin()[sel].first.out_state; }
        isel_cost_t final_cost() const { return sels.begin()[sel].second.cost; }
    };

    extern TLS std::vector<cfg_d> _data_vec;
    inline cfg_d& data(cfg_ht h) { assert(h.id < _data_vec.size()); return _data_vec[h.id]; }
} // end namespace isel

// Returns size in bytes of proc:
std::size_t select_instructions(log_t* log, fn_t& fn, ir_t& ir);

#endif
