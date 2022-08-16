#include "cg_ptr.hpp"

#include <vector>

#include "alloca.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "locator.hpp"

static ssa_value_t first_bank_switch(cfg_ht cfg, ssa_value_t* memoized)
{
    if(cfg->test_flags(FLAG_IN_WORKLIST))
        return {};

    if(memoized[cfg.id])
        return memoized[cfg.id];

    auto const& d = cg_data(cfg);
    for(ssa_ht h : d.schedule)
        if(ssa_banks(h->op()))
            if(ssa_value_t bank = h->input(ssa_bank_input(h->op())))
                return (memoized[cfg.id] = bank);

    cfg->set_flags(FLAG_IN_WORKLIST);

    ssa_value_t bank = {};
    for(unsigned i = 0; i < cfg->output_size(); ++i)
    {
        if(ssa_value_t output_bank = first_bank_switch(cfg->output(i), memoized))
        {
            if(bank)
                return locator_t(LOC_NONE); // Signifies multiple banks, but none dominate
            bank = output_bank;
        }
    }

    cfg->clear_flags(FLAG_IN_WORKLIST);
    return (memoized[cfg.id] = bank);
}

locator_t first_bank_switch(ir_t& ir)
{
#ifndef NDEBUG
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
        assert(!cfg->test_flags(FLAG_IN_WORKLIST));
#endif

    ssa_value_t* const memoized = ALLOCA_T(ssa_value_t, cfg_pool::array_size());
    std::fill(memoized, memoized + cfg_pool::array_size(), ssa_value_t());

    ssa_value_t const first = first_bank_switch(ir.root, memoized);
    return first.is_locator() ? first.locator() : locator_t(LOC_NONE);
}

