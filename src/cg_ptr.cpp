#include "cg_ptr.hpp"

#include <vector>

#include "robin/set.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "locator.hpp"
#include "guard.hpp"
#include "worklist.hpp"
#include "globals.hpp"

void cg_hoist_bank_switches(ir_t& ir)
{
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        for(ssa_ht ssa = cfg->ssa_begin(); ssa; ++ssa)
        {
            // Identify bank

        }

        // Propagate to each header:
        for(cfg_ht header = this_loop_header(cfg); header; header = algo(header).iloop_header)
        {
        }
    }

    // For each header, add a bank op
}

locator_t cg_calc_bank_switches(fn_ht fn, ir_t& ir)
{
#ifndef NDEBUG
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    for(ssa_ht ssa = cfg->ssa_begin(); ssa; ++ssa)
        assert(!ssa->test_flags(FLAG_BANK_PRELOADED));
#endif

    array_pool_t<bitset_uint_t> bs_pool;

    using namespace ssai::rw_ptr;

    // Identify all banks:
    rh::batman_set<ssa_value_t> banks;
    banks.insert(ssa_value_t()); // Dummy value.

    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        assert(cfg->test_flags(FLAG_IN_WORKLIST) == false);

        auto& d = cg_data(cfg);

        d.banks.first_ssa = {};
        d.banks.first = -1;
        d.banks.last = -1;

        ssa_value_t prev_bank = {};

        for(ssa_ht ssa : d.schedule)
        {
            assert(ssa->test_flags(FLAG_BANK_PRELOADED) == false);

            ssa_value_t bank;

            if(!(ssa_flags(ssa->op()) & SSAF_BANK_INPUT))
                continue;

            bank = orig_def(ssa->input(BANK));

            if(!bank)
                continue;

            if(bank == prev_bank)
                ssa->set_flags(FLAG_BANK_PRELOADED);

        have_bank:

            auto result = banks.insert(bank);

            d.banks.last = int(result.first - banks.begin());
            assert(d.banks.last > 0);

            if(d.banks.first < 0)
            {
                d.banks.first_ssa = ssa;
                d.banks.first = d.banks.last;
            }

            prev_bank = bank;
        }
    }

    // Init bitsets:
    unsigned const bs_size = bitset_size<>(banks.size());
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = cg_data(cfg).banks;
        d.in  = bs_pool.alloc(bs_size);
        d.out = bs_pool.alloc(bs_size);

        if(d.last < 0)
            cfg_worklist.push(cfg);
        else
        {
            bitset_set(d.in, d.first);
            bitset_set(d.out, d.last);
        }
    }

    auto* bs_temp = ALLOCA_T(bitset_uint_t, bs_size);
    auto const run_data_flow = [&]()
    {
        while(!cfg_worklist.empty())
        {
            cfg_ht const cfg = cfg_worklist.pop();
            auto& d = cg_data(cfg).banks;

            // We only care about CFG nodes that don't change banks:
            assert(d.first < 0);
            assert(d.last < 0);

            unsigned const input_size = cfg->input_size();
            unsigned const output_size = cfg->output_size();

            // Update 'd.in':

            bitset_copy(bs_size, bs_temp, d.in);

            for(unsigned i = 0; i < output_size; ++i)
                bitset_or(bs_size, bs_temp, cg_data(cfg->output(i)).banks.in);

            if(!bitset_eq(bs_size, bs_temp, d.in))
            {
                bitset_copy(bs_size, d.in, bs_temp);

                for(unsigned i = 0; i < input_size; ++i)
                {
                    cfg_ht const input = cfg->input(i);
                    if(cg_data(input).banks.first < 0)
                        cfg_worklist.push(input);
                }
            }

            // Update 'd.out':

            bitset_copy(bs_size, bs_temp, d.out);

            for(unsigned i = 0; i < input_size; ++i)
                bitset_or(bs_size, bs_temp, cg_data(cfg->input(i)).banks.out);

            if(!bitset_eq(bs_size, bs_temp, d.out))
            {
                bitset_copy(bs_size, d.out, bs_temp);

                for(unsigned i = 0; i < output_size; ++i)
                {
                    cfg_ht const output = cfg->output(i);
                    if(cg_data(output).banks.first < 0)
                        cfg_worklist.push(output);
                }
            }
        }
    };

    // Run data flow:
    run_data_flow();

    // Check if the root has a single input:
    auto& root_d = cg_data(ir.root).banks;
    ssa_value_t first_bank_switch = {};
    locator_t first_bank_switch_loc = {};
    unsigned first_bank_switch_index = 0;
    if(bitset_popcount(bs_size, root_d.in) == 1 && false)
    {
        first_bank_switch_index = bitset_lowest_bit_set(bs_size, root_d.in);
        first_bank_switch = banks.begin()[first_bank_switch_index];
        if(first_bank_switch.is_num())
            first_bank_switch_loc = locator_t::const_byte(first_bank_switch.whole());
        else if(first_bank_switch.is_locator())
            first_bank_switch_loc = first_bank_switch.locator();
        else if(first_bank_switch.holds_ref())
        {
            // We can't handle non-constant banks unless it's an argument to this fn.

            if(first_bank_switch->op() == SSA_read_global)
            {
                locator_t const loc = first_bank_switch->input(1).locator();

                // For now, only handle arguments that aren't changed by byteify.
                if(loc.lclass() == LOC_ARG && loc.fn() == fn && first_bank_switch->input(0)->op() == SSA_entry
                    && loc.offset() == 0 && loc.atom() == 0 && is_byteified(loc.with_byteified(false).type().name()))
                {
                    first_bank_switch_loc = loc;
                }
                else
                    first_bank_switch = {};
            }
            else
                first_bank_switch = {};
        }
    }

    if(!first_bank_switch)
        first_bank_switch_index = 0;

    // Adjust data flow:
    if(root_d.first < 0)
    {
        assert(!first_bank_switch || bitset_test(root_d.in, first_bank_switch_index));
        assert(cfg_worklist.empty());

        bitset_set(root_d.out, first_bank_switch_index);

        unsigned const output_size = ir.root->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht const output = ir.root->output(i);
            if(cg_data(output).banks.first < 0)
                cfg_worklist.push(output);
        }
        cfg_worklist.push(ir.root);

        run_data_flow();
    }

#ifndef NDEBUG
    if(!fn->iasm && mod_test(fn->mods(), MOD_static) && banks.size() > 1)
        passert(fn->returns_in_different_bank(), banks.size());
#endif

    // Identify banks which are already loaded:
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = cg_data(cfg).banks;

        if(d.first < 0)
            continue;

        ssa_value_t const bank = banks.begin()[d.first];

        bitset_clear_all(bs_size, bs_temp);

        unsigned const input_size = cfg->input_size();
        for(unsigned i = 0; i < input_size; ++i)
            bitset_or(bs_size, bs_temp, cg_data(cfg->input(i)).banks.out);

        assert(bitset_popcount(bs_size, bs_temp) > 0 || !first_bank_switch);

        unsigned const popcount = bitset_popcount(bs_size, bs_temp);
        if((popcount == 1 && bitset_test(bs_temp, d.first))
           || (popcount == 0 && bank == first_bank_switch))
        {
            // This bank is already loaded!
            assert(d.first_ssa);
            assert(!d.first_ssa->test_flags(FLAG_BANK_PRELOADED));
            d.first_ssa->set_flags(FLAG_BANK_PRELOADED);
        }
    }

    return first_bank_switch_loc;
}
