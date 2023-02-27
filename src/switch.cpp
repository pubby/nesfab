#include "switch.hpp"

#include <boost/container/static_vector.hpp>

#include "ir.hpp"

namespace bc = ::boost::container;

bool switch_partial_to_full(ssa_node_t& switch_node)
{
    assert(switch_node.op() == SSA_switch_partial);

    if(switch_node.input_size() == 1)
        return false; // Can't convert; there are no cases.

    ssa_value_t* const inputs = ALLOCA_T(ssa_value_t, switch_node.input_size());
    ssa_value_t* end = inputs;

    unsigned const to = switch_node.input_size() - 1;

    *end++ = switch_node.input(0);
    *end++ = switch_node.input(to);
    for(unsigned i = 1; i < to; ++i)
        *end++ = switch_node.input(i);

    assert(end - inputs == switch_node.input_size());

    switch_node.link_clear_inputs();
    switch_node.link_append_input(inputs, end);
    switch_node.unsafe_set_op(SSA_switch_full);

    assert(switch_node.cfg_node()->output_size() >= 1);
    switch_node.cfg_node()->link_remove_output(0);

    assert(switch_node.cfg_node()->last_daisy() == switch_node.handle());

    return true;
}

bool switch_partial_to_full(ir_t& ir)
{
    bool updated = false;

    struct rep_t
    {
        std::uint16_t rshift;
        std::uint8_t common;
        std::uint8_t start;
        std::uint16_t size;
        std::uint16_t popcount;
    };

    auto calc_rep = [](static_bitset_t<256> const& cases)
    {
        if(cases.all_clear())
            return rep_t{};

        rep_t result = {};

        // For rshift, common:
        std::uint8_t common_mismatch = 0;
        std::uint8_t const common_bits = cases.lowest_bit_set();

        // For start, size:
        int prev = cases.highest_bit_set() - 256;
        std::uint8_t start = cases.lowest_bit_set();
        std::uint8_t max_span = 0;

        cases.for_each([&](std::uint8_t c)
        { 
            // For rshift, common:
            common_mismatch |= c ^ common_bits;

            // For start, size:
            std::uint8_t const span = c - prev;
            if(span > max_span)
            {
                max_span = span;
                start = c;
            }
            prev = c;
        });

        if(common_mismatch)
        {
            result.rshift = builtin::ctz(common_mismatch);
            result.common = common_bits & ~common_mismatch;
        }

        result.start = start;
        result.size = 256 - std::uint8_t(max_span - 1);
        result.popcount = cases.popcount();

        return result;
    };

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        ssa_ht const branch = cfg_it->last_daisy();
        
        if(!branch || branch->op() != SSA_switch_partial)
            continue;

        // Gather all cases into 'cases':
        static_bitset_t<256> cases = {};
        unsigned const input_size = branch->input_size();
        for(unsigned i = 1; i < input_size; ++i)
            cases.set(std::uint8_t(branch->input(i).whole()));

        // Calculate
        auto const rep = calc_rep(cases);

        // Transform the branch.

        cfg_ht const default_cfg = cfg_it->output(0); // Where the 'default' case leads.
        cfg_ht const entry = ir.emplace_cfg();
        cfg_ht current_cfg = entry;
        ssa_value_t current_ssa = branch->input(0);
        type_t const condition_type = current_ssa.type();

        auto const do_branch = [&](bool swap)
        {
            cfg_ht next_cfg = ir.emplace_cfg();

            current_cfg->alloc_output(1);
            current_cfg->build_set_output(0, next_cfg);

            current_cfg->link_append_output(default_cfg, [&](ssa_ht phi)
            {
                return phi->input(cfg_it->output_edge(0).index);
            });

            if(swap)
                current_cfg->link_swap_outputs(0, 1);

            current_cfg = next_cfg;
        };

        // Comparisons:

        if(current_ssa.type() != TYPE_U)
            current_ssa = current_cfg->emplace_ssa(SSA_cast, TYPE_U, current_ssa);

        current_ssa = current_cfg->emplace_ssa(SSA_sub, TYPE_U, current_ssa, ssa_value_t(rep.start, TYPE_U), ssa_value_t(1u, TYPE_BOOL));
        ssa_ht const less = current_cfg->emplace_ssa(SSA_lt, TYPE_BOOL, current_ssa, ssa_value_t(rep.size, TYPE_U));
        ssa_ht const if_ = current_cfg->emplace_ssa(SSA_if, TYPE_VOID, less);
        if_->append_daisy();
        do_branch(true);

        // Shift:
        for(unsigned i = 0 ; i < rep.rshift; ++i)
        {
            current_ssa = current_cfg->emplace_ssa(SSA_ror, condition_type, current_ssa, ssa_value_t(0u, TYPE_BOOL));
            ssa_ht const carry_out = current_cfg->emplace_ssa(SSA_carry, TYPE_BOOL, current_ssa);
            ssa_ht const if_ = current_cfg->emplace_ssa(SSA_if, TYPE_VOID, carry_out);
            if_->append_daisy();
            assert(((rep.common - rep.start) & ((1 << rep.rshift) - 1)) == 0);
            do_branch(false);
        }

        ir.assert_valid();

        // Rewrite the switch and move it 'current_cfg': 

        assert(input_size == branch->input_size());
        branch->link_change_input(0, current_ssa);
        for(unsigned i = 1; i < input_size; ++i)
        {
            std::uint8_t v = branch->input(i).whole();
            v -= rep.start;
            v >>= rep.rshift;
            branch->link_change_input(i, ssa_value_t(v, TYPE_U));
        }

        for(unsigned i = 0, j = 0; i < rep.size; i += 1 << rep.rshift, ++j)
        {
            std::uint8_t const c = rep.start + i;
            if(!cases.test(c))
            {
                branch->link_append_input(ssa_value_t(j, TYPE_U));
                cfg_it->link_append_output(cfg_it->output(0), [&](ssa_ht phi)
                {
                    return phi->input(cfg_it->output_edge(0).index);
                });
            }
        }

        ir.assert_valid();

        bool const result = switch_partial_to_full(*branch);
        assert(result);

        ir.assert_valid();

        current_cfg->steal_outputs(*cfg_it);
        current_cfg->steal_ssa(branch, true);

        // Final link

        assert(cfg_it->output_size() == 0);
        cfg_it->link_append_output(entry, [](ssa_ht){ assert(false); return ssa_value_t(); });

        updated = true;
    }

    return updated;
}

