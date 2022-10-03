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

    return true;
}

/* TODO: Remove
1 2 4 8

0101
00010001
0000000100000001

static constexpr std::array<std::uint64_t, 8> _gen_bits()
{
    unsigned r = 0;
    std::array<std::uint64_t, 8> ret;

    for(unsigned j = 1; j < 8; j *= 2)
    {
        std::uint64_t bits = 0;

        for(unsigned i = 0; i < 64; i += 1 << j)
            bits |= 1 << i;

        for(unsigned i = 0; i <= j; ++i)
            ret[r++] = bits << i;
    }

    return ret;
}

std::uint8_t _reverse_bit_order(uint8_t n) 
{
    constexpr std::uint8_t table[16] = 
    {
        0x0, 0x8, 0x4, 0xC, 0x2, 0xA, 0x6, 0xE,
        0x1, 0x9, 0x5, 0xD, 0x3, 0xB, 0x7, 0xF, 
    };

   // Reverse the top and bottom nibble then swap them.
   return (table[n & 0b1111] << 4) | table[n >> 4];
}
*/

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

/* TODO: remove
///////////////////////////////////////////////////////////////////////////////

void switch_table_t::replace_labels(locator_t from, locator_t to)
{
    for(locator_t& loc : labels)
        if(loc == from)
            loc = to;
}

switch_manager_t::switch_manager_t(ir_t const& ir)
{
    for(cfg_node_t const& cfg_node : ir)
    {
        ssa_ht const branch = cfg_node.last_daisy();

        assert(!branch || branch->op() != SSA_switch_partial);

        if(!branch || branch->op() != SSA_switch_full)
            continue;

        std::uint8_t min = 0xFF;
        std::uint8_t max = 0;

        unsigned const input_size = branch->input_size();
        for(unsigned j = 1; j < input_size; ++j)
        {
            std::uint8_t const whole = branch->input(j).whole();
            min = std::min(min, whole);
            max = std::max(max, whole);
        }

        unsigned const size = max - min + 1;

        switch_table_t table = { .offset = min };
        table.labels.resize(size, locator_t::const_byte(0));

        unsigned const cases = ssa_switch_cases(branch->op());
        for(unsigned i = cases, j = 1; j < input_size; ++i, ++j)
        {
            std::uint8_t const whole = branch->input(j).whole();
            unsigned const k = whole - min;
            passert(k < table.labels.size(), k, table.labels.size(), whole, min);

            cfg_ht const output = cfg_node.output(i);

            table.labels[k] = locator_t::cfg_label(output);
        }

        m_map.insert({ cfg_node.handle(), std::move(table) });
    }
}
*/
