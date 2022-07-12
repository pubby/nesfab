#include "o_id.hpp"

#include <cstdint>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "flat/small_map.hpp"

#include "ir.hpp"
#include "type.hpp"
#include "type_mask.hpp"
#include "alloca.hpp"
#include "worklist.hpp"

namespace bc = ::boost::container;

// Replaces single nodes with one of their inputs.
// (e.g. X + 0 becomes X, or Y & ~0 becomes Y)
static bool o_simple_identity(ir_t& ir)
{
    bool updated = false;

    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it;)
    {
        if(!is_arithmetic(ssa_it->type().name()))
        {
            ++ssa_it;
            continue;
        }

        ssa_node_t& node = *ssa_it;
        fixed_t const all_set = { numeric_bitmask(node.type().name()) };

        switch(node.op())
        {
        case SSA_add:
            {
                if(!node.input(2).is_num())
                    break;
                std::uint64_t const carry = node.input(2).fixed().value;
                if(node.input(0).is_num() && !((node.input(0).fixed().value + carry) & all_set.value))
                    goto replaceWith1;
                if(node.input(1).is_num() && !((node.input(1).fixed().value + carry) & all_set.value))
                    goto replaceWith0;
            }
            break;
        case SSA_sub:
            {
                if(!node.input(2).is_num())
                    break;
                std::uint64_t const carry = node.input(2).fixed().value;
                if(node.input(1).is_num() && !((~node.input(1).fixed().value + carry) & all_set.value))
                    goto replaceWith0;
            }
            break;
        case SSA_or:
            if(ssa_it->input(0) == ssa_it->input(1))
                goto replaceWith0;
            // fall through
        case SSA_xor:
            if(ssa_it->input(0).eq_fixed({0}))
                goto replaceWith1;
            if(ssa_it->input(1).eq_fixed({0}))
                goto replaceWith0;
            break;
        case SSA_and:
            if(ssa_it->input(0).eq_fixed(all_set))
                goto replaceWith1;
            if(ssa_it->input(1).eq_fixed(all_set))
                goto replaceWith0;
            if(ssa_it->input(0) == ssa_it->input(1))
                goto replaceWith0;
            break;
        case SSA_rol:
        case SSA_ror:
            {
                if(!node.input(2).is_num())
                    break;
                std::uint64_t const carry = node.input(2).fixed().value;
                if(carry == 0ull && ssa_it->input(1).eq_fixed({0}))
                    goto replaceWith0;
            }
            break;
        default:
            break;
        }

        ++ssa_it;
        continue;

    replaceWith0:
        node.replace_with(node.input(0));
        goto prune;

    replaceWith1:
        node.replace_with(node.input(1));
        goto prune;

    prune:
        ssa_it = node.prune();
        updated = true;
    }

    return updated;
}

// What do we want to do?

// IDEA: 
// First, determine a set of nodes.
// Second, calculate the sums.

struct ssa_monoid_d
{
    bitset_uint_t* post_dom = nullptr;
    bool sealed_singleton = false;
};

class run_monoid_t
{
public:
    run_monoid_t(ir_t& ir);

    static ssa_op_t defining_op(ssa_op_t op);

    ssa_monoid_d& data(ssa_ht h) { return h.data<ssa_monoid_d>(); }

    void mark_singleton(ssa_ht h) 
    { 
        assert(!data(h).sealed_singleton);
        data(h).sealed_singleton = true;
        singletons.push_back(h);
    }

    void build(ssa_ht h);

    bool updated = false;
private:
    std::size_t bs_size = 0;
    array_pool_t<bitset_uint_t> bs_pool;
    bc::small_vector<ssa_ht, 32> singletons;

    // These are used inside 'build':
    ssa_ht root;
    fc::small_map<ssa_value_t, unsigned, 16> leafs;
    using internals_map_t = fc::vector_map<ssa_ht, unsigned>;
    internals_map_t internals;
};

ssa_op_t run_monoid_t::defining_op(ssa_op_t op)
{
    switch(op)
    {
    // TODO: implement
    //case SSA_add:
    //case SSA_sub:
        //return SSA_add;
    case SSA_and:
    case SSA_or:
    case SSA_xor:
        return op;
    default:
        return SSA_null;
    }
}

run_monoid_t::run_monoid_t(ir_t& ir)
{
    bs_size = bitset_size<>(ssa_pool::array_size());
    bitset_uint_t* const temp_bs = ALLOCA_T(bitset_uint_t, bs_size);

#ifndef NDEBUG
    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        assert(ssa_it->test_flags(FLAG_IN_WORKLIST) == false);
#endif

    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        auto& d = data(ssa_it);
        assert(d.post_dom == nullptr);

        ssa_op_t const def_op = defining_op(ssa_it->op());
        
        // Ignore nodes that don't have an op we care about.
        if(!def_op)
            continue;

        d.post_dom = bs_pool.alloc(bs_size);
        assert(bitset_all_clear(bs_size, d.post_dom));

        type_t const type = ssa_it->type();

        // If a node has an output that isn't compatible,
        // set its post-dom set to only itself.
        unsigned const output_size = ssa_it->output_size();
        if(output_size == 0)
            goto singleton; // Also do this for no-output nodes.
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_ht const output = ssa_it->output(i);
            if(def_op != defining_op(output->op()) || type != output->type())
                goto singleton;
        }

        // Otherwise optimistically assume every node is a post dominator.
        // (We'll then prove those which aren't.)
        bitset_set_all(bs_size, d.post_dom);
        continue;

    singleton:
        bitset_set(d.post_dom, ssa_it.index);
        mark_singleton(ssa_it);

        unsigned const input_size = ssa_it->input_size();
        for(unsigned j = 0; j < input_size; ++j)
            if(ssa_it->input(j).holds_ref())
                ssa_worklist.push(ssa_it->input(j).handle());
    }
    
    // Now run a data-flow equation until reaching a fixed point,
    // calculating the post-dominator sets:
    while(!ssa_worklist.empty())
    {
        ssa_ht const h = ssa_worklist.pop();
        auto& d = data(h);

        if(d.sealed_singleton || !d.post_dom)
            continue;

        // Calculate post-dominance by intersecting the sets of this node's outputs.
        if(unsigned const output_size = h->output_size())
        {
            bitset_set_all(bs_size, temp_bs);
            for(unsigned i = 0; i < output_size; ++i)
            {
                ssa_ht const output = h->output(i);
                auto const& output_d = data(output);

                assert(defining_op(h->op()) == defining_op(output->op()));
                assert(output_d.post_dom);
                assert(h->type() == output->type());

                bitset_and(bs_size, temp_bs, output_d.post_dom);
            }

            if(bitset_all_clear(bs_size, temp_bs))
                mark_singleton(h);
            else
                assert(bitset_popcount(bs_size, temp_bs) != 1 || !bitset_test(temp_bs, h.index));
        }
        else
        {
            bitset_clear_all(bs_size, temp_bs);
            mark_singleton(h);
        }

        // Then set the node's own bit:
        bitset_set(temp_bs, h.index);

        if(bitset_eq(bs_size, d.post_dom, temp_bs))
            continue;

        bitset_copy(bs_size, d.post_dom, temp_bs);

        // Now consider inputs:
        unsigned const input_size = h->input_size();
        for(unsigned i = 0; i < input_size; ++i)
            if(h->input(i).holds_ref())
                ssa_worklist.push(h->input(i).handle());
    }

    // Sanity checks
#ifndef NDEBUG
    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        assert(ssa_it->test_flags(FLAG_IN_WORKLIST) == false);
        
        auto& d = data(ssa_it);
        if(!d.post_dom)
            continue;

        assert(d.sealed_singleton == (bitset_popcount(bs_size, d.post_dom) == 1));

        unsigned count = 0;
        bitset_for_each(bs_size, d.post_dom, [&](unsigned bit)
        {
            if(data(ssa_ht{bit}).sealed_singleton)
                ++count;
        });

        assert(count == 1);
    }
#endif

    std::vector<ssa_value_t> operands;
    std::vector<ssa_value_t> replacements;
    std::vector<internals_map_t> to_prune;

    for(ssa_ht h : singletons)
    {
        root = h;
        leafs.clear();
        internals.clear();
        build(h);

        ssa_op_t const def_op = defining_op(h->op());
        type_t const type = h->type();

        // Calculate the constant while building 'operands':

        operands.clear();

        unsigned num_nums = 0;
        fixed_t acum = {0};
        for(auto const& p : leafs)
        {
            ssa_value_t const v = p.first;

            if(v.is_num())
            {
                fixed_t const f = v.fixed();

                if(num_nums == 0)
                    acum = f;

                switch(def_op)
                {
                case SSA_and: acum.value &= f.value; break;
                case SSA_or:  acum.value |= f.value; break;
                case SSA_xor: acum.value ^= f.value; break;
                default: assert(0); break;
                }

                ++num_nums;
            }
            else
            {
                switch(def_op)
                {
                case SSA_and:
                case SSA_or:
                    // Ignore duplicates.
                    operands.push_back(p.first);
                    break;

                case SSA_xor:
                    // An even number cancels out.
                    if(p.second % 2)
                        operands.push_back(p.first);
                    else
                        ++num_nums; // Treat it as a 0.
                    break;

                default: assert(0); break;
                }
            }
        }

        if(num_nums > 0)
        {
            acum.value &= numeric_bitmask(type.name());
            operands.push_back(ssa_value_t(acum, type.name()));
        }

        // Now use 'operands' to build a replacement for 'h':

        cfg_ht const cfg = h->cfg_node();

        assert(!operands.empty());
        while(operands.size() > 1)
        {
            ssa_ht const ssa = cfg->emplace_ssa(def_op, type, operands.rbegin()[0], operands.rbegin()[1]);
            operands.pop_back();
            operands.back() = ssa;
        }

        replacements.push_back(operands[0]);
        to_prune.push_back(std::move(internals));
    }

    // Now replace all singletons and prune:

    assert(singletons.size() == replacements.size());
    assert(singletons.size() == to_prune.size());

    for(unsigned i = 0; i < singletons.size(); ++i)
    {
        if(singletons[i] == replacements[i])
            continue;

        singletons[i]->replace_with(replacements[i]);

        for(auto const& p : to_prune[i])
            p.first->prune();
    }
}

void run_monoid_t::build(ssa_ht h)
{
    internals[h] += 1;

    unsigned const input_size = h->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t const input = h->input(i);

        if(input.holds_ref() && data(input.handle()).post_dom && bitset_test(data(input.handle()).post_dom, root.index))
        {
            assert(defining_op(input->op()) == defining_op(h->op()));
            assert(input->type() == h->type());

            build(h->input(i).handle());
        }
        else
            leafs[input] += 1;
    }
}

bool o_identities(ir_t& ir)
{
    bool updated = o_simple_identity(ir);

    {
        ssa_data_pool::scope_guard_t<ssa_monoid_d> sg(ssa_pool::array_size());
        run_monoid_t run(ir);
        updated |= run.updated;
    }

    return updated;
}

