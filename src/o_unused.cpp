#include "o_unused.hpp"

#include <boost/container/small_vector.hpp>

#include "globals.hpp"
#include "ir.hpp"
#include "worklist.hpp"

namespace bc = ::boost::container;

static bool _can_prune(ssa_node_t& node)
{
    SSA_VERSION(1);

    // Linked nodes will all be pruned at once.
    // The pruning only happens at the root of the links,
    // so skip any non-roots:
    if(ssa_input0_class(node.op()) == INPUT_LINK)
        return false;

    switch(node.op())
    {
    default: return true;
    case SSA_if:
    case SSA_return:
        return false;
    case SSA_fn_call:
        return get_fn(node).fn().io_pure();
    }
}

template<typename Vec>
static bool _build_linked(ssa_ht ssa_node, Vec& vec)
{
    for(unsigned i = 0; i < ssa_node->output_size(); ++i)
    {
        auto oe = ssa_node->output_edge(i);
        if(oe.input_class() != INPUT_LINK || !_build_linked(oe.handle, vec))
            return false;
    }
    vec.push_back(ssa_node);
    return true;
}

static ssa_ht _get_link_head(ssa_ht h)
{
    if(ssa_input0_class(h->op()) == INPUT_LINK)
        return _get_link_head(h->input(0).handle());
    return h;
}

bool o_remove_unused_ssa(ir_t& ir)
{
    bool changed = false;

    ssa_worklist.clear();

    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        assert(!ssa_it->test_flags(FLAG_IN_WORKLIST));
        if(_can_prune(*ssa_it))
            ssa_worklist.push(ssa_it);
    }

    bc::small_vector<ssa_ht, 16> linked;

    while(!ssa_worklist.empty())
    {
        ssa_ht ssa_it = ssa_worklist.pop();
        assert(_can_prune(*ssa_it));

        linked.clear();
        if(!_build_linked(ssa_it, linked))
            continue;

        assert(linked.size() > 0);

        // Prune all the linked nodes:
        for(ssa_ht h : linked)
        {
            // We'll also check all inputs again.
            for_each_node_input(h, [ssa_it](ssa_ht input, unsigned)
            {
                input = _get_link_head(input);
                if(input != ssa_it && _can_prune(*input))
                    ssa_worklist.push(input);
            });

            assert(!h->test_flags(FLAG_IN_WORKLIST));
            h->prune();
        }

        changed = true;
    }

    ir.assert_valid();
    return changed;
}
