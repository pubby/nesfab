#include "o_unused.hpp"

#include <boost/container/small_vector.hpp>

#include "globals.hpp"
#include "ir.hpp"
#include "ir_util.hpp"
#include "worklist.hpp"

namespace bc = ::boost::container;

static bool _can_prune(ssa_node_t& node)
{
    // Linked nodes will all be pruned at once.
    // The pruning only happens at the root of the links,
    // so skip any non-roots:
    if(ssa_input0_class(node.op()) == INPUT_LINK)
        return false;

    switch(node.op())
    {
    default: 
        assert(!(ssa_flags(node.op()) & SSAF_CONDITIONAL));
        return pure(node);
    case SSA_if:
    case SSA_switch_full:
    case SSA_switch_partial:
    case SSA_return:
        return false;
    }
}

template<typename Vec>
static bool _build_linked(ssa_ht ssa_node, Vec& vec)
{
    for(unsigned i = 0; i < ssa_node->output_size(); ++i)
    {
        auto oe = ssa_node->output_edge(i);

        // If the node has an output that isn't a link, the node is used and shouldn't be pruned.
        if(oe.input_class() != INPUT_LINK)
            return false;

        // Recurse.
        if(!_build_linked(oe.handle, vec))
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

bool o_remove_unused_linked(log_t* log, ir_t& ir)
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
            for_each_node_input(h, [ssa_it](ssa_ht input)
            {
                input = _get_link_head(input);
                if(input != ssa_it && _can_prune(*input))
                    ssa_worklist.push(input);
            });

            assert(!h->test_flags(FLAG_IN_WORKLIST));
            dprint(log, "PRUNE_UNUSED", h);
            h->prune();
        }

        changed = true;
    }

    ir.assert_valid();
    return changed;
}

bool o_remove_no_effect(log_t* log, ir_t& ir)
{
    ssa_worklist.clear();

    // Assume every node will be pruned, then prove which nodes shouldn't be.
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        assert(!ssa_it->test_flags(FLAG_IN_WORKLIST));
        ssa_it->set_flags(FLAG_PRUNED);
    }

    // Certain nodes will never be pruned by this. 
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        if((ssa_flags(ssa_it->op()) & SSAF_CONDITIONAL)
           || (ssa_flags(ssa_it->op()) & SSAF_WRITE_GLOBALS)
           || (ssa_flags(ssa_it->op()) & SSAF_IO_IMPURE))
        {
            ssa_it->clear_flags(FLAG_PRUNED);
            ssa_worklist.push(ssa_it);
        }
    }

    while(!ssa_worklist.empty())
    {
        ssa_ht ssa_it = ssa_worklist.pop();

        // Inputs to unprunable nodes are also unprunable.
        for_each_node_input(ssa_it, [](ssa_ht input)
        {
            if(input->test_flags(FLAG_PRUNED))
            {
                input->clear_flags(FLAG_PRUNED);
                ssa_worklist.push(input);
            }
        });
    }

    bool changed = false;

    // Do the pruning:
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it;)
    {
        if(ssa_it->test_flags(FLAG_PRUNED))
        {
            dprint(log, "PRUNE_NO_EFFECT", ssa_it);
            ssa_it = ssa_it->prune();
            changed = true;
        }
        else
            ++ssa_it;
    }

    return changed;
}

bool o_remove_unused_ssa(log_t* log, ir_t& ir)
{
    bool changed = false;
    changed |= o_remove_unused_linked(log, ir);
    changed |= o_remove_no_effect(log, ir);
    return changed;
}
