#include "ir_util.hpp"

#include "flat/small_set.hpp"

#include "globals.hpp"
#include "worklist.hpp"

bool io_pure(ssa_node_t const& ssa_node)
{
    if(ssa_flags(ssa_node.op()) & SSAF_IO_IMPURE)
        return false;
    if(ssa_node.op() == SSA_fn_call)
        return get_fn(ssa_node)->ir_io_pure();
    if(ssa_node.op() == SSA_read_ptr)
    {
        using namespace ssai::rw_ptr;
        return !ssa_node.input(BANK);
    }
    return true;
}

bool pure(ssa_node_t const& ssa_node)
{
    if(!io_pure(ssa_node))
       return false;
    if(ssa_node.op() == SSA_fn_call)
        return get_fn(ssa_node)->ir_writes().all_clear();
    return true;
}

unsigned estimate_cost(ssa_node_t const& ssa_node)
{
    if(ssa_input0_class(ssa_node.op()) == INPUT_LINK)
        return 0;

    if(ssa_flags(ssa_node.op()) & SSAF_FREE)
        return 0;

    if(ssa_flags(ssa_node.op()) & SSAF_EXPENSIVE)
        return 256;

    if(ssa_flags(ssa_node.op()) & SSAF_CONDITIONAL)
        return 4; // somewhat arbitrary

    if(ssa_flags(ssa_node.op()) & SSAF_INDEXES_PTR)
        return 2; // somewhat arbitrary

    if(ssa_flags(ssa_node.op()) & SSAF_INDEXES_ARRAY8)
        return 2; // somewhat arbitrary

    if(ssa_flags(ssa_node.op()) & SSAF_INDEXES_ARRAY16)
        return 8; // somewhat arbitrary

    unsigned cost = 0;

    unsigned const input_size = ssa_node.input_size();
    for(unsigned i = 0; i < input_size; ++i)
        cost += ssa_node.input(i).type().size_of();

    cost = std::max<unsigned>(cost, ssa_node.type().size_of());

    return cost;
}

void steal_ssa_after(ssa_ht ssa, cfg_ht steal_dest)
{
    cfg_ht const cfg = ssa->cfg_node();

    // Determine the set of ssa nodes to occur before the branch
    fc::small_set<ssa_ht, 32> pre;
    pre.container.reserve(cfg->ssa_size());
    pre.insert(ssa);

    // All phi nodes are in 'pre':
    for(ssa_ht ssa_it = cfg->ssa_begin(); ssa_it; ++ssa_it)
        if(ssa_it->op() == SSA_phi)
            pre.insert(ssa_it);

    // All inputs to 'ssa' are in pre, recursively:
    assert(ssa_worklist.empty());
    ssa_worklist.push(ssa);
    while(!ssa_worklist.empty())
    {
        ssa_ht h = ssa_worklist.pop();

        assert(h);
        assert(h->cfg_node() == cfg);
        
        // Handle inputs: (this modifies 'pre')
        unsigned const input_size = h->input_size();
        for(unsigned i = 0; i < input_size; ++i)
            if(ssa_ht const input = h->input(i).maybe_handle())
                if(input->cfg_node() == cfg && pre.insert(input).second)
                    ssa_worklist.push(input);

        // Handle daisy: (this modifies 'h' and 'pre')
        if(h->in_daisy() && (h = h->prev_daisy()) && pre.insert(h).second)
            ssa_worklist.push(h);
    }

    // Transfer the the nodes:

    bc::small_vector<ssa_ht, 32> to_steal;
    for(ssa_ht h = cfg->ssa_begin(); h; ++h)
        if(!pre.count(h))
            to_steal.push_back(h);

    for(ssa_ht h : to_steal)
        steal_dest->steal_ssa(h, true);

    assert(ssa->cfg_node() == cfg);
}
