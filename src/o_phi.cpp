#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_set.hpp"

#include "alloca.hpp"
#include "ir.hpp"
#include "worklist.hpp"

namespace bc = ::boost::container;

// A trivial phi node has exactly 1 input, exactly 0 inputs, or has every 
// input pointing to itself, or equal to each other. 
ssa_value_t get_trivial_phi_value(ssa_node_t const& node)
{
    assert(node.op() == SSA_phi);
    ssa_value_t unique = {};
    assert(!unique);
    for(unsigned i = 0; i < node.input_size(); ++i)
    {
        ssa_value_t input = node.input(i);

        if(input.is_handle() && input.handle() == node.handle())
            continue;

        if(unique)
        {
            if(!input.targets_eq(unique))
                return {};
        }
        else
            unique = input;
    }

    return unique ? unique : node.handle();
}

bool o_remove_trivial_phis(ir_t& ir)
{
    ssa_worklist::clear();

    // Queue all phi nodes into the worklist.
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
    {
        assert(phi_it->op() == SSA_phi);
        assert(phi_it->test_flags(FLAG_IN_WORKLIST) == false);
        ssa_worklist::push(phi_it);
    }
    
    bool changed = false;

    // Check each phi node to see if it's trivial.
    while(!ssa_worklist::empty())
    {
        ssa_ht phi_h = ssa_worklist::pop();
        ssa_node_t& phi = *phi_h;
        if(ssa_value_t value = get_trivial_phi_value(phi))
        {
            // It's trivial! Add all dependent phi nodes to the worklist.
            for(unsigned i = 0; i < phi.output_size(); ++i)
            {
                ssa_ht output_h = phi.output(i);
                if(output_h->op() == SSA_phi && output_h != phi_h) 
                    ssa_worklist::push(output_h);
            }

            // Delete the trivial phi.
            phi.link_clear_inputs();
            phi.replace_with(value);
            phi.unsafe_prune();

            changed = true;
        }
    }

    return changed;
}

namespace
{

struct phi_data_t
{
    unsigned index;
    unsigned low_link;
    unsigned subgraph_i;
};

class tarjan_t
{
private:
    static constexpr unsigned UNDEFINED_INDEX = ~0;
    unsigned index = 0;
    unsigned subgraph_i;
    void visit(ssa_ht phi_h);
public:
    using scc_t = fc::small_set<ssa_ht, 2>;
    inline static thread_local std::vector<scc_t> sccs = {};
    std::size_t max_scc_size = 0;
    tarjan_t(ir_t& ir, unsigned subgraph_i,
             ssa_ht* phis, std::size_t phis_size);
};

tarjan_t::tarjan_t(ir_t& ir, unsigned subgraph_i,
                   ssa_ht* phis, std::size_t phis_size)
: subgraph_i(subgraph_i)
{
    ssa_worklist::clear();
    sccs.clear();

    for(std::size_t i = 0; i < phis_size; ++i)
        phis[i].data<phi_data_t>() = { UNDEFINED_INDEX, 0, subgraph_i };

    for(std::size_t i = 0; i < phis_size; ++i)
        if(phis[i].data<phi_data_t>().index == UNDEFINED_INDEX)
            visit(phis[i]);
}

void tarjan_t::visit(ssa_ht phi_h)
{
    ssa_node_t& phi = *phi_h;
    phi_data_t& phi_data = phi_h.data<phi_data_t>();
    assert(phi.op() == SSA_phi);

    phi_data.index = index;
    phi_data.low_link = index;

    ++index;

    ssa_worklist::push(phi_h);

    unsigned const output_size = phi.output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht output_h = phi.output(i);
        ssa_node_t& output = *output_h;
        phi_data_t& output_data = output_h.data<phi_data_t>();

        if(output.op() != SSA_phi || output_data.subgraph_i != subgraph_i)
            continue;

        if(output_data.index == UNDEFINED_INDEX)
            visit(output_h);
        else if(!(output.test_flags(FLAG_IN_WORKLIST)))
            continue;

        if(phi_data.low_link > output_data.low_link)
            phi_data.low_link = output_data.low_link;
    }

    if(phi_data.low_link == phi_data.index)
    {
        scc_t scc;
        ssa_ht h;
        do
        {
            h = ssa_worklist::pop();
            // Add 'node' to the SCC:
            scc.container.push_back(h); // Unsorted insertion.
        }
        while(h != phi_h);

        if(scc.size() > max_scc_size)
            max_scc_size = scc.size();

        std::sort(scc.container.begin(), scc.container.end());
        sccs.push_back(std::move(scc));
    }
}

// Paper: Simple and Efficient Construction of Static Single Assignment Form
// https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf
void o_remove_redundant_phis(ir_t& ir, bool& changed, unsigned& subgraph_i,
                             ssa_ht* phis, std::size_t phis_size)
{
    tarjan_t tarjan(ir, subgraph_i, phis, phis_size);

    ssa_ht* inner_begin = ALLOCA_T(ssa_ht, tarjan.max_scc_size);

    for(auto it = tarjan.sccs.rbegin(); it < tarjan.sccs.rend(); ++it)
    {
        auto const& scc = *it;
        assert(scc.size() > 0);

        // This will get handled by trivial phi checks.
        if(scc.size() == 1)
            continue;

        ssa_ht* inner_end = inner_begin;

        ssa_value_t outer = {};
        std::size_t outer_count = 0;

        for(ssa_ht phi_h : scc)
        {
            ssa_node_t& phi = *phi_h;
            bool is_inner = true;

            unsigned const input_size = phi.input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input = phi.input(i);
                if(input.is_const() || scc.find(input.handle()) == scc.end())
                {
                    if(!outer.targets_eq(input))
                        ++outer_count;
                    outer = input;
                    is_inner = false;
                }
            }

            if(is_inner)
            {
                *inner_end = phi_h;
                ++inner_end;
                assert(inner_end - inner_begin 
                       <= (std::ptrdiff_t)tarjan.max_scc_size);
            }
        }

        if(outer_count == 1)
        {
            for(ssa_ht phi_h : scc)
            {
                ssa_node_t& phi = *phi_h;
                phi.link_clear_inputs();
                phi.replace_with(outer);
                phi.unsafe_prune();
                changed = true;
            }
        }
        else if(outer_count > 1)
        {
            ++subgraph_i;
            o_remove_redundant_phis(ir, changed, subgraph_i, 
                                    inner_begin, inner_end - inner_begin);
        }
    }
}

} // end anonymous namespace

bool o_remove_redundant_phis(ir_t& ir)
{
    ssa_data_pool::scope_guard_t<phi_data_t> sg(ssa_pool::array_size());
    ssa_workvec.clear();

    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
    {
        assert(phi_it->op() == SSA_phi);
        ssa_workvec.push_back(phi_it);
    }

    bool changed = false;
    unsigned subgraph_i = 0;
    o_remove_redundant_phis(ir, changed, subgraph_i, 
                            ssa_workvec.data(), ssa_workvec.size());
    return changed;
}

bool o_phis(ir_t& ir)
{
    bool changed = false;
    changed |= o_remove_trivial_phis(ir);
    changed |= o_remove_redundant_phis(ir);
    return changed;
}
