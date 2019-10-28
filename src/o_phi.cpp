#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_set.hpp"

#include "alloca.hpp"
#include "array_pool.hpp"
#include "ir.hpp"
#include "o.hpp"

namespace bc = ::boost::container;

// A trivial phi node has exactly 1 input, exactly 0 inputs, or has every 
// input pointing to itself, or equal to each other. 
ssa_value_t get_trivial_phi_value(ssa_node_t const& node)
{
    assert(node.op() == SSA_phi);
    ssa_value_t unique = nullptr;
    for(unsigned i = 0; i < node.input_size(); ++i)
    {
        ssa_value_t input = node.input(i);

        if(input.is_ptr() && input.ptr() == &node)
            continue;

        if(unique)
        {
            if(input != unique)
                return nullptr;
        }
        else
            unique = input;
    }
    return unique;
}

void o_remove_trivial_phis(ir_t& ir)
{
    ssa_worklist::clear();

    // Queue all phi nodes into the worklist.
    for(cfg_iterator_t cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_iterator_t phi_it = cfg_it->phi_begin(); phi_it; ++phi_it)
    {
        assert(phi_it->op() == SSA_phi);
        ssa_worklist::push(*phi_it);
    }

    // Check each phi node to see if it's trivial.
    while(!ssa_worklist::empty())
    {
        ssa_node_t& phi = ssa_worklist::pop();
        if(ssa_value_t value = get_trivial_phi_value(phi))
        {
            // It's trivial! Add all dependent phi nodes to the worklist.
            for(unsigned i = 0; i < phi.output_size(); ++i)
            {
                ssa_node_t& output = phi.output(i);
                if(output.op() == SSA_phi && &output != &phi) 
                    ssa_worklist::push(output);
            }

            // Delete the trivial phi.
            phi.link_clear_inputs();
            phi.replace_with(value);
            phi.cfg_node().unsafe_prune_ssa(ir, phi);
        }
    }

    ir.reclaim_pools();
}

namespace
{

class tarjan_t
{
private:
    static constexpr unsigned UNDEFINED_INDEX = ~0;
    unsigned index = 0;
    unsigned subgraph_i;
    void visit(ssa_node_t& phi);
public:
    using scc_t = fc::small_set<ssa_node_t*, 2>;
    std::vector<scc_t> sccs;
    std::size_t max_scc_size = 0;
    tarjan_t(ir_t& ir, unsigned subgraph_i,
             ssa_node_t** phis, std::size_t phi_size);
};

tarjan_t::tarjan_t(ir_t& ir, unsigned subgraph_i,
                   ssa_node_t** phis, std::size_t phi_size)
: subgraph_i(subgraph_i)
{
    ssa_worklist::clear();

    for(std::size_t i = 0; i < phi_size; ++i)
        phis[i]->phi_data = { UNDEFINED_INDEX, 0, subgraph_i };

    for(std::size_t i = 0; i < phi_size; ++i)
        if(phis[i]->phi_data.index == UNDEFINED_INDEX)
            visit(*phis[i]);
}

void tarjan_t::visit(ssa_node_t& phi)
{
    assert(phi.op() == SSA_phi);

    phi.phi_data.index = index;
    phi.phi_data.low_link = index;
    ++index;

    ssa_worklist::push(phi);

    for(unsigned i = 0; i < phi.output_size(); ++i)
    {
        ssa_node_t& output = phi.output(i);

        if(output.op() != SSA_phi || output.phi_data.subgraph_i != subgraph_i)
            continue;

        if(output.phi_data.index == UNDEFINED_INDEX)
        {
            visit(output);
            goto find_min;
        }
        else if(output.flags & FLAG_IN_WORKLIST)
        {
        find_min:
            if(phi.phi_data.low_link > output.phi_data.low_link)
                phi.phi_data.low_link = output.phi_data.low_link;
        }
    }

    if(phi.phi_data.low_link == phi.phi_data.index)
    {
        scc_t scc;
        ssa_node_t* node;
        do
        {
            node = &ssa_worklist::pop();
            // Add 'node' to the SCC:
            scc.container.push_back(node); // Unsorted insertion.
        }
        while(node != &phi);

        if(scc.size() > max_scc_size)
            max_scc_size = scc.size();

        std::sort(scc.container.begin(), scc.container.end());
        sccs.push_back(std::move(scc));
    }
}

// Paper: Simple and Efficient Construction of Static Single Assignment Form
// https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf
void o_remove_redundant_phis(ir_t& ir, unsigned& subgraph_i,
                             ssa_node_t** phis, std::size_t phis_size)
{
    tarjan_t tarjan(ir, subgraph_i, phis, phis_size);

    ssa_node_t** inner_begin = ALLOCA_T(ssa_node_t*, tarjan.max_scc_size);

    for(auto it = tarjan.sccs.rbegin(); it < tarjan.sccs.rend(); ++it)
    {
        auto const& scc = *it;
        assert(scc.size() > 0);

        // This will get handled by trivial phi checks.
        if(scc.size() == 1)
            continue;

        ssa_node_t** inner_end = inner_begin;

        ssa_value_t outer = nullptr;
        std::size_t outer_count = 0;

        for(ssa_node_t* phi : scc)
        {
            bool is_inner = true;

            for(unsigned i = 0; i < phi->input_size(); ++i)
            {
                ssa_value_t input = phi->input(i);
                if(input.is_const() || scc.find(input.ptr()) == scc.end())
                {
                    if(outer != input)
                        ++outer_count;
                    outer = input;
                    is_inner = false;
                }
            }

            if(is_inner)
            {
                *inner_end = phi;
                ++inner_end;
                assert(inner_end - inner_begin <= tarjan.max_scc_size);
            }
        }

        if(outer_count == 1)
        {
            for(ssa_node_t* phi : scc)
            {
                phi->link_clear_inputs();
                phi->replace_with(outer);
                phi->cfg_node().unsafe_prune_ssa(ir, *phi);
            }
        }
        else if(outer_count > 1)
        {
            ++subgraph_i;
            o_remove_redundant_phis(ir, subgraph_i, 
                                    inner_begin, inner_end - inner_begin);
        }
    }
}

} // end anonymous namespace

void o_remove_redundant_phis(ir_t& ir)
{
    ssa_workvec.clear();

    for(cfg_iterator_t cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_iterator_t phi_it = cfg_it->phi_begin(); phi_it; ++phi_it)
    {
        assert(phi_it->op() == SSA_phi);
        ssa_workvec.push_back(phi_it.ptr);
    }

    unsigned subgraph_i = 0;
    o_remove_redundant_phis(ir, subgraph_i,
                            ssa_workvec.data(), ssa_workvec.size());
    ir.reclaim_pools();
}

void o_optimize_phis(ir_t& ir)
{
    o_remove_trivial_phis(ir);
    o_remove_redundant_phis(ir);
}
