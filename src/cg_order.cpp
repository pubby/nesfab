#include "cg_order.hpp"
#include <iostream> // TODO

#include <random>

#include "cg.hpp"
#include "ir.hpp"

namespace
{

unsigned order_cost(std::vector<cfg_ht> const& order)
{
    unsigned offset = 0;
    for(cfg_ht h : order)
    {
        auto& d = cg_data(h).order;
        d.offset = offset;
        offset += d.bytes;
    }

    unsigned cost = 0;

    for(cfg_ht h : order)
    {
        auto& d = cg_data(h).order;
        int branch_offset = d.offset + d.bytes;

        unsigned const output_size = h->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht output = h->output(i);
            unsigned const dist = std::abs(branch_offset - (int)cg_data(output).order.offset);

            if(dist > 127 - 4)
                cost += op_cycles(JMP_ABSOLUTE);
            if(dist > 0)
                cost += op_cycles(BEQ_RELATIVE);
        }
    }

    return cost;
}

// aco = ant colony optimization
class aco_t
{
public:
    struct ant_t
    {
        std::vector<cfg_ht> path;
        unsigned cost;

        void swap(ant_t& o)
        {
            std::swap(cost, o.cost);
            path.swap(o.path);
        }
    };

    struct candidate_t
    {
        unsigned i;
        unsigned weight;
    };

    ir_t& ir;

    ant_t ant = {};
    ant_t best_ant = {};

    std::vector<unsigned> starting_pheramones;
    std::vector<candidate_t> candidates;

    std::vector<cfg_ht> starting;
    std::vector<cfg_ht> remaining;

    std::minstd_rand gen;

    aco_t(ir_t& ir);
    void run_ant();
};

aco_t::aco_t(ir_t& ir) : ir(ir), gen(0xDEADBEEF)
{
    // Initialize 'starting':
    starting.reserve(ir.cfg_size());
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        starting.push_back(cfg_it);

    // Initialize pheramones:
    starting_pheramones.clear();
    starting_pheramones.resize(cfg_pool::array_size(), 4);

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it).order;

        d.pheramones.clear();
        d.pheramones.resize(cfg_pool::array_size(), 4);

        // Weight successor nodes highly:
        for(unsigned i = 0; i < cfg_it->output_size(); ++i)
            d.pheramones[cfg_it->output(i).index] = 256;
    }

    constexpr unsigned TRIPS = 64;
    constexpr unsigned ANTS_PER_COLONY = 16;

    // Run the algorithm:
    best_ant.cost = ~0u;
    for(unsigned t = 0; t < TRIPS; ++t)
    {
        for(unsigned a = 0; a < ANTS_PER_COLONY; ++a)
        {
            run_ant();

            if(ant.cost < best_ant.cost)
                best_ant.swap(ant);

            std::cout << '\n';
            for(cfg_ht h : best_ant.path)
                std::cout << "REMAINING = " << h.index << '\n';

            std::cout << "COSTS: " << ant.cost << ' ' << best_ant.cost << '\n';
        }

        // Evaporate pheramones:
        for(cfg_ht h = ir.cfg_begin(); h; ++h)
            for(unsigned& f : cg_data(h).order.pheramones)
                f = (f * 7) / 8;

        // Apply pheramones along best path:
        unsigned const amount = 256;
        std::vector<unsigned>* prev_pheramones = &starting_pheramones;
        for(cfg_ht& h : best_ant.path)
        {
            (*prev_pheramones)[h.index] += amount;
            prev_pheramones = &cg_data(h).order.pheramones;
        }
    }
}

void aco_t::run_ant()
{
    // reset the ant:
    ant.path.clear();
    ant.cost = 0;

    // initialize ready:
    remaining = starting;

    std::vector<unsigned>* prev_pheramones = &starting_pheramones;

    while(remaining.size())
    {
        unsigned total_weight = 0;

        candidates.clear();
        for(unsigned i = 0; i < remaining.size(); ++i)
        {
            cfg_ht h = remaining[i];
            unsigned const weight = (*prev_pheramones)[h.index] + 1;
            total_weight += weight;
            candidates.push_back({ i, weight });
        }
        assert(candidates.size() == remaining.size());

        unsigned chosen_i = 0;

        // Pick a random weighted choice:
        std::uniform_int_distribution<unsigned> distrib(0, total_weight);
        unsigned roll = distrib(gen);
        while(roll > candidates[chosen_i].weight)
        {
            roll -= candidates[chosen_i].weight;
            ++chosen_i;
        }

        assert(chosen_i < candidates.size());
        
        // OK! the next path step has been chosen.

        candidate_t const& chosen = candidates[chosen_i];
        cfg_ht h = remaining[chosen.i];

        // reduce the pheramones along this path.
        (*prev_pheramones)[h.index] *= 7;
        (*prev_pheramones)[h.index] /= 8;

        // update the ant's path:
        ant.path.push_back(h);

        // update 'prev_pheramones':
        prev_pheramones = &cg_data(h).order.pheramones;

        // update the remaining set:
        std::swap(remaining.back(), remaining[chosen.i]);
        remaining.pop_back();
    }

    // Calculate the ant's cost:
    ant.cost = order_cost(ant.path);
}

} // end anon namespace


std::vector<cfg_ht> order_ir(ir_t& ir)
{
    // Setup 'bytes' members:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& d = cg_data(cfg_it);
        d.order.bytes = 0;
        for(asm_inst_t const& inst : d.code)
            d.order.bytes += op_size(inst.op);
    }

    // For small CFG graphs, brute-force every combination.
    constexpr unsigned MAX_SIZE = 6;
    if(ir.cfg_size() <= MAX_SIZE)
    {
        std::vector<cfg_ht> order, best_order;
        unsigned best_cost = ~0u;
        for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
            order.push_back(cfg_it);

        std::sort(order.begin(), order.end());
        do 
        {
            unsigned cost = order_cost(order);
            std::cout << "best cost = " << cost << '\n';
            if(cost < best_cost)
            {
                best_order = order;
                best_cost = cost;
            }
        } 
        while(std::next_permutation(order.begin(), order.end()));

        return best_order;
    }

    // Otherwise use ant-colony optimization.
    aco_t aco(ir);
    return std::move(aco.best_ant.path);
}

