#include "cg_schedule.hpp"

struct cfg_schedule_d
{
    // All nodes:
    std::vector<ssa_ht> nodes;

    // Global modifying nodes:
    //std::vector<ssa_ht> gmod_nodes;
    //bitset_uint_t* gmod_nodes_set = nullptr;

    //fc::small_map<locator_ht, ssa_ht> loc_inputs;
    //fc::small_map<locator_ht, ssa_ht> loc_outputs;

    // A set of all the nodes that have been scheduled.
    bitset_uint_t* scheduled;

    //std::vector<ssa_ht> worklist;
    //unsigned initial_num_ready = 0;
    //unsigned num_ready = 0;

    //float* start_edge_phera = nullptr;

    // Whatever locators are currently live, and their value.
    //std::vector<loc_live_t> loc_live;
};

struct ssa_schedule_d
{
    unsigned index = 0;
    //unsigned edge_index = 0;
    bitset_uint_t* deps = nullptr;
    //bitset_uint_t* extra_deps = nullptr;
    //float* edge_phera = nullptr;

    // Tracks which locators this node reads, but doesn't modify.
    //fc::vector_map<ssa_ht, locator_ht> loc_only_reads;

    // Tracks which locators this node modifies,
    // and contains a count of all 'loc_only_reads' dependent on
    // this modification.
    //fc::vector_map<locator_ht, unsigned> loc_writes;
};

class aco_t
{
public:
    struct candidate_t
    {
        unsigned worklist_i;
        unsigned cost;
        unsigned weight;
    };
private:

    ir_t& ir;
    cfg_ht cfg_node;

    // TODO: thread_local

    // 'nodes' holds all nodes to be ordered.
    std::vector<ssa_ht> nodes;


    ant_t ant;
    ant_t best_ant;
public:
private:
    void aco_t();
    void run():
    void run_ant();

    template<bool Execute>
    unsigned cost_execute(sel_t sel);
}




class scheduler_t
{
public:
    explicit scheduler_t(ir_t& ir);
private:
    // Assigns 'deps' and 'index' for each ssa_node in cfg_node.
    // Returns all nodes in the cfg_node, toposorted
    std::vector<ssa_ht> build_deps(cfg_ht cfg_node);

private:
    ir_t& ir;
    array_pool_t<bitset_uint_t> bitset_pool;
    array_pool_t<float> float_pool;
};

void propagate_deps_change(ssa_schedule_d const& changed_d)
{
    for(ssa_ht ssa_node : toposorted)
    {
        auto& d = data(ssa_node);
        if(bitset_test(d.deps, changed_d.index))
            bitset_or(set_size, d.deps, changed_d.deps);
    }
}

std::vector<ssa_ht> build_deps(cfg_ht cfg_node)
{
    unsigned const set_size = bitset_size<>(cfg_it->ssa_size());

    std::vector<ssa_ht> toposorted(cfg_node->ssa_size());
    toposort_cfg_node(cfg_node, toposorted.data());

    for(unsigned i = 0; i < toposorted.size(); ++i)
        data(toposorted[i]).index = i;

    for(ssa_ht ssa_node : toposorted)
    {
        // Ignore phi node deps. They can introduce cycles.
        if(ssa_node->op() == SSA_phi)
            continue;

        auto& d = data(ssa_node);
        d.deps = bitset_pool.alloc(set_size);
        d.exrta_deps = bitset_pool.alloc(set_size);

        // Assign deps based on all inputs:
        for_each_node_input(ssa_node, [this, &d](ssa_ht input)
        {
            if(input->cfg_node() != cfg_node)
                return;
            auto& input_d = data(input);
            bitset_set(d.deps, input_d.index);
            bitset_or(set_size, d.deps, input_d.deps);
        });

        // Daisy inputs are deps too:
        if(ssa_ht prev = ssa_node->prev_daisy())
        {
            auto& prev_d = data(prev);
            bitset_set(d.deps, prev_d.index);
            bitset_or(set_size, d.deps, prev_d.deps);
        }
    }

    // Now add extra deps to aid scheduling efficiency.

    // In chains of carry operations, setup deps to avoid cases where
    // a carry would need to be stored.
    bitset_uint_t* temp_set = ALLOCA_T(bitset_uint_t, set_size);
    for(auto it = toposorted.rbegin(); it != toposorted.rend(); ++it)
    {
        ssa_ht ssa_node = *it;

        // Determine if this node produces a carry used by a single output.
        ssa_ht carry_out = {};
        for(unsigned i = 0; i < ssa_node->output_size(); ++i)
        {
            auto oe = ssa_node->output_edge(i);
            if(oe.input_edge() != INPUT_CARRY)
                continue;
            if(carry_out)
                goto next_iter;
            carry_out = oe.handle;
        }
        if(carry_out->cfg_node() != cfg_node)
            next_iter: continue;

        // OK! This node produces a carry used by a single output.

        auto& carry_d = data(carry_out);

        // 'temp_set' will hold all deps we'll try adding to 'd.deps':
        for(unsigned i = 0; i < set_size; ++i)
            temp_set[i] = carry_d.deps[i] & ~d.deps[i] & carry_clobberers[i];
        bitset_clear(temp_set, d.index);

        // Can't add a dep if a cycle would be created:
        unsigned const index = d.index;
        if(bitset_for_each_test(set_size, temp_set, [index](unsigned bit)
           { return !bitset_test(data(toposorted[bit]).deps, index); }))
        {
            // Found cycle.
            continue;
        }

        // Add em':
        bitset_or(set_size, d.deps, temp_set);

        // Propagate the change
        for(ssa_ht prop : toposorted)
            if(bitset_test(data(prop).deps, d.index))
                bitset_or(set_size, data(prop).deps, d.deps);
    }

    // If a node's result will be stored in a locator eventually,
    // it should come after previous writes/reads to that locator.
    for(ssa_ht ssa_node : toposorted)
    {
        for(unsigned i = 0; i < ssa_node->output_size(); ++i)
        {
            auto oe = ssa_node->output_edge(i);
            if(!is_locator_write(oe))
                continue;

            locator_t const loc = oe.handle->input(oe.index + 1).locator();

            assert(oe.handle->is_daisy());

            // Find the previous reader/writer of 'loc':
            for(ssa_ht daisy = oe.handle->prev_daisy(); daisy; --daisy)
            {
                if(!(ssa_flags(h->op()) & SSAF_WRITE_GLOBALS))
                    continue;

                assert(daisy->cfg_node() == cfg_node);

                if(locator_input(daisy, loc) >= 0
                   || locator_output(daisy, loc) >= 0)
                {
                    // Can't add a dep if a cycle would be created:
                    if(bitset_test(daisy_d.deps, d.index))
                        break;

                    // Add a dep!
                    auto& daisy_d = data(daisy);
                    bitset_set(d.deps, daisy_d.index);
                    bitset_or(set_size, d.deps, daisy_d.deps);

                    // Propagate the change
                    for(ssa_ht prop : toposorted)
                        if(bitset_test(data(prop).deps, d.index))
                            bitset_or(set_size, data(prop).deps, d.deps);

                    break;
                }
            }
        }
    }

    return toposorted;
}

scheduler_t::scheduler_t(ir_t& ir)
: ir(ir)
{
    for(cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& cd = data(cfg_it);
        cd.nodes = build_deps(cfg_it);
        unsigned const set_size = bitset_size<>(cfg_it->ssa_size());
        cd.scheduled = bitset_pool.alloc(set_size);

        // Allocate initial edge pheramones:
        unsigned const nodes_size = cd.nodes.size();
        for(unsigned i = 0; i < nodes_size; ++i)
        {
            auto& d = data(cd.nodes[i]);
            d.edge_phera = float_pool.alloc(nodes_size);
            constexpr intial_phera = 10.0f;
            std::fill_n(d.edge_phera, nodes_size, initial_phera);
        }
    }
}

/*
void scheduler_t::run()
{
    constexpr unsigned trips = 32;
    constexpr unsigned ants_per_colony = 16;
    constexpr float evap_retain_rate = 0.875f;

    float const recip_num_nodes = 1.0f / (float)toposorted.size();

    for(unsigned t = 0; t < trips; ++t)
    {
        for(unsigned a = 0; a < ants_per_colony; ++a)
        {
            run_ant();
            if(ant.cost < best_ant.cost)
                best_ant = ant;
        }

        // Evaporate pheramones:
        for(ssa_ht h : toposorted)
        {
            auto& d = data(h);

            for(unsigned i = 0; i < nodes.size(); ++i)
                d.edge_phera[i] *= evap_retain_rate;
        }

        // Apply pheramones along best path:
        float const add_amount = (float)best_ant.cost * recip_num_nodes;
        float** prev_edge_phera = &start_edge_phera;
        for(ssa_ht h : best_ant.path)
        {
            auto& d = data(h);
            (*prev_edge_phera)[d.to_order_index] += add_amount;
            prev_d = &d.edge_phera;
        }
    }
}

void scheduler_t::run_ant()
{
    // Reset the ant:
    ant.cost = 0;
    ant.schedule.clear();

    // initialize ready:
    ready = starting_ready;

    float** prev_edge_phera = &start_edge_phera;
    unsigned const num_nodes = to_order.size();
    while(ant.schedule.size() < num_nodes)
    {
        assert(ready.size() > 0);

        unsigned total_weight = 0;
        for(unsigned i = 0; i < num_ready; ++i)
        {
            ssa_ht ready = worklist[i];
            auto& d = data(ready);
            unsigned const cost = calc_cost(ready);
            float const pheramones = (*prev_edge_phera)[d.to_order_index];
            unsigned const weight = static_cast<unsigned>(
                (16.0f * pheramones) / (float)(1 + cost * cost));

            total_weight += weight;
            candidates.push_back({ i, cost, weight });
        }

        assert(candidates.size() > 0);

        unsigned candidate_i = 0;
        if(total_weight > 0)
        {
            if(gen() & 1)
            {
                // 50% chance to just take the best:
                for(unsigned i = 1 i < candidates.size(); ++i)
                    if(candidates[i].weight > candidates[candidate_i].weight)
                        candidate_i = i;
            }
            else
            {
                // otherwise pick a random weighted choice:
                std::uniform_int_distribution<unsigned> 
                    distrib(0, total_weight-1);
                unsigned roll = distrib(gen);
                do
                {
                    assert(candidate_i < candidates.size());
                    roll -= candidates[candidate_i].weight;
                    ++candidate_i;
                }
                while(roll >= 0);
                --candidate_i;
            }
        }
        
        // Ok! the next path step has been chosen.

        candidate_t const& chosen = candidates[candidate_i];
        auto& d = data(chosen);

        constexpr float local_retain_rate = 0.75f;

        // Reduce the pheramones along this path:
        (*prev_edge_phera)[d.index] *= local_retain_rate;

        // Update the ant's path:
        ant.cost += chosen.cost;
        ant.path.push_back(chosen);

        // Apply the effect:
        do_choice(chosen);

        // Update the worklist, first removing the old node:
        std::swap(worklist[choice.worklist_i], worklist.back());
        worklist.pop_back();
        --num_ready;
        // Then find newly ready nodes:
        update_ready(worklist, num_ready);

    }
}

void update_ready(std::vector<ssa_ht>& worklist, unsigned& num_ready)
{
    // Then add new ready nodes:
    for(unsigned i = num_ready; i < worklist.size(); ++i)
    {
        auto& d = data(worklist[i]);
        for(unsigned i = 0; i < toposorted_set_size; ++i)
            if(d.deps[i] & to_order_set[i] & ~scheduled_set[i])
                goto not_ready;
        std::swap(worklist[num_ready], worklist[i]);
        ++num_ready;
    not_ready:;
    }
}

unsigned calc_cost(ssa_ht h)
{
    unsigned cost = 0;
    auto& d = data(h);

    for(auto const& pair : d.loc_writes)
    {
        live_t const& live = loc_live[pair.first.value];
        if(live.node && live.readers_left > 0)
            cost += ir.locators.size_of(pair.first);
    }

    return cost;
}

void do_choice(candidate_t const& candidate)
{
    ssa_ht node = worklist[candidate.worklist_i];
    auto& d = data(node);

    bitset_set(scheduled, d.index);

    for(auto const& pair : d.loc_only_reads)
    {
        locator_ht loc = pair.second;
        if(pair.first == loc_live[loc.value].node)
        {
            assert(loc_live[loc.value].readers_left > 0);
            loc_live[loc.value].readers_left -= 1;
        }
    }

    for(auto const& pair : d.loc_writes)
        loc_live[pair.first.value] = { node, pair.second };
}








void schedule(ir_t& ir)
{
    // Whenever a 'read_global' outputs into a 'write_global',
    // and the locators are different,
    // insert a copy, splitting the edge.
    for(cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->op() != SSA_read_globals)
            continue;

        locator_ht loc = ssa_it->input(1).locator();
        ssa_ht copy = {};

        for(unsigned i = 0; i < ssa_it->output_size(); ++i)
        {
            auto oe = ssa_it->output_edge(i);
            if(oe.handle->op() != SSA_write_globals)
                continue;
            if(loc != oe.handle->input(oe.index ^ 1).locator())
                continue;
            if(!copy)
                copy = cfg_it->emplace_ssa(SSA_copy, ssa_it->type(), ssa_it);
            oe.handle->link_change_input(oe.index, copy);
        }

    }

    cfg_data_pool::scope_guard_t<cfg_schedule_d> cg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_schedule_d> sg(ssa_pool::array_size());

    // TODO
}




/*








struct ssa_aco_d
{
    unsigned index;
    unsigned num_variations;
    
    // If the node has a single output that uses either its carry or value,
    // it will be tracked in these handles:
    ssa_ht single_carry_use;
    ssa_ht single_value_use;

    bitset_uint_t* deps;

    // Artificially-added dependencies which entice this node
    // to be scheduled after some other nodes.
    // These may introduce cycles in the graph, but that's okay
    // because the scheduler doesn't have to listen to them.
    bitset_uint_t* suggested_deps;

    float* edge_phera;
    float* variation_phera;
};




template<typename Policy>
class aco_t
{
public:
    struct candidate_t
    {
        ssa_ht node;
        unsigned variation;
        unsigned cost;
        unsigned weight;
    };
private:
    struct path_node_t
    {
        ssa_ht node;
        unsigned variation;
    };


    struct ant_t
    {
        std::vector<path_node_t> path;
        unsigned cost = 0;
    };

    cfg_ht cfg_node;
    float* start_edge_phera;
    std::vector<ssa_ht> toposorted;
    ant_t ant;
    ant_t best_ant;
public:
private:
    void aco_t();
    void run():
    void run_ant();

    template<bool Execute>
    unsigned cost_execute(sel_t sel);
}

aco_t::aco_t()
{
    set_size = bitset_size<>(ssa_pool::array_size());
    // TODO: resize ssa_data ?

    std::vector<ssa_ht> toposorted;
    for(cfg_ht cfg_it = ir.ssa_begin(); cfg_it; ++cfg_it)
    {
        toposorted.resize(cfg_it->ssa_size());
        toposort_cfg_node(cfg_it, toposorted.data());

        for(unsigned i = 0; i < toposorted.size(); ++i)
            toposorted[i].data<TODO>().index = i;

        for(ssa_ht ssa_node : toposorted)
        {
            auto& d = ssa_node.data<TODO>();

            d.deps = bitset_pool.alloc(set_size);
            d.suggested_deps = bitset_pool.alloc(set_size);

            // Set 'single_value_use' and 'single_carry_use',
            // if they exist for this node.
            {
                unsigned value_used_count = 0;
                unsigned carry_used_count = 0;

                unsigned const output_size = ssa_node->output_size();
                for(unsigned i = 0; i < output_size; ++i)
                {
                    if(value_used_on_output(*ssa_node, i))
                    {
                        d.single_value_use = ssa_node->output(i);
                        ++value_used_count;
                    }
                     
                    if(carry_used_on_output(*ssa_node, i))
                    {
                        d.single_carry_use = ssa_node->output(i);
                        ++carry_used_count;
                    }
                }

                if(value_used_count != 1)
                    d.single_value_use = {};

                if(carry_used_count != 1)
                    d.single_carry_use = {};
            }

            // Assign deps based on all inputs:
            unsigned const input_size = ssa_node->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = ssa_it->input(i);
                if(!input_v.holds_ref())
                    continue;

                ssa_ht input = input_v.handle();
                auto& input_d = input_v.handle().data<TODO>();

                if(input->cfg_node() == cfg_it)
                {
                    bitset_set(d.deps, input_d.index);
                    bitset_or(set_size, d.deps, input_d.deps);
                }

                if(ssa_node->op() == SSA_write_globals)
                {
                    // Get the associated locator:
                    locator_ht loc = ssa_node->input(i ^ 1).locator();

                    // Add it to a vector:
                    if(input->cfg_node() == cfg_it)
                        first_loc_users[loc.value].push_back(ssa_node);
                    else
                        later_loc_users[loc.value].push_back(ssa_node);
                }

            }
        }

        // Now add extra dependencies to aid the scheduler.

        // For each locator, everything in 'later_loc_users' depends on
        // everything in 'first_loc_users'.
        for(unsigned i = 0; i < ir.locators.size(); ++i)
        {
            for(ssa_ht later : later_loc_users[i])
            {
                auto& d = later.data<TOOD>();
                for(ssa_ht first : first_loc_users[i])
                    if(first != later)
                        bitset_set(d.suggested_deps, first.data<TODO>().index);
            }
        }

        TODO: the other extra dependency check

        // For each TODO
        if(ssa_it->op() != SSA_fn_call)
            continue;

        for(unsigned i = 0; i < ssa_it->output_size(); ++i)
        {
            ssa_ht output = ssa_it->output(i);
            if(output->op() != SSA_read_global)
                continue;

            for(unsigned j = 0; j < output->output_size(); ++j)
            {
                ssa_ht output_output = output->output(j);

            }
        }
    }
}



void aco_t::run()
{
    constexpr unsigned trips = 32;
    constexpr unsigned ants_per_colony = 16;
    constexpr float evap_retain_rate = 0.875f;

    float const recip_num_nodes = 1.0f / (float)toposorted.size();

    for(unsigned t = 0; t < trips; ++t)
    {
        for(unsigned a = 0; a < ants_per_colony; ++a)
        {
            run_ant();
            if(ant.cost < best_ant.cost)
                best_ant = ant;
        }

        // Evaporate pheramones:
        for(ssa_ht h : toposorted)
        {
            auto& d = data(h);

            for(unsigned i = 0; i < nodes.size(); ++i)
                d.edge_phera[i] *= evap_retain_rate;

            for(unsigned i = 0; i < d.num_variations; ++i)
                d.variation_phera[i] *= evap_retain_rate;
        }

        // Apply pheramones along best path:
        float const add_amount = (float)best_ant.cost * recip_num_nodes;
        float** prev_edge_phera = &start_edge_phera;
        for(path_node_t const& pn : best_ant.path)
        {
            auto& d = data(pn.node);
            (*prev_edge_phera)[d.index] += add_amount;
            d.variation_phera[pn.variation] += add_amount;
            prev_d = &d.edge_phera;
        }
    }
}


void aco_t::run_ant()
{
    // Reset the ant:
    ant.cost = 0;
    ant.schedule.clear();

    // initialize ready:
    ready = starting_ready;

    float** prev_edge_phera = &start_edge_phera;
    unsigned const num_nodes = toposorted.size();
    while(ant.schedule.size() < num_nodes)
    {
        assert(ready.size() > 0);

        unsigned total_weight = 0;
        for(ssa_ht h : policy.ready)
        {
            auto& d = data(h);
            auto sels = possible_selections(h);

            unsigned const num_variations = d.num_variations;
            for(unsigned v = 0; v < num_variations; ++v)
            {
                if(!sels[v])
                    continue;

                unsigned const cost = calc_cost(h, sels[v]);
                float const pheramones = 
                    (*prev_edge_phera)[i] + d.varaition_phera[v];
                unsigned const weight = 1 + static_cast<unsigned>(
                    pheramones / (float)(cost * cost));

                total_weight += weight;
                candidates.push_back({ h, v, cost, weight });
            }
        }

        assert(candidates.size() > 0);

        unsigned candidate_i = 0;
        if(gen() & 1)
        {
            // 50% chance to just take the best:
            for(unsigned i = 1 i < candidates.size(); ++i)
                if(candidates[i].weight > candidates[candidate_i].weight)
                    candidate_i = i;
        }
        else
        {
            // otherwise pick a random weighted choice:
            std::uniform_int_distribution<unsigned> distrib(0, total_weight-1);
            unsigned roll = distrib(gen);
            do
            {
                assert(candidate_i < candidates.size());
                roll -= candidates[candidate_i].weight;
                ++candidate_i;
            }
            while(roll >= 0);
            --candidate_i;
        }
        
        // Ok! the next path step has been chosen.

        candidate_t const& chosen = candidates[candidate_i];
        auto& d = data(chosen);

        constexpr float local_retain_rate = 0.75f;

        // Reduce the pheramones along this path:
        (*prev_edge_phera)[d.index] *= local_retain_rate;
        d.variation_phera[chosen.variation] *= local_retain_rate;

        // Update the ant's path:
        ant.cost += chosen.cost;
        ant.path.push_back(chosen);

        // Apply the effect:
        do_choice(chosen);
    }
}

unsigned calc_cost(ssa_ht h, selection_t sel)
{

}

struct policy
{
    static constexpr bool USE_VARIATIONS = false;
    static constexpr unsigned TRIPS = 64;
    static constexpr unsigned ANTS_PER_COLONY = 8;
    static constexpr float EVAP_RETAIN_RATE  = 0.875f;
    static constexpr float LOCAL_RETAIN_RATE = 0.875f;

    struct
    {
        ssa_ht node;
        std::vector<locator_t> 
        std::vector<locator_t> outputs;
    };

    std::vector<unsigned> ready;
    std::vector<ssa_ht> fns;
    std::vector<ssa_value_t> live;

    inline static unsigned num_nodes() { return fns.size(); }
    [[gnu::always_inline]]
    inline static unsigned num_variations(unsigned) { return 0; }

    unsigned cost(unsigned i)
    {
        unsigned ret = 0;
        for_each_locator_write(fns[i],
        [this, &ret](locator_ht loc, ssa_value_t v)
        {
            if(!v.holds_ref())
                return;

            if(v->op() != SSA_read_global)
                return;

            if(live[loc.value] != v)
                ret += ir.locators.size_of(loc);
        });
        return ret;
    }

    void do_choice(aco_choice_t choice)
    {
        // Update all 'live':
        for(

    }

    void made_choice(aco_candidate_t const& chosen)
    {
        // update the ready set: todo
        bitset_set(scheduled_set, d.index);
        unsigned const output_size = chosen.node->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_ht output = chosen.node->output(i);
            if(output->cfg_node() != cfg_node)
                continue;
            if(output->test_flags(flag_in_worklist))
               continue;
            auto& output_d = data(output);
            if(bitset_test(scheduled_set, output_d.index))
                continue;
            for(unsigned j = 0; j < node_set_size; ++j)
                if(output_d.deps[j] & ~scheduled_set[j])
                    goto not_ready;
            ready.push_back(output);
            output->set_flags(flag_in_worklist);
        not_ready:;
        }
    }

};

        */

        /*
        // Partially build 'loc_writes', finding which globals
        // will be written to after the operation executes.
        for(ssa_ht ssa_node : cd.nodes)
        {
            ssa_ht head = ssa_it;
            auto* sd = &data(head);

            if(ssa_node->op() == SSA_read_global)
            {
                head = ssa_node->input(0).handle();
                assert(head);
                sd = &data(head);
            }

            for(unsigned i = 0; i < ssa_it->output_size(); ++i)
            {
                auto oe = ssa_node->output_edge(i);
                if(oe.handle->op() != SSA_write_globals)
                    continue;

                locator_ht loc = oe.handle->input(oe.index ^ 1).locator();
                sd->loc_writes.emplace(loc, 0);

                if(oe.handle->cfg_node() != cfg_it)
                {
                    auto result = cd.loc_inputs.emplace(loc, oe.handle);
                    if(!result.second)
                    {
                        if(ir.locators.size_of(loc)
                           < ir.locators.size_of(result.first->first))
                        {
                            result.first.underlying->first = 
                        }
                    }
                }
            }
        }

        // Now build 'loc_only_reads', which is every global locator
        // read from, but not written to by this operation.
        // Also finalize 'loc_writes', adding the read count.
        for(ssa_ht ssa_node : cd.nodes)
        {
            if(ssa_node->op() == SSA_write_globals)
            {
                assert(ssa_node->output_size() == 1);
                assert(ssa_node->output(0));

                ssa_ht head = ssa_it;
                auto& sd = data(head);

                for_each_written_global(ssa_node,
                [&sd](ssa_value_t v, locator_ht loc)
                {
                    if(!v.holds_ref())
                        return;
                    auto result = sd.loc_only_reads.emplace(v.handle(), loc);
                    assert(result.second);
                });
            }
            else
            {
                auto& sd = data(ssa_it);
                for_each_node_input(ssa_node, [](ssa_ht input)
                {
                    if(input->op() != SSA_read_globals)
                        return;
                    if(sd->loc_writes.count(loc) != 0)
                        return;
                    locator_ht loc = input->input(1).locator();
                    if(sd->loc_only_reads.emplace(input, loc).second)
                        data(input->input(0).handle()).loc_writes[loc] += 1;
                });
            }

            // Build 'gmod_nodes' here:
            auto& sd = data(ssa_it);
            if(sd.loc_writes.size() || sd.loc_only_reads.size())
            {
                cd.gmod_nodes.push_back(ssa_node);
                bitset_set(cd.gmod_nodes_set, sd.index);
            }
        }

        // Sort 'gmode_nodes', putting all ready nodes at the front,
        // and all other nodes at the back.
        assert(initial_num_ready == 0);
        update_ready(cd.gmod_nodes, cd.initial_num_ready);
        */
