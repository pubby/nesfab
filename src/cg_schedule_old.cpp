#include "cg_schedule.hpp"

#include "alloca.hpp"
#include "array_pool.hpp"
#include "bitset.hpp"
#include "worklist.hpp"

namespace 
{

class scheduler_t
{
public:
    std::vector<ssa_ht> schedule;
private:

    static thread_local array_pool_t<bitset_uint_t> m_bitset_pool;
    static thread_local std::vector<ssa_scheduler_d> m_scheduler_data;

    cfg_ht m_cfg_h;
    ssa_ht m_carry_input_waiting;

    unsigned m_bitset_size;

    // Each SSA node in the CFG node that has been scheduled.
    bitset_uint_t* m_scheduled_bitset;

    // All SSA nodes in a given CFG node that don't clobber the carry:
    bitset_uint_t* m_carry_preserve_bitset;

    int& index(ssa_ht h) const { return h.data<ssa_cg_d>().index; }

    ssa_scheduler_d& get_data(ssa_ht h) const
    {
#ifdef NDEBUG
        return m_scheduler_data[h.data<ssa_cg_d>().index];
#else
        return m_scheduler_data.at(h.data<ssa_cg_d>().index);
#endif
    }

    ssa_ht successor_search(ssa_ht last_scheduled_h) const;
    ssa_ht full_search(bool relax) const;
    bool ready(ssa_ht h, bitset_uint_t const* scheduled_bitset, 
               bool relax) const;
    int path_length(ssa_ht h, bitset_uint_t const* scheduled_bitset, 
                    bool relax) const;

public:
    explicit scheduler_t(cfg_ht cfg_h);

};

thread_local array_pool_t<bitset_uint_t> scheduler_t::m_bitset_pool;
thread_local std::vector<scheduler_t::ssa_scheduler_d> 
             scheduler_t::m_scheduler_data;

scheduler_t::scheduler_t(cfg_ht cfg_h)
: m_cfg_h(cfg_h)
{
    m_bitset_pool.clear();

    m_bitset_size = bitset_size<bitset_uint_t>(m_cfg_h->ssa_size());
    m_scheduled_bitset      = m_bitset_pool.alloc(m_bitset_size);
    m_carry_preserve_bitset = m_bitset_pool.alloc(m_bitset_size);
    assert(bitset_all_clear(m_bitset_size, m_scheduled_bitset));
    assert(bitset_all_clear(m_bitset_size, m_carry_preserve_bitset));

    m_scheduler_data.resize(m_cfg_h->ssa_size());

    // Initialize data:
    unsigned bitset_i = 0;
    for(ssa_ht ssa_it = m_cfg_h->ssa_begin(); ssa_it; ++ssa_it, ++bitset_i)
    {
        ssa_node_t& ssa_node = *ssa_it;
        index(ssa_it) = bitset_i;
        auto& ssa_data = m_scheduler_data[bitset_i];

        ssa_data.deps = m_bitset_pool.alloc(m_bitset_size);
        assert(bitset_all_clear(m_bitset_size, ssa_data.deps));

        if(!(ssa_flags(ssa_node.op()) & SSAF_CLOBBERS_CARRY))
            bitset_set(m_carry_preserve_bitset, bitset_i);

        int carry_i = carry_input(ssa_node);
        if(carry_i >= 0)
        {
            // Mark this as the input's 'carry_user',
            // Overwriting any previous values, if they exist.
            ssa_value_t carry_v = ssa_node.input(carry_i);
            if(carry_v.holds_ref() && carry_v->cfg_node() == m_cfg_h)
                get_data(carry_v.handle()).carry_user = ssa_it;
        }
    }
    assert(bitset_i == cfg_h->ssa_size());

    ssa_worklist::clear();

    // Now build the dependency sets in a two part process.
    // First, initialize all deps to immediate inputs.
    for(ssa_ht ssa_it = m_cfg_h->ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        // Phi nodes and arguments always come first and have no deps.
        if(ssa_it->op() == SSA_phi
           || ssa_it->op() == SSA_argument)
        {
            schedule.push_back(ssa_it);
            ssa_it->set_flags(FLAG_PROCESSED);
            bitset_set(m_scheduled_bitset, index(ssa_it));
            continue;
        }

        auto& data = get_data(ssa_it);

        unsigned const input_size = ssa_it->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t input_v = ssa_it->input(i);
            if(input_v.holds_ref() && input_v->cfg_node() == m_cfg_h)
                bitset_set(data.deps, index(input_v.handle()));
        }

        ssa_worklist::push(ssa_it);
    }

    bitset_uint_t* old_deps = ALLOCA_T(bitset_uint_t, m_bitset_size);

    // Then run until a fixed-point is reached, propagating deps along outputs.
    while(!ssa_worklist::empty())
    {
        ssa_ht h = ssa_worklist::pop();
        ssa_node_t& node = *h;
        auto& data = get_data(h);

        assert(node.op() != SSA_phi);

        bitset_copy(m_bitset_size, old_deps, data.deps);

        // The new 'deps' is the union of all input 'deps' with itself.
        unsigned const input_size = node.input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t input_v = node.input(i);
            if(input_v.holds_ref() && input_v->cfg_node() == m_cfg_h)
            {
                bitset_or(m_bitset_size, data.deps,
                          get_data(input_v.handle()).deps);
            }
        }

        // If 'deps' changed, push all output nodes onto the worklist.
        if(!bitset_eq(m_bitset_size, old_deps, data.deps))
        {
            unsigned const output_size = node.output_size();
            for(unsigned i = 0; i < output_size; ++i)
            {
                ssa_ht output = node.output(i);
                if(output->cfg_node() == m_cfg_h && output->op() != SSA_phi)
                    ssa_worklist::push(node.output(i));
            }
        }
    }

    // OK! Everything was initialized. Now to run the greedy algorithm.

    m_carry_input_waiting = {};
    ssa_ht candidate_h = {};
    for(unsigned i = schedule.size() + 1; i < m_cfg_h->ssa_size(); ++i)
    {
        // First priority: try to find a successor node that's ready:
        if(candidate_h)
            candidate_h = successor_search(candidate_h);

        // Second priority: try to find *any* node that's ready:
        if(!candidate_h)
            candidate_h = full_search(false);

        // Third priortiy: try to find *any* node that can be scheduled,
        // with relaxed constraints:
        if(!candidate_h)
            candidate_h = full_search(true);

        // OK, we should definitely have a candidate_h now.
        assert(candidate_h);

        // Schedule it:
        assert(!candidate_h->test_flags(FLAG_PROCESSED));
        candidate_h->set_flags(FLAG_PROCESSED);
        bitset_set(m_scheduled_bitset, index(candidate_h));
        if(candidate_h->op() != SSA_fence)
            schedule.push_back(candidate_h);

        // If this node inputs a carry, stop tracking it:
        if(carry_input(*candidate_h) >= 0)
            m_carry_input_waiting = {};

        // If this node outputs a carry, track it:
        if(ssa_ht carry_user = get_data(candidate_h).carry_user)
            m_carry_input_waiting = carry_user;
    }

    // Schedule the 'exit' node last:
    assert(m_cfg_h->exit);
    schedule.push_back(m_cfg_h->exit);

    // Finally, re-assign 'index' to hold the position in the schedule:

    for(unsigned i = 0; i < schedule.size(); ++i)
        index(schedule[i]) = i;
}

ssa_ht scheduler_t::successor_search(ssa_ht last_scheduled_h) const
{
    int best_path_length = -1;
    ssa_ht best_h = {};

    unsigned const output_size = last_scheduled_h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht succ_h = last_scheduled_h->output(i);

        if(succ_h->cfg_node() != m_cfg_h)
            continue;

        if(ready(succ_h, m_scheduled_bitset, false))
        {
            int l = path_length(succ_h, m_scheduled_bitset, false);
            if(l > best_path_length)
            {
                best_path_length = l;
                best_h = succ_h;
            }
        }
    }

    return best_h;
}

ssa_ht scheduler_t::full_search(bool relax) const
{
    int best_path_length = -1;
    ssa_ht best_h = {};

    for(ssa_ht ssa_it = m_cfg_h->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(!ready(ssa_it, m_scheduled_bitset, relax))
            continue;

        int l = path_length(ssa_it, m_scheduled_bitset, relax);
        if(l > best_path_length)
        {
            best_path_length = l;
            best_h = ssa_it;
        }
    }

    return best_h;
}

bool scheduler_t::ready(ssa_ht h, bitset_uint_t const* scheduled_bitset, 
                        bool relax) const
{
    assert(h->cfg_node() == m_cfg_h);

    if(h->test_flags(FLAG_PROCESSED)) // If already scheduled.
        return false;

    if(h == m_cfg_h->exit) // Exit is always scheduled last.
        return false;

    // A node is ready when all of its inputs are scheduled.
    for(unsigned i = 0; i < m_bitset_size; ++i)
        if((get_data(h).deps[i] & ~scheduled_bitset[i]) != 0)
            return false;

    if(relax)
        return true;

    // If a carry is live, we can't schedule any carry-clobbering ops.
    if(m_carry_input_waiting && h != m_carry_input_waiting
       && (ssa_flags(h->op()) & SSAF_CLOBBERS_CARRY))
    {
        return false;
    }

    // If the node's output is used as the carry of another node,
    // the node is only ready if the other node is ready too.
    if(ssa_ht carry_user = get_data(h).carry_user)
    {
        auto* new_bitset = ALLOCA_T(bitset_uint_t, m_bitset_size);
        bitset_copy(m_bitset_size, new_bitset, scheduled_bitset);
        // The second readiness check is slightly relaxed, in that it
        // only checks carry-clobbering operations:
        bitset_or(m_bitset_size, new_bitset, m_carry_preserve_bitset);
        // The second readiness check also assumes 'h' will be scheduled:
        bitset_set(new_bitset, index(h));
        return ready(carry_user, new_bitset, relax);
    }

    return true;
}

int scheduler_t::path_length(ssa_ht h, 
                             bitset_uint_t const* scheduled_bitset,
                             bool relax) const
{
    ssa_node_t& node = *h;

    // 'new_bitset' assumes 'h' will be scheduled:
    auto* new_bitset = ALLOCA_T(bitset_uint_t, m_bitset_size);
    bitset_copy(m_bitset_size, new_bitset, scheduled_bitset);
    bitset_set(new_bitset, index(h));
    
    int max_length = 0;
    int outputs_in_cfg_node = 0; // Number of outputs in the same CFG node.
    unsigned output_size = node.output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht output_h = node.output(i);
        ssa_node_t& output = *output_h;

        if(output.cfg_node() != node.cfg_node())
            continue;

        if(!ready(output_h, new_bitset, relax))
            continue;

        ++outputs_in_cfg_node;

        max_length = std::max(max_length, 
                              path_length(output_h, new_bitset, relax));
    }

    return max_length + std::max<int>(0, outputs_in_cfg_node - 1);
}

} // end anon namespace

schedule_t schedule_cfg_node(cfg_ht cfg_h)
{
    scheduler_t scheduler(cfg_h);
    return std::move(scheduler.schedule);
}


// TODO
// TODO
// TODO

/*

struct ssa_scheduler_d
{
    // Bitset of every node required to schedule this.
    // (This is deep - it counts dependencies of dependencies, and so on)
    bitset_uint_t* deps;

    // Bitset of every locator that's input or output by this node.
    // (These are shallow - they only counts in/out edges)
    bitset_uint_t* gvar_inputs;
    bitset_uint_t* gvar_outputs;
    unsigned gvar_outputs_size;

    // Position in 'to_do' vector:
    unsigned to_do_i;
};

class locator_scheduler_t
{
public:
    std::vector<ssa_ht> schedule;
private:
    unsigned locator_set_size;
    unsigned node_set_size;;

    bitset_uint_t* scheduled;
    bitset_uint_t* live;
    bitset_uint_t* live1;

    void backtrack_search(std::uint16_t const* outputs_remaining);
    bool ready(ssa_ht h) const;
};

void locator_scheduler_t::backtrack_search(
    std::uint16_t const* outputs_remaining)
{
    bitset_clear_all(locator_set_size, live);
    bitset_clear_all(locator_set_size, live1);
    for(unsigned i = 0; i < num_gvars; ++i)
    {
        if(outputs_remaining[i] > 0)
            bitset_set(live, i);
        if(outputs_remaining[i] == 1)
            bitset_set(live1, i);
    }

    auto next_outputs_remaining = ALLOCA_T(std::uint16_t, num_gvars);

    for(unsigned i = 0; i < to_do.size(); ++i)
    {
        auto& d = data(to_do[i]);

        if(!ready(d))
            continue;

        std::copy_n(outputs_remaining, num_locators, next_outputs_remaining);

        // If this node inputs a live var, decrement 'next_outputs_remaining'.
        if(d.inputs)
        {
            bitset_for_each_bit(locator_set_size, d.inputs,
            [next_live, next_live1, next_outputs_remaining](unsigned loc)
            {
                assert(next_outputs_remaining[loc] > 0);
                --next_outputs_remaining[loc];
            });
        }

        // If this node outputs a live var, set 'next_outputs_remaining'.
        if(d.outputs)
        {
            bitset_for_each_bit(locator_set_size, d.outputs,
            [next_outputs_remaining, &d](unsigned loc)
            {
                assert(next_outputs_remaining[loc] == 0);
                next_outputs_remaining[loc] = d.outputs_size;
            });
        }

        bitset_set(scheduled, d.node_i);
        if(backtrack_search(next_outputs_remaining))
        {
            schedule.push_back(to_do[i]);
            return true;
        }
        bitset_clear(scheduled, d.node_i);
    }

    return false;
}

bool locator_scheduler_t::ready(ssa_schedule_d& d) const
{
    // If the node was already scheduled, ignore it:
    if(bitset_test(scheduled, d.node_i))
        return false;

    // A node is ready when all of its inputs are scheduled:
    for(unsigned i = 0; i < node_set_size; ++i)
        if((d.deps[i] & ~scheduled[i]))
            return false;

    // If the node clobbers a variable that is currently live,
    // it's not ready, UNLESS it's the last the last use of said variable.
    for(unsigned i = 0; i < set_size; ++i)
        if(d.outputs[i] & live[i] & ~(d.inputs[i] & live1[i]))
            return false;

    // Otherwise it's ready!
    return true;
}

*/











using slot_ht = handle_t<unsigned, struct slot_ht_tag, ~0>;

struct sel_t // an instruction selection
{
    struct input_t;
    {
        // A null slot means the node is loaded through an addressing mode.
        slot_ht slot = {slot_ht::null};
        ssa_ht node = {ssa_ht::null};
    };

    sel_input_t* inputs;
    unsigned inputs_size;
    unsigned op_cost; // How many cycles this takes.
    float pheramones;
};

struct ssa_aco_d
{
    bitset_uint_t* deps;

    float* edge_pheramones;

    sel_t*; sels;
    unsigned sels_size;

    unsigned index;
};




    // Initialize data:
    unsigned index = 0;
    for(ssa_ht ssa_it = m_cfg_h->ssa_begin(); ssa_it; ++ssa_it, ++index)
    {
        auto& d = data(ssa_it);
        d.index = index;

        d.deps = bitset_pool.alloc(TODO);
        d.edge_pheramones = float_pool.alloc(TODO);

        // TODO: create sels?

        // TODO
        if(!(ssa_flags(ssa_node.op()) & SSAF_CLOBBERS_CARRY))
            bitset_set(m_carry_preserve_bitset, bitset_i);
    }
    assert(bitset_i == cfg_h->ssa_size());

    ssa_worklist::clear();

    // Now build the dependency sets in a two part process.
    // First, initialize all deps to immediate inputs.
    for(ssa_ht ssa_it = m_cfg_h->ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_it->clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        // Phi nodes and arguments always come first and have no deps.
        if(ssa_it->op() == SSA_phi
           || ssa_it->op() == SSA_argument)
        {
            schedule.push_back(ssa_it);
            ssa_it->set_flags(FLAG_PROCESSED);
            bitset_set(m_scheduled_bitset, index(ssa_it));
            continue;
        }

        auto& data = get_data(ssa_it);

        unsigned const input_size = ssa_it->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t input_v = ssa_it->input(i);
            if(input_v.holds_ref() && input_v->cfg_node() == m_cfg_h)
                bitset_set(data.deps, index(input_v.handle()));
        }

        ssa_worklist::push(ssa_it);
    }

    bitset_uint_t* old_deps = ALLOCA_T(bitset_uint_t, m_bitset_size);

    // Then run until a fixed-point is reached, propagating deps along outputs.
    while(!ssa_worklist::empty())
    {
        ssa_ht h = ssa_worklist::pop();
        ssa_node_t& node = *h;
        auto& data = get_data(h);

        assert(node.op() != SSA_phi);

        bitset_copy(m_bitset_size, old_deps, data.deps);

        // The new 'deps' is the union of all input 'deps' with itself.
        unsigned const input_size = node.input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t input_v = node.input(i);
            if(input_v.holds_ref() && input_v->cfg_node() == m_cfg_h)
            {
                bitset_or(m_bitset_size, data.deps,
                          get_data(input_v.handle()).deps);
            }
        }

        // If 'deps' changed, push all output nodes onto the worklist.
        if(!bitset_eq(m_bitset_size, old_deps, data.deps))
        {
            unsigned const output_size = node.output_size();
            for(unsigned i = 0; i < output_size; ++i)
            {
                ssa_ht output = node.output(i);
                if(output->cfg_node() == m_cfg_h && output->op() != SSA_phi)
                    ssa_worklist::push(node.output(i));
            }
        }
    }






class aco_t
{
private:
    struct path_node_t
    {
        ssa_ht node;
        unsigned sel_i;
    };

    struct ant_t
    {
        unsigned cost;
        std::vector<path_node_t> path;
    };

    cfg_ht cfg_node;
    ant_t ant;
    ant_t best_ant;
    float* starting_edge_pheramones;

    std::vector<ssa_ht> ready;
    std::vector<ssa_ht> starting_ready;

    std::vector<ssa_ht> nodes;
public:
private:
    void run():
    void run_ant();

    template<bool Execute>
    unsigned cost_execute(sel_t sel);
}

void aco_t::run()
{
    constexpr unsigned TRIPS = 64;
    constexpr unsigned ANTS_PER_COLONY = 64;

    for(unsigned t = 0; t < TRIPS; ++t)
    {
        for(unsigned a = 0; a < ANTS_PER_COLONY; ++a)
        {
            run_ant();
            if(ant.cost < best_ant.cost)
                best_ant = ant;
        }

        // Evaporate pheramones:
        for(ssa_ht& h : nodes)
        {
            constexpr float RETAIN_PERCENT = 0.75f;

            auto& d = data(h);

            for(unsigned i = 0; i < nodes.size(); ++i)
                d.edge_pheramones[i] *= retain_percent;

            for(unsigned i = 0; i < d.sels_size; ++i)
                d.sels[i].pheramones *= retain_percent;
            }
        }

        // apply pheramones along best path:
        // todo: precompute recip?
        float const amount = (float)best_ant.cost / (float)schedule.size();

        float** prev_edge_pheramones = &starting_edge_pheramones;

        for(path_node_t& p : best_ant.schedule)
        {
            auto& d = data(p.node);
            d.sels[d.sel_i] += amount;
            (*prev_edge_pheramones)[d.index] += amount;

            prev_edge_pheramones = &d.edge_pheramones
        }
    }
}

void aco_t::run_ant()
{
    struct candidate_t
    {
        ssa_ht node;
        unsigned sel_i;
        unsigned cost;
        float weight;
    };

    // reset the ant:
    ant.cost = 0;
    ant.schedule.clear();

    // initialize ready:
    ready = starting_ready;

    ssa_ant_d* prev_d = start_d;

    while(!ready.empty())
    {
        float total_weight = 0.0f;
        for(ssa_ht h : ready)
        {
            auto& d = data(h);
            assert(d.sels_size > 0);
            for(unsigned i = 0; i < d.sels_size; ++i)
            {
                sel_t& sel = d.sels[i];

                constexpr float base_pheramones = 1.0f;

                float const pheramones = 
                    (base_pheramones
                     + sel.pheramones 
                     + prev_d->edge_pheramones[d.index]);

                unsigned const cost = this->cost(h);
                float const weight = pheramones / (float)cost;

                total_weight += weight;
                candidates.push_back({ h, i, cost, weight });
            }
        }

        assert(candidates.size() > 0);

        unsigned chosen_i = 0;

        if(gen() & 1)
        {
            // 50% chance to just take the best:
            for(unsigned i = 1 i < candidates.size(); ++i)
                if(candidates[i].weight > candidates[chosen_i].weight)
                    chosen_i = i;
        }
        else
        {
            // otherwise pick a random weighted choice:
            std::uniform_real_distribution<float> distrib(0, total_weight);
            float roll = distrib(gen);
            while(roll > candidates[chosen_i].weight 
                  && chosen_i < candidates.size())
            {
                roll -= candidates[chosen_i].weight;
                ++chosen_i;
            }
        }
        
        // ok! the next path step has been chosen.

        candidate_t const& chosen = candidates[chosen_i];
        auto& d = data(chosen.node);
        sel_t& sel = d.sels[chosen.sel_i];

        // reduce the pheramones along this path.
        constexpr float retain_percent = 0.875;
        prev_d->edge_pheramones[d.index] *= retain_percent;
        sel.pheramones *= retain_percent;

        // update the ant's path:
        ant.cost += chosen.cost;
        ant.path.push_back({ chosen.node, chosen.sel_i });

        // update the liveness side effects: todo
        cost_execute<true>(sel);

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
}

template<bool execute>
unsigned aco_t::cost_execute(sel_t sel)
{
    unsigned cost = sel.op_cost;
    for(unsigned i = 0; i < sel.inputs_size; ++i)
    {
        ssa_ht h = sel.inputs[i].node;
        slot_ht slot = sel.inputs[i].slot;
        if(live[input.slot] != input.node)
        {

            if(input.slot)
            {
            // If the input isn't live, we have to load it:
                switch(input.slot)
                {
                case SLOT_A:
                    if(live[SLOT_X] == h || live[SLOT_Y] == h) // TXA, TYA
                    {
                        cost += op_cycles(TXA_IMPLIED);
                        break;
                    }
                    goto load_register;

                case SLOT_X:
                case SLOT_Y:
                    if(live[SLOT_A] == h) // TAX, TAY
                    {
                        cost += op_cycles(TAX_IMPLIED);
                        break;
                    }
                    // fall-through
                load_register:
                    if(data(h).stored_zp)
                        cost += op_cycles(LDA_ZERO_PAGE);
                    else if(h->op() == SSA_read_global
                       && live[get_slot(h->input(1).locator())] == h)
                    {
                        cost += op_cycles(LDA_ABSOLUTE);
                    }
                    else
                    {
                        if(Execute)
                            data(h).stored_zp = true;
                        cost += op_cycles(STA_ZERO_PAGE);
                        cost += op_cycles(LDA_ZERO_PAGE);
                    }
                    break;
                    
                case SLOT_C:
                    // Storing/loading carries is a huge EXPENSIVE mess...
                    if(!data(h).stored_carry)
                    {
                        if(Execute)
                            data(h).stored_carry = true;
                        cost += op_cycles(PHA_IMPLIED);
                        cost += op_cycles(PHP_IMPLIED);
                        cost += op_cycles(PLA_IMPLIED);
                        cost += op_cycles(STA_ZERO_PAGE);
                        cost += op_cycles(PLA_IMPLIED);
                    }
                    cost += op_cycles(PHA_IMPLIED);
                    cost += op_cycles(LDA_ZERO_PAGE);
                    cost += op_cycles(PHA_IMPLIED);
                    cost += op_cycles(PLP_IMPLIED);
                    cost += op_cycles(PLA_IMPLIED);
                    break;

                default: 
                    if(live[SLOT_A] == h 
                       || live[SLOT_X] == h 
                       || live[SLOT_Y] == h)
                    {
                        cost += op_cycles(STA_ABSOLUTE);
                    }
                    else
                    {
                        unsigned num_bytes = TODO;
                        if(!data(h).stored_zp)
                        {
                            if(Execute)
                                data(h).stored_zp = true;
                            cost += op_cycles(STA_ZERO_PAGE) * num_bytes;
                        }
                        cost += op_cycles(LDA_ZERO_PAGE) * num_bytes;
                    }
                    break;
                }

                live[input.slot] = input.node;
            }
            else
            {
                if(!data(h).stored_zp ||
                   (h->op() != SSA_read_global 
                    || live[get_slot(h->input(1).locator())] != h))
                {
                    if(Execute)
                        data(h).stored_zp = true;
                    cost += op_cycles(STA_ZERO_PAGE);
                }
            }
        }
    }

    return Execute ? 0 : cost;
}










///////




// We're trying to find a fn ordering for each basic block
// - Minimize stores


// Each node is defined by:
- a number of inputs
- a number of outputs (clobbers)
- a number of dependencies

// Each basic block is defined by:
- a set of inputs
- a set of outputs
- a set of live inputs
- a set of live outputs

for(ssa_ht ssa_it = cfg_it.ssa_begin(); ssa_it; ++ssa_it)
{
    if(ssa_it->op() != SSA_write_global)
        continue;

    ssa_value_t input_v = ssa_it->input(0);
    if(!input.holds_ref())
        continue;

    ssa_ht input = input_v.handle();
    if(input->cfg_node() != cfg_it)
        continue;
}



for(each SSA in CFG)
{
    if(SSA has INPUT not in CFG)
        add INPUT to BLOCK.INPUTS
}


{
    calc_liveness(ir);


}


// Create an ordering of all functions
