#include "cg_schedule.hpp"

#include <iostream> // TODO
#include <vector>

#include "alloca.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "ir_util.hpp"

namespace { // anon namespace

class scheduler_t
{
public:
    std::vector<ssa_ht> schedule;

    scheduler_t(ir_t& ir, cfg_ht cfg_node);
private:

    static inline thread_local array_pool_t<bitset_uint_t> bitset_pool;

    ir_t& ir;
    cfg_ht cfg_node;
    unsigned set_size = 0;

    ssa_ht carry_input_waiting;

    // Each SSA node in the CFG node that has been scheduled.
    bitset_uint_t* scheduled = nullptr;

    // All the SSA nodes that maybe clobber the carry.
    bitset_uint_t* carry_clobberers = nullptr;

    ssa_schedule_d& data(ssa_ht h) const
        { return cg_data(h).schedule; }
    unsigned& index(ssa_ht h) const { return data(h).index; }

    void append_schedule(ssa_ht h);
    void run();
    
    template<bool Relax>
    bool ready(ssa_ht h, bitset_uint_t const* scheduled) const;

    template<bool Relax>
    int path_length(ssa_ht h, bitset_uint_t const* scheduled) const;

    ssa_ht successor_search(ssa_ht last_scheduled) const;

    template<bool Relax>
    ssa_ht full_search() const;

    void calc_exit_distance(ssa_ht ssa, int exit_distance=0) const;
};

// 'exit_distance' will penalize nodes used by the exit SSA node,
// causing their work to be done closer to the exit.
void scheduler_t::calc_exit_distance(ssa_ht ssa, int exit_distance) const
{
    if(ssa->cfg_node() != cfg_node)
        return;

    if(data(ssa).exit_distance <= exit_distance)
        return;

    data(ssa).exit_distance = exit_distance++;

    if(ssa->op() != SSA_phi)
        for_each_node_input(ssa, [&](ssa_ht input){ calc_exit_distance(input, exit_distance); });
}

scheduler_t::scheduler_t(ir_t& ir, cfg_ht cfg_node)
: ir(ir)
, cfg_node(cfg_node)
{
    bitset_pool.clear();
    set_size = bitset_size<>(cfg_node->ssa_size());

    std::vector<ssa_ht> toposorted(cfg_node->ssa_size());
    toposort_cfg_node(cfg_node, toposorted.data());

    scheduled = bitset_pool.alloc(set_size);

    for(unsigned i = 0; i < toposorted.size(); ++i)
    {
        index(toposorted[i]) = i;
        data(toposorted[i]).deps = bitset_pool.alloc(set_size);
        data(toposorted[i]).exit_distance = INT_MAX / 4; // Some sufficiently large number.
    }

#ifndef NDEBUG
    for(ssa_ht it = cfg_node->ssa_begin(); it; ++it)
        assert(data(it).deps);
#endif

    // The last daisy in the cfg_node should be scheduled last.
    if(ssa_ht exit = cfg_node->last_daisy())
    {
        auto& exit_d = data(exit);
        assert(exit->output_size() == 0);
        for(ssa_ht ssa_node : toposorted)
            if(ssa_node != exit)
                bitset_set(exit_d.deps, index(ssa_node));
        calc_exit_distance(exit);
    }

    for(ssa_ht ssa_node : toposorted)
    {
        // Ignore phi node deps. They can introduce cycles.
        if(ssa_node->op() == SSA_phi)
            continue;

        auto& d = data(ssa_node);

        // Assign deps based on all inputs:
        for_each_node_input(ssa_node, [this, &d, ssa_node](ssa_ht input)
        {
            if(input->cfg_node() != this->cfg_node)
                return;
            assert(index(ssa_node) > index(input));
            bitset_set(d.deps, index(input));
            bitset_or(set_size, d.deps, data(input).deps);
        });

        // Daisy inputs are deps too:
        if(ssa_ht prev = ssa_node->prev_daisy())
        {
            bitset_set(d.deps, index(prev));
            bitset_or(set_size, d.deps, data(prev).deps);
        }
    }

    // Determine which ops will probably clobber the carry:
    // TODO: this could be made more precise.
    carry_clobberers = bitset_pool.alloc(set_size);
    for(ssa_ht ssa_node : toposorted)
        if(ssa_flags(ssa_node->op()) & SSAF_CLOBBERS_CARRY)
            bitset_set(carry_clobberers, index(ssa_node));


    // Now add extra deps to aid scheduling efficiency.
    auto propagate_deps_change = [&](ssa_ht changed)
    {
        for(ssa_ht ssa_node : toposorted)
        {
            auto& d = data(ssa_node);
            assert(d.deps);
            if(bitset_test(d.deps, index(changed)))
                bitset_or(set_size, d.deps, data(changed).deps);
        }
    };

    // In chains of carry operations, setup deps to avoid cases where
    // a carry would need to be stored.
    bitset_uint_t* temp_set = ALLOCA_T(bitset_uint_t, set_size);
    for(auto it = toposorted.rbegin(); it != toposorted.rend(); ++it)
    {
        ssa_ht ssa_node = *it;

        // Determine if this node produces a carry used by a single output.

        ssa_ht const carry = carry_output(*ssa_node);
        if(!carry || carry->output_size() != 1)
            continue;
        ssa_ht const carry_user = carry->output(0);

        // OK! This node produces a carry used by a single output.

        auto& d = data(ssa_node);
        unsigned const index_ = index(ssa_node);
        auto& carry_d = data(carry_user);

        d.carry_user = carry_user;

        // 'temp_set' will hold all deps we'll try adding to 'd.deps':
        for(unsigned i = 0; i < set_size; ++i)
            temp_set[i] = carry_d.deps[i] & ~d.deps[i] & carry_clobberers[i];
        bitset_clear(temp_set, index_);

        // Can't add a dep if a cycle would be created:
        bool const cycle = bitset_for_each_test(set_size, temp_set, 
        [index_, &toposorted, this](unsigned bit)
        { 
            assert(bit < toposorted.size());
            auto& d = data(toposorted[bit]);
            assert(d.deps);
            return !bitset_test(d.deps, index_); 
        });

        if(cycle)
            continue;

        // Add em':
        bitset_or(set_size, d.deps, temp_set);
        propagate_deps_change(ssa_node);
    }

    // If a node's result will be stored in a locator eventually,
    // it should come after previous writes/reads to that locator.
    for(ssa_ht ssa_node : toposorted)
    {
        auto& d = data(ssa_node);

        for(unsigned i = 0; i < ssa_node->output_size(); ++i)
        {
            auto oe = ssa_node->output_edge(i);
            if(!is_locator_write(oe))
                continue;

            locator_t const loc = oe.handle->input(oe.index + 1).locator();

            assert(oe.handle->in_daisy());

            // Find the previous reader/writer of 'loc':
            for(ssa_ht daisy = oe.handle->prev_daisy(); daisy; --daisy)
            {
                if(!(ssa_flags(daisy->op()) & SSAF_WRITE_GLOBALS))
                    continue;

                assert(daisy->cfg_node() == cfg_node);

                if(locator_input(daisy, loc) >= 0
                   || locator_output(daisy, loc) >= 0)
                {
                    // Can't add a dep if a cycle would be created:
                    if(bitset_test(data(daisy).deps, index(ssa_node)))
                        break;

                    // Add a dep!
                    bitset_set(d.deps, index(daisy));
                    bitset_or(set_size, d.deps, data(daisy).deps);
                    propagate_deps_change(ssa_node);

                    break;
                }
            }
        }
    }

    // OK! Everything was initialized. Now to run the greedy algorithm.
    run();
}

void scheduler_t::append_schedule(ssa_ht h)
{
    bitset_set(scheduled, index(h));
    schedule.push_back(h);

    // Recursively schedule any linked, too:
    for_each_output_matching(h, INPUT_LINK,
    [this](ssa_ht link)
    {
        assert(ready<true>(link, scheduled));
        append_schedule(link);
    });
}

void scheduler_t::run()
{
    assert(bitset_all_clear(set_size, scheduled));

    carry_input_waiting = {};
    ssa_ht candidate = {};

    while(schedule.size() < cfg_node->ssa_size())
    {
        // First priority: try to find a successor node that's ready:
        if(candidate)
            candidate = successor_search(candidate);

        // Second priority: try to find *any* node that's ready:
        if(!candidate)
            candidate = full_search<false>();

        // Third priority: relax constraints
        if(!candidate)
            candidate = full_search<true>();

        // OK, we should definitely have a candidate_h now.
        assert(candidate);
        assert(ready<false>(candidate, scheduled));
        auto& d = data(candidate);

        // Schedule it:
        append_schedule(candidate);

        // If this node inputs or clobbers a carry, stop tracking it:
        if(candidate == carry_input_waiting 
           || (ssa_flags(candidate->op()) & SSAF_CLOBBERS_CARRY))
        {
            carry_input_waiting = {};
        }

        // If this node outputs a carry, track it:
        if(d.carry_user)
            carry_input_waiting = d.carry_user;
    }

    // Finally, re-assign 'index' to hold the position in the schedule:

    for(unsigned i = 0; i < schedule.size(); ++i)
    {
        index(schedule[i]) = i;
        data(schedule[i]).deps = nullptr;
    }
}

template<bool Relax>
bool scheduler_t::ready(ssa_ht h, bitset_uint_t const* scheduled) const
{
    assert(h->cfg_node() == cfg_node);

    auto& d = data(h);

    if(bitset_test(scheduled, index(h))) // If already scheduled
        return false;

    // A node is ready when all of its inputs are scheduled.
    for(unsigned i = 0; i < set_size; ++i)
        if(d.deps[i] & ~scheduled[i])
            return false;

    if(Relax)
        return true;

    // If a carry is live, we can't schedule any carry-clobbering ops.
    if(carry_input_waiting && h != carry_input_waiting
       && bitset_test(carry_clobberers, index(h)))
        return false;

    return true;
}

template<bool Relax>
int scheduler_t::path_length(ssa_ht h, bitset_uint_t const* scheduled) const
{
    auto* new_bitset = ALLOCA_T(bitset_uint_t, set_size);
    bitset_copy(set_size, new_bitset, scheduled);
    // 'new_bitset' assumes 'h' will be scheduled:
    bitset_set(new_bitset, index(h));
    
    int max_length = 0;
    int outputs_in_cfg_node = 0; // Number of outputs in the same CFG node.
    unsigned output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht output = h->output(i);

        if(output->cfg_node() != cfg_node)
            continue;

        if(!ready<Relax>(output, new_bitset))
            continue;

        ++outputs_in_cfg_node;

        max_length = std::max(max_length, path_length<Relax>(output, new_bitset));
    }

    return (max_length + std::max<int>(0, outputs_in_cfg_node - 1));
}

ssa_ht scheduler_t::successor_search(ssa_ht last_scheduled) const
{
    int best_path_length = -1;
    ssa_ht best = {};

    unsigned const output_size = last_scheduled->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht succ = last_scheduled->output(i);

        if(succ->cfg_node() != cfg_node)
            continue;

        if(ready<false>(succ, scheduled))
        {
            // Copies are so trivial we might as well schedule them next:
            if(ssa_flags(succ->op()) & SSAF_COPY)
                return succ;

            // Otherwise find the best successor node by comparing path lengths:
            int l = path_length<false>(succ, this->scheduled);
            if(l > best_path_length)
            {
                best_path_length = l;
                best = succ;
            }
        }
    }

    return best;
}

template<bool Relax>
ssa_ht scheduler_t::full_search() const
{
    int best_weight = INT_MIN;
    ssa_ht best = {};

    for(ssa_ht ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(!ready<Relax>(ssa_it, scheduled))
            continue;

        int w = path_length<Relax>(ssa_it, scheduled) * 8 + data(ssa_it).exit_distance;
        if(w > best_weight)
        {
            best_weight = w;
            best = ssa_it;
        }
    }

    return best;
}

} // end anon namespace

void schedule_ir(ir_t& ir)
{
    cg_data_resize();
    for(cfg_ht h = ir.cfg_begin(); h; ++h)
    {
        scheduler_t s(ir, h);
        cg_data(h).schedule = std::move(s.schedule);
    }
}

