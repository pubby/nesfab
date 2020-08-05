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
    struct ssa_scheduler_d
    {
        bitset_uint_t* deps; // Bitset of every node required to schedule this
        ssa_ht carry_user = {}; // Points to one use of this node's carry.
    };

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

ssa_ht* in
ssa_ht* out

in<>
out<>


struct stack_val_t
{
    std::vector<>

    std::vector<unsigned> remaining;
};

is_valid()
{
    if(op == SSA_fn_call)
    {
        // A fn call can only be scheduled if it doesn't clash with
        // any globals currently live:

        for(each global clobbered by fn call)
        {
            if(live)
                return false;
        }
    }
}

// If we're stuck:
// - Rewind stack to the last valid point.
// - Mark said path as invalid
// - If any paths remain, check them
// - Otherwise, rewind once more
*/
/*

live_t
{
    ssa_ht def;
    unsigned outputs_remaining;
};



std::vector<ssa_ht> schedule;
std::vector<ssa_ht> to_do;

struct stack_val
{
    std::vector<> live;
    unsigned scheduled;
    bitset* tried;
};

unsigned gvar_num;

    ssa_ht candidate = {};
    while(!to_do.empty())
    {
        // First priority: try to find a successor node that's ready:
        if(candidate)
            candidate = successor_search(candidate);

        // Second priority: try to find *any* node that's ready:
        if(!candidate)
            candidate = full_search();

        if(!candidate)
        {
            // TODO
            // Backtrack
        }

        // Schedule it:
        assert(!candidate->test_flags(FLAG_PROCESSED));
        candidate->set_flags(FLAG_PROCESSED);
        bitset_set(m_scheduled_bitset, index(candidate_h));

        schedule.push_back(candidate);

        // Remove from to-do
        // TODO

        // If this node inputs a live var, stop tracking it:
        bitset_for_each_bit(TODO, candidate->INPUTS,
        [](unsigned v)
        {
            live[v].outputs_remaining -= 1;

            if(live[v].outputs_remaining == 1)
                bitset_set(ONE_LIVE, bit);
            else if(live[v].outputs_remaining == 0)
                live[v].def = {};
        });

        // If this node outputs a live var, track it:
        bitset_for_each_bit(TODO, candidate->OUTPUTS,
        [](unsigned v)
        {
            assert(!live[v].def);
            live[v].def = candidate;
        });

        // If this node is a fn call, we don't prioritize successors
        if(candidate->op() == SSA_fn_call)
            candidate = {};
    }

bool scheduler_t::ready2(ssa_ht h, bitset_uint_t const* scheduled_bitset) const
{
    assert(h->cfg_node() == m_cfg_h);
    assert(!h->test_flags(FLAG_PROCESSED)); // If already scheduled.

    // A node is ready when all of its inputs are scheduled.
    for(unsigned i = 0; i < m_bitset_size; ++i)
        if((get_data(h).deps[i] & ~scheduled_bitset[i]) != 0)
            return false;

    // If the node clobbers a variable that is currently live,
    // it's not ready, UNLESS it's the last the last use of said variable.
    if(OUTPUTS & LIVE & ~(INPUTS & ONE_LIVE))
        return false;

    // If the node was previously tried, it's not ready.
    if(NODE & TRIED)
        return false;

    // Otherwise it's ready!
    return true;
}
*/
