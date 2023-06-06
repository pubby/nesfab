#include "cg_schedule.hpp"

#include <vector>

#include "flat/small_set.hpp"

#include "alloca.hpp"
#include "cg.hpp"
#include "ir.hpp"
#include "ir_algo.hpp"
#include "thread.hpp"

namespace { // anon namespace

constexpr int MAX_EXIT_DISTANCE = INT_MAX / 4; // Some sufficiently large number.

class scheduler_t
{
public:
    std::vector<ssa_ht> schedule;

    scheduler_t(ir_t& ir, cfg_ht cfg_node);
private:

    static inline TLS array_pool_t<bitset_uint_t> bitset_pool;

    ir_t& ir;
    cfg_ht const cfg_node;
    unsigned set_size = 0;

    ssa_ht carry_input_waiting;
    fc::small_set<ssa_ht, 16> unused_global_reads;
    ssa_value_t ptr_banker = {};

    // All the SSA nodes that are used to index arrays.
    std::array<ssa_value_t, 2> array_indexers = {};

    // Some successor searches will restart from this:
    ssa_ht retry_from = {};

    // Each SSA node in the CFG node that has been scheduled.
    bitset_uint_t* scheduled = nullptr;

    // All the SSA nodes that maybe clobber the carry.
    bitset_uint_t* carry_clobberers = nullptr;

    ssa_schedule_d& data(ssa_ht h) const { return cg_data(h).schedule; }
    unsigned& index(ssa_ht h) const { return data(h).index; }

    void append_schedule(ssa_ht h);
    void run();
    
    bool ready(unsigned relax, ssa_ht h, bitset_uint_t const* scheduled) const;

    int path_length(unsigned relax, ssa_ht h, bitset_uint_t const* scheduled) const;
    int indexer_score(ssa_ht h) const;
    int banker_score(ssa_ht h) const;

    ssa_ht successor_search(ssa_ht last_scheduled);

    ssa_ht full_search(unsigned relax);

    void calc_exit_distance(ssa_ht ssa, int exit_distance=0) const;

    void add_array_index(ssa_value_t index)
    {
        if(array_indexers[0] == index)
            return;
        else if(array_indexers[1] == index)
            std::swap(array_indexers[0], array_indexers[1]);
        else
        {
            array_indexers[1] = array_indexers[0];
            array_indexers[0] = index;
        }
    }
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

scheduler_t::scheduler_t(ir_t& ir, cfg_ht cfg_node_)
: ir(ir)
, cfg_node(cfg_node_)
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
        data(toposorted[i]).exit_distance = MAX_EXIT_DISTANCE; // Some sufficiently large number.
    }

#ifndef NDEBUG
    for(ssa_ht it = cfg_node->ssa_begin(); it; ++it)
        assert(data(it).deps);
#endif

    // The last daisy in the cfg_node should be scheduled last.
    if(ssa_ht exit = cfg_node->last_daisy())
    {
        if(exit->output_size() == 0)
        {
            auto& exit_d = data(exit);
            for(ssa_ht ssa_node : toposorted)
                if(ssa_node != exit)
                    bitset_set(exit_d.deps, index(ssa_node));
            calc_exit_distance(exit);
        }
    }

    for(ssa_ht ssa_node : toposorted)
    {
        // Ignore phi node deps. They can introduce cycles.
        if(ssa_node->op() == SSA_phi)
            continue;

        auto& d = data(ssa_node);

        // Assign deps based on all inputs:
        unsigned const input_size = ssa_node->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            // We only care about inputs that order.
            if(i == 0 && !provides_ordering(ssa_input0_class(ssa_node->op())))
                continue;

            if(!ssa_node->input(i).holds_ref())
                continue;

            ssa_ht const input = ssa_node->input(i).handle();

            if(input->cfg_node() != this->cfg_node)
                continue;

            assert(index(ssa_node) > index(input));
            bitset_set(d.deps, index(input));
            bitset_or(set_size, d.deps, data(input).deps);
        }

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

    // Reads should be used before they're rewritten
    for(ssa_ht ssa_node : toposorted)
    {
        if(ssa_node->op() != SSA_read_global)
            continue;

        locator_t const read_loc = ssa_node->input(1).locator();

        ssa_ht const writer = ssa_node->input(0).handle();
        if(writer->cfg_node() != cfg_node)
            continue;

        auto& d = data(writer);
        unsigned const index_ = index(writer);

        // - identify if the writer depends on other writes

        bool updated = false;

        bitset_for_each(set_size, d.deps, 
        [&](unsigned bit)
        { 
            ssa_ht dep = toposorted[bit];

            assert(dep != ssa_node);

            if(dep->cfg_node() != cfg_node || dep->op() != SSA_read_global || dep->input(1).locator() != read_loc)
                return;

            for(unsigned i = 0; i < dep->output_size(); ++i)
            {
                ssa_ht const o = orig_use(dep->output(i));

                if(o->cfg_node() != cfg_node || o == writer)
                    continue;

                auto& od = data(o);

                // Can't add a dep if a cycle would be created:
                if(bitset_test(od.deps, index_))
                    return;

                // Add a dep!
                bitset_set(d.deps, index(o));
                bitset_or(set_size, d.deps, od.deps);
                updated = true;
            }
        });

        if(updated)
            propagate_deps_change(ssa_node);
    }

    // In chains of carry operations, setup deps to avoid cases where
    // a carry would need to be stored.
    bitset_uint_t* temp_set = ALLOCA_T(bitset_uint_t, set_size);
    for(auto it = toposorted.rbegin(); it != toposorted.rend(); ++it)
    {
        ssa_ht ssa_node = *it;

        // Determine if this node produces a carry used by a single output.

        ssa_ht const carry = carry_output(*ssa_node);
        ssa_ht carry_user = {};

        if(carry && carry->output_size() == 1)
            carry_user = carry->output(0);
        else if(ssa_node->type().name() == TYPE_BOOL 
                && (ssa_flags(ssa_node->op()) & SSAF_CLOBBERS_CARRY)
                && ssa_node->output_size() == 1)
        {
            auto oe = ssa_node->output_edge(0);
            if(carry_input_i(oe.handle->op()) != static_cast<int>(oe.index))
                continue;
            carry_user = oe.handle;
        }
        else
            continue;

        // OK! This node produces a carry used by a single output.

        assert(carry_user);
        assert(ssa_node);

        if(carry_user->cfg_node() != ssa_node->cfg_node())
            continue;

        auto& d = data(ssa_node);
        unsigned const index_ = index(ssa_node);
        auto& carry_user_d = data(carry_user);

        assert(d.deps);
        assert(carry_user_d.deps);

        d.carry_user = carry_user;

        // 'temp_set' will hold all deps we'll try adding to 'd.deps':
        for(unsigned i = 0; i < set_size; ++i)
            temp_set[i] = carry_user_d.deps[i] & ~d.deps[i] & carry_clobberers[i];
        bitset_clear(temp_set, index_);

        bool updated = false;

        bitset_for_each(set_size, temp_set, 
        [&](unsigned bit)
        { 
            assert(bit < toposorted.size());
            assert(index(toposorted[bit]) == bit);
            auto& od = data(toposorted[bit]);
            assert(od.deps);

            // Can't add a dep if a cycle would be created:
            if(bitset_test(od.deps, index_))
                return;

            // Add a dep!
            bitset_set(d.deps, bit);
            bitset_or(set_size, d.deps, od.deps);
            updated = true;
        });

        if(updated)
            propagate_deps_change(ssa_node);
    }

    for(auto it = toposorted.rbegin(); it != toposorted.rend(); ++it)
    {
        ssa_ht ssa_node = *it;

        // Determine if this node produces a carry used by a single output.

        ssa_ht const carry = carry_output(*ssa_node);
        if(!carry || carry->output_size() != 1)
            continue;

        // OK! This node produces a carry used by a single output.

        auto& carry_d = data(carry);
        auto& d = data(ssa_node);
        unsigned const index_ = index(ssa_node);

        // When a node outputs a carry, 
        // make that node depend on other carry-clobering ops.
        // This makes it unlikely that a carry-clobbering op will get scheduled
        // in-between the generation of the carry, and its use.

        // 'temp_set' will hold all deps we'll try adding to 'd.deps'.
        for(unsigned i = 0; i < set_size; ++i)
            temp_set[i] = ~carry_d.deps[i] & carry_clobberers[i];

        assert(!bitset_test(temp_set, index_));
        assert(!bitset_test(temp_set, index(carry)));

        bool updated = false;

        bitset_for_each(set_size, temp_set, 
        [&](unsigned bit)
        { 
            assert(bit < toposorted.size());
            assert(index(toposorted[bit]) == bit);
            auto& od = data(toposorted[bit]);
            assert(od.deps);

            // Can't add a dep if a cycle would be created:
            if(bitset_test(od.deps, index_))
                return;

            // Add a dep!
            bitset_set(d.deps, bit);
            bitset_or(set_size, d.deps, od.deps);
            updated = true;
        });

        if(updated)
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

            // We can only do this when everything is in the same CFG node:
            if(oe.handle->cfg_node() != cfg_node)
                continue;

            // Likewise, we need a daisy chain:
            if(!oe.handle->in_daisy())
                continue;

            locator_t const loc = oe.handle->input(oe.index + 1).locator();

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

    // Phis
    for(ssa_ht ssa_node : toposorted)
    {
        assert(cfg_node == ssa_node->cfg_node());

        bool const exits_cfg = !for_each_output_with_links(ssa_node, [&](ssa_ht from, ssa_ht output)
        {
            while((ssa_flags(output->op()) & SSAF_COPY) && output->output_size() == 1)
                output = output->output(0);

            bool const is_phi = output->op() == SSA_phi;
            bool const same_cfg = output->cfg_node() == cfg_node;

            // For tight self-loops, phis should be used before they are re-written.
            if(is_phi && same_cfg)
            {
                auto& d = data(ssa_node);

                for_each_output(output, [&](ssa_ht phi_output)
                {
                    if(phi_output->cfg_node() != cfg_node)
                        return;

                    if(phi_output == ssa_node)
                        return;

                    // Can't add a dep if a cycle would be created:
                    if(bitset_test(data(phi_output).deps, index(ssa_node)))
                        return;

                    // Add a dep!
                    bitset_set(d.deps, index(phi_output));
                    bitset_or(set_size, d.deps, data(phi_output).deps);
                    propagate_deps_change(ssa_node);
                });
            }

            return !is_phi && same_cfg;
        });

        if(exits_cfg)
            calc_exit_distance(ssa_node, MAX_EXIT_DISTANCE / 2);
    }

    // Schedule cheap nodes before other uses
    for(ssa_ht ssa_node : toposorted)
    {
        assert(cfg_node == ssa_node->cfg_node());

        if(!(ssa_flags(ssa_node->op()) & SSAF_CHEAP_SCHEDULE))
            continue;

        unsigned const input_size = ssa_node->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t const input = ssa_node->input(i);
            if(!input.holds_ref() || input->cfg_node() != cfg_node)
                continue;

            unsigned const output_size = input->output_size();
            for(unsigned j = 0; j < output_size; ++j)
            {
                ssa_ht const output = input->output(j);
                if(output->cfg_node() != cfg_node || (ssa_flags(output->op()) & SSAF_CHEAP_SCHEDULE))
                    continue;

                // Can't add a dep if a cycle would be created:
                if(bitset_test(data(ssa_node).deps, index(output)))
                    continue;

                // Add a dep!
                auto& d = data(output);
                bitset_set(d.deps, index(ssa_node));
                bitset_or(set_size, d.deps, data(ssa_node).deps);
                propagate_deps_change(output);
            }
        }
    }

    // ARRAYS:
    // - Schedule write_arrays after all read_arrays from previous write_arrays

    for(ssa_ht ssa_node : toposorted)
    {
        using namespace ssai::array;

        if(!(ssa_flags(ssa_node->op()) & SSAF_WRITE_ARRAY))
            continue;

        auto& d = data(ssa_node);

        assert(ssa_node->input(ARRAY).holds_ref());
        ssa_ht const array_input = ssa_node->input(ARRAY).handle();

        for_each_output(array_input, [&](ssa_ht read)
        {
            if(ssa_node == read)
                return;

            if(!(ssa_flags(read->op()) & (SSAF_READ_ARRAY)))
                return;

            if(!(ssa_flags(read->op()) & (SSAF_INDEXES_ARRAY8 | SSAF_INDEXES_ARRAY16)))
                return;

            // We can only do this when the read is in the same CFG node
            if(read->cfg_node() != cfg_node)
                return;

            // Can't add a dep if a cycle would be created:
            if(bitset_test(data(read).deps, index(ssa_node)))
                return;

            // Add a dep!
            bitset_set(d.deps, index(read));
            bitset_or(set_size, d.deps, data(read).deps);
            propagate_deps_change(ssa_node);

            // We'll also try to schedule read's outputs before the write.
            // This improves code gen!
            for_each_output(read, [&](ssa_ht output)
            {
                if(ssa_node == output)
                    return;

                // We can only do this when the read is in the same CFG node
                if(read->cfg_node() != cfg_node)
                    return;

                // Can't add a dep if a cycle would be created:
                if(bitset_test(data(output).deps, index(ssa_node)))
                    return;

                // Add a dep!
                bitset_set(d.deps, index(output));
                bitset_or(set_size, d.deps, data(output).deps);
                propagate_deps_change(ssa_node);

            });
        });
    }

    // Try to use make_ptrs immediately.
    for(ssa_ht ssa_node : toposorted)
    {
        if(!is_make_ptr(ssa_node->op()))
            continue;

        ssa_value_t const copy = ssa_node->input(ssa_copy_input(ssa_node->op()));

        if(!copy.holds_ref())
            continue;

        unsigned const ptr_outputs = ssa_node->output_size();
        unsigned const copy_outputs = copy->output_size();
        for(unsigned i = 0; i < copy_outputs; ++i)
        {
            ssa_ht const copy_output = copy->output(i);

            if(copy_output == ssa_node)
                continue;

            if(copy_output->cfg_node() != cfg_node)
                continue;

            for(unsigned j = 0; j < ptr_outputs; ++j)
            {
                ssa_ht const ptr_output = ssa_node->output(j);

                if(ptr_output->cfg_node() != cfg_node)
                    continue;

                if(copy_output == ptr_output)
                    continue;

                auto& d = data(copy_output);

                // Can't add a dep if a cycle would be created:
                if(bitset_test(data(ptr_output).deps, index(copy_output)))
                    continue;

                // Add a dep!
                bitset_set(d.deps, index(ptr_output));
                bitset_or(set_size, d.deps, data(ptr_output).deps);
                propagate_deps_change(copy_output);
            }
        }
    }

    // Try to use indexers immediately,
    // scheduling other nodes that use them afterwards.
    for(ssa_ht ssa_node : toposorted)
    {
        if(!ssa_indexes8(ssa_node->op()))
            continue;

        if(!ssa_node->input(ssa_index8_input(ssa_node->op())).holds_ref())
            continue;

        ssa_ht const indexer = ssa_node->input(ssa_index8_input(ssa_node->op())).handle();

        unsigned const size = indexer->output_size();
        for(unsigned i = 0; i < size; ++i)
        {
            auto oe = indexer->output_edge(i);

            if(ssa_indexes8(oe.handle->op()) && oe.index == ssa_index8_input(oe.handle->op()))
                continue;

            if(oe.handle == ssa_node)
                continue;

            // We can only do this when the read is in the same CFG node
            if(oe.handle->cfg_node() != cfg_node)
                continue;

            auto& d = data(oe.handle);

            // Can't add a dep if a cycle would be created:
            if(bitset_test(data(ssa_node).deps, index(oe.handle)))
                continue;

            // Add a dep!
            bitset_set(d.deps, index(ssa_node));
            bitset_or(set_size, d.deps, data(ssa_node).deps);
            propagate_deps_change(oe.handle);
        }
    }

    // OK! Everything was initialized. Now to run the greedy algorithm.
    run();
    assert(schedule.size() == cfg_node->ssa_size());
}

void scheduler_t::append_schedule(ssa_ht h)
{
    bitset_set(scheduled, index(h));
    schedule.push_back(h);

    // Handle array indexes
    if(ssa_indexes8(h->op()))
        add_array_index(h->input(ssa_index8_input(h->op())));

    // Handle banks
    if(ssa_banks(h->op()))
        if(ssa_value_t bank = h->input(ssa_bank_input(h->op())))
            ptr_banker = bank;

    // If this is a global read, add it to our set:
    if(h->op() == SSA_read_global)
        unused_global_reads.insert(h);
    if(fn_like(h->op()))
    {
        // When calling a function, clear our set:
        unused_global_reads.clear();
    }
    else 
    {
        // When using a read, remove it from the set:
        for_each_node_input(h, [&](ssa_ht input)
        {
            if(input->op() == SSA_read_global)
                unused_global_reads.erase(input);
        });
    }

    // Recursively schedule any linked, too:
    for_each_output_matching(h, INPUT_LINK,
    [this](ssa_ht link)
    {
        //assert(ready<true>(link, scheduled));
        assert(link->cfg_node() == cfg_node);
        append_schedule(link);
    });
}

void scheduler_t::run()
{
    assert(bitset_all_clear(set_size, scheduled));
    assert(unused_global_reads.empty());

    carry_input_waiting = {};
    ssa_ht candidate = {};

    assert(schedule.empty());

    // Always schedule the entry first:
    if(ssa_ht h = cfg_node->first_daisy())
        if(h->op() == SSA_entry)
            append_schedule(h);

    while(schedule.size() < cfg_node->ssa_size())
    {
        // First priority: try to find a successor node that's ready:
        if(candidate)
            candidate = successor_search(candidate);

        if(!candidate && retry_from)
            candidate = successor_search(retry_from);

        // Second priority: try to find *any* node that's ready,
        // expanding the search until we succeed.
        for(unsigned relax = 0; !candidate; ++relax)
        {
            candidate = full_search(relax);
            assert(relax < 100);
        }

        // OK, we should definitely have a candidate_h now.
        assert(candidate);
        assert(ready(~0, candidate, scheduled));
        auto& d = data(candidate);

        // Schedule it:
        append_schedule(candidate);

        // If this node inputs or clobbers a carry, stop tracking it:
        if(candidate == carry_input_waiting || bitset_test(carry_clobberers, index(candidate)))
            carry_input_waiting = {};

        // If this node outputs a carry, track it:
        if(d.carry_user)
            carry_input_waiting = d.carry_user;
    }

    passert(schedule.size() == cfg_node->ssa_size(), schedule.size(), cfg_node->ssa_size());

    // Finally, re-assign 'index' to hold the position in the schedule:

    for(unsigned i = 0; i < schedule.size(); ++i)
    {
        index(schedule[i]) = i;
        data(schedule[i]).deps = nullptr;
    }
}

bool scheduler_t::ready(unsigned relax, ssa_ht h, bitset_uint_t const* scheduled) const
{
    assert(h->cfg_node() == cfg_node);

    auto& d = data(h);

    if(bitset_test(scheduled, index(h))) // If already scheduled
        return false;

    // A node is ready when all of its inputs are scheduled.
    for(unsigned i = 0; i < set_size; ++i)
        if(d.deps[i] & ~scheduled[i])
            return false;

    if(relax >= 2)
        return true;

    // If a carry is live, we can't schedule any carry-clobbering ops.
    if(carry_input_waiting && h != carry_input_waiting && bitset_test(carry_clobberers, index(h)))
        return false;

    if(relax >= 1)
        return true;

    if(!unused_global_reads.empty() && fn_like(h->op()))
        return false;

    return true;
}

// Estimates how many operations can be chained together.
// The score is used to weight different nodes for scheduling.
int scheduler_t::path_length(unsigned relax, ssa_ht h, bitset_uint_t const* scheduled) const
{
    auto* new_bitset = ALLOCA_T(bitset_uint_t, set_size);
    bitset_copy(set_size, new_bitset, scheduled);
    // 'new_bitset' assumes 'h' will be scheduled:
    bitset_set(new_bitset, index(h));

    if(ssa_flags(h->op()) & SSAF_PRIO_SCHEDULE)
        return 0;
    
    int max_length = 0;
    int outputs_in_cfg_node = 0; // Number of outputs in the same CFG node.
    unsigned output_size = h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto oe = h->output_edge(i);

        if(oe.handle->cfg_node() != cfg_node)
            continue;

        if(!ready(relax, oe.handle, new_bitset))
        {
            // TODO
            //if((ssa_flags(oe.handle->op()) & SSAF_INDEXES_ARRAY) && oe.index == 2)
                //return -1;
            continue;
        }

        if(ssa_banks(oe.handle->op()))
            if(ptr_banker && oe.handle->input(ssa_bank_input(oe.handle->op())) != ptr_banker)
                continue;

        if(oe.input_class() == INPUT_VALUE)
            ++outputs_in_cfg_node;

        int const l = path_length(relax, oe.handle, new_bitset);

        //assert(l >= 0);
        if(l < 0) // Only enable this if -1 can be returned.
            return l;

        max_length = std::max(max_length, l);
    }


    return (max_length + std::max<int>(0, outputs_in_cfg_node - 1));
}

// Estimates if an array operation should be scheduled.
// The score is used to weight different nodes for scheduling.
int scheduler_t::indexer_score(ssa_ht h) const
{
    if(ssa_indexes8(h->op()))
    {
        ssa_value_t index = h->input(ssa_index8_input(h->op()));
        if(index == array_indexers[0])
            return 64; // Fairly arbitrary numbers
        else if(index == array_indexers[1])
            return 32; // Fairly arbitrary numbers
        return -16; // Delay indexers.
    }
    return 0;
}

int scheduler_t::banker_score(ssa_ht h) const
{
    if(ssa_banks(h->op()))
    {
        if(ssa_value_t bank = h->input(ssa_bank_input(h->op())))
        {
            if(bank == ptr_banker)
                return 256; // Fairly arbitrary numbers
            return -256;
        }
    }
    return 0;
}

ssa_ht scheduler_t::successor_search(ssa_ht last_scheduled)
{
    int best_score = -1;
    ssa_ht best = {};

    auto const step = [&](ssa_ht succ, bool prio) -> ssa_ht
    {
        if(succ->cfg_node() != cfg_node)
            return {};

        if(prio && data(succ).exit_distance != MAX_EXIT_DISTANCE)
            return {};

        if(ready(0, succ, scheduled))
        {
            // Some nodes are so trivial we might as well schedule them next:
            if((ssa_flags(succ->op()) & SSAF_CHEAP_SCHEDULE)
               && !bitset_test(scheduled, index(succ)))
            {
                retry_from = last_scheduled;
                return succ;
            }

            // Otherwise find the best successor node by comparing path lengths:
            int score = path_length(0, succ, this->scheduled);
            score += indexer_score(succ);
            score += banker_score(succ);

            if(score > best_score)
            {
                best_score = score;
                best = succ;
            }
        }

        return {};
    };

    unsigned const output_size = last_scheduled->output_size();
    for(unsigned i = 0; i < output_size; ++i)
        if(ssa_ht ret = step(last_scheduled->output(i), true))
            return ret;

    if(!best)
    {
        for(unsigned i = 0; i < output_size; ++i)
            if(ssa_ht ret = step(last_scheduled->output(i), false))
                return ret;
    }

    retry_from = {};
    return best;
}

ssa_ht scheduler_t::full_search(unsigned relax)
{
    int best_score = INT_MIN;
    ssa_ht best = {};

    for(ssa_ht ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(!ready(relax, ssa_it, scheduled))
            continue;

        int score;

        if(ssa_flags(ssa_it->op()) & SSAF_PRIO_SCHEDULE)
            score = 1 << 16;
        else
        {
            // Fairly arbitrary formula.
            score = path_length(relax, ssa_it, scheduled);
            score += indexer_score(ssa_it);
            score += banker_score(ssa_it);
        }

        // Full searches also care about exit distance:
        score = (score * 8) + data(ssa_it).exit_distance;

        if(score > best_score)
        {
            best_score = score;
            best = ssa_it;
        }
    }

    if(best)
    {
        assert(best_score != INT_MIN);
        assert(ready(relax, best, scheduled));
    }

    retry_from = {};
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
        assert(cg_data(h).schedule.size() == h->ssa_size());
    }
}

void o_schedule(ir_t& ir)
{
    auto const valid_parent = [](ssa_ht h, unsigned input) -> ssa_ht
    {
        if(carry_used(*h))
            return {};

        ssa_value_t const parent = h->input(input);
        if(parent.holds_ref() && parent->cfg_node() == h->cfg_node())
        {
            unsigned const input_size = h->input_size();
            for(unsigned i = 0; i < input_size; ++i)
                if(i != input && !h->input(i).is_num())
                    return {};
            return parent.handle();
        }
        return {};
    };

    auto const sum = [&](ssa_ht h, unsigned& index, fixed_sint_t& v) -> ssa_ht
    {
        if(h->op() == SSA_add)
        {
            if(ssa_ht parent = valid_parent(h, index = 0))
            {
                v = h->input(1).fixed().value;
                if(h->input(2).whole())
                    v += low_bit_only(numeric_bitmask(h->type().name()));
                return parent;
            }

            if(ssa_ht parent = valid_parent(h, index = 1))
            {
                v = h->input(0).fixed().value;
                if(h->input(2).whole())
                    v += low_bit_only(numeric_bitmask(h->type().name()));
                return parent;
            }
        }
        else if(h->op() == SSA_sub)
        {
            if(ssa_ht parent = valid_parent(h, index = 1))
            {
                v = -h->input(0).fixed().value;
                if(!h->input(2).whole())
                    v -= low_bit_only(numeric_bitmask(h->type().name()));
                return parent;
            }
        }

        return {};
    };

    auto const sub = [&](ssa_ht h, unsigned& index, fixed_sint_t& v) -> ssa_ht
    {
        if(h->op() == SSA_sub)
        {
            if(ssa_ht parent = valid_parent(h, index = 0))
            {
                v = h->input(1).fixed().value;
                if(!h->input(2).whole())
                    v -= low_bit_only(numeric_bitmask(h->type().name()));
                return parent;
            }
        }

        return {};
    };

    auto const optimize_sums = [&](ssa_ht child, auto const& fn) -> bool
    {
        unsigned a_index, b_index;
        fixed_sint_t a_operand, b_operand;

        ssa_ht const parent = fn(child, a_index, a_operand);
        if(!parent)
            return false;

        for(unsigned i = 0; i < parent->output_size(); ++i)
        {
            ssa_ht const output = parent->output(i);
            if(output == child || output == parent || output->cfg_node() != child->cfg_node() || output->type() != child->type())
                continue;

            if(fn(output, b_index, b_operand) != parent)
                continue;

            ssa_ht a = child;
            ssa_ht b = output;

            if(cg_data(a).schedule.index > cg_data(b).schedule.index)
            {
                std::swap(a, b);
                std::swap(a_index, b_index);
                std::swap(a_operand, b_operand);
            }

            fixed_sint_t diff = b_operand - a_operand;
            diff &= numeric_bitmask(b->type().name());

            assert(b->input(b_index) == parent);
            assert(b_index < 2);

            b->link_change_input(b_index, a);
            b->link_change_input(!b_index, ssa_value_t(fixed_t{diff}, b->type().name()));
            b->link_change_input(2, ssa_value_t(0u, TYPE_BOOL));
            b->unsafe_set_op(SSA_add);

            return true;
        }

        return false;
    };

    bool updated;
    do
    {
        updated = false;
        for(cfg_node_t const& cfg : ir)
        for(ssa_ht ssa = cfg.ssa_begin(); ssa; ++ssa)
        {
            while(optimize_sums(ssa, sum) || optimize_sums(ssa, sub))
                updated = true;
        }
    }
    while(updated);
}
