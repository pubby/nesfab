#include "o_motion.hpp"

#include <cstdint>
#include <vector>

#include "robin/map.hpp"
#include "robin/hash.hpp"

#include "ir.hpp"
#include "ir_algo.hpp"
#include "ir_util.hpp"
#include "worklist.hpp"

////////////////////////////
// GLOBAL VALUE NUMBERING //
////////////////////////////

using gvn_t = std::uint64_t;

struct ssa_gvn_d
{
    gvn_t gvn = 0;
};

struct gvn_key_t
{
    ssa_op_t op;
    std::uint16_t num_args;
    gvn_t const* args;
    type_t type;

    gvn_t const* begin() const { return args; }
    gvn_t const* end() const { return args + num_args; }

    bool operator==(gvn_key_t const& o) const
    {
        return op == o.op && type == o.type && std::equal(begin(), end(), o.begin(), o.end());
    }
};

template<>
struct std::hash<gvn_key_t>
{
    std::size_t operator()(gvn_key_t const& key) const noexcept
    {
        std::size_t h = rh::hash_finalize(key.op);
        h = rh::hash_combine(h, key.type.hash());
        for(gvn_t vn : key)
            h = rh::hash_combine(h, vn);
        return h;
    }
};

class run_gvn_t
{
public:
    run_gvn_t(log_t* log, ir_t& ir)
    : log(log)
    {
        m_key_map.reserve(ssa_pool::array_size());
        m_gvn_sets.reserve(ssa_pool::array_size());

        // Assign unique GVNs to ops not valid for GVN:
        for(cfg_node_t const& cfg_node : ir)
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
            if(valid_for_gvn(ssa_it))
                data(ssa_it).gvn = m_next_gvn++;

        // Build GVNs for the rest of the IR:
        for(cfg_node_t const& cfg_node : ir)
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
            m_gvn_sets[to_gvn(ssa_it)].push_back(ssa_it);

        // Needed for merging
        build_order(ir);
        build_dominators_from_order(ir);

        // Now merge GVN sets:
        for(auto const& pair : m_gvn_sets)
            merge_gvn_set(pair.second);

        // HACK:
        // Ensure all linked nodes are in their parent's CFG node.
        /*
        for(cfg_node_t& cfg_node : ir)
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
            if(ssa_input0_class(ssa_it->op()) == INPUT_LINK && ssa_it->input(0)->cfg_node() != cfg_node.handle())
                ssa_it->input(0)->cfg_node()->steal_ssa(ssa_it, true);
                */

    }

    static bool valid_for_gvn(ssa_ht ssa_it)
    {
        if(ssa_input0_class(ssa_it->op()) == INPUT_LINK)
            return valid_for_gvn(ssa_it->input(0).handle());

        return (ssa_it->in_daisy() 
                || (ssa_flags(ssa_it->op()) & (SSAF_NO_GVN | SSAF_WRITE_ARRAY | SSAF_IO_IMPURE)) 
                || !pure(*ssa_it));
    }

    gvn_t to_gvn(ssa_value_t v)
    {
        if(!v.holds_ref())
        {
            assert(v.target() == 0 || v.target() >= (1ull << 32ull));
            return v.target();
        }

        ssa_ht node = v.handle();
        auto& d = data(node);

        if(d.gvn)
            return d.gvn;

        unsigned const input_size = node->input_size();
        gvn_t* args = m_pool.alloc(input_size);
        for(unsigned i = 0; i < input_size; ++i)
            args[i] = to_gvn(node->input(i));

        if(ssa_flags(node->op()) & SSAF_COMMUTATIVE)
        {
            assert(input_size >= 2);
            if(args[0] > args[1])
                std::swap(args[0], args[1]);
        }

        auto result = m_key_map.emplace(gvn_key_t{ node->op(), input_size, args, node->type() }, [&]()
        { 
            gvn_t const gvn = m_next_gvn++; 
            return gvn;
        });

        return d.gvn = result.first->second;
    }

    void merge_gvn_set(std::vector<ssa_ht> const& set)
    {
        if(set.size() < 2)
            return;

        assert(set[0]->op() != SSA_null);
        assert(!set[0]->test_flags(FLAG_PRUNED));

        auto owning_cfg = [&](ssa_ht h)
        {
            if(ssa_input0_class(h->op()) == INPUT_LINK)
                return h->input(0)->cfg_node();
            return h->cfg_node();
        };

        // Find a CFG node that dominates all
        cfg_ht dom = owning_cfg(set[0]);
        for(unsigned i = 1; i < set.size(); ++i)
            dom = dom_intersect(dom, owning_cfg(set[i]));

        dom->steal_ssa(set[0], true);

        // Now replace the old nodes with the defining one:
        for(unsigned i = 1; i < set.size(); ++i)
        {
            dprint(log, "GVN_REPLACE", set[i], set[i]->op(), "with", set[0], set[0]->op());
            set[i]->replace_with(set[0]);
            set[i]->prune();
        }

        updated = true;
    }

    static ssa_gvn_d& data(ssa_ht h) { return h.data<ssa_gvn_d>(); }

    bool updated = false;

private:
    array_pool_t<gvn_t> m_pool;
    rh::robin_map<gvn_key_t, gvn_t> m_key_map;
    rh::batman_map<gvn_t, std::vector<ssa_ht>> m_gvn_sets;
    gvn_t m_next_gvn = 1ull;

    log_t* log = nullptr;
};

///////////////////
// LOOP HOISTING //
///////////////////

static bool o_hoist(log_t* log, ir_t& ir)
{
    dprint(log, "HOIST");

    bool updated = false;

    assert(ssa_worklist.empty());

    for(cfg_node_t const& cfg_node : ir)
    {
        cfg_ht const header = this_loop_header(cfg_node.handle());

        if(header)
        {
            for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
            {
                ssa_it->clear_flags(FLAG_PROCESSED | FLAG_IN_WORKLIST);

                if(ssa_it->in_daisy() 
                   || (ssa_flags(ssa_it->op()) & (SSAF_NO_GVN | SSAF_WRITE_ARRAY)) 
                   || (ssa_input0_class(ssa_it->op()) == INPUT_LINK)
                   || !pure(*ssa_it))
                {
                    // Cache unmovable nodes using FLAG_PROCESSED:
                    ssa_it->set_flags(FLAG_PROCESSED);
                }
                else
                    ssa_worklist.push(ssa_it);
            }
        }
        else for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            // Cache unmovable nodes using FLAG_PROCESSED:
            ssa_it->clear_flags(FLAG_IN_WORKLIST);
            ssa_it->set_flags(FLAG_PROCESSED);
        }
    }

    while(!ssa_worklist.empty())
    {
        ssa_ht const ssa = ssa_worklist.pop();
        assert(!ssa->test_flags(FLAG_PROCESSED));

        assert(ssa_input0_class(ssa->op()) != INPUT_LINK);

        while(true)
        {
            cfg_ht const cfg = ssa->cfg_node();
            cfg_ht const header = this_loop_header(cfg);
            if(!header)
                break;

            // We'll try to relocate SSA nodes to this CFG:
            cfg_ht const hoist_to = algo(header).idom;
            if(!hoist_to || hoist_to == cfg)
                break;

            if(!dominates(hoist_to, cfg))
                break;

            assert(hoist_to != header);
            assert(this_loop_header(hoist_to) != header);
            assert(loop_is_parent_of(header, cfg));
            passert(!loop_is_parent_of(header, hoist_to), header, hoist_to, algo(hoist_to).iloop_header);

            // Make sure our inputs are valid in 'hoist_to'.
            unsigned const input_size = ssa->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t const input = ssa->input(i);
                if(input.holds_ref() && !dominates(input->cfg_node(), hoist_to))
                    goto next_iter;
            }

            // Move the node:
            hoist_to->steal_ssa(ssa, true);
            dprint(log, "-HOIST_STEAL", ssa, hoist_to);

            // Enqueue outputs:
            for_each_output_with_links(ssa, [&](ssa_ht from, ssa_ht output)
            {
                if(!output->test_flags(FLAG_PROCESSED))
                    ssa_worklist.push(output);
            });

            updated = true;
        }
    next_iter:;
    }

    return updated;
}

////////////
// MOTION //
////////////

bool o_motion(log_t* log, ir_t& ir)
{
    build_loops_and_order(ir);
    build_dominators_from_order(ir);

    bool updated = false;

    {
        ssa_data_pool::scope_guard_t<ssa_gvn_d> sg(ssa_pool::array_size());
        run_gvn_t runner(log, ir);
        updated |= runner.updated;
    }

    ir.assert_valid();

    updated |= o_hoist(log, ir);

    return updated;
}
