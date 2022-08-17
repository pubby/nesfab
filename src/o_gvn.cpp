#include "o_gvn.hpp"

#include <cstdint>
#include <vector>

#include "robin/map.hpp"
#include "robin/hash.hpp"

#include "ir.hpp"
#include "ir_algo.hpp"
#include "ir_util.hpp"

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
        {
            if(ssa_it->in_daisy() 
               || (ssa_flags(ssa_it->op()) & (SSAF_NO_GVN | SSAF_WRITE_ARRAY)) 
               || !io_pure(*ssa_it))
            {
                data(ssa_it).gvn = m_next_gvn++;
            }
        }

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

        // Find a CFG node that dominates all
        cfg_ht dom = set[0]->cfg_node();
        for(unsigned i = 1; i < set.size(); ++i)
            dom = dom_intersect(dom, set[i]->cfg_node());

        dom->steal_ssa(set[0], true);

        // Now replace the old nodes with the defining one:
        for(unsigned i = 1; i < set.size(); ++i)
        {
            dprint(log, "GVN_REPLACE", set[i], "with", set[0]);
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

bool o_global_value_numbering(log_t* log, ir_t& ir)
{
    ssa_data_pool::scope_guard_t<ssa_gvn_d> sg(ssa_pool::array_size());
    run_gvn_t runner(log, ir);
    return runner.updated;
}
