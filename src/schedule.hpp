#ifndef SCHEDULE_HPP
#define SCHEDULE_HPP

#include <boost/container/small_vector.hpp>

#include "array_pool.hpp"
#include "handle.hpp"
#include "ir.hpp"

namespace bc = ::boost::container;

enum edge_type_t : short
{
    BACK_EDGE,
    CROSS_EDGE,
    FORWARD_EDGE,
};

struct usage_t
{
    cfg_node_t* node;
    unsigned index;

    inline edge_type_t edge_type() const;
};

class cfg_node_t
{
public:
    explicit cfg_node_t(ssa_node_t& block) : block(&block) {}

    ssa_node_t* block;
    cfg_node_t* idom;
    unsigned preorder_i;
    unsigned postorder_i;
    unsigned num_descendents;
    std::array<cfg_node_t*, 2> succs = {};
    std::array<edge_type_t, 2> succ_edge_types;
    bc::small_vector<usage_t, 2> preds;

    // Used in loop detection
    cfg_node_t* highpt;

    // Union find var
    cfg_node_t* uf_ancestor;

    cfg_node_t* iloop_header; // Immediate loop header

    void reset_uf() { uf_ancestor = this; } 
    cfg_node_t* find()
    {
        if(uf_ancestor == this)
            return this;
        return uf_ancestor = uf_ancestor->find();
    }

    // Sets 'with' to be an ancestor of 'this'.
    cfg_node_t* union_with(cfg_node_t* with)
    {
        cfg_node_t* this_root = find();
        cfg_node_t* with_root = with->find();
        if(this_root == with_root)
            return this_root;
        return with_root->uf_ancestor = this_root;
    }
};

class scheduler_t
{
public:
    explicit scheduler_t(ir_t& ir);

    void build();
    void build_cfg();
    void build_order();
    void build_dominators();
    bool build_loops();

    std::ostream& gv(std::ostream& o);
private:
    ir_t& ir() { return *ir_ptr; }

    cfg_node_t* build_cfg(ssa_node_t& ssa_node);
    unsigned visit_order(cfg_node_t& node);

    ir_t* ir_ptr;
    array_pool_t<cfg_node_t> cfg_pool;
    cfg_node_t* cfg_root;
    std::vector<cfg_node_t*> preorder;
    std::vector<cfg_node_t*> postorder;

    std::vector<usage_t> reentry_edges;
};

inline edge_type_t usage_t::edge_type() const
{ 
    assert(node);
    return node->succ_edge_types[index]; 
}

#endif
