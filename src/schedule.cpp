#include "schedule.hpp"

static constexpr unsigned UNVISITED = -1u;

scheduler_t::scheduler_t(ir_t& ir)
: ir_ptr(&ir)
{
    build();
}

void scheduler_t::build()
{
    build_cfg();
    build_order();
    build_dominators();
    build_loops();
}

// Creates the cfg graph, setting 'succs' and 'preds'.
void scheduler_t::build_cfg()
{
    for(ssa_node_t& ssa_node : ir().ssa)
        ssa_node.cfg_node = nullptr;
    cfg_pool.clear();
    cfg_root = nullptr;
    build_cfg(ir().return_h);
}

cfg_node_t* scheduler_t::build_cfg(ssa_handle_t ssa_node_h)
{
    // Find the region this node belongs to.
    ssa_handle_t region_h = ir().region_h(ssa_node_h);
    ssa_node_t& region = ir()[region_h];
    assert(region.op == SSA_cfg_region);

    // Has the region already been turned into a CFG node?
    if(region.cfg_node)
        return region.cfg_node; 

    // It hasn't. Create it.
    cfg_node_t& node = cfg_pool.emplace(region_h);
    region.cfg_node = &node;

    if(region.input_size == 0)
    {
        assert(!cfg_root);
        return cfg_root = &node;
    }

    // Recurse through the region's inputs.
    for(unsigned i = 0; i < region.input_size; ++i)
    {
        ssa_handle_t input_h = region.input(ir())[i];
        ssa_node_t& input = ir()[input_h];

        cfg_node_t* pred = build_cfg(input_h);

        bool index = (input.op == SSA_true_branch);

        assert(!pred->succs[index]);
        pred->succs[index] = &node;
        node.preds.push_back({ pred, index });
    }

    return &node;
}

// Fills 'postorder' and 'preorder' and sets 'postorder_i' and 'preorder_i'.
// Also sets 'succ_edge_types', hurray!
void scheduler_t::build_order()
{
    // This is a basic depth-first traversal of the graph.

    // 'preorder_i' will temporarily be used to track nodes that are visited,
    // and 'postorder_i' will temporarily be used to track nodes that
    // are currently in the recursion stack.
    cfg_pool.foreach([](cfg_node_t& node) 
    { 
        node.preorder_i = UNVISITED; 
        node.postorder_i = UNVISITED; 
    });

    preorder.clear();
    preorder.reserve(cfg_pool.size());

    postorder.clear();
    postorder.reserve(cfg_pool.size());

    visit_order(*cfg_root);

    assert(preorder.empty() || preorder.front() == cfg_root);
    assert(postorder.empty() || postorder.back() == cfg_root);
}

unsigned scheduler_t::visit_order(cfg_node_t& node)
{
    node.num_descendents = 1;
    node.preorder_i = preorder.size();
    preorder.push_back(&node);
    for(int i = 0; i < 2; ++i)
    if(cfg_node_t* succ = node.succs[i])
    {
        if(succ->preorder_i == UNVISITED)
        {
            node.num_descendents += visit_order(*succ);
            node.succ_edge_types[i] = FORWARD_EDGE;
        }
        else if(succ->postorder_i == UNVISITED)
            node.succ_edge_types[i] = BACK_EDGE;
        else
            node.succ_edge_types[i] = CROSS_EDGE;
    }
    node.postorder_i = postorder.size();
    postorder.push_back(&node);
    return node.num_descendents;
}

// Finds the immediate dominator of every cfg node.
// 
// Paper: A Simple, Fast Dominance Algorithm
// By Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
void scheduler_t::build_dominators()
{
    cfg_pool.foreach([](cfg_node_t& node){ node.idom = nullptr; });

    for(bool changed = true; changed;)
    {
        changed = false;

        // Reverse postorder, but skip start node.
        for(auto it = postorder.rbegin()+1; it < postorder.rend(); ++it)
        {
            cfg_node_t& node = **it;
            assert(&node != cfg_root);
            assert(!node.preds.empty());

            cfg_node_t* new_idom = node.preds[0].node;
            assert(new_idom);
            for(std::size_t j = 1; j < node.preds.size(); ++j)
            {
                cfg_node_t* pred = node.preds[j].node;
                assert(pred);

                while(pred != new_idom)
                {
                    if(pred->postorder_i < new_idom->postorder_i)
                    {
                        if(!(pred = pred->idom))
                            pred = cfg_root;
                    }
                    else if(!(new_idom = new_idom->idom))
                        new_idom = cfg_root;
                }
            }

            if(node.idom != new_idom)
            {
                node.idom = new_idom;
                changed = true;
            }
        }
    }
}

// Paper: "Testing Flow Graph Reducibility" 
// by R. Endre Tarjan.
bool scheduler_t::build_loops()
{
    // Reset everything.
    for(cfg_node_t* node : preorder)
    {
        node->reset_uf();
        node->highpt = nullptr;
    }

    // Do a reverse preorder traversal to find nodes with back edges.
    for(auto it = preorder.rbegin(); it != preorder.rend(); ++it)
    {
        // Given a back edge (from, to): 
        // - 'from' is called the latch.
        // - 'to' is called the header.
        // 
        // If header H has a back edge from latch L, we can define the set P
        // as all nodes that path to L while avoiding H.
        // (This set R. Endre Tarjan calls the reachunder set)
        //
        // If an element of set P is not a descendent of H in the depth-first
        // tree, the graph is irreducible.

        // Here's the tentative header H:
        cfg_node_t* h = *it;
        assert(h);

        // Here's set P:
        using set_t = fc::small_set<cfg_node_t*, 32>;
        set_t p;

        // Check to see if H is actually a header:
        for(usage_t usage : h->preds)
        if(usage.edge_type() == BACK_EDGE)
            p.insert(usage.node->find()); // Yep, it paths to P.

        // If H isn't actually a header, try the next node.
        if(p.empty())
            continue;

        set_t q(p);

        // Now construct p(x) by exploring backward from vertices in Q
        while(!q.empty())
        {
            cfg_node_t* x = q.container.back();
            q.container.pop_back();

            for(usage_t usage : x->preds)
            if(usage.edge_type() != BACK_EDGE)
            {
                cfg_node_t* y = usage.node->find();

                if(h->preorder_i > y->preorder_i
                   || h->preorder_i + h->num_descendents <= y->preorder_i)
                {
                    return false; // Not reducible
                }

                if(p.count(y) && y != h)
                {
                    p.insert(y);
                    q.insert(y);
                }

                if(y->highpt == nullptr)
                    y->highpt = h;
            }
        }

        // Set P is now complete.

        for(cfg_node_t* s : p.container)
            h->union_with(s);
    }

    return true;
}

std::ostream& scheduler_t::gv(std::ostream& o)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";

    cfg_pool.foreach([&o](cfg_node_t& node){ 
        o << node.postorder_i << ";\n";
    });

    for(cfg_node_t* node : postorder)
    {
        for(cfg_node_t* succ : node->succs)
            if(succ)
                o << node->postorder_i << " -> " << succ->postorder_i << '\n';

        if(node->idom)
        {
            o << node->postorder_i << " -> " << node->idom->postorder_i;
            o << " [color=\"red\", constraint=false];\n";
        }
    }

    o << "}\n";
    return o;
}
