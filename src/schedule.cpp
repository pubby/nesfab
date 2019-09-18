#include "schedule.hpp"

#include "flat/small_set.hpp"

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
    ir().ssa_pool.foreach([](ssa_node_t& node){ node.cfg_node = nullptr; });
    cfg_pool.clear();
    cfg_root = nullptr;
    build_cfg(*ir().exit);
}

cfg_node_t* scheduler_t::build_cfg(ssa_node_t& ssa_node)
{
    // Find the block this node belongs to.
    ssa_node_t* block = ssa_node.block(); assert(block);

    // Has the region already been turned into a CFG node?
    if(block->cfg_node)
        return block->cfg_node; 

    // It hasn't. Create it.
    cfg_node_t& cfg_node = cfg_pool.emplace(*block);
    block->cfg_node = &cfg_node;

    if(block->input_size == 0)
    {
        assert(!cfg_root);
        return cfg_root = &cfg_node;
    }

    // Recurse through the block's inputs.
    for(unsigned i = 0; i < block->input_size; ++i)
    {
        cfg_node_t* pred = build_cfg(*block->input[i]);

        bool const index = (block->input[i]->op == SSA_true_branch);

        assert(!pred->succs[index]);
        pred->succs[index] = &cfg_node;
        cfg_node.preds.push_back({ pred, index });
    }

    return &cfg_node;
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
    node.num_descendents = 0;
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

    // Here's set P:
    using set_t = fc::small_set<cfg_node_t*, 16>;
    set_t p;
    set_t::container_type q;
    set_t headers;


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
        // -- BUT that's not enough to handle irreducible graphs. --
        //
        // To handle irreducible graphs, we instead say P is all nodes that
        // path to L, of which the nodes path through descendents of H 
        // (but not H itself) in the depth-first tree.

        // Here's the tentative header H:
        cfg_node_t* h = *it;
        assert(h);

        // Here's set P:
        p.clear();

        set_t entry_points;

        // Check to see if H is actually a header:
        for(usage_t usage : h->preds)
        if(usage.edge_type() == BACK_EDGE)
            p.insert(usage.node->find()); // Yep, it paths to P.

        // If H isn't actually a header, try the next node.
        if(p.empty())
            continue;

        // 'q' will be the working set of nodes as we path backwards
        // to construct 'p'.
        for(q = p.container; !q.empty();)
        {
            cfg_node_t* x = q.back();
            q.pop_back();

            for(usage_t usage : x->preds)
            if(usage.edge_type() != BACK_EDGE)
            {
                cfg_node_t* y = usage.node->find();

                // Test if 'y' is an entry point to the loop
                // (i.e. 'y' is not a descendent of 'h' in the DFST)
                if(y->preorder_i < h->preorder_i
                   || h->preorder_i + h->num_descendents + 1 <= y->preorder_i)
                {
                    entry_points.insert(y);
                    continue;
                }

                if(p.count(y) && y != h)
                {
                    p.insert(y);
                    q.push_back(y);
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

/*
// Paper: A New Algorithm for Identifying Loops in Decompilation
// By Tao Wei, Jian Mao, Wei Zou, Yu Chen 
void shedule_t::visit_loops(cfg_node_t* node)
{
    node->preorder_i = preorder.size();
    preorder.push_back(node);

    for(int i = 0; i < 2; ++i)
    if(cfg_node_t* succ = node.succs[i])
    {
        if(succ->preorder_i == UNVISITED)
            tag_loop_header(succ, visit_loops(succ));
        else if(succ->postorder_i == UNVISITED) // Is back edge?
            tag_loop_header(node, succ);
        else if(cfg_node_t* header = succ->iloop_header)
        {
            if(header->postorder_i == UNVISITED) // Is back edge?
                tag_loop_header(node, header);
            else
            {
                // We've found a re-entry point.
                goto mark_reentry;
                while(header = header->iloop_header)
                {
                    if(header->postorder_i == UNVISITED) // Is back edge?
                    {
                        tag_loop_header(node, header);
                        break;
                    }
                mark_reentry:
                    reentry_edges.push_back({ node, i });
                }
            }
        }
    }

    node->postorder_i = postorder.size();
    postorder.push_back(node);

    return node->iloop_header;
}

void scheduler_t::tag_loop_header(cfg_node_t* node, cfg_node_t* header)
{
    if(node == header || !header)
        return;

    while(cfg_node_t* iheader = node->iloop_header)
    {
        if(iheader == header)
            return;

        if(iheader->preorder_i < header->preorder_i)
        {
            node->iloop_header = header;
            node = header;
            header = iheader;
        }
        else
            node = iheader;
    }

    node->iloop_header = header;
}
*/

std::ostream& scheduler_t::gv(std::ostream& o)
{
    o << "digraph {\n";
    o << "forcelabels=true;\n";

    cfg_pool.foreach([&o](cfg_node_t& node){ 
        o << node.postorder_i << ";\n";
    });

    for(cfg_node_t* node : preorder)
    {
        for(cfg_node_t* succ : node->succs)
            if(succ)
                o << node->preorder_i << " -> " << succ->preorder_i << '\n';

        if(node->idom)
        {
            o << node->preorder_i << " -> " << node->idom->preorder_i;
            o << " [color=\"red\", constraint=false];\n";
        }
    }

    o << "}\n";
    return o;
}
