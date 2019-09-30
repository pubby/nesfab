#include "o_abstract_interpret.hpp"

#include "alloca.hpp"
#include "fixed.hpp"
#include "ir.hpp"

#include "flat/small_map.hpp"

#include "sizeof_bits.hpp"

#include <iostream> // todo

/*
static void prune(worklists_t& worklists, cfg_node_t& cfg_node)
{
    if(!node.exit) // If already pruned.
        return;

    for(ssa_node_t* ssa_node : cfg_node.ssa_nodes)
    {
        ssa_node->op = SSA_pruned;
        for(unsigned i = 0; i < ssa_node->in.size(); ++i)
        {
            ssa_value_t value = ssa_node->in[i];
            if(value.is_ptr())
                value->link_remove_out({ ssa_node, i });
        }
        ssa_node->in.clear();
    }

    cfg_node.exit = nullptr;
    cfg_node.ssa_nodes.clear();

    auto out = cfg_node.out;
    cfg_node.out = {};

    for(unsigned i = 0; i < out; ++i)
        if(out[i] && !out[i]->idom->exit)
            out[i]->link_remove_in({ &cfg_node, i });

    for(bool changed = true; changed;)
    {
        changed = false;
        for(unsigned i = 0; i < out; ++i)
        {
            if(out[i] && !out[i]->idom->exit)
            {
                output->idom = best_idom(*output);
            }
        }
    }

    for(unsigned i = 0; i < out; ++i)
    {
        if(cfg_node_t* output = out[i])
        {
            output->link_remove_in({ &cfg_node, i });
            if(output->idom->exit)
                output->idom = best_idom(*output);
        }
    }

    for(unsigned i = 0; i < out; ++i)
        if(out[i] && !out[i]->idom->exit)
            prune(*output);




    node.exit = nullptr;
    auto inputs = std::move(node.inputs); node.inputs.clear();

    // We have to unlink this node from its inputs and users.

    // Unlink the inputs. Do this before clearing 'node.users',
    // to properly deal with inputs to self.
    for(unsigned i = 0; i < inputs.size(); ++i)
    {
        if(inputs[i].is_ptr())
        {
            inputs[i]->remove_usage({ &node, i });
            worklists.users_changed.insert(*inputs[i]);
        }
    }

    // Now we can clear the users array.
    auto users = std::move(node.users); node.users.clear();

    // Unlink outgoing block edges.
    for(ssa_usage_t usage : users)
    {
        // All self-inputs should have been removed by the previous step.
        assert(usage.node != &node);

        if(usage.node->op == SSA_block)
        {
            // Remove the block's input.
            std::swap(usage.input(),
                      usage.node->inputs.back());
            usage.node->inputs.pop_back();
            worklists.inputs_changed.insert(*usage.node);

            // We also have to remove input in all dependent phi nodes.
            for(ssa_usage_t block_usage : usage.node->users)
            {
                if(block_usage.node->op != SSA_phi)
                    continue;

                std::swap(block_usage.input(),
                          block_usage.node->inputs.back());
                block_usage.node->inputs.pop_back();
                worklists.inputs_changed.insert(*block_usage.node);
            }
        }
    }

    // Recursively prune (some) users of the node.
    for(ssa_usage_t usage : users)
    {
        if(usage.node->op == SSA_block)
        {
            if(usage.node->inputs.empty())
                prune(worklists, *usage.node);
        }
        else if(usage.node->op == SSA_phi && usage.index != 0)
        {
            // Phi nodes keep their input reference to the pruned node.
            // They only get pruned if their owning block gets pruned.
        }
        else
            prune(worklists, *usage.node);
    }
}

static void try_prune_unused_node(worklists_t& worklists, ssa_node_t& node)
{
    if(node.op != SSA_pruned
       && node.users.empty() && !node.has_side_effects())
    {
        prune(worklists, node);
    }
}

static void try_prune_conditional(worklists_t& worklists, ssa_node_t& node)
{
    if(node.op == SSA_if && node.users.size() == 1)
    {
        ssa_node_t* branch = node.users[0].node; 
        assert(branch);
        assert(branch->op == SSA_true_branch 
               || branch->op == SSA_false_branch);

        // Change the node from a conditional into a fence node.
        node.op = SSA_fence;

        // Merge the links.
        assert(branch->users.size() == 1);
        node.users[0] = branch->users[0];
        node.users[0].input() = &node;

        worklists.users_changed.insert(node);
        worklists.inputs_changed.insert(*node.users[0].node);

        // Prune the branch node.
        branch->op = SSA_pruned;
        branch->inputs.clear();
        branch->users.clear();
    }
}

// A trivial phi node has exactly 1 input, exactly 0 inputs, or has every 
// input pointing to itself, or equal to each other. 
static ssa_value_t get_trivial_phi_value(ssa_node_t const& node)
{
    if(node.op != SSA_phi)
        return nullptr;

    ssa_value_t unique = nullptr;
    for(unsigned i = 1; i < node.in.size(); ++i)
    {
        ssa_value_t input = node.in[i];

        if(input.ptr() == &node)
            continue;

        if(unique && input != unique)
            return nullptr;
        else
            unique = input;
    }
    return unique;
}

static void try_prune_trivial_phi(worklists_t& worklists, ssa_node_t& node)
{
    if(ssa_value_t trivial = get_trivial_phi_value(node))
    {
        assert(!trivial.is_ptr() || trivial.ptr() != &node);
        for(ssa_usage_t usage : node.users)
        {
            if(usage.node == &node)
            {
                usage.input() = 0u; // Remove the self-reference.
                continue;
            }

            usage.input() = trivial;
            worklists.inputs_changed.insert(*usage.node);

            if(trivial.is_ptr())
            {
                trivial->users.push_back(usage);
                worklists.users_changed.insert(*trivial);
            }
        }
        node.users.clear();
        prune(worklists, node);
    }
}
*/

/*
void o_sccp(worklist_t& worklist, ssa_node_t& node)
{
    fixed_t const_value;
    switch(node.op)
    {
    case SSA_if:
        assert(node.inputs.size() == 2);
        if(node.inputs[1].is_const())
        { 
            bool condition = node.inputs[1].fixed.value;

            assert(node.users.size() == 2);
            if(node.users[0].op == SSA_branch(condition))
                o_force_prune(node.users[1]);
            else
                o_force_prune(node.users[0]);
        }
        break;

    case SSA_phi:
        // TODO
        break;

    case SSA_add:
        if(node.inputs[1].is_const() && node.inputs[2].is_const())
        {
            // Hurray! Solve for const.
            fixed_t const_value = fixed_add(node.type, 
                                            node.inputs[1].fixed(), 
                                            node.inputs[2].fixed());
            goto replace_with_const;
        }
        else if(node.inputs[1].constraints && node.inputs[2].constraints)
        {
            // Join the constraints.
            *node.constraints = join_add(node.type,
                                         node.inputs[1].constraints, 
                                         node.inputs[2].constraints);
            if(node.constraints->is_const())
            {
                const_value = node.constraints.bounds.min;
                goto replace_with_const;
            }
        }
        else
        {
            // TODO
            // If the node depends on bottom -> mark it bottom.
            // Otherwise, keep it top.
        }
    }

    return;

replace_with_const:
    // Replaces the node with a const one, pruning it.
    // TODO
    return;
}
*/


namespace // Anonymous namespace
{

bool has_constraints(ssa_value_t value)
{
    return value.is_const() || value->constraints;
}

constraints_t to_constraints(ssa_value_t value)
{
    assert(has_constraints(value));
    if(value.is_ptr())
        return *value->constraints;
    else
        return constraints_t::const_(value.fixed().value);
}

void mark_dom_reachable(cfg_node_t& cfg_node, 
                               cfg_node_t::reachable_int_t reachable_flag)
{
    cfg_node.reachable |= reachable_flag;
    for(cfg_node_t* output : cfg_node.out)
    {
        if(output->reachable & reachable_flag)
            continue;
        if(output->idom && (output->idom->reachable & reachable_flag))
            mark_dom_reachable(*output, reachable_flag);
    }
}

struct insert_trace_data_t
{
    ir_t* const ir;
    cfg_node_t* const branch_node;
    unsigned branch_i;
    fc::small_map<ssa_node_t*, ssa_node_t*, 16> trace_map;
};

void insert_trace(insert_trace_data_t& data, ssa_node_t& original, 
                  ssa_node_t* parent_trace, unsigned arg_i)
{
    // A single node can appear multiple times in the condition expression.
    // Check to see if that's the case by checking if a trace already exists
    // for this node.
    auto it = data.trace_map.find(&original);
    if(it != data.trace_map.end())
    {
        // The trace already exists.
        ssa_node_t* ptr = it->second;
        if(ptr != parent_trace)
        {
            ptr->link_append_input(parent_trace);
            ptr->link_append_input(arg_i);
        }
        return;
    }

    ssa_node_t& trace = data.branch_node->out[data.branch_i]->emplace_ssa(
        *data.ir, SSA_trace, original.type);
    data.trace_map.insert({ &original, &trace });

    assert(original.visited = true); // Debug-only

    // Look at every user of this node, finding spots where the
    // trace can be substituted into.
    cfg_node_t::reachable_int_t const reachable_flag = 1ull << data.branch_i;
    for(unsigned i = 0; i < original.out.size();)
    {
        ssa_reverse_edge_t& edge = original.out[i];
        cfg_node_t* cfg_node = edge.node->cfg_node; assert(cfg_node);

        // If the user is ONLY reachable by taking this branch,
        // or if the user is a Phi node that only takes input on this branch.
        if(cfg_node->reachable == reachable_flag
           || (edge.node->op == SSA_phi
               && cfg_node->in[edge.index].node->reachable == reachable_flag))
        {
            assert(edge.node->op != SSA_trace);
            assert(edge.node->visited == false);
            trace.out.push_back(edge);
            edge.in() = &trace;
            std::swap(edge, original.out.back());
            original.out.pop_back();
        }
        else
            ++i;
    }

    // The first argument is the original expression it represents.
    trace.link_append_input(&original);

    // The remaining arguments come in pairs.
    // - First comes the parent trace.
    // - Second comes the argument index into the parent trace.
    if(parent_trace)
    {
        trace.link_append_input(parent_trace);
        trace.link_append_input(arg_i);
    }
    else
    {
        // If there is no parent trace, append only a single argument:
        // the branch index as a constant.
        trace.link_append_input(data.branch_i);
    }

    if(original.flags() & SSAF_TRACE_INPUTS)
    {
        // Recursively trace 'original', turning its inputs into traces too.
        for(unsigned i = 0; i < original.in.size(); ++i)
            if(original.in[i].is_ptr())
                insert_trace(data, *original.in[i], &trace, i);
    }
}

void insert_traces(ir_t& ir)
{
    // Debug-only
    assert((ir.ssa_pool.foreach([](ssa_node_t& ssa_node)
                                { ssa_node.visited = false; }), true));

    // Reverse post-order iteration.
    for(auto it = ir.postorder.rbegin(); it != ir.postorder.rend(); ++it)
    {
        cfg_node_t& cfg_branch_node = **it;
        ssa_node_t& ssa_branch_node = *cfg_branch_node.exit; 

        // A branch is any cfg node with more than 1 successor.
        std::size_t const num_succ = cfg_branch_node.out.size();
        std::cout << "SUCC: " << num_succ << '\n';
        if(num_succ < 2)
            continue;

        // Our algorithm can only handle so many successors.
        // If there's too many, don't create traces.
        if(num_succ > sizeof_bits<cfg_node_t::reachable_int_t>)
            continue;

        // If the condition is const, there's no point
        // in doing a trace partition.
        if(ssa_branch_node.in[1].is_const())
            continue;

        // For each successor, mark all nodes reachable from it.
        ir.cfg_pool.foreach([](cfg_node_t& cfg_node)
                            { cfg_node.reachable = 0; });
        cfg_branch_node.reachable = (1ull << num_succ) - 1ull;
        for(std::size_t i = 0; i < num_succ; ++i)
            mark_dom_reachable(*cfg_branch_node.out[i], 1ull << i);

        // Now create the traces.
        insert_trace_data_t data = { &ir, &cfg_branch_node };
        for(std::size_t i = 0; i < num_succ; ++i)
        {
            if(cfg_branch_node.out[i]->reachable != 1ull << i)
                continue;
            data.branch_i = i;
            insert_trace(data, *ssa_branch_node.in[1], nullptr, 0);
            data.trace_map.clear();
        }
    }
}

void remove_traces(ir_t& ir)
{
    ir.ssa_pool.foreach([](ssa_node_t& ssa_node)
    {
        if(ssa_node.op != SSA_trace)
            return;

        assert(ssa_node.in[0].is_ptr());
        ssa_node_t& original = *ssa_node.in[0];

        for(ssa_reverse_edge_t output : ssa_node.out)
        {
            if(output.node->op == SSA_trace || output.node->op == SSA_pruned)
                continue;

            // TODO

        }
    });
}

// AI = abstract interpretation, NOT artificial intelligence.
struct ai_t
{
public:
    explicit ai_t(ir_t&);

private:
    // Threshold points where widening occurs.
    // Keep these in ascending order!!
    static constexpr unsigned WIDEN_TRACE_BOUNDS =  8;
    static constexpr unsigned WIDEN_TRACE        = 16;
    static constexpr unsigned WIDEN_OP_BOUNDS    = 24;
    static constexpr unsigned WIDEN_OP           = 32;

    ir_t& ir() { return *ir_ptr; }

    void queue_edge(cfg_node_t& node, unsigned out_i);
    void queue_node(ssa_node_t& node);
    void run_trace(ssa_node_t& trace);
    void visit(ssa_node_t& node);
    void run();

    ir_t* ir_ptr;
    std::vector<ssa_node_t*> ssa_worklist;
    std::vector<cfg_node_t*> cfg_worklist;
    array_pool_t<constraints_t> constraints_pool;
};

ai_t::ai_t(ir_t& ir_) : ir_ptr(&ir_)
{
    insert_traces(ir_);

    ir().cfg_pool.foreach([](cfg_node_t& cfg_node)
    {
        cfg_node.in_worklist = false;
        cfg_node.executed = false;
        cfg_node.out_executable = {};
    });

    ir().ssa_pool.foreach([this](ssa_node_t& ssa_node)
    {
        ssa_node.in_worklist = false;
        ssa_node.visited = 0;
        if(is_arithmetic(ssa_node.type.name))
            ssa_node.constraints = &constraints_pool.insert(
                constraints_t::top());
        else
            ssa_node.constraints = nullptr;
    });

    cfg_worklist.push_back(ir().root);
    ir().root->in_worklist = true;
    run();
}

void ai_t::queue_edge(cfg_node_t& node, unsigned out_i)
{
    assert(node.out[out_i]);
    if(node.out[out_i]->in_worklist || node.out_executable[out_i])
        return;
    node.out_executable[out_i] = true;
    node.out[out_i]->in_worklist = true;
    cfg_worklist.push_back(node.out[out_i]);
}

void ai_t::queue_node(ssa_node_t& node)
{
    if(node.in_worklist)
        return;
    if(!node.cfg_node->executed)
        return;
    node.in_worklist = true;
    ssa_worklist.push_back(&node);
}

void ai_t::run_trace(ssa_node_t& trace)
{
    // If there's only two arguments that means we have a 'root' trace.
    // i.e. the trace's original node is the input to the branch node.
    // The constraints of this is always constant.
    if(trace.in.size() == 2)
    {
        assert(trace.in[1].is_const());
        *trace.constraints = constraints_t::const_(trace.in[1].fixed().value);
        return;
    }

    assert(trace.in.size() > 2);
    assert(trace.in.size() % 2 == 1);

    // Do a quick check to make sure all our inputs are non-top.
    for(unsigned i = 1; i < trace.in.size(); i += 2)
    {
        ssa_node_t& parent_trace = *trace.in[i];
        assert(parent_trace.op == SSA_trace);
        if(parent_trace.constraints->is_top())
            return;
    }

    // For each parent, perform a narrowing operation.
    fixed_t::int_type const mask = arithmetic_bitmask(trace.type.name);
    constraints_t narrowed = constraints_t::bottom(mask);
    for(unsigned i = 1; i < trace.in.size(); i += 2)
    {
        ssa_node_t& parent_trace = *trace.in[i];
        assert(parent_trace.op == SSA_trace);
        assert(!parent_trace.constraints->is_top()); // Handled earlier.

        assert(parent_trace.in[0].is_ptr());
        ssa_node_t& parent_original = *parent_trace.in[0];
        
        assert(trace.in[i+1].is_const());
        unsigned const arg_i = trace.in[i+1].whole();
        unsigned const num_args = parent_original.in.size();

        // The narrow funciton expects a mutable array of constraints
        // that it modifies. Create that array here.
        constraints_t* c = ALLOCA_T(constraints_t, num_args);
        for(unsigned j = 0; j < num_args; ++j)
            c[j] = to_constraints(parent_original.in[j]);

        // When widening, replace this trace's original constraint 
        // with bottom.
        if(trace.visited > WIDEN_TRACE)
            c[arg_i] = constraints_t::bottom(mask);
        else if(trace.visited > WIDEN_TRACE_BOUNDS)
        {
            c[arg_i].bounds = bounds_t::bottom(mask);
            c[arg_i] = normalize(c[arg_i]);
        }

        assert(narrow_fn(parent_original.op));
        narrow_fn(parent_original.op)(mask, *parent_trace.constraints, 
                                      c, num_args);
        narrowed = intersect(narrowed, c[arg_i]);
    }
    *trace.constraints = normalize(narrowed);
}

void ai_t::visit(ssa_node_t& node)
{
    static int i = 0;
    std::cout << to_string(node.op) << ' ' << i++ << '\n';

    if(node.op == SSA_if)
    {
        if(has_constraints(node.in[1]))
        {
            constraints_t cond = to_constraints(node.in[1]);
            if(cond.is_top())
                return;
            if(!cond.is_const())
                goto queue_both;
            if(cond.get_const() == 0)
                queue_edge(*node.cfg_node, 0);
            else
                queue_edge(*node.cfg_node, 1);
        }
        else
        {
        queue_both:
            queue_edge(*node.cfg_node, 0);
            queue_edge(*node.cfg_node, 1);
        }
        return; // Done
    }
    else if(!node.constraints)
        return;

    constraints_t const old_constraints = normalize(*node.constraints);
    fixed_int_t const mask = arithmetic_bitmask(node.type.name);

    if(node.visited > WIDEN_OP+1)
    {
        assert(node.constraints->bit_eq(constraints_t::bottom(mask)));
        return;
    }
    else if(node.visited == WIDEN_OP+1)
        *node.constraints = constraints_t::bottom(mask);
    else if(node.op == SSA_trace)
        run_trace(node);
    else
    {
        constraints_t* c = ALLOCA_T(constraints_t, node.in.size());
        for(unsigned i = 0; i < node.in.size(); ++i)
            c[i] = to_constraints(node.in[i]);

        for(unsigned i = 0; i < node.in.size(); ++i)
            std::cout << c[i] << '\n';

        assert(abstract_fn(node.op));
        *node.constraints = abstract_fn(node.op)(mask, c, node.in.size());

        if(node.visited > WIDEN_OP_BOUNDS)
            node.constraints->bounds = bounds_t::bottom(mask);

        *node.constraints = normalize(*node.constraints);
    }

    std::cout << *node.constraints << '\n';

    assert(node.constraints->is_normalized());
    if(!node.constraints->bit_eq(old_constraints))
    {
        // Update the visited count.
        node.visited += 1;

        // Queue all outputs
        for(ssa_reverse_edge_t output : node.out)
            queue_node(*output.node);
    }
}

void ai_t::run()
{
    while(ssa_worklist.size() || cfg_worklist.size())
    {
        while(ssa_worklist.size())
        {
            ssa_node_t& ssa_node = *ssa_worklist.back();
            ssa_worklist.pop_back();
            ssa_node.in_worklist = false;

            visit(ssa_node);
        }

        while(cfg_worklist.size())
        {
            cfg_node_t& cfg_node = *cfg_worklist.back();
            cfg_worklist.pop_back();
            cfg_node.in_worklist = false;

            if(!cfg_node.executed)
            {
                cfg_node.executed = true;

                // Visit all expressions in this node.
                for(ssa_node_t* ssa_node : cfg_node.ssa_nodes)
                    queue_node(*ssa_node);
            }
            else
            {
                // Visit all phis in this node
                for(ssa_node_t* ssa_node : cfg_node.ssa_nodes)
                    if(ssa_node->op == SSA_phi)
                        queue_node(*ssa_node);
            }

            // Queue successor cfg nodes.
            if(cfg_node.out.size() == 1)
                queue_edge(cfg_node, 0);
        }
    }

    ir().ssa_pool.foreach([](ssa_node_t& ssa_node)
    {
        if(ssa_node.constraints && ssa_node.constraints->is_const())
        {
            // Remove all inputs.
            // Do this first to properly deal with self-referential nodes.
            ssa_node.link_clear_in();

            // Replace all usages with a const value.
            ssa_value_t const const_ = 
                fixed_t{ ssa_node.constraints->get_const() };
            for(ssa_reverse_edge_t output : ssa_node.out)
                output.in() = const_;
            ssa_node.out.clear();

            ssa_node.op = SSA_pruned;
        }
    });

    ir().ssa_pool.foreach([](ssa_node_t& ssa_node)
    {
        if(ssa_node.op == SSA_if && ssa_node.in[1].is_const())
        {
            // Calculate the branch index to remove first.
            bool const prune_i = !ssa_node.in[1].whole();

            // Then remove the conditional from the SSA node.
            ssa_node.op = SSA_fence;
            ssa_node.in.pop_back();
            assert(ssa_node.in.size() == 1);

            // Finally remove the branch from the CFG node.
            cfg_node_t& cfg_node = *ssa_node.cfg_node;
            assert(cfg_node.out.size() == 2);
            cfg_node.link_remove_out(prune_i);
            assert(cfg_node.out.size() == 1);
        }
    });

    ir().cfg_pool.foreach([](cfg_node_t& cfg_node)
    {
        if(!cfg_node.executed)
        {
            // Prune it!
            for(ssa_node_t* ssa_node : cfg_node.ssa_nodes)
            {
                ssa_node->link_clear_in();
                ssa_node->op = SSA_pruned;
            }

            for(unsigned i = 0; i < cfg_node.out.size(); ++i)
                cfg_node.out[i]->remove_in({ &cfg_node, i });
            cfg_node.out.clear();
        }
    });
}

} // End anonymous namespace

void o_abstract_interpret(ir_t& ir)
{
    ai_t ai(ir);
}
