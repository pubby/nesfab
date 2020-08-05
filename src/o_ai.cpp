#include "o_ai.hpp"
#include "o.hpp"

#include <array>

#include "flat/flat_map.hpp"
#include "flat/small_set.hpp"

#include "alloca.hpp"
#include "bitset.hpp"
#include "fixed.hpp"
#include "ir.hpp"
#include "o_phi.hpp"
#include "sizeof_bits.hpp"
#include "worklist.hpp"


#include <iostream> // TODO
#include <bitset> // TODO

namespace {

// These are used as indices into 'executable' and 'output_executable' arrays.
enum executable_index_t
{
    EXEC_PROPAGATE = 0,
    EXEC_JUMP_THREAD = 1,
};

struct cfg_ai_d
{
    cfg_ai_d() 
    {
        std::printf("CONSTRUCTING %i\n", (int)output_executable[0]);
    }

    ~cfg_ai_d() 
    {
        std::puts("DESTRUCTING");
    }

    // This bitset tracks if the abstract interpreter has executed along
    // a given output edge.
    std::array<std::uint64_t, 2> output_executable = {};
    static constexpr unsigned max_output_size = 64; // Number of bits in bitset 

    // This tracks if the owning CFG node has been executed.
    std::array<bool, 2> executable = {};
    
    // Used in branch threading to track the path.
    unsigned input_taken = 0;

    // Used to rebuild the SSA after inserting trace nodes.
    using rebuild_map_t = fc::vector_map<ssa_ht, ssa_ht>;
    rebuild_map_t rebuild_map;

    // Tracks if the node can be skipped over by the jump threading pass.
    // (A node can be skipped over if it contains no code that would need
    //  to be duplicated along the threaded jump.)
    // Also used to mark CFG nodes created by the trace pass - these will
    // get removed after the optimization runs!
    bool skippable = false;
};

struct ssa_ai_d
{
    // The imprecise set of all values this node can have during runtime.
    std::array<constraints_t, 2> constraints_array = 
        {{ constraints_t::top(), constraints_t::top() }};
    unsigned constraints_i = 0;

    // If this node is a key to 'ai_cfg_data_t::rebuild_map', this pointer
    // holds the mapped value.
    // i.e. it holds the original value.
    ssa_ht rebuild_mapping = {};

    // How many times this node has been visited by the abstract interpreter.
    // This is used to determine when to widen.
    unsigned visited_count = 0;

    // If any of the value's inputs were modified by the jump threading pass.
    bool touched = false;

    constraints_t& constraints() { return constraints_array[constraints_i]; } 
    void set_active_constraints(executable_index_t e) { constraints_i = e; }
};

} // End anonymous namespace

namespace // Anonymous namespace
{

bool has_constraints(ssa_node_t& node)
    { return is_arithmetic(node.type()); }
bool has_constraints(ssa_value_t value)
    { return value.is_const() || has_constraints(*value); }

constraints_t to_constraints(ssa_value_t value)
{
    assert(has_constraints(value));
    if(value.is_handle())
        return value.handle().data<ssa_ai_d>().constraints();
    else
        return constraints_t::const_(value.fixed().value, 
                                     value.fixed().value ? CARRY_SET
                                                         : CARRY_CLEAR);
}

// AI = abstract interpretation, NOT artificial intelligence.
struct ai_t
{
public:
    explicit ai_t(ir_t&);

private:
    // Threshold points where widening occurs.
    // Keep these in ascending order!!
    static constexpr unsigned WIDEN_OP_BOUNDS = 16;
    static constexpr unsigned WIDEN_OP        = 24;

    ir_t& ir() { return *ir_ptr; }

    void mark_skippable();
    void remove_skippable();

    ssa_ht local_lookup(cfg_ht cfg_h, ssa_ht ssa_h);
    void insert_trace(cfg_ht cfg_trace_h, ssa_ht original_h, 
                      ssa_value_t parent_trace, unsigned arg_i);
    void insert_traces();

    void queue_edge(cfg_ht h, unsigned out_i);
    void queue_node(executable_index_t exec_i, ssa_ht h);

    void compute_trace_constraints(executable_index_t exec_i, ssa_ht trace_h);
    void compute_constraints(executable_index_t exec_i, ssa_ht ssa_h);
    void visit(ssa_ht ssa_h);
    void range_propagate();
    void prune_unreachable_code();
    void fold_consts();

    // Jump threading
    void jump_thread_visit(ssa_ht ssa_h);
    void run_jump_thread(cfg_ht start_h, unsigned start_branch_i);
    void thread_jumps();

    ir_t* ir_ptr;

    std::vector<ssa_ht> needs_rebuild;
    std::vector<cfg_ht> threaded_jumps;

public:
    bool updated = false;
};

ai_t::ai_t(ir_t& ir_) : ir_ptr(&ir_)
{
    // Currently, the AI implementation has a limit on the number of
    // output edges a node can have. This could be worked around, but 
    // it's rare in practice and simpler to code this way.
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
        if(cfg_it->output_size() > cfg_ai_d::max_output_size)
            return;

    assert(ir().valid());

    std::puts("TRACE");
    insert_traces();
    assert(ir().valid());

    std::puts("PROPAGATE");
    range_propagate();
    assert(ir().valid());

    std::puts("PRUNE");
    prune_unreachable_code();
    assert(ir().valid());

    std::puts("MARK SKIP");
    mark_skippable();
    assert(ir().valid());

    std::puts("THREAD");
    thread_jumps();
    assert(ir().valid());

    std::puts("REMOVE SKIP");
    remove_skippable();
    assert(ir().valid());
    
    std::puts("FOLD");
    fold_consts();
    assert(ir().valid());
}

////////////////////////////////////////
// HELPERS                            //
////////////////////////////////////////

void ai_t::queue_edge(cfg_ht h, unsigned out_i)
{
    cfg_node_t& node = *h;
    auto& data = h.data<cfg_ai_d>();

    if(data.output_executable[EXEC_PROPAGATE] & (1ull << out_i))
        return;

    data.output_executable[EXEC_PROPAGATE] |= (1ull << out_i);
    cfg_worklist::push(node.output(out_i));
}

void ai_t::queue_node(executable_index_t exec_i, ssa_ht h)
{
    if(!h->cfg_node().data<cfg_ai_d>().executable[exec_i])
        return;
    ssa_worklist::push(h);
}

////////////////////////////////////////
// SKIPPABLE                          //
////////////////////////////////////////

void ai_t::mark_skippable()
{
    // A skippable CFG is one where every SSA node is either:
    // - A node inserted during SSA reconstruction
    // - A node that is used in its own CFG node but not anywhere else
    // These CFG nodes are what can be skipped over by jump threading.
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        assert(cfg_it->test_flags(FLAG_IN_WORKLIST) == false);

        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;
            auto& ssa_data = ssa_it.data<ssa_ai_d>();

            if(ssa_data.rebuild_mapping)
                continue;

            assert(ssa_node.op() != SSA_trace);

            unsigned const output_size = ssa_node.output_size();
            for(unsigned i = 0; i < output_size; ++i)
            {
                ssa_node_t& output = *ssa_node.output(i);
                if(output.cfg_node() != cfg_it && output.op() != SSA_trace)
                    goto not_skippable;
            }
        }

        // Mark it as skippable!
        cfg_it.data<cfg_ai_d>().skippable = true;
    not_skippable:;
    }
}

// Prunes nodes found by 'mark_skippable' -- but only when they are
// nodes with 1 input and 1 output.
void ai_t::remove_skippable()
{
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it;)
    {
        cfg_node_t& cfg_node = *cfg_it;

        if((cfg_it.data<cfg_ai_d>().skippable)
           && cfg_node.input_size() == 1 
           && cfg_node.output_size() == 1)
        {
            // Before pruning the CFG node, all SSA nodes it contains must be
            // pruned too.
            while(ssa_ht ssa_it = cfg_it->ssa_begin())
            {
                ssa_node_t& ssa_node = *ssa_it;
                auto& ssa_data = ssa_it.data<ssa_ai_d>();

                // Nodes inserted during the SSA rebuild can simply be
                // replaced with their original value.
                if(ssa_ht mapping_h = ssa_data.rebuild_mapping)
                    ssa_node.replace_with(mapping_h);

                ssa_node.link_clear_inputs();
                cfg_node.unsafe_prune_ssa(ssa_it);
            }

            // Prune the skippable node.
            cfg_it = ir().merge_edge(cfg_it);
            std::puts("done merge");
        }
        else
            ++cfg_it;
    }
    std::puts("done shit");

    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
        std::cout << "iterating " << cfg_it.index << '\n';
}

////////////////////////////////////////
// TRACES                             //
////////////////////////////////////////

// For inserting traces, we use the same SSA-generation algorithm
// we used to generate the SSA from the AST. The difference is we
// lookup existing SSA nodes instead of local variables.
// Relevant paper:
//   Simple and Efficient Construction of Static Single Assignment Form
ssa_ht ai_t::local_lookup(cfg_ht cfg_h, ssa_ht ssa_h)
{
    cfg_node_t& cfg_node = *cfg_h;
    ssa_node_t& ssa_node = *ssa_h;

    if(ssa_node.cfg_node() == cfg_h)
        return ssa_h;

    auto& cfg_data = cfg_h.data<cfg_ai_d>();
    auto lookup = cfg_data.rebuild_map.find(ssa_h);

    if(lookup != cfg_data.rebuild_map.end())
        return lookup->second;
    else
    {
        // If 'cfg_node' doesn't contain a definition for 'ssa_node',
        // recursively look up its definition in predecessor nodes.
        // If there are multiple predecessors, a phi node will be created.
        switch(cfg_node.input_size())
        {
        case 0:
            throw std::runtime_error("Local lookup failed.");
        case 1:
            return local_lookup(cfg_node.input(0), ssa_h);
        default:
            ssa_ht phi_h = cfg_node.emplace_ssa(SSA_phi, ssa_node.type());
            ssa_data_pool::resize<ssa_ai_d>(phi_h.index + 1);

            auto& phi_data = phi_h.data<ssa_ai_d>();
            phi_data.rebuild_mapping = ssa_h;
            cfg_data.rebuild_map.emplace(ssa_h, phi_h);

            // Fill using local lookups:
            ssa_node_t& phi = *phi_h;
            unsigned const input_size = cfg_node.input_size();
            phi.alloc_input(input_size);
            for(unsigned i = 0; i < input_size; ++i)
                phi.build_set_input(
                    i, local_lookup(cfg_node.input(i), ssa_h));

            /* TODO
            // Potentially optimize it out.
            if(ssa_value_t v = get_trivial_phi_value(phi))
            {
                phi.replace_with(ssa_h);
                phi_h = ssa_h;
            }
            */

            return phi_h;
        }
    }
}

void ai_t::insert_trace(cfg_ht cfg_trace_h, ssa_ht original_h, 
                        ssa_value_t parent_trace, unsigned arg_i)
{
    cfg_node_t& cfg_trace_node = *cfg_trace_h;
    auto& cfg_trace_data = cfg_trace_h.data<cfg_ai_d>();
    auto& rebuild_map = cfg_trace_data.rebuild_map;

    // A single node can appear multiple times in the condition expression.
    // Check to see if that's the case by checking if a trace already exists
    // for this node.
    auto it = rebuild_map.find(original_h);
    if(it != rebuild_map.end())
    {
        // The trace already exists.
        ssa_ht h = it->second;
        assert(parent_trace.is_handle());
        if(h != parent_trace.handle())
        {
            ssa_node_t& node = *h;
            node.link_append_input(parent_trace);
            node.link_append_input(arg_i);
        }
        return;
    }

    ssa_ht trace_h = cfg_trace_node.emplace_ssa(SSA_trace, original_h->type());
    ssa_data_pool::resize<ssa_ai_d>(ssa_pool::array_size());
    ssa_node_t& trace = *trace_h;
    // All references to SSA nodes have been invalidated by the new node!!

    rebuild_map.insert({ original_h, trace_h });

    auto& trace_data = trace_h.data<ssa_ai_d>();
    assert(original_h);
    trace_data.rebuild_mapping = original_h;

    if(parent_trace.is_handle())
    {
        trace.alloc_input(3);

        // The first argument is the original expression it represents.
        trace.build_set_input(0, original_h);

        // The remaining arguments come in pairs.
        // - First comes the parent trace.
        // - Second comes the argument index into the parent trace.
        trace.build_set_input(1, parent_trace);
        trace.build_set_input(2, arg_i);
    }
    else
    {
        // If 'parent_trace' is a constant, that means this is the first
        // trace in the trace-graph. We represent this node slightly
        // differently, using only the first two arguments.
        trace.alloc_input(2);
        trace.build_set_input(0, original_h);
        trace.build_set_input(1, parent_trace);
    }

    ssa_node_t& original = *original_h;
    if(ssa_flags(original.op()) & SSAF_TRACE_INPUTS)
    {
        // Recursively trace 'original', turning its inputs into traces too.
        unsigned const input_size = original.input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t input = original.input(i);
            if(input.is_handle())
                insert_trace(cfg_trace_h, input.handle(), trace_h, i);
        }
    }

    // All outputs of the original node will need rebuilding.
    needs_rebuild.push_back(original_h);
}

void ai_t::insert_traces()
{
    for(cfg_ht it = ir().cfg_begin(); it; ++it)
    {
        cfg_node_t& cfg_branch_node = *it;
        ssa_node_t& ssa_branch_node = *cfg_branch_node.exit; 

        // A branch is any cfg node with more than 1 successor.
        unsigned const output_size = cfg_branch_node.output_size();
        if(output_size < 2)
            continue;

        // If the condition is const, there's no point
        // in making a trace partition out of it.
        ssa_value_t condition = ssa_branch_node.input(1);
        if(condition.is_const())
            continue;

        // Create new CFG nodes along each branch and insert SSA_traces
        // into them.
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht cfg_trace_h = 
                ir().split_edge(cfg_branch_node.output_edge(i));
            cfg_data_pool::resize<cfg_ai_d>(cfg_trace_h.index + 1);
            insert_trace(cfg_trace_h, condition.handle(), i, 0);
        }
    }
    assert(ir().valid());

    for(ssa_ht ssa_h : needs_rebuild)
    {
        ssa_node_t& ssa_node = *ssa_h;
        auto& ssa_data = ssa_h.data<ssa_ai_d>();

        ssa_ht look_for = ssa_data.rebuild_mapping;
        if(!look_for)
            look_for = ssa_h;

        for(unsigned i = 0; i < ssa_node.output_size();)
        {
            ssa_bck_edge_t edge = ssa_node.output_edge(i);
            ssa_node_t& edge_node = *edge.handle;

            assert(edge_node.op() != SSA_trace || edge.index == 0);

            ssa_ht lookup = local_lookup(edge_node.input_cfg(edge.index), 
                                         look_for);

            if(!edge_node.link_change_input(edge.index, lookup))
               ++i;
        }
    }
    needs_rebuild.clear();
}

// Doesn't normalize. You must normalize at call site.
void ai_t::compute_trace_constraints(executable_index_t exec_i, ssa_ht trace_h)
{
    ssa_node_t& trace = *trace_h;
    auto& trace_data = trace_h.data<ssa_ai_d>();

    // If there's only two arguments that means we have a 'root' trace.
    // i.e. the trace's original node is the input to the branch node.
    // The constraints of this is always constant.
    if(trace.input_size() == 2)
    {
        assert(trace.input(1).is_const());
        trace_data.constraints() = 
            constraints_t::const_(trace.input(1).fixed().value, CARRY_BOTTOM);
        return;
    }

    assert(trace.input_size() > 2);
    assert(trace.input_size() % 2 == 1);

    // Do a quick check to make sure all our inputs are non-top.
    unsigned const input_size = trace.input_size();
    for(unsigned i = 1; i < input_size; i += 2)
    {
        ssa_ht parent_trace_h = trace.input(i).handle();
        assert(parent_trace_h->op() == SSA_trace);
        if(parent_trace_h.data<ssa_ai_d>().constraints().is_top())
            return;
    }

    // For each parent, perform a narrowing operation.
    fixed_t::int_type const mask = arithmetic_bitmask(trace.type());
    constraints_t narrowed = constraints_t::bottom(mask);
    for(unsigned i = 1; i < input_size; i += 2)
    {
        ssa_ht parent_trace_h = trace.input(i).handle();
        ssa_node_t& parent_trace = *parent_trace_h;
        auto& parent_trace_data = parent_trace_h.data<ssa_ai_d>();

        assert(parent_trace.op() == SSA_trace);
        assert(!parent_trace_data.constraints().is_top()); // Handled earlier

        assert(parent_trace.input(0).is_handle());
        ssa_ht parent_original_h = parent_trace.input(0).handle();
        ssa_node_t& parent_original = *parent_original_h;
        
        assert(trace.input(i+1).is_const());
        unsigned const arg_i = trace.input(i+1).whole();
        unsigned const num_args = parent_original.input_size();

        // The narrow function expects a mutable array of constraints
        // that it modifies. Create that array here.
        constraints_t* c = ALLOCA_T(constraints_t, num_args);
        for(unsigned j = 0; j < num_args; ++j)
            c[j] = to_constraints(parent_original.input(j));

        assert(parent_trace_data.rebuild_mapping);
        ssa_op_t const op = parent_original.op();
        assert(narrow_fn(op));
        // Call the narrowing op:
        narrow_fn(op)(mask, parent_trace_data.constraints(), c, num_args);
        narrowed = intersect(narrowed, c[arg_i]);
    }
    trace_data.set_active_constraints(exec_i);
    trace_data.constraints() = union_(trace_data.constraints(), narrowed);
}

////////////////////////////////////////
// RANGE PROPAGATION                  //
////////////////////////////////////////

// Doesn't normalize. You must normalize at call site.
void ai_t::compute_constraints(executable_index_t exec_i, ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;
    fixed_int_t const mask = arithmetic_bitmask(ssa_node.type());

    if(ssa_node.op() == SSA_trace)
        compute_trace_constraints(exec_i, ssa_h);
    else
    {
        // Build an array holding all the argument's constraints.
        unsigned const input_size = ssa_node.input_size();
        constraints_t* c = ALLOCA_T(constraints_t, input_size);
        if(ssa_node.op() == SSA_phi)
        {
            // For phi nodes, if an input CFG edge hasn't been marked
            // executable, treat the argument's constraint as TOP.
            cfg_ht cfg_h = ssa_node.cfg_node();
            cfg_node_t& cfg_node = *cfg_h;

            assert(input_size == cfg_node.input_size());
            for(unsigned i = 0; i < input_size; ++i)
            {
                auto edge = cfg_node.input_edge(i);
                auto& edge_data = edge.handle.data<cfg_ai_d>();

                if(edge_data.output_executable[exec_i] & (1ull << edge.index))
                    c[i] = to_constraints(ssa_node.input(i));
                else
                    c[i] = constraints_t::top();
            }
        }
        else for(unsigned i = 0; i < input_size; ++i)
            c[i] = to_constraints(ssa_node.input(i));

        auto& ssa_data = ssa_h.data<ssa_ai_d>();

        // Call the ai op:
        assert(abstract_fn(ssa_node.op()));
        ssa_data.set_active_constraints(exec_i);
        ssa_data.constraints() =
            abstract_fn(ssa_node.op())(mask, c, input_size);
    }
}

// Performs range propagatation on a single SSA node.
void ai_t::visit(ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;
    std::cout << "visit " << to_string(ssa_node.op()) << '\n';

    if(ssa_node.op() == SSA_if)
    {
        if(has_constraints(ssa_node.input(1)))
        {
            constraints_t cond = to_constraints(ssa_node.input(1));
            if(cond.is_top())
                return;
            if(!cond.is_val_const())
                goto queue_both;
            if(cond.get_val_const() == 0)
                queue_edge(ssa_node.cfg_node(), 0);
            else
                queue_edge(ssa_node.cfg_node(), 1);
        }
        else
        {
        queue_both:
            queue_edge(ssa_node.cfg_node(), 0);
            queue_edge(ssa_node.cfg_node(), 1);
        }
        return; // Done
    }
    else if(!has_constraints(ssa_h))
        return;

    auto& ssa_data = ssa_h.data<ssa_ai_d>();

    constraints_t const old_constraints = normalize(ssa_data.constraints());
    assert(old_constraints.is_normalized());

    fixed_int_t const mask = arithmetic_bitmask(ssa_node.type());

    if(ssa_data.visited_count >= WIDEN_OP)
        ssa_data.constraints() = constraints_t::bottom(mask);
    else
    {
        compute_constraints(EXEC_PROPAGATE, ssa_h);
        if(ssa_data.visited_count > WIDEN_OP_BOUNDS)
            ssa_data.constraints().bounds = bounds_t::bottom(mask);
        ssa_data.constraints().normalize();
    }

    std::cout << "C = " << ssa_data.constraints() << '\n';
    std::cout << "O = " << old_constraints << '\n';

    assert(ssa_data.constraints().is_normalized());
    if(!ssa_data.constraints().bit_eq(old_constraints))
    {
        std::cout << "UPP\n";
        assert(is_subset(old_constraints, ssa_data.constraints()));

        // Update the visited count. Traces increment twice as fast, which
        // was chosen to improve widening behavior.
        ssa_data.visited_count += ssa_node.op() == SSA_trace ? 2 : 1;

        // Queue all outputs
        unsigned const output_size = ssa_node.output_size();
        for(unsigned i = 0; i < output_size; ++i)
            queue_node(EXEC_PROPAGATE, ssa_node.output(i));
    }
}

void ai_t::range_propagate()
{
    assert(ssa_worklist::empty());
    assert(cfg_worklist::empty());

    // Reset the flags.
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_ai_d>();
        cfg_data.skippable = false;
        assert(cfg_node.test_flags(FLAG_IN_WORKLIST) == false);
        assert(cfg_data.output_executable[EXEC_PROPAGATE] == 0ull);

        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;
            auto& ssa_data = ssa_it.data<ssa_ai_d>();
            ssa_data.touched = false;
            assert(ssa_node.test_flags(FLAG_IN_WORKLIST) == false);
            assert(ssa_data.visited_count == 0);
            assert(ssa_data.constraints_i == EXEC_PROPAGATE);
            assert(ssa_data.constraints().bit_eq(constraints_t::top()));
        }
    }

    cfg_worklist::push(ir().root);

    assert(ir().valid());
    while(!ssa_worklist::empty() || !cfg_worklist::empty())
    {
        while(!ssa_worklist::empty())
            visit(ssa_worklist::pop());

        while(!cfg_worklist::empty())
        {
            cfg_ht cfg_h = cfg_worklist::pop();
            std::printf("CFG VISIT %i\n", cfg_h.index);
            cfg_node_t& cfg_node = *cfg_h;
            auto& cfg_data = cfg_h.data<cfg_ai_d>();

            if(!cfg_data.executable[EXEC_PROPAGATE])
            {
                cfg_data.executable[EXEC_PROPAGATE] = true;

                // Visit all expressions in this node.
                for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
                    queue_node(EXEC_PROPAGATE, ssa_it);
            }
            else
            {
                // Visit all phis in this node
                for(auto phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
                {
                    assert(phi_it->op() == SSA_phi);
                    queue_node(EXEC_PROPAGATE, phi_it);
                }
            }

            // Queue successor cfg nodes.
            if(cfg_node.output_size() == 1)
                queue_edge(cfg_h, 0);
        }
    }
    assert(ir().valid());
}

////////////////////////////////////////
// PRUNING                            //
////////////////////////////////////////

void ai_t::prune_unreachable_code()
{
    // Replace branches with constant conditionals with non-branching jumps.
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        if(!cfg_node.exit)
            continue;

        ssa_ht exit_h = cfg_node.exit;
        ssa_node_t& exit = *exit_h;
        assert(exit.cfg_node() == cfg_it);

        // Only handles if, not switch. TODO!
        if(exit.op() != SSA_if)
            continue;
        assert(cfg_node.output_size() == 2);

        constraints_t c = to_constraints(exit.input(1));
        if(!c.is_val_const())
            continue;

        // Replace the conditional exit with a fence.
        assert(exit.input(0).is_handle());
        cfg_node.exit = exit.input(0).handle();

        // First calculate the branch index to remove.
        bool const prune_i = !(c.get_val_const() >> fixed_t::shift);

        // Then remove the conditional.
        exit.link_clear_inputs();
        exit.unsafe_prune();
        assert(cfg_node.exit);

        // Finally remove the branch from the CFG node.
        cfg_node.link_remove_output(prune_i);
        assert(cfg_node.output_size() == 1);

        updated = true;
    }

    std::puts("pruned");
    assert(ir().valid());

    // Remove all CFG nodes that weren't executed by the abstract interpreter.
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it;)
    {
        auto& cfg_data = cfg_it.data<cfg_ai_d>();

        if(cfg_data.executable[EXEC_PROPAGATE])
            ++cfg_it;
        else
        {
            cfg_node_t& cfg_node = *cfg_it;

            cfg_node.link_clear_inputs();
            cfg_node.link_clear_outputs();

            std::cout << "pruning " << cfg_it.index << '\n';
            cfg_node.unsafe_prune_ssa();
            cfg_it = ir().unsafe_prune_cfg(cfg_it);

            updated = true;
        }
    }

    // TODO
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
        std::cout << "iterating " << cfg_it.index << '\n';

    std::puts("poop");
    assert(ir().valid());
    std::puts("poop");
}

void ai_t::fold_consts()
{
    // Replace nodes determined to be constant with a constant ssa_value_t.
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it;)
        {
            ssa_node_t& ssa_node = *ssa_it;
            auto& ssa_data = ssa_it.data<ssa_ai_d>();

            if(has_constraints(ssa_node)
               && ssa_data.constraints().is_val_const())
            {
                // Replace the node with a constant.
                ssa_node.link_clear_inputs();
                ssa_node.replace_with_const(
                    { ssa_data.constraints().get_val_const() });

                // Delete the node.
                ssa_it = ssa_node.unsafe_prune();

                updated = true;
            }
            else
                ++ssa_it;
        }
    }
    assert(ir().valid());
}

////////////////////////////////////////
// JUMP THREADING                     //
////////////////////////////////////////

void ai_t::jump_thread_visit(ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;

    if(!has_constraints(ssa_node))
        return;

    auto& ssa_data = ssa_h.data<ssa_ai_d>();

    constraints_t const old_constraints = normalize(ssa_data.constraints());

    compute_constraints(EXEC_JUMP_THREAD, ssa_h);
    ssa_data.constraints().normalize();

    if(!ssa_data.constraints().bit_eq(old_constraints))
    {
        // Queue all outputs
        unsigned const output_size = ssa_node.output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_ht output = ssa_node.output(i);
            output.data<ssa_ai_d>().touched = true;
            queue_node(EXEC_JUMP_THREAD, output);
        }
    }
}

void ai_t::run_jump_thread(cfg_ht start_h, unsigned start_branch_i)
{
    // Reset the state.
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_ai_d>();

        cfg_data.executable[EXEC_JUMP_THREAD] = false;
        cfg_data.output_executable[EXEC_JUMP_THREAD] = 0ull;

        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_ai_d& ssa_data = ssa_it.data<ssa_ai_d>();
            ssa_data.set_active_constraints(EXEC_PROPAGATE);
            ssa_data.touched = false;
            assert(ssa_it->test_flags(FLAG_IN_WORKLIST) == false);
        }
    }

    cfg_node_t& start_node = *start_h;
    auto& start_data = start_h.data<cfg_ai_d>();
    start_data.executable[EXEC_JUMP_THREAD] = true;

    // Step through CFG nodes, taking forced paths until reaching a branch
    // that is not forced.

    cfg_ht cfg_h = start_h;
    unsigned branch_i = start_branch_i;
    unsigned branches_skipped = 0;
    while(true)
    {
        cfg_node_t& prior_node = *cfg_h;
        auto& prior_data = cfg_h.data<cfg_ai_d>();

        // Take the branch.
        prior_data.output_executable[EXEC_JUMP_THREAD] |= 1ull << branch_i;
        unsigned const input_i = prior_node.output_edge(branch_i).index;
        cfg_h = prior_node.output(branch_i);

        cfg_node_t& cfg_node = *cfg_h;
        auto& cfg_data = cfg_h.data<cfg_ai_d>();

        cfg_data.input_taken = input_i;

        // If we've ended up in a loop, abort!
        if(cfg_data.executable[EXEC_JUMP_THREAD])
            break;
        cfg_data.executable[EXEC_JUMP_THREAD] = true;

        // If we've reached an unskippable, abort!
        if(!cfg_data.skippable)
            break;

        // If we've reached an endpoint node, abort!
        if(cfg_node.output_size() == 0)
            break;

        // Queue and then update all relevant SSA nodes in this CFG node.
        for(auto ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
            if(ssa_it->op() == SSA_phi || ssa_it.data<ssa_ai_d>().touched)
                queue_node(EXEC_JUMP_THREAD, ssa_it);

        // Here's where we update the SSA nodes:
        while(!ssa_worklist::empty())
            jump_thread_visit(ssa_worklist::pop());

        // Check to see if the next path is forced, taking it if it is.
        assert(cfg_node.output_size() != 0); // Handled earlier.
        if(cfg_node.exit)
        {
            constraints_t const c = to_constraints(cfg_node.exit->input(1));
            if(!c.is_val_const())
                break;
            branch_i = c.get_val_const() >> fixed_t::shift;
            ++branches_skipped;
        }
        else
        {
            // Non-conditional nodes are always forced!
            assert(cfg_node.output_size() == 1
                   || (!cfg_node.exit && cfg_node.output_size() > 1));
            branch_i = 0;
        }
    }

    if(branches_skipped == 0)
        return;

    cfg_ht end_h = cfg_h;

    // We've found the path. Now modify the IR.

    cfg_ht trace_h = start_node.output(start_branch_i);
    cfg_node_t& trace = *trace_h;

    assert(trace.output_size() == 1);
    trace.link_append_output(end_h, [&](ssa_ht phi_h) -> ssa_value_t
    {
        // Phi nodes in the target need a new input argument.
        // Find that here by walking the path backwards until
        // reaching a node outside of the path or in the first path node.
        ssa_ht vh = phi_h;
        while(true)
        {
            ssa_node_t& v = *vh;

            if(v.op() != SSA_phi || v.cfg_node() == start_h)
                break;

            auto& cfg_data = v.cfg_node().data<cfg_ai_d>();

            if(!cfg_data.executable[EXEC_JUMP_THREAD])
                break;

            unsigned input_i = cfg_data.input_taken;
            ssa_value_t input = v.input(input_i);

            if(input.is_const())
                return input;

            vh = input.handle();

            if(ssa_ht mapping = vh.data<ssa_ai_d>().rebuild_mapping)
                vh = mapping;
        }
        assert(vh->cfg_node() == start_h
               || !(vh->cfg_node().data<cfg_ai_d>()
                    .executable[EXEC_JUMP_THREAD]));
        return vh;
    });

    threaded_jumps.push_back(trace_h);
}

void ai_t::thread_jumps()
{
    // Find all jump threads, creating new edges and storing the endpoints in
    // 'threaded_jumps'.
    threaded_jumps.clear();
    for(cfg_ht cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        if(!cfg_it.data<cfg_ai_d>().skippable)
            continue;

        if(cfg_node.output_size() <= 1)
            continue;

        // Ok! 'cfg_node' is a jump thread target.

        unsigned const input_size = cfg_node.input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            // Traverse until finding a non-forced node.
            cfg_fwd_edge_t input = cfg_node.input_edge(i);
            while(true)
            {
                cfg_node_t& node = *input.handle;

                if(!input.handle.data<cfg_ai_d>().skippable)
                    break;

                if(node.input_size() != 1 || node.output_size() != 1)
                    break;

                input = node.input_edge(0);
            }

            // For the time being, only handle 'if' nodes.
            // TODO: support switch
            if(input.handle->output_size() != 2)
                continue;

            std::puts("running jump thread");
            run_jump_thread(input.handle, input.index);
        }
    }

    std::cout << "THREADS: " << threaded_jumps.size() << '\n';

    if(threaded_jumps.size() == 0)
        return;
    updated = true;

    // Remove prior edges that are no longer used.
    assert(cfg_worklist::empty());
    for(cfg_ht jump_h : threaded_jumps)
    {
        cfg_node_t& jump = *jump_h;
        assert(jump.output_size() == 2);
        cfg_worklist::push(jump_h); // For the next step.
        jump.link_remove_output(0);
    }

    // Prune unreachable nodes with no inputs here.
    // (Jump threading can create such nodes)
    while(!cfg_worklist::empty())
    {
        cfg_ht cfg_h = cfg_worklist::pop();
        cfg_node_t& cfg_node = *cfg_h;

        if(cfg_node.input_size() == 0)
        {
            unsigned const output_size = cfg_node.output_size();
            for(unsigned i = 0; i < output_size; ++i)
                if(cfg_node.output(i) != cfg_h)
                    cfg_worklist::push(cfg_node.output(i));

            cfg_node.link_clear_outputs();
            cfg_node.unsafe_prune_ssa();
            ir().unsafe_prune_cfg(cfg_h);
        }
    }
}

} // End anonymous namespace

bool o_abstract_interpret(ir_t& ir)
{
    cfg_data_pool::scope_guard_t<cfg_ai_d> cg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_ai_d> sg(ssa_pool::array_size());
    ai_t ai(ir);
    return ai.updated;
}
