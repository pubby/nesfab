#include "alloca.hpp"
#include "bitset.hpp"
#include "constraints.hpp"
#include "fixed.hpp"
#include "ir.hpp"
#include "o_phi.hpp"
#include "sizeof_bits.hpp"

#include "flat/flat_map.hpp"
#include "flat/small_set.hpp"

#include <iostream> // TODO
#include <bitset> // TODO

namespace {
// These are used as indices into 'executable' and 'out_executable' arrays.
enum executable_index_t
{
    EXEC_PROPAGATE = 0,
    EXEC_JUMP_THREAD = 1,
};

struct trace_params_t
{
    cfg_node_t* cfg_trace_node;
    ssa_node_t* parent_trace;
};

} // End anonymous namespace

// Tracks if the node can be skipped over by the jump threading pass.
// (A node can be skipped over if it contains no code that would need
//  to be duplicated along the threaded jump.)
// Also used to mark CFG nodes created by the trace pass - these will
// get removed after the optimization runs!
constexpr std::uint64_t CFG_FLAG_SKIPPABLE  = 1 << 4;

// Track if the abstract interpreter has executed the given 
// node ('executable') or edge ('out_executable').
// These are in arrays for different passes - one for regular propagation
// and the other for jump threading.
constexpr std::uint64_t CFG_FLAG_EXECUTABLE = 1 << 5; // 2 bits

struct ai_cfg_data_t
{
    std::uint64_t* out_executable;


    
    // Used in branch threading to track the path.
    unsigned input_taken;

    // These track if the abstract interpreter has executed the given 
    // node ('executable') or edge ('out_executable').
    // These are in arrays for different passes - one for regular propagation
    // and the other for jump threading.
    std::array<bool, 2> executable;
    std::array<std::uint64_t*, 2> out_executable; // Bitsets

    // Used to rebuild the SSA after inserting trace nodes.
    fc::vector_map<ssa_node_t*, ssa_node_t*> rebuild_map;

};

struct ai_ssa_data_t
{
    bool in_worklist;
    bool in_rebuild;
    bool jump_thread_touched;

    // How many times this node has been visited by the abstract interpreter.
    // This is used to determine when to widen.
    unsigned visited_count;

    // The imprecise set of all values this node can have during runtime.
    constraints_t* active_constraints;
    // 'active_constraints' points to one of these values.
    std::array<constraints_t, 2> constraints_data;

    // When a trivial phi node is found, it can be traced.
    // This vector facilitates that by storing the state needed to extend
    // the trace.
    std::vector<trace_params_t> phi_trace_extend;

    // If this node is a key to 'ai_cfg_data_t::rebuild_map', this pointer
    // holds the mapped value.
    // i.e. it holds the original value.
    ssa_node_t* rebuild_mapping;

    void set_active_constraints(executable_index_t index)
        { active_constraints = &constraints_data[index]; }
};

namespace // Anonymous namespace
{

std::size_t out_executable_size(cfg_node_t const& node)
    { return (node.output_size() + 63) / 64; }

bool has_constraints(ssa_node_t& node)
    { return is_arithmetic(node.type().name); }
bool has_constraints(ssa_value_t value)
    { return value.is_const() || has_constraints(*value); }

constraints_t to_constraints(ssa_value_t value)
{
    assert(has_constraints(value));
    if(value.is_ptr())
        return *value->ai_data->active_constraints;
    else
        return constraints_t::const_(value.fixed().value);
}

// AI = abstract interpretation, NOT artificial intelligence.
struct ai_t
{
public:
    explicit ai_t(ir_t&);

private:
    // Threshold points where widening occurs.
    // Keep these in ascending order!!
    static constexpr unsigned WIDEN_OP_BOUNDS    = 16;
    static constexpr unsigned WIDEN_OP           = 24;

    ir_t& ir() { return *ir_ptr; }

    void mark_skippable();
    void remove_skippable();

    ssa_node_t& local_lookup(cfg_node_t& cfg_node, ssa_node_t& ssa_node);
    void insert_trace(cfg_node_t& cfg_trace_node, ssa_node_t& original, 
                      ssa_value_t parent_trace, unsigned arg_i);
    void insert_traces();
    void rebuild_ssa();

    void alloc_transient_data(cfg_node_t& cfg_node);
    void alloc_transient_data(ssa_node_t& ssa_node);

    void queue_edge(cfg_node_t& node, unsigned out_i);
    void queue_node(cfg_node_t& node);
    void queue_node(executable_index_t exec_i, ssa_node_t& node);
    void queue_rebuild(ssa_node_t& node);

    void compute_trace_constraints(executable_index_t exec_i, 
                                   ssa_node_t& trace);
    void compute_constraints(executable_index_t exec_i, ssa_node_t& node);
    void visit(ssa_node_t& node);
    void range_propagate();
    void prune_dead_code();
    void prune_unreachable_code();
    void fold_consts();

    // Jump threading
    void jump_thread_visit(ssa_node_t& node);
    void run_jump_thread(cfg_node_t& start_node, unsigned start_branch_i);
    void thread_jumps();

    // Phi removal
    void replace_trivial_phis();

    /* TODO: remove
    struct jump_t
    {
        cfg_node_t* from;
        cfg_node_t* to;
        unsigned branch_taken;
        std::vector<unsigned> inputs_taken;
    };
    */

    ir_t* ir_ptr;

    array_pool_t<ai_cfg_data_t> cfg_data_pool;
    array_pool_t<ai_ssa_data_t> ssa_data_pool;
    array_pool_t<std::uint64_t> bitset_pool;
    array_pool_t<constraints_t> constraints_pool;

    std::vector<ssa_node_t*> ssa_worklist;
    std::vector<cfg_node_t*> cfg_worklist;
    std::vector<cfg_node_t*> threaded_jumps;
    std::vector<ssa_node_t*> needs_rebuild;

    bool updated;
};

ai_t::ai_t(ir_t& ir_) : ir_ptr(&ir_)
{
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        alloc_transient_data(*cfg_it);
        for(ssa_iterator_t ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
            alloc_transient_data(*ssa_it);
    }

    assert(ir().valid());

    do
    {
        updated = false;

        std::puts("DEAD");
        prune_dead_code();
        assert(ir().valid());

        std::puts("TRACE");
        insert_traces();
        assert(ir().valid());

        std::puts("REBUILD");
        replace_trivial_phis();
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
    while(updated);

    ir().reclaim_pools();
}

////////////////////////////////////////
// HELPERS                            //
////////////////////////////////////////

void ai_t::alloc_transient_data(cfg_node_t& cfg_node)
{
    cfg_node.ai_data = &cfg_data_pool.emplace();
    for(std::uint64_t*& bitset : cfg_node.ai_data->out_executable)
        bitset = bitset_pool.alloc(out_executable_size(cfg_node));
}

void ai_t::alloc_transient_data(ssa_node_t& ssa_node)
{
    ssa_node.ai_data = &ssa_data_pool.emplace();
}

void ai_t::queue_edge(cfg_node_t& node, unsigned out_i)
{
    assert(node.ai_data);
    if(bitset_test(node.ai_data->out_executable[EXEC_PROPAGATE], out_i))
        return;
    bitset_set(node.ai_data->out_executable[EXEC_PROPAGATE], out_i);
    if(node.output(out_i).ai_data->in_worklist)
        return;
    node.output(out_i).ai_data->in_worklist = true;
    cfg_worklist.push_back(&node.output(out_i));
}

void ai_t::queue_node(cfg_node_t& node)
{
    if(node.ai_data->in_worklist)
        return;
    node.in_worklist = true;
    cfg_worklist.push_back(&node);
}

void ai_t::queue_node(executable_index_t exec_i, ssa_node_t& node)
{
    assert(node.ai_data);
    if(node.ai_data->in_worklist)
        return;
    if(!node.cfg_node().ai_data->executable[exec_i])
        return;
    node.ai_data->in_worklist = true;
    ssa_worklist.push_back(&node);
}

// TODO: remove?
void ai_t::queue_rebuild(ssa_node_t& node)
{
    if(node.ai_data->in_rebuild)
        return;
    node.ai_data->in_rebuild = true;
    needs_rebuild.push_back(&node);
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
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        assert(cfg_it->ai_data);
        cfg_it->ai_data->skippable = false;
        for(ssa_iterator_t ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            assert(ssa_it->ai_data);
            if(ssa_it->ai_data->rebuild_mapping)
                continue;
            assert(ssa_it->op() != SSA_trace);
            for(unsigned i = 0; i < ssa_it->output_size(); ++i)
                if(&ssa_it->output(i).cfg_node() != cfg_it.ptr
                   && ssa_it->output(i).op() != SSA_trace)
                    goto not_skippable;
        }
        std::puts("SKIP");
        cfg_it->ai_data->skippable = true;
    not_skippable:;
    }
}

void ai_t::remove_skippable()
{
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it;)
    {
        if(cfg_it->ai_data->skippable
           && cfg_it->input_size() == 1 && cfg_it->output_size() == 1)
        {
            while(ssa_iterator_t ssa_it = cfg_it->ssa_begin())
            {
                if(ssa_node_t* mapping = ssa_it->ai_data->rebuild_mapping)
                {
                    ssa_it->replace_with(mapping);
                    ssa_it->link_clear_inputs();
                }
                cfg_it->unsafe_prune_ssa(ir(), *ssa_it);
            }
            cfg_it = ir().merge_edge(*cfg_it);
        }
        else
            ++cfg_it;
    }
}

////////////////////////////////////////
// TRACES                             //
////////////////////////////////////////

// For inserting traces, we use the same SSA-generation algorithm
// we used to generate the SSA from the AST. The difference is we
// lookup existing SSA nodes instead of local variables.
// Relevant paper:
//   Simple and Efficient Construction of Static Single Assignment Form
ssa_node_t& ai_t::local_lookup(cfg_node_t& cfg_node, ssa_node_t& ssa_node)
{
    if(&ssa_node.cfg_node() == &cfg_node)
        return ssa_node;

    assert(cfg_node.ai_data);
    auto lookup = cfg_node.ai_data->rebuild_map.find(&ssa_node);

    if(lookup != cfg_node.ai_data->rebuild_map.end())
    {
        assert(lookup->second->pruned() == false);
        return *lookup->second;
    }
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
            return local_lookup(cfg_node.input(0), ssa_node);
        default:
            ssa_node_t& phi = cfg_node.emplace_ssa(ir(), SSA_phi, 
                                                   ssa_node.type());
            alloc_transient_data(phi);
            cfg_node.ai_data->rebuild_map.emplace(&ssa_node, &phi);
            phi.ai_data->rebuild_mapping = &ssa_node;
            // Fill using local lookups:
            for(unsigned i = 0; i < phi.cfg_node().input_size(); ++i)
                phi.link_append_input(
                    &local_lookup(phi.cfg_node().input(i), ssa_node));
            return phi;
        }
    }
}

void ai_t::insert_trace(cfg_node_t& cfg_trace_node, ssa_node_t& original, 
                        ssa_value_t parent_trace, unsigned arg_i)
{
    auto& rebuild_map = cfg_trace_node.ai_data->rebuild_map;

    // A single node can appear multiple times in the condition expression.
    // Check to see if that's the case by checking if a trace already exists
    // for this node.
    auto it = rebuild_map.find(&original);
    if(it != rebuild_map.end())
    {
        // The trace already exists.
        ssa_node_t* ptr = it->second;
        assert(parent_trace.is_ptr());
        if(ptr != parent_trace.ptr())
        {
            ptr->link_append_input(parent_trace);
            ptr->link_append_input(arg_i);
        }
        return;
    }

    ssa_node_t& trace = cfg_trace_node.emplace_ssa(ir(), SSA_trace, 
                                                   original.type());
    alloc_transient_data(trace);
    rebuild_map.insert({ &original, &trace });
    trace.ai_data->rebuild_mapping = &original;

    // The first argument is the original expression it represents.
    trace.link_append_input(&original);

    // The remaining arguments come in pairs.
    // - First comes the parent trace.
    // - Second comes the argument index into the parent trace.
    trace.link_append_input(parent_trace);
    if(parent_trace.is_ptr())
        trace.link_append_input(arg_i); // First trace never has 'arg_i'.

    if(ssa_flags(original.op()) & SSAF_TRACE_INPUTS)
    {
        // Recursively trace 'original', turning its inputs into traces too.
        for(unsigned i = 0; i < original.input_size(); ++i)
            if(original.input(i).is_ptr())
                insert_trace(cfg_trace_node, *original.input(i), &trace, i);
    }
    else if(original.op() == SSA_phi)
        original.ai_data->phi_trace_extend.push_back(
            { &cfg_trace_node, &trace });

    // All outputs of the original node will need rebuilding.
    queue_rebuild(original);
}

void ai_t::insert_traces()
{
    for(cfg_iterator_t it = ir().cfg_begin(); it; ++it)
    {
        // Reset 'ai_data'.
        //TODO: remove this?
        // TODO TODO
        assert(it->ai_data);
        it->ai_data->rebuild_map.clear();
    }

    for(cfg_iterator_t it = ir().cfg_begin(); it; ++it)
    {
        cfg_node_t& cfg_branch_node = *it;
        ssa_node_t& ssa_branch_node = *cfg_branch_node.exit; 

        // A branch is any cfg node with more than 1 successor.
        std::size_t const num_succ = cfg_branch_node.output_size();
        if(num_succ < 2)
            continue;

        // If the condition is const, there's no point
        // in making a trace partition out of it.
        ssa_value_t condition = ssa_branch_node.input(1);
        if(condition.is_const())
            continue;

        // Create new CFG nodes along each branch and insert SSA_traces
        // into them.
        for(std::size_t i = 0; i < num_succ; ++i)
        {
            cfg_node_t& cfg_trace_node = 
                ir().split_edge(cfg_branch_node.output_edge(i));
            alloc_transient_data(cfg_trace_node);
            cfg_trace_node.ai_data->is_trace = true;
            insert_trace(cfg_trace_node, *condition, i, 0);
        }
    }
    assert(ir().valid());
}

// Doesn't normalize. You must normalize at call site.
void ai_t::compute_trace_constraints(executable_index_t exec_i, 
                                     ssa_node_t& trace)
{
    // If there's only two arguments that means we have a 'root' trace.
    // i.e. the trace's original node is the input to the branch node.
    // The constraints of this is always constant.
    if(trace.input_size() == 2)
    {
        assert(trace.input(1).is_const());
        *trace.ai_data->active_constraints = 
            constraints_t::const_(trace.input(1).fixed().value);
        return;
    }

    assert(trace.input_size() > 2);
    assert(trace.input_size() % 2 == 1);

    // Do a quick check to make sure all our inputs are non-top.
    for(unsigned i = 1; i < trace.input_size(); i += 2)
    {
        ssa_node_t& parent_trace = *trace.input(i);
        assert(parent_trace.op() == SSA_trace);
        if(parent_trace.ai_data->active_constraints->is_top())
            return;
    }

    // For each parent, perform a narrowing operation.
    fixed_t::int_type const mask = arithmetic_bitmask(trace.type().name);
    constraints_t narrowed = constraints_t::bottom(mask);
    for(unsigned i = 1; i < trace.input_size(); i += 2)
    {
        ssa_node_t& parent_trace = *trace.input(i);
        assert(parent_trace.op() == SSA_trace);
        // Handled earlier:
        assert(!parent_trace.ai_data->active_constraints->is_top());

        assert(parent_trace.input(0).is_ptr());
        ssa_node_t& parent_original = *parent_trace.input(0);
        
        assert(trace.input(i+1).is_const());
        unsigned const arg_i = trace.input(i+1).whole();
        unsigned const num_args = parent_original.input_size();

        // The narrow function expects a mutable array of constraints
        // that it modifies. Create that array here.
        constraints_t* c = ALLOCA_T(constraints_t, num_args);
        for(unsigned j = 0; j < num_args; ++j)
            c[j] = to_constraints(parent_original.input(j));

        assert(trace.ai_data->rebuild_mapping);
        //ssa_op_t op = trace.ai_data->rebuild_mapping->op(); TODO?
        ssa_op_t const op = parent_original.op();
        assert(narrow_fn(op));
        narrow_fn(op)(mask, *parent_trace.ai_data->active_constraints, 
                      c, num_args);
        narrowed = intersect(narrowed, c[arg_i]);
    }
    trace.ai_data->set_active_constraints(exec_i);
    *trace.ai_data->active_constraints = 
        union_(*trace.ai_data->active_constraints, narrowed);
}

void ai_t::rebuild_ssa()
{
    for(ssa_node_t* ssa_node : needs_rebuild)
    {
        if(ssa_node->pruned())
            continue;

        ssa_node_t* look_for = ssa_node->ai_data->rebuild_mapping;
        if(!look_for)
            look_for = ssa_node;

        assert(!look_for->pruned());

        for(unsigned i = 0; i < ssa_node->output_size();)
        {
            ssa_reverse_edge_t edge = ssa_node->output_edge(i);

            assert(!edge.node->pruned());
            assert(edge.node->op() != SSA_trace || edge.index == 0);

            if(!edge.node->link_change_input(edge.index,
                   &local_lookup(edge.node->input_cfg(edge.index), *look_for)))
               ++i;
        }
    }
    needs_rebuild.clear();
    assert(ir().valid());
}

////////////////////////////////////////
// RANGE PROPAGATION                  //
////////////////////////////////////////

// Doesn't normalize. You must normalize at call site.
void ai_t::compute_constraints(executable_index_t exec_i, ssa_node_t& node)
{
    fixed_int_t const mask = arithmetic_bitmask(node.type().name);

    if(node.op() == SSA_trace)
        compute_trace_constraints(exec_i, node);
    else
    {
        constraints_t* c = ALLOCA_T(constraints_t, node.input_size());
        if(node.op() == SSA_phi)
        {
            cfg_node_t& cfg_node = node.cfg_node();
            assert(node.input_size() == cfg_node.input_size());
            for(unsigned i = 0; i < node.input_size(); ++i)
            {
                auto edge = cfg_node.input_edge(i);
                if(bitset_test(edge.node->ai_data->out_executable[exec_i], 
                               edge.index))
                    c[i] = to_constraints(node.input(i));
                else
                    c[i] = constraints_t::top();
            }
        }
        else
            for(unsigned i = 0; i < node.input_size(); ++i)
                c[i] = to_constraints(node.input(i));

        assert(abstract_fn(node.op()));
        node.ai_data->set_active_constraints(exec_i);
        *node.ai_data->active_constraints = 
            abstract_fn(node.op())(mask, c, node.input_size());
    }
}

void ai_t::visit(ssa_node_t& node)
{
    assert(!node.pruned());
    //std::cout << "visit " << node.op() << '\n';
    if(node.op() == SSA_if)
    {
        if(has_constraints(node.input(1)))
        {
            constraints_t cond = to_constraints(node.input(1));
            if(cond.is_top())
                return;
            if(!cond.is_const())
                goto queue_both;
            if(cond.get_const() == 0)
                queue_edge(node.cfg_node(), 0);
            else
                queue_edge(node.cfg_node(), 1);
        }
        else
        {
        queue_both:
            queue_edge(node.cfg_node(), 0);
            queue_edge(node.cfg_node(), 1);
        }
        return; // Done
    }
    else if(!has_constraints(&node))
        return;

    constraints_t const old_constraints = 
        normalize(*node.ai_data->active_constraints);
    fixed_int_t const mask = arithmetic_bitmask(node.type().name);
    assert(old_constraints.is_normalized());

    if(node.ai_data->visited_count >= WIDEN_OP)
        *node.ai_data->active_constraints = constraints_t::bottom(mask);
    else
    {
        compute_constraints(EXEC_PROPAGATE, node);
        if(node.ai_data->visited_count > WIDEN_OP_BOUNDS)
            node.ai_data->active_constraints->bounds = bounds_t::bottom(mask);
        node.ai_data->active_constraints->normalize();
    }

    //std::cout << old_constraints << '\n';
    //std::cout << *node.ai_data->active_constraints << '\n';

    assert(node.ai_data->active_constraints->is_normalized());
    if(!node.ai_data->active_constraints->bit_eq(old_constraints))
    {
        assert(is_subset(old_constraints, *node.ai_data->active_constraints));

        // Update the visited count.
        // Traces increment twice as fast, which is beneficial for widening.
        node.ai_data->visited_count += node.op() == SSA_trace ? 2 : 1;

        // Queue all outputs
        for(unsigned i = 0; i < node.output_size(); ++i)
            queue_node(EXEC_PROPAGATE, node.output(i));
    }
}

void ai_t::range_propagate()
{
    assert(ssa_worklist.empty());
    assert(cfg_worklist.empty());

    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_it->ai_data->in_worklist = false;
        cfg_it->ai_data->executable[EXEC_PROPAGATE] = 0;
        bitset_reset_all(out_executable_size(*cfg_it),
                         cfg_it->ai_data->out_executable[EXEC_PROPAGATE]);

        for(ssa_iterator_t ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_it->ai_data->in_worklist = false;
            ssa_it->ai_data->visited_count = 0;
            ssa_it->ai_data->set_active_constraints(EXEC_PROPAGATE);
            *ssa_it->ai_data->active_constraints = constraints_t::top();
        }
    }

    cfg_worklist.push_back(ir().root);
    ir().root->ai_data->in_worklist = true;

    assert(ir().valid());
    while(ssa_worklist.size() || cfg_worklist.size())
    {
        while(ssa_worklist.size())
        {
            ssa_node_t& ssa_node = *ssa_worklist.back();
            ssa_worklist.pop_back();
            ssa_node.ai_data->in_worklist = false;
            visit(ssa_node);
        }

        while(cfg_worklist.size())
        {
            assert(cfg_worklist.back());
            cfg_node_t& cfg_node = *cfg_worklist.back();
            cfg_worklist.pop_back();
            cfg_node.ai_data->in_worklist = false;

            if(!cfg_node.ai_data->executable[EXEC_PROPAGATE])
            {
                cfg_node.ai_data->executable[EXEC_PROPAGATE] = true;

                // Visit all expressions in this node.
                for(auto ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
                    queue_node(EXEC_PROPAGATE, *ssa_it);
            }
            else
            {
                // Visit all phis in this node
                for(auto ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
                    if(ssa_it->op() == SSA_phi)
                        queue_node(EXEC_PROPAGATE, *ssa_it);
            }

            // Queue successor cfg nodes.
            if(cfg_node.output_size() == 1)
                queue_edge(cfg_node, 0);
        }
    }
    assert(ir().valid());
}

////////////////////////////////////////
// PRUNING                            //
////////////////////////////////////////

void ai_t::prune_dead_code()
{
    /* TODO
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        for(ssa_iterator_t ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_it->ai_data->effectful = ssa_it->has_side_effects();
            if(ssa_it->ai_data->effectful)
            {

            }
        }
    }
    */
}

void ai_t::prune_unreachable_code()
{
    // Replace branches with constant conditionals with non-branching jumps.
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        if(!cfg_it->exit)
            continue;
        ssa_node_t& exit = *cfg_it->exit;
        assert(&exit.cfg_node() == cfg_it.ptr);
        // Only handles if, not switch. TODO
        if(exit.op() != SSA_if)
            continue;

        constraints_t c = to_constraints(exit.input(1));
        if(!c.is_const())
            continue;

        // Calculate the branch index to remove first.
        bool const prune_i = !(c.get_const() >> fixed_t::shift);

        // Then replace the conditional with a fence.
        exit.link_clear_inputs();
        assert(exit.output_size() == 0);
        cfg_it->unsafe_prune_ssa(ir(), exit);

        // Finally remove the branch from the CFG node.
        assert(cfg_it->output_size() == 2);
        cfg_it->link_remove_output(prune_i);
        assert(cfg_it->output_size() == 1);

        updated = true;
    }

    assert(ir().valid());

    // Remove all CFG nodes that weren't executed by the abstract interpreter.
    for(cfg_iterator_t it = ir().cfg_begin(); it;)
    {
        if(it->ai_data->executable[EXEC_PROPAGATE])
            ++it;
        else
        {
            it->link_clear_outputs();
            it->unsafe_prune_ssa(ir());
            it = ir().unsafe_prune_cfg(*it);

            updated = true;
        }
    }

    assert(ir().valid());
}

void ai_t::fold_consts()
{
    // Replace nodes determined to be constant with a constant ssa_value_t.
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        for(ssa_iterator_t ssa_it = cfg_it->ssa_begin(); ssa_it;)
        {
            if(has_constraints(*ssa_it) 
               && ssa_it->ai_data->active_constraints->is_const())
            {
                // Replace the node with a constant.
                ssa_it->link_clear_inputs();
                ssa_it->replace_with_const({ 
                    ssa_it->ai_data->active_constraints->get_const() });

                // Delete the node.
                ssa_it = cfg_it->unsafe_prune_ssa(ir(), *ssa_it);

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

void ai_t::jump_thread_visit(ssa_node_t& node)
{
    if(!has_constraints(node))
        return;

    constraints_t const old_constraints = 
        normalize(*node.ai_data->active_constraints);

    compute_constraints(EXEC_JUMP_THREAD, node);
    node.ai_data->active_constraints->normalize();

    if(!node.ai_data->active_constraints->bit_eq(old_constraints))
    {
        // Queue all outputs
        for(unsigned i = 0; i < node.output_size(); ++i)
        {
            node.output(i).ai_data->jump_thread_touched = true;
            queue_node(EXEC_JUMP_THREAD, node.output(i));
        }
    }
}

void ai_t::run_jump_thread(cfg_node_t& start_node, unsigned start_branch_i)
{
    // Reset state
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_it->ai_data->executable[EXEC_JUMP_THREAD] = false;
        bitset_reset_all(out_executable_size(*cfg_it),
                         cfg_it->ai_data->out_executable[EXEC_JUMP_THREAD]);
        for(ssa_iterator_t ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_it->ai_data->jump_thread_touched = false;
            ssa_it->ai_data->set_active_constraints(EXEC_PROPAGATE);
        }
    }

    start_node.ai_data->executable[EXEC_JUMP_THREAD] = true;
    cfg_node_t* cfg_node = &start_node;
    unsigned branch_i = start_branch_i;
    unsigned branches_skipped = 0;
    while(true)
    {
        // Take the branch.
        bitset_set(cfg_node->ai_data->out_executable[EXEC_JUMP_THREAD],
                   branch_i);
        unsigned const input_i = cfg_node->output_edge(branch_i).index;
        cfg_node = &cfg_node->output(branch_i);
        cfg_node->ai_data->input_taken = input_i;

        // If we've ended up in a loop, abort!
        if(cfg_node->ai_data->executable[EXEC_JUMP_THREAD])
            break;
        cfg_node->ai_data->executable[EXEC_JUMP_THREAD] = true;

        // If we've reached an unskippable or end node, abort!
        if(!cfg_node->ai_data->skippable || cfg_node->output_size() == 0)
            break;

        // Queue and then update all relevant SSA nodes in this CFG node.
        for(auto ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
            if(ssa_it->op() == SSA_phi || ssa_it->ai_data->jump_thread_touched)
                queue_node(EXEC_JUMP_THREAD, *ssa_it);

        // Here's where we update the SSA nodes:
        while(ssa_worklist.size())
        {
            ssa_node_t& ssa_node = *ssa_worklist.back();
            ssa_worklist.pop_back();
            ssa_node.in_worklist = false;
            jump_thread_visit(ssa_node);
        }

        // Check to see if the next path is forced, taking it if it is.
        assert(cfg_node->output_size() != 0); // Handled earlier.
        if(cfg_node->exit)
        {
            constraints_t const c = to_constraints(cfg_node->exit->input(1));
            if(!c.is_const())
                break;
            branch_i = c.get_const() >> fixed_t::shift;
            ++branches_skipped;
        }
        else
        {
            // Non-conditional nodes are always forced!
            assert(cfg_node->output_size() == 1
                   || (!cfg_node->exit && cfg_node->output_size() > 1));
            branch_i = 0;
        }
    }

    if(branches_skipped == 0)
        return;

    cfg_node_t& end_node = *cfg_node;

    cfg_node_t& trace = start_node.output(start_branch_i);
    assert(trace.output_size() == 1);
    trace.link_append_output(end_node, [&](ssa_node_t& phi) -> ssa_value_t
    {
        // Phi nodes in the target need a new input argument.
        // Find that here by walking the path backwards until
        // reaching a node outside of the path or in the first path node.
        ssa_value_t v = &phi;
        while(v.is_ptr()
              && v->op() == SSA_phi 
              && &v->cfg_node() != &start_node
              && v->cfg_node().ai_data->executable[EXEC_JUMP_THREAD])
        {
            unsigned input_i = v->cfg_node().ai_data->input_taken;
            std::cout << "step " << input_i <<'\n';
            v = v->input(input_i);
            if(v.is_const())
                break;
            if(ssa_node_t* mapping = v->ai_data->rebuild_mapping)
                v = mapping;
        }
        assert(v.is_const() 
               || &v->cfg_node() == &start_node
               || !v->cfg_node().ai_data->executable[EXEC_JUMP_THREAD]);
        return v;
    });

    threaded_jumps.push_back(&trace);
}

void ai_t::thread_jumps()
{
    threaded_jumps.clear();

    // Determine all jump thread targets.
    // TODO: simplify this
    bc::small_vector<cfg_node_t*, 32> targets;
    for(cfg_iterator_t cfg_it = ir().cfg_begin(); cfg_it; ++cfg_it)
        if(cfg_it->ai_data->skippable && cfg_it->output_size() > 1)
            targets.push_back(cfg_it.ptr);

    std::cout << "targets size: " << targets.size() << '\n';

    // Now perform the jump threading!
    for(cfg_node_t* target : targets)
    {
        for(unsigned i = 0; i < target->input_size(); ++i)
        {
            // Traverse until finding a non-forced node.
            cfg_forward_edge_t input = target->input_edge(i);
            while(input.node->ai_data->skippable 
                  && input.node->input_size() == 1
                  && input.node->output_size() == 1)
            {
                input = input.node->input_edge(0);
            }

            // For the time being, only handle 'if' nodes.
            // TODO: support switch
            if(input.node->output_size() != 2)
                continue;

            std::puts("running jump thread");
            run_jump_thread(*input.node, input.index);
        }
    }

    std::cout << "THREADS: " << threaded_jumps.size() << '\n';

    if(threaded_jumps.size() == 0)
        return;
    updated = true;

    // Find what nodes need to be rebuilt.
    /*
    for(jump_t const& jump : threaded_jumps)
    {
        unsigned const start_branch_i = jump.from->ai_data->branch_taken;
        cfg_node_t* cfg_node = &jump.from->output(start_branch_i);
        while(cfg_node != jump.to
              && cfg_node->ai_data->executable[EXEC_JUMP_THREAD])
        {
            for(auto ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
                if(ssa_it->ai_data->rebuild_mapping)
                    queue_rebuild(*ssa_it);
            cfg_node = &cfg_node->output(cfg_node->ai_data->branch_taken);
        }
    }
    */

    // Make changes to edges.
    /*
    assert(cfg_worklist.empty());
    bc::small_vector<std::pair<ssa_reverse_edge_t, ssa_node_t*>, 32> 
        phis_to_rebuild;
    for(jump_t const& jump : threaded_jumps)
    {
        cfg_node_t* const from = jump.from; // For the lambda capture.
        unsigned const start_branch_i = from->ai_data->branch_taken;
        cfg_node_t* cfg_node = &from->output(start_branch_i);

        // The first node in the path should always be a trace.
        // The jump thread will happen from this trace, so that
        // we don't have to destroy and remake it.
        assert(cfg_node->ai_data->is_trace);
        assert(cfg_node->input_size() == 1);
        assert(cfg_node->output_size() == 1);

        // Queue the first output for the prune step.
        queue_node(cfg_node->output(0));

        // Change the edge's destination to point to the jump thread target:
        cfg_node->link_change_output(0, *jump.to, 
        [from, &phis_to_rebuild](ssa_node_t& phi) -> ssa_value_t
        {
            // Phi nodes in the target need a new input argument.
            // Find that here by walking the path backwards until
            // reaching a node outside of the path or in the first path node.
            ssa_value_t v = &phi;
            while(v.is_ptr()
                  && v->op() == SSA_phi 
                  && &v->cfg_node() != from)
                  //&& v->cfg_node().ai_data->executable[EXEC_JUMP_THREAD])
            {
                unsigned input_i = v->cfg_node().ai_data->input_taken;
                std::cout << "step " << input_i <<'\n';
                v = v->input(input_i);
                if(v.is_const())
                    break;
                //if(ssa_node_t* mapping = v->ai_data->rebuild_mapping)
                    //v = mapping;
            }
            assert(v.is_const() 
                   || &v->cfg_node() == from
                   || !v->cfg_node().ai_data->executable[EXEC_JUMP_THREAD]);
            if(v.is_const())
                std::cout << "const " << v.whole() << '\n';

                return v;
            //phis_to_rebuild.emplace_back(
                //ssa_reverse_edge_t{ &phi, phi.input_size() }, v.ptr());
            //return 0u;
        });
    }
    */

    for(cfg_node_t* jump : threaded_jumps)
    {
        assert(jump->output_size() == 2);
        queue_node(jump->output(0));
        jump->link_remove_output(0);
    }

    // Prune unreachable nodes with no inputs here.
    // (Jump threading can create such nodes)
    while(cfg_worklist.size())
    {
        cfg_node_t* cfg_node = cfg_worklist.back();
        cfg_node->ai_data->in_worklist = false;
        cfg_worklist.pop_back();

        if(cfg_node->input_size() == 0)
        {
            for(unsigned i = 0; i < cfg_node->output_size(); ++i)
                if(&cfg_node->output(i) != cfg_node)
                    queue_node(cfg_node->output(i));

            cfg_node->link_clear_outputs();
            cfg_node->unsafe_prune_ssa(ir());
            ir().unsafe_prune_cfg(*cfg_node);
        }
    }

    // Rebuild the SSA.
    /*
    rebuild_ssa();
    for(auto const& pair : phis_to_rebuild)
    {
        ssa_reverse_edge_t const& edge = pair.first;
        if(edge.node->pruned())
            continue;
        assert(!pair.second->pruned());
        edge.node->link_change_input(edge.index,
            &local_lookup(edge.node->input_cfg(edge.index), *pair.second));
    }
    */
}

////////////////////////////////////////
// JUMP THREADING                     //
////////////////////////////////////////

void ai_t::replace_trivial_phis()
{
    ir().ssa_foreach([&](ssa_node_t& ssa_node)
    {
        if(ssa_node.op() == SSA_phi)
        {
            ssa_node.ai_data->in_worklist = true;
            ssa_worklist.push_back(&ssa_node);
        }
        // Don't even bother setting 'in_worklist' for non-phi nodes.
    });

    while(ssa_worklist.size())
    {
        ssa_node_t& phi = *ssa_worklist.back();
        phi.ai_data->in_worklist = false;
        ssa_worklist.pop_back();

        if(ssa_value_t value = get_trivial_phi_value(phi))
        {
            // Add all dependent phi nodes to the worklist.
            for(unsigned i = 0; i < phi.output_size(); ++i)
            {
                ssa_node_t& output = phi.output(i);
                if(output.op() == SSA_phi 
                   && &output != &phi 
                   && output.ai_data->in_worklist == false)
                {
                    ssa_worklist.push_back(&output);
                    output.ai_data->in_worklist = true;
                }
            }

            // Replace the trivial phi with a dummy cast.
            phi.link_clear_inputs();
            phi.link_append_input(value);
            //phi.set_op(SSA_cast);
            throw 0; // TODO!
            assert(false);

            // Extend traces tied to this phi.
            if(value.is_ptr())
            {
                for(trace_params_t& params : phi.ai_data->phi_trace_extend)
                {
                    if(params.cfg_trace_node->pruned())
                        continue;
                    if(params.parent_trace->pruned())
                        continue;
                    insert_trace(*params.cfg_trace_node, *value,
                                 params.parent_trace, 0);
                }
                phi.ai_data->phi_trace_extend.clear();
            }
        }
    }

    // Inserting a trace node schedules it for a SSA rebuild.
    // Do that rebuild here.
    rebuild_ssa();
}

} // End anonymous namespace

void o_abstract_interpret(ir_t& ir)
{
    ai_t ai(ir);
}
