#include "o_ai.hpp"
#include "o.hpp"

#include <array>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "flat/flat_map.hpp"
#include "flat/small_set.hpp"

#include "alloca.hpp"
#include "bitset.hpp"
#include "fixed.hpp"
#include "ir.hpp"
#include "ir_util.hpp"
#include "ir_algo.hpp"
#include "o_phi.hpp"
#include "sizeof_bits.hpp"
#include "worklist.hpp"
#include "type_mask.hpp"
#include "assert.hpp"
#include "constraints.hpp"
#include "multi.hpp"
#include "switch.hpp"

namespace bc = ::boost::container;

namespace {

// These are used as indices into 'executable' and 'output_executable' arrays.
enum executable_index_t
{
    EXEC_PROPAGATE = 0,
    EXEC_THREAD = 1,
    NUM_EXECUTABLE_INDEXES,
};

struct cfg_ai_d
{
    // This bitset tracks if the abstract interpreter has executed along
    // a given output edge.
    static constexpr unsigned max_output_size = MAX_CFG_OUTPUT; // Number of bits in bitset 
    std::array<static_bitset_t<max_output_size>, NUM_EXECUTABLE_INDEXES> output_executable = {};

    // This tracks if the owning CFG node has been executed.
    std::array<bool, NUM_EXECUTABLE_INDEXES> executable = {};
    
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
    std::array<constraints_def_t, NUM_EXECUTABLE_INDEXES> constraints_array;
    executable_index_t executable_index = {};

    // If this node is a key to 'ai_cfg_data_t::rebuild_map', this pointer
    // holds the mapped value.
    // i.e. it holds the original value.
    ssa_ht rebuild_mapping = {};

    // How many times this node has been visited by the abstract interpreter.
    // This is used to determine when to widen.
    unsigned visited_count = 0;

    // If any of the value's inputs were modified by the jump threading pass.
    bool touched = false;

    constraints_def_t& constraints() { return constraints_array[executable_index]; } 
};

cfg_ai_d& ai_data(cfg_ht h) { return h.data<cfg_ai_d>(); }
ssa_ai_d& ai_data(ssa_ht h) { return h.data<ssa_ai_d>(); }

} // End anonymous namespace

namespace // Anonymous namespace
{

std::size_t constraints_size(ssa_node_t const& node)
{
    auto const type_size = [&](type_name_t name) -> std::size_t
    {
        assert(!is_banked_ptr(name));
        if(name == TYPE_TEA)
        {
            std::size_t const size = node.type().size();
            // Only do page-sized arrays or smaller:
            return (size > 256) ? 0 : size;
        }
        if(is_scalar(name))
            return 1;
        return 0;
    };

    switch(node.op())
    {
    case SSA_add:
    case SSA_sub:
    case SSA_rol:
    case SSA_ror:
        return 2; // Second constraint is for the carry.
    case SSA_trace:
        return constraints_size(*node.input(0));
        /* TODO
    case SSA_phi:
        {
            std::size_t min_size = std::size_t(~0ull);
            unsigned const input_size = node.input_size();
            for(unsigned i = 0; i < input_size; ++i)
                if(node.input(i).holds_ref())
                    min_size = std::min(min_size, constraints_size(*node.input(i)));
            if(min_size != std::size_t(~0ull))
                return min_size;
        }
        // fall-through
        */
    default:
        // TODO
        //if(is_array_like(node.type()))
            //return node.type().size();
        //if(!is_numeric(node.type()))
            //std::printf("not numeric: %s\n", to_string(node.op()).data());
        return type_size(node.type().name());
    }
}

bool has_constraints(ssa_ht node)
{
    assert(node);
    return ai_data(node).constraints().vec.size();
}

bool has_constraints(ssa_value_t v)
{
    if(v.is_handle())
        return has_constraints(v.handle());
    return v.is_num() || v.is_locator();
}

void copy_constraints(ssa_value_t value, constraints_def_t& def)
{
    if(value.is_handle())
        def = ai_data(value.handle()).constraints();
    else if(value.is_num())
        def = { type_constraints_mask(value.num_type_name()), { constraints_t::const_(value.fixed().value, REAL_MASK) }};
    else if(value.is_locator())
    {
        locator_t const loc = value.locator();
        type_t const type = loc.type();

        assert(!is_banked_ptr(type.name()));

        if(is_scalar(type.name()))
            def = { type_constraints_mask(type.name()), { constraints_t::bottom(type_constraints_mask(type.name())) }};
        else
            def = {};
    }
    else
        def = {};
}

constraints_def_t get_constraints(ssa_value_t value)
{
    constraints_def_t ret;
    copy_constraints(value, ret);
    return ret;
}

// AI = abstract interpretation, NOT artificial intelligence.
struct ai_t
{
public:
    ai_t(log_t* log, ir_t& ir_, bool byteified);

private:
    // Threshold points where widening occurs.
    // Keep these in ascending order!!
    static constexpr unsigned WIDEN_OP_BOUNDS = 16;
    static constexpr unsigned WIDEN_OP        = 24;

    void mark_skippable();
    void remove_skippable();

    ssa_ht local_lookup(cfg_ht cfg_h, ssa_ht ssa_h);
    void insert_trace(cfg_ht cfg_trace_h, ssa_ht original_h, 
                      ssa_value_t parent_trace, unsigned arg_i);
    void insert_traces();

    void queue_edge(cfg_ht h, unsigned out_i);
    void queue_node(executable_index_t exec_i, ssa_ht h);

    bool simple_visit(ssa_ht ssa_h);

    void init_constraints();
    void compute_trace_constraints(executable_index_t exec_i, ssa_ht trace_h);
    void compute_constraints(executable_index_t exec_i, ssa_ht ssa_h);
    void visit(ssa_ht ssa_h);
    void range_propagate();
    void prune_unreachable_code();
    void fold_consts();

    // Loop rewrite:
    void rewrite_loops_visit(cfg_ht branch_cfg, ssa_ht ssa_node);
    cfg_ht try_rewrite_loop(cfg_ht branch_cfg, unsigned back_edge_input, bool exit_output);
    void rewrite_loops();

    // Jump threading
    void jump_thread_visit(ssa_ht ssa_h);
    void run_jump_thread(cfg_ht start_h, unsigned start_branch_i);
    void thread_jumps();

    ir_t& ir;

    bc::small_vector<ssa_ht, 4> needs_rebuild;
    bc::small_vector<cfg_ht, 4> threaded_jumps;

    log_t* log = nullptr;

public:
    int updated = false;
};

ai_t::ai_t(log_t* log, ir_t& ir_, bool byteified) 
: ir(ir_), log(log)
{
    static int count = 0;
    ++count;

#ifndef NDEBUG
    for(cfg_node_t& node : ir)
        assert(node.output_size() <= cfg_ai_d::max_output_size);
#endif

    ir.assert_valid();

    dprint(log, "\nBEGIN TRACE");
    insert_traces();
    ir.assert_valid();

    dprint(log, "\nBEGIN INIT CONSTRAINTS");
    init_constraints(); // Do this after inserting traces, before propagating.
    ir.assert_valid();

    dprint(log, "\nBEGIN PROPAGATE");
    range_propagate();
    ir.assert_valid();

    dprint(log, "\nPRUNE");
    prune_unreachable_code();
    ir.assert_valid();

    dprint(log, "\nMARK SKIP");
    mark_skippable();
    ir.assert_valid();

    dprint(log, "\nTHREAD");
    thread_jumps();
    ir.assert_valid();

    dprint(log, "\nFOLD");
    fold_consts();
    ir.assert_valid();

    //if(byteified)
        //return;

    dprint(log, "\nREMOVE SKIP");
    remove_skippable();
    ir.assert_valid();

    dprint(log, "\n REWRITE LOOPS");
    rewrite_loops();
    ir.assert_valid();
}

/* TODO: remove?
ai_t::debug_print()
{
#ifndef NDEBUG
    if(!log)
        return;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        *log << " CFG " << cfg_it.index << '\n';
        *log << "  executable: " << ai_data(cfg_it).executable[0] << ' ' << ai_data(cfg_it).executable[1] << '\n';
        *log << "  executable out: " << ai_data(cfg_it).output_executable[0] << ' ' << ai_data(cfg_it).output_executable[1] << '\n';
        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            *log << "   ssa " << ssa_it.index << '\n';
            if(ai_data(ssa_it).constraints().vec.size())
                *log << "    constraints:" << ai_data(ssa_it).constraints()[0] << '\n';
        }
    }
#endif
}
*/

////////////////////////////////////////
// HELPERS                            //
////////////////////////////////////////

void ai_t::queue_edge(cfg_ht h, unsigned out_i)
{
    auto& d = ai_data(h);

    if(d.output_executable[EXEC_PROPAGATE].test(out_i))
        return;

    d.output_executable[EXEC_PROPAGATE].set(out_i);
    cfg_worklist.push(h->output(out_i));
}

void ai_t::queue_node(executable_index_t exec_i, ssa_ht h)
{
    if(ai_data(h->cfg_node()).executable[exec_i])
        ssa_worklist.push(h);
}

////////////////////////////////////////
// INIT CONSTRAINTS                   //
////////////////////////////////////////

void ai_t::init_constraints()
{
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        type_t const type = ssa_it->type();
        unsigned const size = constraints_size(*ssa_it);

        constraints_mask_t cm = {};
        if(size > 0)
        {
            if(type.name() == TYPE_TEA)
                cm = type_constraints_mask(type.elem_type().name());
            else if(type.name() == TYPE_PAA)
                cm = type_constraints_mask(TYPE_U);
            else if(is_banked_ptr(type.name()))
                cm = type_constraints_mask(type.with_banked(false).name());
            else
                cm = type_constraints_mask(type.name());
        }

        for(auto& constraints : ai_data(ssa_it).constraints_array)
        {
            constraints.vec.clear();
            constraints.vec.resize(size, constraints_t::top());
            constraints.cm = cm;
        }
    }
}

////////////////////////////////////////
// SKIPPABLE                          //
////////////////////////////////////////

static bool _search_skippable(cfg_ht cfg_h, ssa_ht ssa_h)
{
    if(!pure(*ssa_h))
        return false;

    unsigned const output_size = ssa_h->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht output = ssa_h->output(i);

        if(output->op() == SSA_trace)
            if(!_search_skippable(cfg_h, output))
                return false;

        if(output->cfg_node() != cfg_h)
            return false;
    }

    return true;
}

void ai_t::mark_skippable()
{
#ifndef NDEBUG
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        assert(ai_data(cfg_it).skippable == false);
#endif

    // A skippable CFG is one where every SSA node is either:
    // - A node inserted during SSA reconstruction
    // - A node that is used in its own CFG node but not anywhere else
    // These CFG nodes are what can be skipped over by jump threading.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        assert(cfg_it->test_flags(FLAG_IN_WORKLIST) == false);

        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            auto& d = ai_data(ssa_it);

            if(d.rebuild_mapping)
                continue;

            assert(ssa_it->op() != SSA_trace);

            if(!_search_skippable(cfg_it, ssa_it))
                goto not_skippable;
        }

        // Mark it as skippable!
        ai_data(cfg_it).skippable = true;
    not_skippable:;
    }
}

// Prunes nodes found by 'mark_skippable' -- but only when they are
// nodes with 1 input and 1 output.
void ai_t::remove_skippable()
{
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it;)
    {
        if((ai_data(cfg_it).skippable)
           && cfg_it->input_size() == 1 && cfg_it->output_size() == 1)
        {
            // Before pruning the CFG node, all SSA nodes it contains must be
            // pruned too.
            while(ssa_ht ssa_it = cfg_it->ssa_begin())
            {
                assert(!(ssa_flags(ssa_it->op()) & SSAF_IO_IMPURE));

                // Nodes inserted during the SSA rebuild can simply be
                // replaced with their original value.
                if(ssa_ht mapping = ai_data(ssa_it).rebuild_mapping)
                {
                    dprint(log, "-PRUNE_SKIPPABLE_REPLACE_SSA", ssa_it, mapping, ai_data(mapping).rebuild_mapping);
                    ssa_it->replace_with(mapping);
                }

                dprint(log, "-PRUNE_SKIPPABLE_SSA", ssa_it, _search_skippable(cfg_it, ssa_it));
                ssa_it->prune();
            }

            // Prune the skippable node.
            dprint(log, "-PRUNE_SKIPPABLE", cfg_it);
            cfg_it = ir.merge_edge(cfg_it);
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
ssa_ht ai_t::local_lookup(cfg_ht cfg_node, ssa_ht ssa_node)
{
    assert(cfg_node);
    assert(ssa_node);

    if(ssa_node->cfg_node() == cfg_node)
        return ssa_node;

    auto& cd = ai_data(cfg_node);
    auto lookup = cd.rebuild_map.find(ssa_node);

    if(lookup != cd.rebuild_map.end())
    {
        assert(lookup->second);
        return lookup->second;
    }
    else
    {
        // If 'cfg_node' doesn't contain a definition for 'ssa_node',
        // recursively look up its definition in predecessor nodes.
        // If there are multiple predecessors, a phi node will be created.
        switch(cfg_node->input_size())
        {
        case 0:
            throw std::runtime_error("Local lookup failed.");
        case 1:
            return local_lookup(cfg_node->input(0), ssa_node);
        default:
            ssa_ht phi = cfg_node->emplace_ssa(SSA_phi, ssa_node->type());
            // TODO:
            ssa_data_pool::resize<ssa_ai_d>(ssa_pool::array_size());

            ai_data(phi).rebuild_mapping = ssa_node;
            cd.rebuild_map.emplace(ssa_node, phi);

            // Fill using local lookups:
            unsigned const input_size = cfg_node->input_size();
            phi->alloc_input(input_size);
            for(unsigned i = 0; i < input_size; ++i)
            {
                // Keep this as two lines. Reference invalidation lurks!
                ssa_ht input = local_lookup(cfg_node->input(i), ssa_node);
                phi->build_set_input(i, input);
            }

            assert(phi);
            return phi;
        }
    }
}

void ai_t::insert_trace(cfg_ht cfg_trace, ssa_ht original, 
                        ssa_value_t parent_trace, unsigned arg_i)
{
    assert(cfg_trace);
    assert(original);
    assert(parent_trace);

    auto& cfg_trace_d = ai_data(cfg_trace);
    auto& rebuild_map = cfg_trace_d.rebuild_map;

    // A single node can appear multiple times in the condition expression.
    // Check to see if that's the case by checking if a trace already exists
    // for this node.
    auto it = rebuild_map.find(original);
    if(it != rebuild_map.end())
    {
        // The trace already exists.
        ssa_ht h = it->second;
        assert(parent_trace.is_handle());
        if(h != parent_trace.handle())
        {
            h->link_append_input(parent_trace);
            h->link_append_input(ssa_value_t(arg_i, TYPE_VOID));
        }
        return;
    }

    ssa_ht trace = cfg_trace->emplace_ssa(SSA_trace, original->type());
    ssa_data_pool::resize<ssa_ai_d>(ssa_pool::array_size());
    // All references to SSA nodes have been invalidated by the new node!!

    rebuild_map.insert({ original, trace });

    auto& d = ai_data(trace);
    assert(original);
    d.rebuild_mapping = original;

    if(parent_trace.is_handle())
    {
        trace->alloc_input(3);

        // The first argument is the original expression it represents.
        trace->build_set_input(0, original);

        // The remaining arguments come in pairs.
        // - First comes the parent trace.
        // - Second comes the argument index into the parent trace's original node.
        trace->build_set_input(1, parent_trace);
        trace->build_set_input(2, ssa_value_t(arg_i, TYPE_VOID));
        assert(parent_trace->input(0)->op() != SSA_trace);
        assert(arg_i < parent_trace->input(0)->input_size());
    }
    else
    {
        // If 'parent_trace' is a constant, that means this is the first
        // trace in the trace-graph. We represent this node slightly
        // differently, using only the first two arguments.
        trace->alloc_input(2);
        trace->build_set_input(0, original);
        trace->build_set_input(1, parent_trace);
    }

    if(ssa_flags(original->op()) & SSAF_TRACE_INPUTS)
    {
        // Recursively trace 'original', turning its inputs into traces too.
        unsigned const input_size = original->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t input = original->input(i);
            if(input.holds_ref())
                insert_trace(cfg_trace, input.handle(), trace, i);
        }
    }

    // All outputs of the original node will need rebuilding.
    needs_rebuild.push_back(original);
}

void ai_t::insert_traces()
{
    for(cfg_ht cfg_branch = ir.cfg_begin(); cfg_branch; ++cfg_branch)
    {
        // A branch is any cfg node with more than 1 successor.
        unsigned const output_size = cfg_branch->output_size();
        if(output_size < 2)
            continue;

        assert(cfg_branch->last_daisy());
        ssa_node_t& ssa_branch = *cfg_branch->last_daisy();

        // If the condition is const, there's no point
        // in making a trace partition out of it.
        ssa_value_t const condition = get_condition(ssa_branch);
        if(!condition.is_handle())
            continue;

        if(ssa_branch.op() == SSA_if)
        {
            // Create new CFG nodes along each branch and insert SSA_traces into them.
            for(unsigned i = 0; i < output_size; ++i)
            {
                constexpr type_name_t type_name = TYPE_BOOL;

                cfg_ht cfg_trace = ir.split_edge(cfg_branch->output_edge(i));
                cfg_data_pool::resize<cfg_ai_d>(cfg_pool::array_size());
                insert_trace(cfg_trace, condition.handle(), ssa_value_t(i, type_name), 0);
            }
        }
        else if(is_switch(ssa_branch.op()))
        {
            // Create new CFG nodes along each non-default branch and insert SSA_traces into them.
            unsigned const cases = ssa_switch_cases(ssa_branch.op());
            for(unsigned i = cases, j = 1; i < output_size; ++i, ++j)
            {
                type_name_t const type_name = condition.type().name();
                passert(type_name == TYPE_U || type_name == TYPE_S, type_name);

                cfg_ht cfg_trace = ir.split_edge(cfg_branch->output_edge(i));
                cfg_data_pool::resize<cfg_ai_d>(cfg_pool::array_size());
                insert_trace(cfg_trace, condition.handle(), ssa_value_t(ssa_branch.input(j).fixed(), type_name), 0);
            }
        }
        else
            assert(false);
    }

    ir.assert_valid();

    // For all the nodes that spawned a trace,
    // modify the inputs of their outputs to use the trace.
    for(ssa_ht h : needs_rebuild)
    {
        auto& d = ai_data(h);
        ssa_ht const look_for = d.rebuild_mapping ? d.rebuild_mapping : h;

        for(unsigned i = 0; i < h->output_size();)
        {
            ssa_bck_edge_t edge = h->output_edge(i);
            assert(edge.handle->op() != SSA_trace || edge.index == 0);

            ssa_ht const lookup = local_lookup(edge.handle->input_cfg(edge.index), look_for);
            assert(lookup);

            if(!edge.handle->link_change_input(edge.index, lookup))
               ++i;
        }
    }

    needs_rebuild.clear();

    ir.assert_valid();
}

// Doesn't normalize (to handle widening more efficiently).
// You must normalize at call site.
void ai_t::compute_trace_constraints(executable_index_t exec_i, ssa_ht trace)
{
    assert(trace->op() == SSA_trace);

    auto& trace_d = ai_data(trace);

    // If there's only two arguments that means we have a 'root' trace.
    // i.e. the trace's original node is the input to the branch node.
    // The constraints of this is always constant.
    if(trace->input_size() == 2)
    {
        passert(trace->type().size_of() == 1, trace->type());
        assert(trace->input(1).is_num());
        assert(trace->input(1).num_type_name() == trace->type().name());
        assert(trace_d.constraints().vec.size() >= 1);
        assert(trace_d.constraints().vec.size() <= 2);

        constraints_mask_t const cm = type_constraints_mask(trace->input(1).num_type_name());

        copy_constraints(trace->input(0), trace_d.constraints());
        assert(trace_d.constraints().vec.size() > 0);
        trace_d.constraints().vec[0] = constraints_t::const_(trace->input(1).fixed().value, cm);

        assert(trace_d.constraints().cm == cm);
        assert(trace_d.constraints()[0].is_const());
        assert(trace_d.constraints().cm == get_constraints(trace->input(0)).cm);
        passert(is_subset(trace_d.constraints()[0], get_constraints(trace->input(0))[0], trace_d.constraints().cm),
                trace_d.constraints()[0], '\n', get_constraints(trace->input(0))[0], '\n', trace->input(0));
        return;
    }

    assert(trace->input_size() > 2);
    assert(trace->input_size() % 2 == 1);
    assert(trace->type() == trace->input(0)->type());
    assert(trace_d.constraints().cm == get_constraints(trace->input(0)).cm);

    // Do a quick check to make sure all our inputs are non-top.
    unsigned const input_size = trace->input_size();
    for(unsigned i = 1; i < input_size; i += 2)
    {
        ssa_ht parent_trace = trace->input(i).handle();
        assert(parent_trace->op() == SSA_trace);
        if(any_top(ai_data(parent_trace).constraints()))
            return;
    }

    bc::small_vector<constraints_def_t, 16> c;

    // For each parent, we'll perform a narrowing operation.

    // Our results will be stored here.
    constraints_vec_t narrowed = get_constraints(trace->input(0)).vec;

    for(unsigned i = 1; i < input_size; i += 2)
    {
        // We'll narrow using this trace as the result.
        ssa_ht const parent_trace = trace->input(i).handle();
        assert(parent_trace->op() == SSA_trace);

        // Find the original instruction to narrow with.
        // Due to how traces are inserted, this *could* be a trace,
        // so we'll have to iterate up until it's not.
        ssa_ht narowing_op = parent_trace->input(0).handle();
        while(narowing_op->op() == SSA_trace)
            narowing_op = narowing_op->input(0).handle();

        unsigned const arg_i = trace->input(i+1).whole();
        unsigned const num_args = narowing_op->input_size();
        passert(arg_i < num_args, arg_i, num_args);

        // The narrow function expects a mutable array of constraints
        // that it modifies. Create that array here.
        c.resize(num_args);
        for(unsigned j = 0; j < num_args; ++j)
            copy_constraints(narowing_op->input(j), c[j]);

        auto& parent_trace_d = ai_data(parent_trace);
        assert(parent_trace_d.rebuild_mapping);

        ssa_op_t const op = narowing_op->op();
        passert(narrow_fn(op), op);

        // Call the narrowing op:
        narrow_fn(op)(c.data(), num_args, parent_trace_d.constraints());
        assert(c[arg_i].vec.size() == narrowed.size());

        // Update narrowed:
        for(unsigned j = 0; j < narrowed.size(); ++j)
            narrowed[j] = intersect(narrowed[j], c[arg_i][j]);
    }

    passert((is_subset(narrowed[0], get_constraints(trace->input(0))[0], trace_d.constraints().cm)),
            narrowed[0], '\n', get_constraints(trace->input(0))[0]);

    trace_d.executable_index = exec_i;
    trace_d.constraints().vec = std::move(narrowed);
}

////////////////////////////////////////
// RANGE PROPAGATION                  //
////////////////////////////////////////

// Doesn't normalize (to handle widening more efficiently).
// You must normalize at call site.
void ai_t::compute_constraints(executable_index_t exec_i, ssa_ht ssa_node)
{
    auto& d = ai_data(ssa_node);

    if(ssa_node->op() == SSA_trace)
        compute_trace_constraints(exec_i, ssa_node);
    else
    {
        assert(!d.constraints().vec.empty());

        // Build an array holding all the argument's constraints.
        unsigned const input_size = ssa_node->input_size();
        bc::small_vector<constraints_def_t, 16> c;
        c.resize(input_size);
        if(ssa_node->op() == SSA_phi)
        {
            // For phi nodes, if an input CFG edge hasn't been marked
            // executable, treat the argument's constraint as TOP.
            cfg_ht cfg_node = ssa_node->cfg_node();

            assert(input_size == cfg_node->input_size());
            for(unsigned i = 0; i < input_size; ++i)
            {
                auto edge = cfg_node->input_edge(i);
                auto& edge_d = ai_data(edge.handle);

                if(edge_d.output_executable[exec_i].test(edge.index))
                    copy_constraints(ssa_node->input(i), c[i]);
                else
                    c[i].vec.assign(d.constraints().vec.size(), constraints_t::top());
            }
        }
        else for(unsigned i = 0; i < input_size; ++i)
            copy_constraints(ssa_node->input(i), c[i]);

        // Call the ai op:
        passert(abstract_fn(ssa_node->op()), ssa_node->op());
        assert(d.constraints().vec.size());

        d.executable_index = exec_i;
        dprint(log, "-COMPUTE_CONSTRAINTS", ssa_node, ssa_node->op());
        abstract_fn(ssa_node->op())(c.data(), input_size, d.constraints());
    }
}

// Performs range propagatation on a single SSA node.
void ai_t::visit(ssa_ht ssa_node)
{
    dprint(log, "-SSA_VISIT ", ssa_node->op(), ssa_node);

    if(ssa_node->op() == SSA_if)
    {
        ssa_value_t const condition = get_condition(*ssa_node);

        assert(has_constraints(condition));
        assert(ssa_node->cfg_node()->output_size() == 2);

        constraints_def_t def = get_constraints(condition);
        assert(def.vec.size() == 1);
        assert(def.cm == BOOL_MASK);

        constraints_t const& c = def[0];
        dprint(log, "--IF_CONDITION ", c);

        if(c.is_top(def.cm))
            return;

        if(!c.is_const())
        {
            queue_edge(ssa_node->cfg_node(), 0);
            queue_edge(ssa_node->cfg_node(), 1);
        }
        else if(c.get_const())
            queue_edge(ssa_node->cfg_node(), 1);
        else
            queue_edge(ssa_node->cfg_node(), 0);

        return;
    }
    else if(is_switch(ssa_node->op()))
    {
        ssa_value_t const condition = get_condition(*ssa_node);

        assert(has_constraints(condition));

        constraints_def_t def = get_constraints(condition);
        assert(def.vec.size() >= 1);

        constraints_t const& c = def[0];
        dprint(log, "--SWITCH_CONDITION ", c);

        if(c.is_top(def.cm))
            return;

        unsigned const output_size = ssa_node->cfg_node()->output_size();
        unsigned const cases = ssa_switch_cases(ssa_node->op());
        for(unsigned i = cases, j = 1; i < output_size; ++i, ++j)
            if(c(ssa_node->input(j).fixed().value, def.cm))
                queue_edge(ssa_node->cfg_node(), i);

        if(ssa_node->op() == SSA_switch_partial)
            queue_edge(ssa_node->cfg_node(), 0);

        return;
    }
    else if(!has_constraints(ssa_node))
        return;

    auto& d = ai_data(ssa_node);

    thread_local constraints_def_t old_constraints;
    old_constraints = d.constraints();
    assert(all_normalized(old_constraints));

    if(d.visited_count >= WIDEN_OP)
    {
        dprint(log, "--WIDEN", ssa_node);
        d.constraints().vec.assign(
            d.constraints().vec.size(), 
            constraints_t::bottom(d.constraints().cm));
    }
    else
    {
        compute_constraints(EXEC_PROPAGATE, ssa_node);

        passert(old_constraints.vec.size() == d.constraints().vec.size(), ssa_node->op());

        if(d.visited_count > WIDEN_OP_BOUNDS)
            for(constraints_t& c : d.constraints().vec)
                c.bounds = bounds_t::bottom(d.constraints().cm);
        for(constraints_t& c : d.constraints().vec)
            c.normalize(d.constraints().cm);
    }

    dprint(log, "--C =", d.constraints()[0]);
    dprint(log, "--O =", old_constraints[0]);

    assert(all_normalized(d.constraints()));
    if(!bit_eq(d.constraints().vec, old_constraints.vec))
    {
        assert(old_constraints.cm == d.constraints().cm);
        passert(all_subset(old_constraints.vec, d.constraints().vec, d.constraints().cm),
                old_constraints.vec.size(),
                d.constraints().vec.size());

        // Update the visited count. 
        // Traces increment twice as fast, which was chosen to improve widening behavior.
        d.visited_count += ssa_node->op() == SSA_trace ? 2 : 1;

        // Queue all outputs
        unsigned const output_size = ssa_node->output_size();
        for(unsigned i = 0; i < output_size; ++i)
            queue_node(EXEC_PROPAGATE, ssa_node->output(i));
    }
}

void ai_t::range_propagate()
{
    assert(ssa_worklist.empty());
    assert(cfg_worklist.empty());

    // Reset the flags.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& cd = ai_data(cfg_it);
        cd.skippable = false;
        assert(cfg_it->test_flags(FLAG_IN_WORKLIST) == false);
        assert(cd.output_executable[EXEC_PROPAGATE].all_clear());

        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            auto& sd = ai_data(ssa_it);
            sd.touched = false;

            assert(ssa_it->test_flags(FLAG_IN_WORKLIST) == false);
            assert(sd.visited_count == 0);
            assert(sd.executable_index == EXEC_PROPAGATE);
        }
    }

#ifndef NDEBUG
    // Check to make sure we've init'd our constraints:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(is_scalar(ssa_it->type().name()))
            assert(ai_data(ssa_it).constraints().cm == type_constraints_mask(ssa_it->type().name()));
        assert(ai_data(ssa_it).constraints().vec.size() == constraints_size(*ssa_it));
    }
#endif

    assert(ir.root);
    cfg_worklist.push(ir.root);

    ir.assert_valid();
    while(!ssa_worklist.empty() || !cfg_worklist.empty())
    {
        while(!ssa_worklist.empty())
            visit(ssa_worklist.pop());

        while(!cfg_worklist.empty())
        {
            cfg_ht const cfg_node = cfg_worklist.pop();
            dprint(log, "-CFG_VISIT", cfg_node);

            auto& d = ai_data(cfg_node);

            if(!d.executable[EXEC_PROPAGATE])
            {
                d.executable[EXEC_PROPAGATE] = true;

                // Visit all expressions in this node.
                for(ssa_ht ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
                    queue_node(EXEC_PROPAGATE, ssa_it);
            }
            else for(auto phi_it = cfg_node->phi_begin(); phi_it; ++phi_it)
            {
                // Visit all phis in this node
                assert(phi_it->op() == SSA_phi);
                queue_node(EXEC_PROPAGATE, phi_it);
            }

            // Queue successor cfg nodes.
            if(cfg_node->output_size() == 1)
                queue_edge(cfg_node, 0);
        }
    }
    ir.assert_valid();
}

////////////////////////////////////////
// PRUNING                            //
////////////////////////////////////////

void ai_t::prune_unreachable_code()
{
    // Replace branches with constant conditionals with non-branching jumps.
    for(cfg_node_t& cfg_node : ir)
    {
        if(cfg_node.output_size() < 2)
            continue;

        ssa_ht const branch = cfg_node.last_daisy();

        assert(branch);
        assert(ssa_flags(branch->op()) & SSAF_CONDITIONAL);
        assert(ai_data(branch).executable_index == EXEC_PROPAGATE);
        assert(branch->cfg_node() == cfg_node.handle());

        constraints_def_t def = get_constraints(get_condition(*branch));
        assert(def.vec.size() >= 1 && def.vec.size() <= 2);
        assert(branch->op() != SSA_if || def.cm == BOOL_MASK);
        constraints_t const& c = def[0];

        /* TODO
#ifndef NDEBUG
        // If 'c' isn't const, all our outputs should have been executed.
        if(!c.is_const() && !ai_data(cfg_node.handle()).output_executable[EXEC_PROPAGATE].all_clear())
            for(unsigned i = 0; i < cfg_node.output_size(); ++i)
                assert(ai_data(cfg_node.output(i)).executable[EXEC_PROPAGATE]);
#endif
*/

        if(branch->op() == SSA_if)
        {
            if(!c.is_const())
                continue;

            // First calculate the branch index to remove.
            bool const prune_i = !(c.get_const() >> fixed_t::shift);

            dprint(log, "-PRUNE_BRANCH", cfg_node.handle(), branch, branch->op());

            // Then remove the conditional.
            branch->prune();
            assert(cfg_node.last_daisy() != branch);

            // Finally remove the branch from the CFG node.
            cfg_node.link_remove_output(prune_i);
            assert(cfg_node.output_size() == 1);

            updated = __LINE__;
        }
        else if(!c.is_top(def.cm))
        {
            assert(is_switch(branch->op()));

            bool const partial = branch->op() == SSA_switch_partial;

            // We'll track which cases appear in the switch.
            // (This is used to test exhaustive-ness)
            static_bitset_t<256> case_set = {};

            unsigned const cases = ssa_switch_cases(branch->op());
            for(unsigned i = cases, j = 1; i < cfg_node.output_size();)
            {
                case_set.set(std::uint8_t(branch->input(j).whole()));

                // Cases to default can be combined into the single default case.
                if((partial && branch->input(0) == branch->input(i)))
                    goto prune_case;

                if(c(branch->input(j).fixed().value, def.cm))
                {
                    ++i;
                    ++j;
                }
                else
                {
                prune_case:
                    // Prune unreachable branch:
                    dprint(log, "-PRUNE_SWITCH_BRANCH", cfg_node.handle(), cfg_node.output(i), branch, c, branch->input(j).fixed().value, def.cm);
                    branch->link_remove_input(j);
                    cfg_node.link_remove_output(i);

                    updated = __LINE__;
                }
            }

            if(partial)
            {
                if(branch->input_size() == 1)
                {
                become_jump:
                    assert(cfg_node.output_size() == 1);
                    dprint(log, "-PRUNE_SWITCH_BECOME_JUMP", cfg_node.handle());

                    // Remove the switch.
                    branch->prune();
                    assert(cfg_node.last_daisy() != branch);

                    updated = __LINE__;
                    assert(cfg_node.output_size() == 1);
                }
                else
                {
                    dprint(log, "-PRUNE_SWITCH_EXHAUSTIVE_TEST", cfg_node.handle());

                    bool const exhaustive = c.for_each(def.cm, [&](fixed_t x)
                    {
                        dprint(log, "--EXHAUSTIVE_I", x.value >> fixed_t::shift);

                        unsigned const bit = std::uint8_t(x.value >> fixed_t::shift);

                        if(!case_set.test(bit))
                        {
                            dprint(log, "--EXHAUSTIVE_FAIL", x.value >> fixed_t::shift);
                            return false;
                        }

                        case_set.clear(bit);
                        return true;
                    });

                    if(exhaustive && case_set.all_clear())
                    {
                        switch_partial_to_full(*branch);
                        dprint(log, "-SWITCH_PARTIAL_TO_FULL", cfg_node.handle());
                        updated = __LINE__;

                        if(branch->input_size() == 2)
                            goto become_jump;
                    }
                }
            }
        }
    }

    ir.assert_valid();

    // Remove all CFG nodes that weren't executed by the abstract interpreter.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it;)
    {
        if(ai_data(cfg_it).executable[EXEC_PROPAGATE])
            ++cfg_it;
        else
        {
            cfg_it = ir.prune_cfg(cfg_it);
            updated = __LINE__;
        }
    }

    ir.assert_valid();
}

void ai_t::fold_consts()
{
#ifndef NDEBUG
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        assert(ai_data(ssa_it).executable_index == EXEC_PROPAGATE);
#endif

    // Replace nodes determined to be constant with a constant ssa_value_t.
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->output_size() == 0 || !has_constraints(ssa_it))
            continue;

        ssa_op_t const op = ssa_it->op();
        auto& d = ai_data(ssa_it);

        assert(d.executable_index == EXEC_PROPAGATE);

        if(is_scalar(ssa_it->type().name()) && d.constraints()[0].is_const())
        {
            fixed_t const constant = { d.constraints()[0].get_const() };

            dprint(log, "-FOLDING", ssa_it->op(), ssa_it, (constant.value >> fixed_t::shift));

            ssa_value_t const replace_with(constant, ssa_it->type().name());
            
            if(ssa_ht mapping = ai_data(ssa_it).rebuild_mapping)
            {
                // If this is a trace, restrict how we want to fold it.

                auto& rm_d = ai_data(mapping);
                ssa_value_t rm_replace_with = {};
                if(rm_d.constraints()[0].is_const())
                    rm_replace_with = ssa_value_t(fixed_t{ rm_d.constraints()[0].get_const() }, ssa_it->type().name());

                for(unsigned i = 0; i < ssa_it->output_size();)
                {
                    auto oe = ssa_it->output_edge(i);

                    if(oe.input_class() == INPUT_VALUE)
                    {
                        if(oe.handle->op() == SSA_phi || (ssa_flags(oe.handle->op()) & SSAF_WRITE_GLOBALS))
                        {
                            // If the trace is used in a phi or WRITE_GLOBALS node,
                            // fold using its rebuild_mapping instead.
                            // This is to prevent unnecessary copying,
                            // as folding removes information.
                            if(!rm_replace_with)
                            {
                                ++i;
                                continue;
                            }
                            oe.handle->link_change_input(oe.index, rm_replace_with);
                        }
                        else
                            oe.handle->link_change_input(oe.index, replace_with);
                        updated = __LINE__;
                    }
                    else
                        ++i;
                }
            }
            else if(ssa_it->replace_with(INPUT_VALUE, replace_with))
                updated = __LINE__;
        }
        else if(op == SSA_read_array16 || op == SSA_write_array16)
        {
            using namespace ssai::array;

            bool const read = op == SSA_read_array16;

            ssa_value_t const index = ssa_it->input(INDEX);

            auto const fits_in_byte = [](ssa_value_t v)
            {
                assert(same_scalar_layout(v.type().name(), TYPE_U20));
                auto const c = get_constraints(v);
                return c[0].bounds.min >= 0 && c[0].bounds.max < (256ll << fixed_t::shift);
            };

            // If the index fits in a byte, convert to byte-based indexing:
            if(fits_in_byte(index))
            {
                ssa_ht const cast = cfg_node.emplace_ssa(SSA_cast, TYPE_U, index);
                ssa_data_pool::resize<ssa_ai_d>(ssa_pool::array_size());
                ssa_it->link_change_input(INDEX, cast);
                ssa_it->unsafe_set_op(read ? SSA_read_array8 : SSA_write_array8);
                updated = __LINE__;
            }
        }
        else if(op == SSA_multi_eq || op == SSA_multi_not_eq)
        {
            assert(ssa_it->input_size() % 2 == 0);

            // Simplify comparisons, removing unnecessary operations:
            for(unsigned i = 0; i < ssa_it->input_size();)
            {
                ssa_value_t const lhs = ssa_it->input(i);
                ssa_value_t const rhs = ssa_it->input(i+1);

                auto const lhs_c = get_constraints(lhs);
                auto const rhs_c = get_constraints(rhs);

                if(abstract_eq(lhs_c[0], lhs_c.cm, rhs_c[0], rhs_c.cm, i + 2 == ssa_it->input_size())
                   .bit_eq(constraints_t::bool_(true)))
                {
                    if(i + 2 == ssa_it->input_size())
                    {
                        if(ssa_it->input(i+0) != ssa_value_t(0u, TYPE_U)
                           || ssa_it->input(i+1) != ssa_value_t(0u, TYPE_U))
                        {
                            updated = __LINE__;
                            ssa_it->link_change_input(i+0, ssa_value_t(0u, TYPE_U));
                            ssa_it->link_change_input(i+1, ssa_value_t(0u, TYPE_U));
                        }
                        break;
                    }
                    else
                    {
                        updated = __LINE__;
                        ssa_it->link_remove_input(i+1);
                        ssa_it->link_remove_input(i);
                        continue;
                    }
                }

                i+= 2;
            }
        }
        else if(op == SSA_multi_lt || op == SSA_multi_lte)
        {
            multi_lt_info_t const info(ssa_it);

            constraints_mask_t const cm = type_constraints_mask(TYPE_U);
            fixed_uint_t const sign_bit = high_bit_only(cm.mask);

            // We'll build equal-sized vectors holding the constraints of lhs and rhs.

            bc::small_vector<constraints_t, max_total_bytes> lhs_vec;
            bc::small_vector<constraints_t, max_total_bytes> rhs_vec;

            for(int i = info.lfrac; i < info.rfrac; ++i)
                lhs_vec.push_back(constraints_t::const_(0, cm));
            for(int i = info.rfrac; i < info.lfrac; ++i)
                rhs_vec.push_back(constraints_t::const_(0, cm));

            int const lvec_offset = lhs_vec.size();
            int const rvec_offset = rhs_vec.size();

            assert(int(lhs_vec.size()) == info.maxfrac);
            assert(int(rhs_vec.size()) == info.maxfrac);

            for(int i = 0; i < info.lsize; ++i)
            {
                int const li = info.lstart + i;
                assert(info.validl(li));
                lhs_vec.push_back(get_constraints(ssa_it->input(li))[0]);
            }
            for(int i = 0; i < info.rsize; ++i)
            {
                int const ri = info.rstart + i;
                assert(info.validr(ri));
                rhs_vec.push_back(get_constraints(ssa_it->input(ri))[0]);
            }

            constraints_t extend; // temporary holding sign extension

            if(info.lsigned)
                extend = abstract_sign_extend(lhs_vec.back(), cm);
            else
                extend = constraints_t::const_(0, cm);
            while(lhs_vec.size() < rhs_vec.size())
                lhs_vec.push_back(extend);

            if(info.rsigned)
                extend = abstract_sign_extend(rhs_vec.back(), cm);
            else
                extend = constraints_t::const_(0, cm);
            while(rhs_vec.size() < lhs_vec.size())
                rhs_vec.push_back(extend);

            // OK! The vectors are built.

            assert(lhs_vec.size() == rhs_vec.size());
            assert(int(lhs_vec.size()) == info.maxfrac + info.maxwhole);

            int begin = 0;
            int end = lhs_vec.size();
            bool flip = false;
            unsigned ignore = 0;

            for(int i = lhs_vec.size() - 1; i >= 0; --i)
            {
                constraints_mask_t lhs_cm = cm;
                constraints_mask_t rhs_cm = cm;

                assert(!lhs_cm.signed_);
                assert(!rhs_cm.signed_);

                if(i == int(lhs_vec.size()) - 1)
                {
                    lhs_cm.signed_ = info.lsigned;
                    rhs_cm.signed_ = info.rsigned;
                }

                constraints_t const eq = abstract_eq(lhs_vec[i], lhs_cm, rhs_vec[i], rhs_cm);

                if(eq.bit_eq(constraints_t::bool_(true)))
                {
                    if(i+1 == end)
                        --end;
                    ignore |= 1 << i;
                    dprint(log, "-MULTI_LT EQ");
                    continue;
                }
                else if(!eq.bit_eq(constraints_t::bool_(false)))
                    continue;

                constraints_t const lt = abstract_lt(lhs_vec[i], lhs_cm, rhs_vec[i], rhs_cm);

                if(lt.bit_eq(constraints_t::bool_(true)))
                {
                    flip = ssa_it->op() == SSA_multi_lt;
                    begin = i+1;
                    dprint(log, "-MULTI_LT LT");
                    break;
                }
                
                if(lt.bit_eq(constraints_t::bool_(false)))
                {
                    flip = ssa_it->op() == SSA_multi_lte;
                    begin = i+1;
                    dprint(log, "-MULTI_LT GT");
                    break;
                }
            }

            unsigned new_lfrac = 0;
            unsigned new_lwhole = 0;
            unsigned new_rfrac = 0;
            unsigned new_rwhole = 0;
            bool new_signedl = false;
            bool new_signedr = false;

            bc::small_vector<ssa_value_t, max_total_bytes> new_lhs;
            bc::small_vector<ssa_value_t, max_total_bytes> new_rhs;

            for(int i = begin; i < end; ++i)
            {
                if(ignore & (1 << i))
                    continue;

                int const li = i - lvec_offset + info.lstart;
                if(info.validl(li))
                {
                    new_lhs.push_back(ssa_it->input(li));

                    if(info.signedl(li) && !(lhs_vec[i].bits.known0 & sign_bit))
                        new_signedl |= true;

                    if(li < info.loffset())
                        ++new_lfrac;
                    else
                        ++new_lwhole;
                }

                int const ri = i - rvec_offset + info.rstart;
                if(info.validr(ri))
                {
                    new_rhs.push_back(ssa_it->input(ri));

                    if(info.signedr(ri) && !(rhs_vec[i].bits.known0 & sign_bit))
                        new_signedr = true;

                    if(ri < info.roffset())
                        ++new_rfrac;
                    else
                        ++new_rwhole;
                }
            }

            if(new_lhs.empty())
            {
                if(new_rhs.empty())
                {
                    updated = __LINE__;
                    ssa_it->replace_with(ssa_value_t((ssa_it->op() == SSA_multi_lte) != flip, TYPE_BOOL));
                    continue;
                }

                new_lhs.push_back(ssa_value_t(0, TYPE_U));
                new_lwhole += 1;
            }

            if(new_rhs.empty())
            {
                new_rhs.push_back(ssa_value_t(0, TYPE_U));
                new_rwhole += 1;
            }

            unsigned const new_input_size = new_lhs.size() + new_rhs.size() + 2;
            assert(new_input_size <= ssa_it->input_size());

            if(new_input_size != ssa_it->input_size())
            {
                updated = __LINE__;

                bc::small_vector<ssa_value_t, max_total_bytes> inputs(2);

                // Set the types
                type_name_t const lt = type_s_or_u(new_lwhole, new_lfrac, new_signedl);
                type_name_t const rt = type_s_or_u(new_rwhole, new_rfrac, new_signedr);
                inputs[flip]  = ssa_value_t(unsigned(lt), TYPE_INT);
                inputs[!flip] = ssa_value_t(unsigned(rt), TYPE_INT);

                if(flip)
                {
                    inputs.insert(inputs.end(), new_rhs.begin(), new_rhs.end());
                    inputs.insert(inputs.end(), new_lhs.begin(), new_lhs.end());

                    if(ssa_it->op() == SSA_multi_lt)
                        ssa_it->unsafe_set_op(SSA_multi_lte);
                    else
                        ssa_it->unsafe_set_op(SSA_multi_lt);
                }
                else
                {
                    inputs.insert(inputs.end(), new_lhs.begin(), new_lhs.end());
                    inputs.insert(inputs.end(), new_rhs.begin(), new_rhs.end());
                }

#ifndef NDEBUG
                for(auto const& input : inputs)
                    assert(input);
#endif

                ssa_it->link_clear_inputs();
                ssa_it->link_append_input(&*inputs.begin(), &*inputs.end());

                dprint(log, "-MULTI_LT_REWRITE", lt, rt);
            }
        }
    }
    ir.assert_valid();
}

////////////////////////////////////////
// JUMP THREADING                     //
////////////////////////////////////////

// Updates the constraints 'ssa_node', without widening.
bool ai_t::simple_visit(ssa_ht ssa_node)
{
    if(!has_constraints(ssa_node))
        return true;

    auto& d = ai_data(ssa_node);

    thread_local constraints_def_t old_constraints;
    old_constraints = d.constraints();
    assert(all_normalized(old_constraints));

    compute_constraints(EXEC_THREAD, ssa_node);
    for(constraints_t& c : d.constraints().vec)
        c.normalize(d.constraints().cm);

    return bit_eq(d.constraints().vec, old_constraints.vec);
}

void ai_t::jump_thread_visit(ssa_ht ssa_node)
{
    dprint(log, "-JUMP_THREAD_VISIT", ssa_node);

    if(!has_constraints(ssa_node))
        return;

    if(!simple_visit(ssa_node))
    {
        // Queue all outputs
        unsigned const output_size = ssa_node->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_ht output = ssa_node->output(i);
            ai_data(output).touched = true;
            queue_node(EXEC_THREAD, output);
        }
    }
}

void ai_t::run_jump_thread(cfg_ht const start, unsigned const start_branch_i)
{
    dprint(log, "-JUMP_THREAD_START", start, start_branch_i);

    // Reset the state.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& cd = ai_data(cfg_it);

        cd.executable[EXEC_THREAD] = false;
        cd.output_executable[EXEC_THREAD].clear_all();

        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            auto& sd = ai_data(ssa_it);
            sd.touched = false;
            sd.executable_index = EXEC_PROPAGATE; // Reset back to PROPAGATE, not JUMP
        }
    }

    ai_data(start).executable[EXEC_THREAD] = true;

    // Step through CFG nodes, taking forced paths until reaching a branch
    // that is not forced.

    cfg_ht h = start;
    unsigned branch_i = start_branch_i;
    unsigned branches_skipped = 0;
    while(true)
    {
        cfg_node_t& prior_node = *h;
        auto& prior_d = ai_data(h);

        // Take the branch.
        prior_d.output_executable[EXEC_THREAD].set(branch_i);
        assert(branch_i < prior_node.output_size());
        unsigned const input_i = prior_node.output_edge(branch_i).index;
        h = prior_node.output(branch_i);

        dprint(log, "--JUMP_THREAD_ITER", prior_node.handle(), "->", h);

        cfg_node_t& cfg_node = *h;
        auto& cfg_data = ai_data(h);

        cfg_data.input_taken = input_i;

        // If we've ended up in a loop, abort!
        if(cfg_data.executable[EXEC_THREAD])
        {
            dprint(log, "---JUMP_THREAD_REACHED_LOOP");
            break;
        }

        cfg_data.executable[EXEC_THREAD] = true;

        // If we've reached an unskippable, abort!
        if(!cfg_data.skippable)
        {
            dprint(log, "---JUMP_THREAD_REACHED_UNSKIPPABLE");
            break;
        }

        // If we've reached an endpoint node, abort!
        if(cfg_node.output_size() == 0)
        {
            dprint(log, "---JUMP_THREAD_REACHED_EXIT");
            break;
        }

        // Queue and then update all relevant SSA nodes in this CFG node.
        for(auto ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
            if(ssa_it->op() == SSA_phi || ai_data(ssa_it).touched)
                queue_node(EXEC_THREAD, ssa_it);

        // Here's where we update the SSA nodes:
        while(!ssa_worklist.empty())
            jump_thread_visit(ssa_worklist.pop());

        // Check to see if the next path is forced, taking it if it is.
        assert(cfg_node.output_size() != 0); // Handled earlier.
        if(cfg_node.output_size() > 1)
        {
            assert(&cfg_node == &*h);

            ssa_ht const branch = cfg_node.last_daisy();
            assert(branch);

            constraints_def_t const def = get_constraints(get_condition(*branch));
            assert(def.vec.size() >= 1 && def.vec.size() <= 2);

            constraints_t const& c = def[0];

            if(branch->op() == SSA_if)
            {
                if(!c.is_const())
                {
                    dprint(log, "---JUMP_THREAD_REACHED_BRANCH_NOT_CONST");
                    break;
                }

                branch_i = c.get_const() >> fixed_t::shift;
            }
            else
            {
                assert(is_switch(branch->op()));

                if(c.is_const())
                {
                    unsigned const cases = ssa_switch_cases(branch->op());
                    for(unsigned i = cases, j = 1; i < cfg_node.output_size(); ++i, ++j)
                    {
                        if(branch->input(j).fixed().value == c.get_const())
                        {
                            branch_i = i;
                            goto branch_skipped;
                        }
                    }

                    if(branch->op() == SSA_switch_partial)
                        goto default_case;
                    else
                        goto no_case;
                }
                else if(branch->op() == SSA_switch_partial)
                {
                    // As the condition isn't constant,
                    // we're only checking if the default case is forced.
                    // Any other case will fail.

                    for(unsigned j = 1; j < branch->output_size(); ++j)
                        if(c(branch->input(j).fixed().value, def.cm))
                            goto no_case;

                default_case:
                    branch_i = 0;
                }
                else
                {
                no_case:
                    dprint(log, "---JUMP_THREAD_REACHED_SWITCH_NO_CASE");
                    break;
                }
            }

        branch_skipped:
            ++branches_skipped;
        }
        else // Non-conditional nodes are always forced
            branch_i = 0;
    }

    dprint(log, "-JUMP_BRANCHES_SKIPPED", branches_skipped);

    if(branches_skipped == 0)
        return;

    dprint(log, "-JUMP_THREADING", start, "->", h);

    // We've found the path. Now modify the IR.

    cfg_ht const end = h;
    cfg_ht const trace = start->output(start_branch_i);

    assert(trace->output_size() == 1);
    trace->link_append_output(end, [&](ssa_ht phi) -> ssa_value_t
    {
        // Phi nodes in the target need a new input argument.
        // Find that here by walking the path backwards until
        // reaching a node outside of the path or in the first path node.
        ssa_ht v = phi;
        while(true)
        {
            if(v->op() != SSA_phi || v->cfg_node() == start)
                break;

            auto& d = ai_data(v->cfg_node());

            if(!d.executable[EXEC_THREAD])
                break;

            unsigned input_i = d.input_taken;
            ssa_value_t input = v->input(input_i);

            if(input.is_const())
                return input;

            v = input.handle();
            if(ssa_ht mapping = ai_data(v).rebuild_mapping)
                v = mapping;
        }
        assert(v->cfg_node() == start
               || !(ai_data(v->cfg_node()).executable[EXEC_THREAD]));
        return v;
    });
    assert(trace->output_size() == 2);
    assert(ai_data(trace->output(0)).skippable);

    // Remove the previous output
    trace->link_remove_output(0);

    // Track it:
    threaded_jumps.push_back(trace);
}

void ai_t::thread_jumps()
{
    // Find all jump threads, creating new edges and storing the endpoints in
    // 'threaded_jumps'.
    //threaded_jumps.clear();
    threaded_jumps.clear();
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        if(!ai_data(cfg_it).skippable)
            continue;

        if(cfg_it->output_size() < 2)
            continue;

        // Ok! 'cfg_node' is a jump thread target.

        unsigned const input_size = cfg_it->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            assert(cfg_it->input_size() == input_size);

            // Traverse until finding a non-forced node.
            cfg_fwd_edge_t input = cfg_it->input_edge(i);
            while(true)
            {
                cfg_node_t& node = *input.handle;

                if(!ai_data(input.handle).skippable)
                    break;

                if(node.input_size() != 1 || node.output_size() != 1)
                    break;

                input = node.input_edge(0);
            }

            if(input.handle->output_size() < 2)
                continue;

            run_jump_thread(input.handle, input.index);
        }
    }

    dprint(log, "-JUMP_THREAD_COUNT", threaded_jumps.size());

    if(threaded_jumps.size() == 0)
        return;
    updated = __LINE__;

    // Prune unreachable nodes with no inputs here.
    // (Jump threading can create such nodes)

    for(cfg_ht cfg_it : threaded_jumps)
        for(unsigned i = 0; i < cfg_it->output_size(); ++i)
            cfg_worklist.push(cfg_it->output(i));

    while(!cfg_worklist.empty())
    {
        cfg_ht const cfg_node = cfg_worklist.pop();

        if(cfg_node->input_size() == 0)
        {
            dprint(log, "-JUMP_THREAD_PRUNE", cfg_node);
            passert(ai_data(cfg_node).skippable, cfg_node);

            unsigned const output_size = cfg_node->output_size();
            for(unsigned i = 0; i < output_size; ++i)
            {
                assert(cfg_node->output(i) != cfg_node);
                cfg_worklist.push(cfg_node->output(i));
            }

            for(ssa_ht ssa_it = cfg_node->ssa_begin(); ssa_it; ++ssa_it)
            {
                assert(ssa_it->cfg_node() == cfg_node);

                if(ai_data(ssa_it).rebuild_mapping)
                {
                    dprint(log, "--JUMP_THREAD_PRUNE_SSA", ssa_it, "->", ai_data(ssa_it).rebuild_mapping);
                    ssa_it->replace_with(ai_data(ssa_it).rebuild_mapping);
                }
            }

            cfg_node->prune_ssa();
            cfg_node->link_clear_outputs();
            ir.prune_cfg(cfg_node);
        }
    }

    // Cleanup executable indexes, just to be safe
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        ai_data(ssa_it).executable_index = EXEC_PROPAGATE;
}

////////////////////
// LOOP REWRITING //
////////////////////

void ai_t::rewrite_loops_visit(cfg_ht branch_cfg, ssa_ht ssa_node)
{
    assert(ssa_node->cfg_node() == branch_cfg);
    dprint(log, "---REWRITE_LOOPS_VISIT", ssa_node);

    if(!simple_visit(ssa_node))
    {
        // Queue outputs, but only those in 'branch_cfg'.
        unsigned const output_size = ssa_node->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_ht const output = ssa_node->output(i);
            if(output->cfg_node() == branch_cfg && output->op() != SSA_phi)
                queue_node(EXEC_THREAD, output);
        }
    }
}

static bool _recursive_steal_test(cfg_ht cfg, ssa_ht h)
{
    if(h->cfg_node() != cfg || h->op() == SSA_phi || h->in_daisy() || !pure(*h))
        return false;

    passert(h->op() != SSA_trace, cfg, h);
    passert(!ai_data(h).rebuild_mapping, cfg, h);

    return true;
}

static void _recursive_steal(cfg_ht from_cfg, cfg_ht to_cfg, ssa_ht h)
{
    while(ssa_input0_class(h->op()) == INPUT_LINK)
    {
        if(h->input_size() != 1)
            return;
        h = h->input(0).handle();
    }

    assert(h->cfg_node() == to_cfg);

    unsigned const input_size = h->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t const input = h->input(i);

        if(!input.holds_ref())
            continue;

        ssa_ht const ih = input.handle();

        if(!_recursive_steal_test(from_cfg, ih))
            continue;

        bool const stealable =
        for_each_output_with_links(ih, [&](ssa_ht from, ssa_ht output)
        {
            return output->cfg_node() == to_cfg;
        });

        if(stealable)
        {
            to_cfg->steal_ssa(ih, true);
            _recursive_steal(from_cfg, to_cfg, ih);
        }
    }
}

cfg_ht ai_t::try_rewrite_loop(cfg_ht branch_cfg, unsigned back_edge_input, bool exit_output)
{
    /*
#ifndef NDEBUG
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& cd = ai_data(cfg_it);

        assert(cd.executable[EXEC_THREAD] == false);
        assert(cd.output_executable[EXEC_THREAD].all_clear());

        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            auto& sd = ai_data(ssa_it);
            assert(sd.executable_index == EXEC_PROPAGATE);
            assert(ssa_it->test_flags(FLAG_IN_WORKLIST) == false);
        }
    }
#endif
    */

    dprint(log, "--REWRITE_LOOPS_TRY", branch_cfg);

    assert(ssa_worklist.empty());

    ssa_ht const branch_ssa = branch_cfg->last_daisy();

    assert(branch_cfg->output_size() == 2);
    assert(branch_ssa);
    assert(branch_ssa->op() == SSA_if);

    unsigned const input_size = branch_cfg->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        if(i == back_edge_input)
            continue;

        auto& cd = ai_data(branch_cfg->input(i));
        cd.executable[EXEC_THREAD] = true;
        cd.output_executable[EXEC_THREAD].clear_all();
        cd.output_executable[EXEC_THREAD].set(branch_cfg->input_edge(i).index);
    }

    {
        auto& cd = ai_data(branch_cfg);
        cd.executable[EXEC_THREAD] = true;
    }

    // Queue and then update all relevant SSA nodes in this CFG node.
    for(auto phi_it = branch_cfg->phi_begin(); phi_it; ++phi_it)
        queue_node(EXEC_THREAD, phi_it);

    // Here's where we update the SSA nodes:
    while(!ssa_worklist.empty())
        rewrite_loops_visit(branch_cfg, ssa_worklist.pop());

    // Now check if the branch is forced.
    ssa_value_t const condition = branch_ssa->input(0);
    assert(has_constraints(condition));

    constraints_def_t def = get_constraints(condition);
    assert(def.vec.size() == 1);
    assert(def.cm == BOOL_MASK);

    constraints_t const& c = def[0];

    if(c.is_top(def.cm) || !c.is_const() || !c.get_const() != exit_output)
    {
        dprint(log, "---REWRITE_LOOPS_FAIL", branch_cfg, c);
        return {};
    }

    // OK! The branch is forced for the first iteration.
    dprint(log, "---REWRITE_LOOPS_REWRITE", branch_cfg);

    // Cleanup executable indexes, just to be safe
    for(ssa_ht ssa_it = branch_cfg->ssa_begin(); ssa_it; ++ssa_it)
        ai_data(ssa_it).executable_index = EXEC_PROPAGATE;

    // Split the back edge:
    cfg_ht const new_cfg = ir.split_edge(branch_cfg->input_edge(back_edge_input).output());
    cfg_data_pool::resize<cfg_ai_d>(cfg_pool::array_size());

    // Make 'new_cfg' a branch:
    new_cfg->steal_ssa(branch_ssa, true);
    new_cfg->link_append_output(branch_cfg->output(exit_output), [&](ssa_ht phi)
    {
        return phi->input(branch_cfg->output_edge(exit_output).index);
    });
    if(!exit_output)
        new_cfg->link_swap_outputs(0, 1);

    // Steal nodes
    _recursive_steal(branch_cfg, new_cfg, branch_ssa);

    // Rewrite inputs of stolen nodes:
    for(ssa_ht ssa_it = new_cfg->ssa_begin(); ssa_it; ++ssa_it)
    {
        unsigned const input_size = ssa_it->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t const phi = ssa_it->input(i);

            // We're looking for phi nodes from the original 'branch_cfg'.
            if(!phi.holds_ref() || phi->op() != SSA_phi || phi->cfg_node() != branch_cfg)
                continue;

            ssa_it->link_change_input(i, phi->input(back_edge_input));
        }
    }

    // Remove the old branch:
    branch_cfg->link_remove_output(exit_output);

    ::build_loops_and_order(ir);

    updated = __LINE__;
    return new_cfg;
}

void ai_t::rewrite_loops()
{
    ::build_loops_and_order(ir);

    for(cfg_ht cfg_it : loop_headers)
    {
        dprint(log, "-REWRITE_LOOPS_HEADER", cfg_it);

        passert(algo(cfg_it).is_loop_header, cfg_it);

        unsigned const output_size = cfg_it->output_size();
        if(output_size < 2)
        {
            dprint(log, "--REWRITE_LOOPS_HEADER_WRONG_SIZE", cfg_it);
            continue;
        }

        ssa_ht const branch = cfg_it->last_daisy();
        if(!branch || branch->op() != SSA_if)
        {
            dprint(log, "--REWRITE_LOOPS_HEADER_WRONG_BRANCH_TYPE", cfg_it);
            continue;
        }

        // Ensure that at least one SSA node (the condition)
        // will be moved if this loop is rewritten.
        // Otherwise, loop rewriting will result in strictly worse code.
        {
            ssa_value_t condition = branch->input(0);

            while(ssa_input0_class(condition->op()) == INPUT_LINK)
            {
                if(condition->input_size() != 1)
                    goto dont_rewrite;
                condition = condition->input(0);
            }

            if(!condition.holds_ref() || condition->output_size() != 1 || !_recursive_steal_test(cfg_it, condition.handle()))
            {
            dont_rewrite:
                dprint(log, "--REWRITE_LOOPS_NO_IMPROVEMENT", cfg_it, condition.holds_ref(), condition->output_size(), condition->op());
                continue;
            }
        }

        // Check if 'branch' is a loop condition;
        // i.e. it's the middle expression of a 'for' statement.
        unsigned exit_i = 0;
        for(; exit_i < output_size; ++exit_i)
            if(!loop_is_parent_of(cfg_it, cfg_it->output(exit_i)))
                goto found_loop_condition;
        continue;
    found_loop_condition:

        // Loops with a single back edge can be rewritten.
        // Find that back edge here:
        constexpr unsigned NO_BACK_EDGE = ~0u;
        unsigned back_edge = NO_BACK_EDGE;
        unsigned const input_size = cfg_it->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            if(cfg_it->input(i) == cfg_it)
                goto next_iter; // Don't optimize self-loops.
            if(loop_is_parent_of(cfg_it, cfg_it->input(i)))
            {
                if(back_edge != NO_BACK_EDGE)
                    goto next_iter; // Multiple back edges.
                back_edge = i;
            }
        }

        assert(!ai_data(cfg_it).skippable);

        // Try to rewrite:
        assert(exit_i <= 1);
        if(cfg_ht new_cfg = try_rewrite_loop(cfg_it, back_edge, exit_i))
        {
            // Did rewrite!

            assert(cfg_it->output_size() == 1);

            // Update 'algo', just to be safe:
            cfg_algo_pool.resize(cfg_pool::array_size());
            algo(new_cfg).iloop_header = cfg_it;
        }
    next_iter:;
    }
}

} // End anonymous namespace

bool o_abstract_interpret(log_t* log, ir_t& ir, bool byteified)
{
    cfg_data_pool::scope_guard_t<cfg_ai_d> cg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_ai_d> sg(ssa_pool::array_size());
    ai_t ai(log, ir, byteified);
    o_remove_trivial_phis(log, ir); // clean-up phis created by ai_t

#ifndef NDEBUG
    for(cfg_node_t const& cfg : ir)
    {
        assert(!cfg.test_flags(FLAG_IN_WORKLIST));
        for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it; ++ssa_it)
            assert(!ssa_it->test_flags(FLAG_IN_WORKLIST));
    }
#endif

    return ai.updated;
}
