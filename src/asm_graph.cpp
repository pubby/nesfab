#include "asm_graph.hpp"

#include <random>

#include <boost/intrusive/list.hpp>

#include "flat/small_set.hpp"
#include "robin/map.hpp"
#include "robin/set.hpp"

#include "locator.hpp"
#include "asm_proc.hpp"
#include "intrusive_pool.hpp"
#include "flags.hpp"
#include "lvar.hpp"
#include "worklist.hpp"
#include "globals.hpp"

namespace { // anonymouse

namespace bi = ::boost::intrusive;

struct asm_node_t;
struct asm_path_t;

class asm_node_t : public bi::list_base_hook<>, public flag_owner_t
{
friend class asm_graph_t;
public:
    explicit asm_node_t(locator_t new_label)
    : label(new_label)
    {}

    void push_output(asm_node_t* o);
    void remove_output(unsigned i);
    void replace_output(unsigned i, asm_node_t* with);

    unsigned find_input(asm_node_t* h) const { return std::find(m_inputs.begin(), m_inputs.end(), h) - m_inputs.begin(); }
    unsigned find_output(asm_node_t* h) const { return std::find(m_outputs.begin(), m_outputs.end(), h) - m_outputs.begin(); }

    auto const& inputs() const { return m_inputs; }
    auto const& outputs() const { return m_outputs; }

public:
    void remove_outputs_input(unsigned i);

    std::vector<asm_inst_t> code;
    asm_inst_t output_inst = {};
    locator_t label = {};

    union
    {
        unsigned vid;
        struct
        {
            int path_input;
            int path_output;
            asm_node_t* list_end;
        } vcover;
        struct
        {
            unsigned code_size;
            int offset;
            asm_path_t* path;
        } vorder;
        struct
        {
            regs_t in;
            regs_t out;
        } vregs;
        struct
        {
            bitset_uint_t* in;
            bitset_uint_t* out; // Also used to hold the 'KILL' set temporarily.
        } vlive;
    };

    unsigned depth = 0;
    unsigned freq() const { return 1 << std::min<unsigned>(16, 4 * depth); }
private:

    bc::small_vector<asm_node_t*, 2> m_inputs;
    bc::small_vector<asm_node_t*, 2> m_outputs;
};

class asm_graph_t
{
public:
    asm_graph_t(log_t* log, std::vector<asm_inst_t> const& code, locator_t entry_label);

    std::vector<asm_node_t*> order();
    std::vector<asm_inst_t> to_linear(std::vector<asm_node_t*> order);
    void liveness(fn_t const& fn, lvars_manager_t& lvars);
    void optimize();
private:
    using list_t = bi::list<asm_node_t>;

    asm_node_t& push_back(locator_t label = LOC_NONE, bool succeed = false);
    list_t::iterator prune(asm_node_t& node);

    bool o_remove_stubs();
    bool o_remove_branches();
    bool o_returns();

    array_pool_t<bitset_uint_t> bitset_pool;
    array_pool_t<asm_node_t> node_pool;
    list_t list;
    rh::batman_map<locator_t, asm_node_t*> label_map;
    locator_t entry_label = {};

    log_t* log;
};

////////////////
// asm_node_t //
////////////////

void asm_node_t::push_output(asm_node_t* o)
{
    m_outputs.push_back(o);
    if(o)
        o->m_inputs.push_back(this);
}

void asm_node_t::remove_outputs_input(unsigned i)
{
    assert(i < m_outputs.size());
    if(asm_node_t* output = m_outputs[i])
    {
        auto it = std::find(output->m_inputs.begin(), output->m_inputs.end(), this);
        assert(it != output->m_inputs.end());
        std::swap(*it, output->m_inputs.back());
        output->m_inputs.pop_back();
    }
}

void asm_node_t::remove_output(unsigned i)
{
    assert(i < m_outputs.size());
    remove_outputs_input(i);
    std::swap(m_outputs[i], m_outputs.back());
    m_outputs.pop_back();
}

void asm_node_t::replace_output(unsigned i, asm_node_t* with)
{
    assert(i < m_outputs.size());
    remove_outputs_input(i);
    if(with)
        with->m_inputs.push_back(this);
    m_outputs[i] = with;
}

/////////////////
// asm_graph_t //
/////////////////

asm_graph_t::asm_graph_t(log_t* log, std::vector<asm_inst_t> const& code, locator_t entry_label)
: entry_label(entry_label)
, log(log)
{
    assert(entry_label.lclass() != LOC_MINOR_LABEL); // minor labels will get rewritten.
    push_back();

    struct delayed_lookup_t
    {
        asm_node_t* node;
        unsigned output;
        locator_t label;
    };

    std::vector<delayed_lookup_t> to_lookup;

    auto const delay_lookup = [&](asm_node_t& node, locator_t label)
    {
        to_lookup.push_back({ &node, node.outputs().size(), label });
        node.push_output(nullptr);
    };

    for(unsigned i = 0; i < code.size(); ++i)
    {
        auto const& inst = code[i];
        asm_node_t& node = list.back();

        if(inst.op == ASM_LABEL)
        {
            node.output_inst = { .op = JMP_ABSOLUTE };
            asm_node_t& node = push_back(inst.arg, true);

            if(inst.alt)
            {
                // 'alt' holds loop depth.
                assert(inst.alt.lclass() == LOC_CONST_BYTE);
                node.depth = std::max<unsigned>(node.depth, inst.alt.data());
            }
        }
        else 
        {
            if(is_return(inst))
            {
                node.code.push_back(inst);
                push_back();
            }
            else if(op_flags(inst.op) & ASMF_JUMP)
            {
                node.output_inst = inst;
                delay_lookup(node, inst.arg);
                push_back();
            }
            else if(op_flags(inst.op) & ASMF_BRANCH)
            {
                node.output_inst = inst;
                delay_lookup(node, inst.arg);
                if(i+1 < code.size() && inst.op == invert_branch(code[i+1].op))
                {
                    delay_lookup(node, code[i+1].arg);
                    i += 1;
                    push_back();
                }
                else
                    push_back(LOC_NONE, true);
            }
            else if(inst.op != ASM_PRUNED)
                node.code.push_back(inst);
        }
    }

    for(auto const& lookup : to_lookup)
    {
        if(auto const* pair = label_map.lookup(lookup.label))
            lookup.node->replace_output(lookup.output, pair->second);
        else
            throw std::runtime_error("Missing label in assembly.");
    }
}

asm_node_t& asm_graph_t::push_back(locator_t label, bool succeed)
{
    asm_node_t& node = node_pool.emplace(label);

    if(succeed && list.size() > 0)
    {
        list.back().push_output(&node);
        node.depth = list.back().depth;
    }

    if(label)
    {
        auto result = label_map.insert({ label, &node });
        if(!result.second)
            throw std::runtime_error("Duplicate label.");
    }

    list.push_back(node);
    return node;
}

auto asm_graph_t::prune(asm_node_t& node) -> list_t::iterator 
{
    assert(node.label != entry_label);
    while(node.outputs().size())
        node.remove_output(0);
    assert(node.inputs().empty());
    return list.erase(list.s_iterator_to(node));
}

void asm_graph_t::optimize()
{
    bool changed;
    do
    {
        changed = false;
        changed |= o_remove_stubs();
        changed |= o_remove_branches();
        changed |= o_returns();
    }
    while(changed);
}

bool asm_graph_t::o_remove_stubs()
{
    bool changed = false;

    for(auto it = list.begin(); it != list.end();)
    {
        if(!it->code.empty() || it->label == entry_label)
            goto next_iter;

        if(it->inputs().size() == 0)
            goto prune;

        // Removes 1-output nodes with no code.
        if(it->outputs().size() == 1 && it->outputs()[0] != &*it)
        {
            asm_node_t* const output = it->outputs()[0];
            assert(output);
            while(it->inputs().size())
            {
                asm_node_t* input = it->inputs()[0];
                assert(input);
                input->replace_output(input->find_output(&*it), output);
            }
            goto prune;
        }

    next_iter:
        ++it;
        continue;
    prune:
        it = prune(*it);
        changed = true;
    }

    return changed;
}

bool asm_graph_t::o_remove_branches()
{
    bool changed = false;

    // Replaces branches with jumps, where applicable. 
    for(asm_node_t& node : list)
    {
        if(node.outputs().size() < 2)
            continue;

        for(unsigned i = 1; i < node.outputs().size(); ++i)
            if(node.outputs()[i] != node.outputs()[0])
                goto next_iter;

        while(node.outputs().size() > 1)
            node.remove_output( 0);

        node.output_inst = { .op = JMP_ABSOLUTE };
        changed = true;
    next_iter:;
    }

    return changed;
}

bool asm_graph_t::o_returns()
{
    bool changed = false;

    bc::small_vector<asm_node_t*, 8> returns;
    for(asm_node_t& node : list)
        if(node.outputs().empty())
            returns.push_back(&node);

    for(unsigned i = 0;   i < returns.size(); ++i)
    for(unsigned j = i+1; j < returns.size(); ++j)
    {
        asm_node_t& a = *returns[i];
        asm_node_t& b = *returns[j];
        assert(a.outputs().empty() && b.outputs().empty());

        if(a.output_inst != b.output_inst)
            continue;

        // Look for duplicated code and combine it.
        unsigned match_len = 0;
        unsigned const min_size = std::min(a.code.size(), b.code.size());
        for(; match_len < min_size; ++match_len)
            if(a.code.rbegin()[match_len] != b.code.rbegin()[match_len])
                break;

        if(match_len >= 2) // Arbitrary length
        {
            // Combine it!
            std::vector<asm_inst_t> new_code(a.code.end() - match_len, a.code.end());

            asm_node_t& new_node = push_back();
            new_node.depth = std::max(a.depth, b.depth);
            new_node.code = std::move(new_code);
            new_node.output_inst = a.output_inst;

            a.code.resize(a.code.size() - match_len);
            b.code.resize(b.code.size() - match_len);

            a.push_output(&new_node);
            b.push_output(&new_node);

            a.output_inst = b.output_inst = { .op = JMP_ABSOLUTE };

            changed = true;
        }
    }

    return changed;
}

/* TODO: implement
{
    // 1. look for LOAD STORE
    // 2. make sure the register value isn't used afterwards
    // 3. look for an identical load above it
    // 4. make sure no JSR occurs between
    // 5. make sure no fence occurs between
    // 6. make sure the memory value isn't used between


    // for each node, we need to 


    // Setup 'regs_in' with the GEN set,
    // and 'regs_out' with the KILL set, bitwise inverted.
    // This mimics 'cg_liveness.cpp"
    for(asm_node_t* h = first_h; h; h = h.next(pool))
    {
        asm_node_t& node = h.get(pool);

        regs_t gen = 0;
        regs_t kill = 0;
        for(asm_inst_t const& inst : node.code)
        {
            gen |= op_input_regs(inst.op) & ~written;
            if(!(op_flags(inst.op) & ASMF_MAYBE_STORE))
                kill |= op_output_regs(inst.op);
        }

        node.regs_in = gen;
        node.regs_out = ~kill;
    }

    std::vector<

    regs_t succ_union = 0;
    for(asm_node_t* output_h : node.outputs)
    {
        asm_node_t& output = output_h.get(pool);
        succ_union |= output.regs_in;
    }
    node.regs_in |= succ_union & node.regs_out;
}
*/

std::vector<asm_inst_t> asm_graph_t::to_linear(std::vector<asm_node_t*> order)
{
    std::vector<asm_inst_t> code;

    unsigned next_id = 0;
    unsigned estimated_size = 0;
    for(asm_node_t* node : order)
    {
        // Assign a unique id to every node:
        node->vid = next_id++;

        // Estimate final code size:
        estimated_size += node->code.size() + 2;
    }

    code.reserve(estimated_size);

    // Ids are used to generate labels:
    auto const get_label = [this](asm_node_t& node)
    {
        if(node.label && node.label.lclass() != LOC_MINOR_LABEL)
            return node.label;
        return locator_t::minor_label(node.vid);
    };

    for(unsigned i = 0; i < order.size(); ++i)
    {
        asm_node_t& node = *order[i];
        asm_node_t* prev = i ? order[i-1] : nullptr;
        asm_node_t* next = i+1 < order.size() ? order[i+1] : nullptr;

        if(node.inputs().size() > 1 
           || (node.inputs().size() == 1 && prev != node.inputs()[0])
           || node.label == entry_label)
        {
            code.push_back({ .op = ASM_LABEL, .arg = get_label(node), .alt = locator_t::const_byte(node.depth) });
        }
        code.insert(code.end(), node.code.begin(), node.code.end());

        if(node.output_inst.op)
        {
            if(node.outputs().empty())
                code.push_back(node.output_inst);
            else
            {
                assert(node.outputs().size() <= 2); // TODO: switch

                for(unsigned j = 0; j < node.outputs().size(); ++j)
                {
                    if(node.outputs()[j] == next)
                        continue;
                    op_t op = node.output_inst.op;
                    if(j > 0 && is_branch(op))
                        op = invert_branch(op);
                    code.push_back({ .op = op, .arg = get_label(*node.outputs()[j]) });
                }
            }
        }
        else
            assert(node.outputs().empty());
    }

    return code;
}

//////////////
// ORDERING //
//////////////

struct asm_path_branch_t
{
    int from_offset;
    int to_offset;
    asm_path_t* to_path;
};

// Implementation detail:
struct asm_path_t
{
    std::vector<asm_node_t*> nodes;
    std::vector<asm_path_branch_t> branches;
    unsigned code_size = 0; // in bytes
    int offset = 0;
};

std::vector<asm_node_t*> asm_graph_t::order()
{
    struct edge_t
    {
        asm_node_t* from;
        unsigned output;
        unsigned weight;
    };

    // First build an elimination order for graph edges.
    std::vector<edge_t> elim_order;
    elim_order.reserve(list.size() * 2);

    for(asm_node_t& node : list)
    {
        unsigned const scale = node.freq();
        assert(scale > 0);

        switch(node.outputs().size())
        {
        case 0:
            break;
        case 1:  
            // Weight 'jmp' the highest:
            elim_order.push_back({ &node, 0, 3 * scale });
            break;
        case 2:
            // It's dumb, but we'll slightly prioritize falling into the smallest branch nodes.
            {
                bool const fat_i = node.outputs()[0]->code.size() < node.outputs()[1]->code.size();
                elim_order.push_back({ &node, fat_i, 2 * scale });
                elim_order.push_back({ &node, !fat_i, 1 * scale });
            }
            break;
        default: 
            for(unsigned i = 0; i < node.outputs().size(); ++i)
                elim_order.push_back({ &node, i, 0 });
            break;
        }

        // Reset state:
        node.vcover.path_input = -1;
        node.vcover.path_output = -1;
        node.vcover.list_end = nullptr;
    }

    std::sort(elim_order.begin(), elim_order.end(), [](auto const& l, auto const& r)
        { return l.weight > r.weight; });

    // Build path cover greedily:
    for(edge_t const& edge : elim_order)
    {
        dprint(log, "PATH_COVER_EDGE", edge.weight);

        if(edge.from->vcover.path_output >= 0)
            continue; // Path already exists

        asm_node_t& to = *edge.from->outputs()[edge.output];
        if(to.vcover.path_input >= 0)
            continue; // Path already exists

        // Verify that no cycle is created:
        asm_node_t* end = &to;
        while(end->vcover.list_end)
            end = end->vcover.list_end;
        if(end == edge.from)
            continue; // Cycle was found

        // OK! Add it to the path:
        edge.from->vcover.list_end = end;
        edge.from->vcover.path_output = edge.output;
        to.vcover.path_input = to.find_input(edge.from);
    }

    bc::small_vector<asm_path_t, 8> paths;

    // Collect the paths:
    for(asm_node_t& node : list)
    {
        // The start of each path will have no inputs.
        if(node.vcover.path_input >= 0)
            continue; // Not the start.

        // Build a path:
        asm_path_t path;
        for(asm_node_t* it = &node;; it = it->outputs()[it->vcover.path_output])
        {
            path.nodes.push_back(it);
            if(it->vcover.path_output < 0)
                break;
        }
        paths.push_back(std::move(path));
    }

    dprint(log, "PATH_COVER_SIZE", paths.size());

    ////////////////////////////////////////////////
    // Stop using 'vcover', start using 'vorder'. //
    ////////////////////////////////////////////////

    // Gather code sizes and offsets.
    for(asm_path_t& path : paths)
    for(asm_node_t* node : path.nodes)
    {
        node->vorder.path = &path;
        node->vorder.offset = path.code_size;
        node->vorder.code_size = size_in_bytes(node->code.begin(), node->code.end());
        switch(node->outputs().size())
        {
        case 2:
            node->vorder.code_size += op_size(node->output_inst.op);
            //fall-through
        case 1:
            if(node == path.nodes.back())
        default:
                node->vorder.code_size += op_size(node->output_inst.op);
        }
        path.code_size += node->vorder.code_size;
    }

    // Gather branches
    for(asm_path_t& path : paths)
    for(asm_node_t* node : path.nodes)
    {
        if(!is_branch(node->output_inst.op))
            continue;
        for(asm_node_t* output : node->outputs())
            if(output->vorder.path != &path)
                path.branches.push_back({ node->vorder.offset, output->vorder.offset, output->vorder.path });
    }

    auto const cost_fn = [](std::vector<asm_path_t*> const& order) -> unsigned
    {
        // Build offset:
        unsigned code_size = 0;
        for(asm_path_t* path : order)
        {
            path->offset = code_size;
            code_size += path->code_size;
        }

        // Compare branches:
        unsigned cost = 0;
        for(asm_path_t* path : order)
        {
            for(asm_path_branch_t const& branch : path->branches)
            {
                int const from = branch.from_offset + path->offset;
                int const to =   branch.to_offset   + branch.to_path->offset;
                int const distance = std::abs(from - to);

                if((from & 0xFF) != (to & 0xFF))
                    cost += 1;
                if(distance > 127 - 4)
                    cost += 3;
            }
        }

        return cost;
    };

    // Order the paths:
    unsigned lowest_cost = ~0;
    std::vector<asm_path_t*> best_order;

    std::vector<asm_path_t*> order(paths.size());
    for(unsigned i = 0; i < paths.size(); ++i)
        order[i] = &paths[i];

    auto const check = [&](std::vector<asm_path_t*> const& order)
    {
        unsigned const cost = cost_fn(order);
        if(cost < lowest_cost)
        {
            lowest_cost = cost;
            best_order = order;
        }
    };

    constexpr unsigned SOLVE_OPTIMALLY_LIMIT = 4;
    if(paths.size() <= SOLVE_OPTIMALLY_LIMIT)
    {
        // For small sizes, we can solve the path order optimally:
        std::sort(order.begin(), order.end());
        do check(order);
        while(std::next_permutation(order.begin(), order.end()));
    }
    else
    {
        std::minstd_rand rng(0xDEADBEEF);
        std::uniform_int_distribution<> dist(0, paths.size() - 1);

        check(order);

        // Check a few initial, random states first:
        constexpr unsigned INITIAL_SHUFFLES = 4;
        for(unsigned i = 0; i < INITIAL_SHUFFLES; ++i)
        {
            std::shuffle(order.begin(), order.end(), rng);
            check(order);
        }

        // Now do simulated annealing:
        constexpr unsigned ATTEMPTS_PER_ITER = 4;
        for(unsigned swaps = order.size(); swaps != 0; --swaps)
        for(unsigned attempt = 0; attempt < ATTEMPTS_PER_ITER; ++attempt)
        {
            order = best_order;
            for(unsigned i = 0; i < swaps; ++i)
            {
                unsigned const a = dist(rng);
                unsigned const b = dist(rng);
                std::swap(order[a], order[b]);
            }
            check(order);
        }
    }

    // Now gather the final result:
    std::vector<asm_node_t*> result;
    result.reserve(list.size());

    for(asm_path_t* path : best_order)
        for(asm_node_t* node : path->nodes)
            result.push_back(node);

    return result;
}

//////////////
// LIVENESS //
//////////////

template<typename ReadWrite>
void do_inst_rw(fn_t const& fn, lvars_manager_t const& lvars, asm_inst_t const& inst, ReadWrite rw)
{
    if(inst.arg.lclass() == LOC_FN)
    {
        fn_ht const call_h = inst.arg.fn();
        fn_t const& call = *call_h;

        // Handle args and returns:
        lvars.for_each_lvar(false, [&](locator_t loc, unsigned i)
        {
            if(!has_fn(loc.lclass()) || loc.fn() != call_h)
                return;
            rw(i, loc.lclass() == LOC_ARG, 
                  loc.lclass() == LOC_RETURN);
        });

        if(call.fclass == FN_MODE)
        {
            // Handle gmembers:
            lvars.for_each_non_lvar([&](locator_t loc, unsigned i)
            {
                if(loc.lclass() == LOC_GMEMBER)
                    rw(i, call.precheck_group_vars().test(loc.gmember()->gvar.group_vars.id), false);
            });
        }
        else
        {
            assert(call.global.compiled());

            // Handle gmembers:
            lvars.for_each_non_lvar([&](locator_t loc, unsigned i)
            {
                if(loc.lclass() == LOC_GMEMBER)
                    rw(i, call.ir_reads().test(loc.gmember().id),
                          call.ir_writes().test(loc.gmember().id));
            });
        }
    }

    if(is_return(inst))
    {
        // Every return will be "read" by the rts:
        lvars.for_each_lvar(true, [&](locator_t loc, unsigned i)
        {
            rw(i, loc.lclass() == LOC_RETURN, false);
        });

        // Handle gmembers:
        lvars.for_each_non_lvar([&](locator_t loc, unsigned i)
        {
            if(loc.lclass() == LOC_GMEMBER)
                rw(i, fn.ir_writes().test(loc.gmember().id), false);
        });
    }
    else if(inst.arg.lclass() != LOC_FN) // fns handled earlier.
    {
        auto test_loc = [&](locator_t loc)
        {
            int const i = lvars.index(loc);
            if(i >= 0)
                rw(i, op_input_regs(inst.op) & REGF_M,
                      op_output_regs(inst.op) & REGF_M);
        };

        test_loc(inst.arg);

        // For indirect modes, also test the hi byte.
        if(indirect_addr_mode(op_addr_mode(inst.op)))
        {
            assert(inst.arg);
            assert(inst.alt && inst.alt != inst.arg);
            test_loc(inst.alt);
        }
    }
}

void asm_graph_t::liveness(fn_t const& fn, lvars_manager_t& lvars)
{
    //////////////////////////
    // Calculating liveness //
    //////////////////////////

    bitset_pool.clear();
    auto bs_size = lvars.bitset_size();

    // Allocate bitsets:
    for(asm_node_t& node : list)
    {
        node.vlive.in  = bitset_pool.alloc(bs_size);
        node.vlive.out = bitset_pool.alloc(bs_size);
        node.clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);
    }

    // Call this before 'do_write'.
    auto const do_read = [](asm_node_t& node, unsigned i)
    {
        if(bitset_test(node.vlive.out, i))
            bitset_set(node.vlive.in, i);
    };

    auto const do_write = [](asm_node_t& node, unsigned i)
    { 
        bitset_clear(node.vlive.out, i); 
    };

    // Every arg will be "written" at root:
    asm_node_t& root = *label_map[entry_label];
    lvars.for_each_lvar(true, [&](locator_t loc, unsigned i)
    {
        if(loc.lclass() == LOC_ARG)
            do_read(root, i);
    });

    for(asm_node_t& node : list)
    {
        // Set 'd.in's initial value to be the set of variables used in
        // this cfg node before an assignment.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'd.out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set is sometimes called 'KILL')

        bitset_set_all(bs_size, node.vlive.out);

        for(asm_inst_t const& inst : node.code)
        {
            do_inst_rw(fn, lvars, inst, [&](unsigned i, bool read, bool write)
            {
                // Order matters here. 'do_write' comes after 'do_read'.
                if(read)
                    do_read(node, i); 
                if(write)
                    do_write(node, i); 
            });
        }
    }

    // temp_set will hold a node's actual out-set while the algorithm is running.
    auto* temp_set = ALLOCA_T(bitset_uint_t, bs_size);

    thread_local worklist_t<asm_node_t*> worklist;
    worklist.clear();

    for(asm_node_t& node : list)
        if(node.outputs().empty())
            worklist.push(&node);

    while(!worklist.empty())
    {
    reenter:
        asm_node_t& node = *worklist.pop();

        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        bitset_clear_all(bs_size, temp_set);
        for(asm_node_t* output : node.outputs())
            bitset_or(bs_size, temp_set, output->vlive.in);

        // Now use that to calculate a new live-in set:
        bitset_and(bs_size, temp_set, node.vlive.out); // (vlive.out holds KILL)
        bitset_or(bs_size, temp_set, node.vlive.in);

        // If 'vlive.in' is changing, add all predecessors to the worklist.
        if(!node.test_flags(FLAG_PROCESSED) || !bitset_eq(bs_size, temp_set, node.vlive.in))
        {
            node.set_flags(FLAG_PROCESSED);
            for(asm_node_t* input : node.inputs())
                worklist.push(input);
        }

        // Assign 'vlive.in':
        bitset_copy(bs_size, node.vlive.in, temp_set);
    }

    // Some nodes (infinite loops, etc) might not be reachable travelling
    // backwards from the exit. Handle those now:
    for(asm_node_t& node : list)
        if(!node.test_flags(FLAG_PROCESSED))
           worklist.push(&node);
           
    if(!worklist.empty())
        goto reenter;

    // Now properly set 'out' to be the union of all successor inputs:
    for(asm_node_t& node : list)
    {
        // Might as well clear flags.
        node.clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        bitset_clear_all(bs_size, node.vlive.out);
        for(asm_node_t* output : node.outputs())
            bitset_or(bs_size, node.vlive.out, output->vlive.in);
    }

    ///////////////////////////////////////
    // Build the lvar interference graph //
    ///////////////////////////////////////

    bitset_uint_t* const live = temp_set; // re-use

    // Step-through the code backwards, maintaining a current liveness set
    // and using that to build an interference graph.
    for(asm_node_t& node : list)
    {
        // Since we're going backwards, reset 'live' to the node's output state.
        bitset_copy(bs_size, live, node.vlive.out);

        for(asm_inst_t& inst : node.code | std::views::reverse)
        {
            if(op_flags(inst.op) & ASMF_CALL)
            {
                if(inst.arg.lclass() == LOC_FN)
                {
                    // Every live lvar will interfere with this fn:
                    bitset_for_each(bs_size, live, [&](unsigned i)
                    {
                        if(lvars.is_lvar(i))
                            lvars.add_fn_interference(i, inst.arg.fn());
                    });
                }
            }
            else if(inst.arg)
            {
                int const lvar_i = lvars.index(inst.arg);
                if(lvar_i >= 0)
                {
                    if(op_flags(inst.op) & ASMF_MAYBE_STORE)
                    {
                        assert(op_output_regs(inst.op) & REGF_M);
                        if(lvar_i < 0 || bitset_test(live, lvar_i))
                        {
                            if(op_t op = change_addr_mode(inst.op, MODE_ABSOLUTE))
                                inst.op = op;
                            else 
                            {
                                switch(inst.op)
                                {
                                case MAYBE_STORE_C: 
                                                assert(false); // TODO
                                                /*
                                    temp_code.push_back({ PHP_IMPLIED, inst.ssa_op });
                                    temp_code.push_back({ PHA_IMPLIED, inst.ssa_op });
                                    temp_code.push_back({ LDA_IMMEDIATE, inst.ssa_op, locator_t::const_byte(0) });
                                    temp_code.push_back({ ROL_IMPLIED, inst.ssa_op });
                                    inst.op = STA_ABSOLUTE;
                                    temp_code.push_back(std::move(inst));
                                    temp_code.push_back({ PLA_IMPLIED, inst.ssa_op });
                                    temp_code.push_back({ PLP_IMPLIED, inst.ssa_op });
                                    continue;
                                    */
                                default: assert(false);
                                }
                            }
                        }
                        else
                        {
                            dprint(log, "ASM_GRAPH_PRUNE", inst.arg);
                            inst.op = ASM_PRUNED;
                        }
                    }
                }
                else
                    passert(!(op_flags(inst.op) & ASMF_MAYBE_STORE), to_string(inst.op), inst.arg);
            }

            do_inst_rw(fn, lvars, inst, [&](unsigned i, bool read, bool write)
            {
                if(read)
                    bitset_set(live, i);
                else if(write) // Only occurs if 'read' is false.
                    bitset_clear(live, i);
            });

            // Variables that are live together interfere with each other.
            // Update the interference graph here:
            lvars.add_lvar_interferences(live);
        }
    }
}

} // end anonymous namespace

std::vector<asm_inst_t> run_asm_graph(
    log_t* log, fn_t const& fn, lvars_manager_t& lvars,
    std::vector<asm_inst_t> const& code, locator_t entry_label)
{
    asm_graph_t graph(log, code, entry_label);
    graph.optimize();
    graph.liveness(fn, lvars);
    return graph.to_linear(graph.order());
}
