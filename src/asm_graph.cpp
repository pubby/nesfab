#include "asm_graph.hpp"

#include <random>

#include <boost/container/static_vector.hpp>

#include "intrusive_pool.hpp"
#include "lvar.hpp"
#include "globals.hpp"
#include "ir.hpp"
#include "ir_algo.hpp"
#include "lvar.hpp"
#include "asm_proc.hpp"
#include "thread.hpp"

TLS worklist_t<asm_node_t*> asm_graph_t::worklist;

////////////////
// asm_node_t //
////////////////

void asm_node_t::push_output(asm_edge_t o)
{
    m_outputs.push_back(o);
    if(o)
        o.node->m_inputs.push_back(this);
}

void asm_node_t::remove_outputs_input(unsigned i)
{
    assert(i < m_outputs.size());
    if(asm_node_t* output = m_outputs[i].node)
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
    m_outputs[i].node = with;
}

void asm_node_t::steal_outputs(asm_node_t& from)
{
    assert(this != &from);

    while(!m_outputs.empty())
        remove_output(0);

    m_outputs = std::move(from.m_outputs);
    from.m_outputs.clear();

    for(asm_edge_t const& edge : m_outputs)
        edge.node->m_inputs[edge.node->find_input(&from)] = this;
}

/////////////////
// asm_graph_t //
/////////////////

asm_graph_t::asm_graph_t(log_t* log, locator_t entry_label)
: m_entry_label(entry_label)
, log(log)
{
    assert(entry_label.lclass() != LOC_MINOR_LABEL); // minor labels will get rewritten.
    push_back();
}

void asm_graph_t::append_code(asm_inst_t const* begin, asm_inst_t const* end, 
                              rh::batman_map<cfg_ht, switch_table_t> const& switch_tables)
{
    auto const delay_lookup = [&](asm_node_t& node, locator_t label, int case_value = -1)
    {
        to_lookup.push_back({ &node, node.outputs().size(), label });
        node.push_output({ nullptr, case_value });
    };

    for(asm_inst_t const* it = begin; it != end; ++it)
    {
        auto const& inst = *it;
        asm_node_t& node = list.back();

        if(inst.op == ASM_LABEL)
        {
            assert(!node.output_inst.op);
            node.output_inst = { .op = JMP_ABSOLUTE };
            asm_node_t& node = push_back(inst.arg, true);

            if(inst.arg.lclass() == LOC_CFG_LABEL)
                node.cfg = inst.arg.cfg_node();
        }
        else 
        {
            if(is_return(inst))
            {
                node.code.push_back(inst);
                push_back();
            }
            else if(op_flags(inst.op) & ASMF_SWITCH)
            {
                passert(node.cfg, to_string(inst.op));

                cfg_ht const cfg = inst.arg.cfg_node();

                switch_table_t const* switch_table = switch_tables.mapped(cfg);
                passert(switch_table, cfg, switch_tables.size());

                ssa_ht const branch = cfg->last_daisy();
                assert(branch && branch->op() == SSA_switch_full);

                for(unsigned i = 0; i < switch_table->size(); ++i)
                    delay_lookup(node, (*switch_table)[i], branch->input(i + 1).whole());

                assert(!node.output_inst.op);
                node.output_inst = inst;
                push_back();
            }
            else if(op_flags(inst.op) & ASMF_JUMP)
            {
                passert(inst.arg, to_string(inst.op));

                assert(!node.output_inst.op);
                node.output_inst = inst;
                delay_lookup(node, inst.arg);
                push_back();
            }
            else if(op_flags(inst.op) & ASMF_BRANCH)
            {
                passert(inst.arg, to_string(inst.op));

                assert(!node.output_inst.op);
                node.output_inst = inst;
                delay_lookup(node, inst.arg);
                if(it+1 < end && inst.op == invert_branch((it+1)->op))
                {
                    delay_lookup(node, (it+1)->arg);
                    it += 1;
                    push_back();
                }
                else
                    push_back(LOC_NONE, true);
            }
            else if(inst.op != ASM_PRUNED)
                node.code.push_back(inst);
        }
    }
}

void asm_graph_t::finish_appending()
{
    for(auto const& lookup : to_lookup)
    {
        if(auto const* pair = label_map.lookup(lookup.label))
            lookup.node->replace_output(lookup.output, pair->second);
        else
            throw std::runtime_error(fmt("Missing label % in assembly.", lookup.label));
    }
    to_lookup.clear();
}

asm_node_t& asm_graph_t::push_back(locator_t label, bool succeed)
{
    asm_node_t& node = node_pool.emplace(label, original_order++);

    if(succeed && list.size() > 0)
    {
        list.back().push_output({ &node });
        node.cfg = list.back().cfg;
    }

    if(label)
    {
        auto result = label_map.insert({ label, &node });
        if(!result.second)
            throw std::runtime_error(fmt("Duplicate label. %", label));
    }

    list.push_back(node);
    return node;
}

auto asm_graph_t::prune(asm_node_t& node) -> list_t::iterator 
{
    assert(node.label != m_entry_label);
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
        changed |= o_merge();
        changed |= o_returns();
        changed |= o_peephole();
    }
    while(changed);
}

bool asm_graph_t::o_remove_stubs()
{
    bool changed = false;

    for(auto it = list.begin(); it != list.end();)
    {
        if(!it->code.empty() || it->label == m_entry_label)
            goto next_iter;

        if(it->inputs().size() == 0)
            goto prune;

        // Removes 1-output nodes with no code.
        if(it->outputs().size() == 1 && it->outputs()[0].node != &*it)
        {
            asm_node_t* const output = it->outputs()[0].node;
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

bool asm_graph_t::o_merge()
{
    bool changed = false;

    // Merges 1-output nodes with 1-input nodes.
    for(auto it = list.begin(); it != list.end(); ++it)
    {
        if(it->output_inst.op != JMP_ABSOLUTE)
            continue;

        if(it->outputs().size() != 1)
            continue;

        asm_node_t* const output = it->outputs()[0].node;

        if(output == &*it || output->inputs().size() != 1)
            continue;

        asm_node_t* const input = output->inputs()[0];
        assert(input == &*it);

        // Merge:

        it->code.insert(it->code.end(), output->code.begin(), output->code.end());
        it->output_inst = output->output_inst;
        it->steal_outputs(*output);

        prune(*output);
        changed = true;
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

    // Tail-call optimize
    for(asm_node_t* node : returns)
    {
        if(node->output_inst.op != RTS_IMPLIED || node->code.empty())
            continue;

        if(op_t new_op = tail_call_op(node->code.back().op))
        {
            node->output_inst = node->code.back();
            node->output_inst.op = new_op;
        }
    }

    // Combine duplicated code
    for(unsigned i = 0;   i < returns.size(); ++i)
    for(unsigned j = i+1; j < returns.size(); ++j)
    {
        asm_node_t& a = *returns[i];
        asm_node_t& b = *returns[j];
        if(!a.outputs().empty() || !b.outputs().empty())
            continue;

        if(a.output_inst != b.output_inst)
            continue;

        if(a.is_switch() || b.is_switch())
            continue;

        // Search for duplicated code:
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
            new_node.cfg = a.cfg ? a.cfg : b.cfg;
            new_node.code = std::move(new_code);
            new_node.output_inst = a.output_inst;

            a.code.resize(a.code.size() - match_len);
            b.code.resize(b.code.size() - match_len);

            a.push_output({ &new_node });
            b.push_output({ &new_node });

            a.output_inst = b.output_inst = { .op = JMP_ABSOLUTE };

            changed = true;
        }
    }

    return changed;
}

bool asm_graph_t::o_peephole()
{
    bool changed = false;

    for(asm_node_t& node : list)
        changed |= ::o_peephole(&*node.code.begin(), &*node.code.end());

    return changed;
}

std::vector<asm_inst_t> asm_graph_t::to_linear(std::vector<asm_node_t*> order)
{
    std::vector<asm_inst_t> code;
    std::vector<asm_inst_t> table_code;

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

    // Prepare switch tables first:

    bc::static_vector<locator_t, 256> table;
    for(asm_node_t* node : order)
    {
        if(!node->is_switch())
            continue;

        int min = 0xFF;
        int max = 0;

        for(auto const& edge : node->outputs())
        {
            passert(edge.case_value >= 0, edge.case_value);
            passert(edge.case_value <= 0xFF, edge.case_value);
            min = std::min(min, edge.case_value);
            max = std::max(max, edge.case_value);
        }

        int const size = max - min + 1;
        assert(size <= 256);

        // Shift the offset so that out table doesn't have to start with [0]:
        node->output_inst.arg.advance_offset(-min);
        node->output_inst.alt.advance_offset(-min);

        table.resize(size, locator_t::const_byte(0));

        for(auto const& edge : node->outputs())
            table[edge.case_value - min] = get_label(*edge.node).with_advance_offset(-1);

        table_code.reserve(table_code.size() + table.size() * 2 + 2);

        cfg_ht const cfg = node->output_inst.arg.cfg_node();

        table_code.push_back({ .op = ASM_LABEL, .arg = locator_t::switch_lo_table(cfg) });
        for(locator_t loc : table)
            table_code.push_back({ .op = ASM_DATA, .arg = loc.with_is(IS_PTR) });

        table_code.push_back({ .op = ASM_LABEL, .arg = locator_t::switch_hi_table(cfg) });
        for(locator_t loc : table)
            table_code.push_back({ .op = ASM_DATA, .arg = loc.with_is(IS_PTR_HI) });
    }

    // Now prepare main code:

    for(unsigned i = 0; i < order.size(); ++i)
    {
        asm_node_t& node = *order[i];
        asm_node_t* prev = i ? order[i-1] : nullptr;
        asm_node_t* next = i+1 < order.size() ? order[i+1] : nullptr;

        goto insert_label;
        if(node.inputs().size() > 1 
           || (node.inputs().size() == 1 && prev != node.inputs()[0])
           || node.label == m_entry_label)
        {
        insert_label:
            code.push_back({ .op = ASM_LABEL, .arg = get_label(node) });
        }
        else for(asm_node_t const* input : node.inputs())
            if(input->is_switch())
                goto insert_label;

        code.insert(code.end(), node.code.begin(), node.code.end());

        if(node.output_inst.op)
        {
            if(node.output_inst.op == JMP_ABSOLUTE)
                assert(node.outputs().size() == 1);

            if(node.is_switch() || node.outputs().empty())
                code.push_back(node.output_inst);
            else
            {
                passert(node.outputs().size() <= 2, node.outputs().size());

                for(unsigned j = 0; j < node.outputs().size(); ++j)
                {
                    if(node.outputs()[j].node == next)
                        continue;
                    op_t op = node.output_inst.op;
                    if(j > 0 && is_branch(op))
                        op = invert_branch(op);
                    code.push_back({ .op = op, .arg = get_label(*node.outputs()[j].node) });
                }
            }
        }
        else
            assert(node.outputs().empty());
    }

    // Append switch:

    code.insert(code.end(), table_code.begin(), table_code.end());

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

// This is used to handle the entrances of CFG nodes which may have multiple labels.
// Specifically, it's used to get an accurate edge_depth.
template<typename Set>
static void build_incoming(Set& incoming, asm_node_t const& node, cfg_ht cfg)
{
    if(node.cfg && node.cfg != cfg)
        incoming.insert(node.cfg);
    else if(node.label.lclass() == LOC_CFG_LABEL && node.label.data() > 0)
        for(asm_node_t* input : node.inputs())
            if(input != &node)
                build_incoming(incoming, *input, cfg);
}

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

    fc::small_set<cfg_ht, 8> incoming;

    for(asm_node_t& node : list)
    {
        auto const scale = [&](asm_node_t& other)
        {
            cfg_ht other_cfg = other.cfg;
            if(!other_cfg)
                other_cfg = node.cfg;
            if(!node.cfg)
                return 1;
            assert(node.cfg && other_cfg);

            incoming.clear();
            if(node.cfg == other_cfg)
                build_incoming(incoming, node, node.cfg);

            unsigned depth = 0;

            if(incoming.empty())
                depth = edge_depth(node.cfg, other_cfg);
            else
                for(cfg_ht cfg : incoming)
                    depth = std::max<unsigned>(depth, edge_depth(cfg, other_cfg));

            return 1 << std::min<unsigned>(16, 2 * depth);
        };

        switch(node.outputs().size())
        {
        case 0:
            break;
        case 1:  
            // Weight 'jmp' the highest:
            elim_order.push_back({ &node, 0, 3 * scale(*node.outputs()[0].node) });
            break;
        case 2:
            // It's dumb, but we'll slightly prioritize the original order.
            {
                bool const i = node.outputs()[0].node->original_order > node.outputs()[1].node->original_order;
                elim_order.push_back({ &node, i, 2 * scale(*node.outputs()[i].node) });
                elim_order.push_back({ &node, !i, 1 * scale(*node.outputs()[!i].node) });
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
        asm_node_t& to = *edge.from->outputs()[edge.output].node;
        dprint(log, "PATH_COVER_EDGE", edge.weight, edge.from->cfg, to.cfg);

        if(edge.from->vcover.path_output >= 0)
            continue; // Path already exists

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

        dprint(log, "PATH_COVER_EDGE_MADE");
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
        for(asm_node_t* it = &node;; it = it->outputs()[it->vcover.path_output].node)
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
        for(auto const& edge : node->outputs())
        {
            asm_node_t* output = edge.node;
            if(output->vorder.path != &path)
                path.branches.push_back({ node->vorder.offset, output->vorder.offset, output->vorder.path });
        }
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
        while(lowest_cost && std::next_permutation(order.begin(), order.end()));
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
            if(lowest_cost == 0)
                goto done;
        }
    done:;
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

template<typename Fn>
void asm_graph_t::liveness_dataflow(Fn const& fn)
{
    for(asm_node_t& node : list)
        node.clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

    worklist.clear();

    for(asm_node_t& node : list)
        if(node.outputs().empty())
            worklist.push(&node);

    while(!worklist.empty())
    {
    reenter:
        asm_node_t& node = *worklist.pop();

        bool const changed = fn(node);

        // Add all predecessors to worklist:
        if(changed || !node.test_flags(FLAG_PROCESSED))
        {
            node.set_flags(FLAG_PROCESSED);
            for(asm_node_t* input : node.inputs())
                worklist.push(input);
        }
    }

    // Some nodes (infinite loops, etc) might not be reachable travelling
    // backwards from the exit. Handle those now:
    for(asm_node_t& node : list)
        if(!node.test_flags(FLAG_PROCESSED))
           worklist.push(&node);
           
    if(!worklist.empty())
        goto reenter;
}

void asm_graph_t::optimize_live_registers()
{
    for(asm_node_t& node : list)
    {
        regs_t gen = 0;
        regs_t kill = 0;
        for(asm_inst_t const& inst : node.code)
        {
            gen |= op_input_regs(inst.op) & ~kill;
            kill |= op_output_regs(inst.op);
        }

        // Set 'vregs.in's initial value to be the set of variables used in
        // this cfg node before an assignment.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'vregs.out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set's inverse is sometimes called 'KILL')
        node.vregs.in = gen;
        node.vregs.out = ~kill;
    }

    liveness_dataflow([&](asm_node_t& node)
    {
        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        regs_t temp = 0;
        for(auto const& output : node.outputs())
            temp |= output.node->vregs.in;

        // Now use that to calculate a new live-in set:
        temp &= node.vregs.out; // vregs.out holds inverted KILL
        temp |= node.vregs.in;

        if(!node.test_flags(FLAG_PROCESSED) || temp != node.vregs.in)
        {
            node.set_flags(FLAG_PROCESSED);
            for(asm_node_t* input : node.inputs())
                worklist.push(input);
        }

        // If 'vlive.in' is changing:
        bool const changing = temp != node.vregs.in;

        // Assign 'vregs.in':
        node.vregs.in = temp;

        return changing;
    });

    // Now properly set 'out' to be the union of all successor inputs:
    for(asm_node_t& node : list)
    {
        // Might as well clear flags.
        node.clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        node.vregs.out = 0;
        for(auto const& output : node.outputs())
            node.vregs.out |= output.node->vregs.in;
    }

    // OK! Register liveness has been calculated per-node.

    auto const simple_addr_mode = [](addr_mode_t addr_mode)
    {
        switch(addr_mode)
        {
        case MODE_IMPLIED:
        case MODE_IMMEDIATE:
        case MODE_ZERO_PAGE:
        case MODE_ABSOLUTE:
        case MODE_RELATIVE:
            return true;
        default:
            return false;
        }
    };

    // Calculate per-op liveness next:
    static TLS std::vector<regs_t> live_regs;
    for(asm_node_t& node : list)
    {
        live_regs.clear();
        live_regs.resize(node.code.size(), 0);

        regs_t live = node.vregs.out;

        for(int i = int(node.code.size()) - 1; i >= 0; --i)
        {
            asm_inst_t const& inst = node.code[i];

            regs_t const outputs = op_output_regs(inst.op);
            regs_t const inputs  = op_input_regs(inst.op);

            live_regs[i] = live;

            live &= ~outputs;
            live |= inputs;

        }

        // Now attempt to optimize out redundant loads following stores.
        // e.g. in:
        //     STX foo
        //     . . .
        //     LDX foo
        // The LDX can be removed in some circumstances.

        {
            constexpr unsigned MAX_SET_SIZE = 8;
            using set_t = fc::small_set<locator_t, MAX_SET_SIZE>;
            set_t a_set;
            set_t x_set;
            set_t y_set;

            auto const replace = [&](asm_inst_t& inst, op_t op)
            {
                assert(op);
                dprint(log, "REGLIVE_PRUNE_1", inst);
                inst.op = op;
                inst.arg = inst.alt = {};
            };

            std::size_t const code_size = node.code.size();
            for(unsigned i = 0; i < code_size; ++i)
            {
                asm_inst_t& inst = node.code[i];

                assert(inst.op != ASM_LABEL);
                if(inst.op == ASM_FENCE
                   || (op_flags(inst.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH))
                   || !simple_addr_mode(op_addr_mode(inst.op)))
                {
                    a_set.clear();
                    x_set.clear();
                    y_set.clear();
                    continue;
                }

                if(!inst.alt && is_var_like(inst.arg.lclass())) 
                {
                    switch(inst.op)
                    {
                    case STA_ZERO_PAGE:
                    case STA_ABSOLUTE:
                        if(a_set.size() < MAX_SET_SIZE)
                        {
                            a_set.insert(inst.arg);
                            continue;
                        }
                        break;

                    case STX_ZERO_PAGE:
                    case STX_ABSOLUTE:
                        if(x_set.size() < MAX_SET_SIZE)
                        {
                            x_set.insert(inst.arg);
                            continue;
                        }
                        break;

                    case STY_ZERO_PAGE:
                    case STY_ABSOLUTE:
                        if(y_set.size() < MAX_SET_SIZE)
                        {
                            y_set.insert(inst.arg);
                            continue;
                        }
                        break;

                    case LDA_ZERO_PAGE:
                    case LDA_ABSOLUTE:
                        if(!(live_regs[i] & (REGF_N | REGF_Z)) && a_set.count(inst.arg))
                        {
                            replace(inst, ASM_PRUNED);
                            continue;
                        }
                        else if(x_set.count(inst.arg))
                        {
                            replace(inst, TXA_IMPLIED);
                            a_set = x_set;
                            continue;
                        }
                        else if(y_set.count(inst.arg))
                        {
                            replace(inst, TYA_IMPLIED);
                            a_set = y_set;
                            continue;
                        }
                        break;

                    case LDX_ZERO_PAGE:
                    case LDX_ABSOLUTE:
                        if(!(live_regs[i] & (REGF_N | REGF_Z)) && x_set.count(inst.arg))
                        {
                            replace(inst, ASM_PRUNED);
                            continue;
                        }
                        else if(a_set.count(inst.arg))
                        {
                            replace(inst, TAX_IMPLIED);
                            x_set = a_set;
                            continue;
                        }
                        break;

                    case LDY_ZERO_PAGE:
                    case LDY_ABSOLUTE:
                        if(!(live_regs[i] & (REGF_N | REGF_Z)) && y_set.count(inst.arg))
                        {
                            replace(inst, ASM_PRUNED);
                            continue;
                        }
                        else if(a_set.count(inst.arg))
                        {
                            replace(inst, TAY_IMPLIED);
                            y_set = a_set;
                            continue;
                        }
                        break;

                    default:
                        break;
                    }
                }

                regs_t const outputs = op_output_regs(inst.op);

                if(outputs & REGF_A)
                    a_set.clear();
                if(outputs & REGF_X)
                    x_set.clear();
                if(outputs & REGF_Y)
                    y_set.clear();
                if(outputs & REGF_M)
                {
                    if(inst.arg)
                    {
                        a_set.erase(inst.arg);
                        x_set.erase(inst.arg);
                        y_set.erase(inst.arg);
                    }

                    if(inst.alt)
                    {
                        a_set.erase(inst.alt);
                        x_set.erase(inst.alt);
                        y_set.erase(inst.alt);
                    }
                }
            }
        }

        // Perform some peep-hole optimization using the liveness analysis:

        {
            for_each_peephole(&*node.code.begin(), &*node.code.end(), 
                              [&](asm_inst_t& a, asm_inst_t& b, asm_inst_t* c)
            {
                unsigned const ai = &a - node.code.data();
                unsigned const bi = &b - node.code.data();

                // Prune ops that have no effect:
                if(!(op_flags(a.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH | ASMF_FAKE | ASMF_IMPURE))
                   && a.op < NUM_NORMAL_OPS
                   && !(REGF_M & op_output_regs(a.op))
                   && !(live_regs[ai] & op_output_regs(a.op))
                   && (!a.arg || is_var_like(a.arg.lclass()))
                   && !a.alt)
                {
                    dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                    a.prune();
                }

                // Convert code like:
                //     LDA foo
                //     TAY
                // To:
                //     LDY foo
                switch(op_name(a.op))
                {
                case LDA:
                    if(b.op == TAX_IMPLIED && !(live_regs[bi] & REGF_A))
                    {
                        if(op_t op = get_op(LDX, op_addr_mode(a.op)))
                        {
                            a.op = op;
                            dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                            b.prune();
                        }
                    }
                    else if(b.op == TAY_IMPLIED && !(live_regs[bi] & REGF_A))
                    {
                        if(op_t op = get_op(LDY, op_addr_mode(a.op)))
                        {
                            a.op = op;
                            dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                            b.prune();
                        }
                    }
                    break;

                case LDX:
                    if(b.op == TXA_IMPLIED && !(live_regs[bi] & REGF_X))
                    {
                        if(op_t op = get_op(LDA, op_addr_mode(a.op)))
                        {
                            a.op = op;
                            dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                            b.prune();
                        }
                    }
                    break;

                case LDY:
                    if(b.op == TYA_IMPLIED && !(live_regs[bi] & REGF_Y))
                    {
                        if(op_t op = get_op(LDA, op_addr_mode(a.op)))
                        {
                            a.op = op;
                            dprint(log, "REGLIVE_PRUNE_2", b, __LINE__);
                            b.prune();
                        }
                    }
                    break;

                case LAX:
                    // Convert LAX to either LDA or LDX:
                    if(!(live_regs[ai] & REGF_A))
                    {
                        if(op_t op = get_op(LDX, op_addr_mode(a.op)))
                            a.op = op;
                    }
                    else if(!(live_regs[ai] & REGF_X))
                    {
                        if(op_t op = get_op(LDA, op_addr_mode(a.op)))
                            a.op = op;
                    }
                    break;

                default:
                    break;
                }

                // Convert code like:
                //    CMP #$80
                //    ROR
                //    CMP #$80
                // To:
                //    CMP #$80
                //    ARR #$FF
                if(c && a.op == CMP_IMMEDIATE 
                   && b.op == ROR_IMPLIED
                   && !a.alt  && a.arg  == locator_t::const_byte(0x80)
                   && !c->alt && c->arg == locator_t::const_byte(0x80)
                   && !(live_regs[ai] & REGF_V))
                {
                    b = a;
                    c->op = ARR_IMMEDIATE;
                    c->arg = locator_t::const_byte(0xFF);
                    a.prune();
                }
            });
        }

        // Now attempt to optimize out redundant loads following loads.
        // e.g. in:
        //     LDX #0
        //     . . .
        //     LDA #0
        // The LDA can be removed in some circumstances.

        {
            using map_t = fc::small_map<locator_t, unsigned, 16>;
            map_t map;

            auto const store_name = [](op_t op) -> op_name_t
            {
                switch(op)
                {
                case LDA_IMMEDIATE: return STA;
                case LDX_IMMEDIATE: return STX;
                case LDY_IMMEDIATE: return STY;
                default: return {};
                }
            };

            for_each_peephole(&*node.code.begin(), &*node.code.end(), 
                              [&](asm_inst_t& a, asm_inst_t& b, asm_inst_t* c)
            {
                unsigned const ai = &a - node.code.data();
                unsigned const bi = &b - node.code.data();

                assert(a.op != ASM_LABEL);
                if(a.op == ASM_FENCE || (op_flags(a.op) & (ASMF_JUMP | ASMF_RETURN | ASMF_CALL | ASMF_SWITCH)))
                {
                    map.clear();
                    return;
                }

                if(a.alt || b.alt)
                    return;

                if(op_name_t store = store_name(a.op))
                {
                    auto result = map.emplace(a.arg, ai);

                    if(!result.second)
                    {
                        // See if we can map.

                        // - next ops must be STA
                        // - no mention of the store in-between

                        unsigned const prev_i = result.first->second;
                        assert(prev_i < ai);

                        if(op_name(b.op) == store 
                           && !(live_regs[bi] & op_output_regs(a.op))
                           && is_var_like(b.arg.lclass()))
                        {
                            for(unsigned i = prev_i; i < ai; ++i)
                            {
                                auto const& inst = node.code[i];
                                if(inst.arg == b.arg || inst.alt == b.arg
                                   || !simple_addr_mode(op_addr_mode(inst.op)))
                                {
                                    goto fail;
                                }
                            }

                            // OK! We can relocate.

                            // Prune 'a':
                            a.prune();

                            // Shift every op forwards one:
                            std::move(node.code.begin() + prev_i + 1, node.code.begin() + ai, 
                                      node.code.begin() + prev_i + 2);

                            // Move 'b' into the new hole:
                            dprint(log, "REGLIVE_PRUNE_3", b);
                            auto& b_dest = node.code[prev_i + 1];
                            b.op = get_op(store_name(node.code[prev_i].op), op_addr_mode(b.op));
                            assert(b.op);
                            b_dest = b;
                            b.prune();
                            return;
                        }

                    fail:
                        result.first.underlying->second = ai;
                    }
                }

            });
        }
    }
}


template<typename ReadWrite>
void do_inst_rw(fn_t const& fn, rh::batman_set<locator_t> const& map, asm_inst_t const& inst, ReadWrite rw)
{
    bool const is_call = op_flags(inst.op) & ASMF_CALL;
    bool const is_jump = op_flags(inst.op) & ASMF_JUMP;
    bool const is_return = ::is_return(inst);

    bool const is_fn = (is_call || is_jump || is_return) && (inst.arg.lclass() == LOC_FN);

    if(inst.op == ASM_PRUNED)
        return;

    if(is_fn)
    {
        fn_ht const call_h = inst.arg.fn();
        fn_t const& call = *call_h;

        for(locator_t const& loc : map)
        {
            unsigned const i = &loc - map.begin();

            if(has_fn(loc.lclass()) && loc.fn() == call_h)
                rw(i, loc.lclass() == LOC_ARG, loc.lclass() == LOC_RETURN);

            if(loc.lclass() == LOC_GMEMBER)
            {
                if(call.fclass == FN_MODE)
                {
                    group_vars_ht gv = loc.gmember()->gvar.group_vars;
                    rw(i, gv && call.precheck_group_vars().test(gv.id), false);
                }
                else
                    rw(i, call.ir_reads().test(loc.gmember().id), call.ir_writes().test(loc.gmember().id));
            }
        }
    }

    if(is_return)
    {
        for(locator_t const& loc : map)
        {
            unsigned const i = &loc - map.begin();

            // Every return will be "read" by the rts:
            if(loc.lclass() == LOC_RETURN)
                rw(i, true, false);

            // Some gmembers will be written:
            if(loc.lclass() == LOC_GMEMBER)
                rw(i, fn.ir_writes().test(loc.gmember().id), false);
        }
    }
    else if(!is_fn) // fns handled earlier.
    {
        auto test_loc = [&](locator_t loc)
        {
            if(locator_t const* it = map.lookup(loc))
            {
                unsigned const i = it - map.begin();
                rw(i, op_input_regs(inst.op) & REGF_M, op_output_regs(inst.op) & REGF_M);
            }
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

// Returns bitset size
unsigned asm_graph_t::calc_liveness(fn_t const& fn, rh::batman_set<locator_t> const& map)
{
    bitset_pool.clear();
    auto const bs_size = bitset_size<>(map.size());

    // Allocate bitsets:
    for(asm_node_t& node : list)
    {
        node.vlive.in  = bitset_pool.alloc(bs_size);
        node.vlive.out = bitset_pool.alloc(bs_size);
        bitset_set_all(bs_size, node.vlive.out);
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
    asm_node_t& root = *label_map[m_entry_label];
    for(locator_t const& loc : map)
    {
        if(loc.lclass() == LOC_ARG)
            bitset_set(root.vlive.in, &loc - map.begin());
    }

    for(asm_node_t& node : list)
    {
        // Set 'd.in's initial value to be the set of variables used in
        // this cfg node before an assignment.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'd.out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set's inverse is sometimes called 'KILL')

        for(asm_inst_t const& inst : node.code)
        {
            do_inst_rw(fn, map, inst, [&](unsigned i, bool read, bool write)
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

    liveness_dataflow([&](asm_node_t& node)
    {
        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        bitset_clear_all(bs_size, temp_set);
        for(auto const& output : node.outputs())
            bitset_or(bs_size, temp_set, output.node->vlive.in);

        // Now use that to calculate a new live-in set:
        bitset_and(bs_size, temp_set, node.vlive.out); // (vlive.out holds inverted KILL)
        bitset_or(bs_size, temp_set, node.vlive.in);

        // If 'vlive.in' is changing:
        bool const changing = !bitset_eq(bs_size, temp_set, node.vlive.in);

        // Assign 'vlive.in':
        if(changing)
            bitset_copy(bs_size, node.vlive.in, temp_set);

        return changing;
    });

    // Now properly set 'out' to be the union of all successor inputs:
    for(asm_node_t& node : list)
    {
        // Might as well clear flags.
        node.clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        bitset_clear_all(bs_size, node.vlive.out);
        for(auto const& output : node.outputs())
            bitset_or(bs_size, node.vlive.out, output.node->vlive.in);
    }

    return bs_size;
}

lvars_manager_t asm_graph_t::build_lvars(fn_t const& fn)
{
    lvars_manager_t lvars(fn.handle(), *this);

    unsigned const bs_size = calc_liveness(fn, lvars.map());
    bitset_uint_t* const live = ALLOCA_T(bitset_uint_t, bs_size);

    // Step-through the code backwards, maintaining a current liveness set
    // and using that to build an interference graph.
    for(asm_node_t& node : list)
    {
        // Since we're going backwards, reset 'live' to the node's output state.
        bitset_copy(bs_size, live, node.vlive.out);

        // Variables that are live together interfere with each other.
        // Update the interference graph here:
        lvars.add_lvar_interferences(live);

        for(asm_inst_t& inst : node.code | std::views::reverse)
        {
            if((op_flags(inst.op) & ASMF_CALL) && inst.arg.lclass() == LOC_FN)
            {
                // Every live lvar will interfere with this fn:
                bitset_for_each(bs_size, live, [&](unsigned i)
                {
                    lvars.add_fn_interference(i, inst.arg.fn());
                });

                // Variables that are live together interfere with each other.
                // Update the interference graph here:
                lvars.add_lvar_interferences(live);
            }

            do_inst_rw(fn, lvars.map(), inst, [&](unsigned i, bool read, bool write)
            {
                // We have to set the value here temporarily,
                // to ensure the interference will be marked.
                // It usually gets cleared right after.
                if(write)
                    bitset_set(live, i);

                if(read)
                    bitset_set(live, i);

                // Variables that are live together interfere with each other.
                // Update the interference graph here:
                lvars.add_lvar_interferences(live);

                if(!read && write)
                    bitset_clear(live, i);
            });

        }
    }

    // All referenced params will interfere with each other:
    bitset_clear_all(bs_size, live);
    fn.for_each_referenced_param_locator([&](locator_t loc)
    { 
        int const i = lvars.index(loc);
        if(i >= 0)
            bitset_set(live, i);
    });
    lvars.add_lvar_interferences(live);

    return lvars;
}

void asm_graph_t::remove_maybes(fn_t const& fn)
{
    // Map locators.

    rh::batman_set<locator_t> map;

    for_each_inst([&](asm_inst_t const& inst)
    {
        if(!(op_flags(inst.op) & ASMF_MAYBE_STORE))
            return;
        if(inst.arg)
            map.insert(inst.arg.mem_head());
        if(inst.alt)
            map.insert(inst.alt.mem_head());
    });

    // Now do liveness:
    unsigned const bs_size = calc_liveness(fn, map);
    bitset_uint_t* const live = ALLOCA_T(bitset_uint_t, bs_size);

    // Step-through the code backwards, maintaining a current liveness set
    // and using that to build an interference graph.
    for(asm_node_t& node : list)
    {
        // Since we're going backwards, reset 'live' to the node's output state.
        bitset_copy(bs_size, live, node.vlive.out);

        for(asm_inst_t& inst : node.code | std::views::reverse)
        {
            if(op_flags(inst.op) & ASMF_MAYBE_STORE)
            {
                if(locator_t const* it = map.lookup(inst.arg))
                {
                    assert(op_output_regs(inst.op) & REGF_M);

                    unsigned const i = it - map.begin();

                    if(bitset_test(live, i))
                    {
                        if(op_t op = change_addr_mode(inst.op, MODE_ABSOLUTE))
                            inst.op = op;
                        else switch(inst.op)
                        {
                        case MAYBE_STORE_C: inst.op = STORE_C_ABSOLUTE; break;
                        case MAYBE_STORE_Z: inst.op = STORE_Z_ABSOLUTE; break;
                        default: assert(false);
                        }
                    }
                    else
                    {
                        dprint(log, "ASM_GRAPH_PRUNE", inst.arg);
                        inst.op = ASM_PRUNED;
                        inst.arg = inst.alt = {};
                    }
                }
                else 
                    passert(false, inst.arg);
            }

            do_inst_rw(fn, map, inst, [&](unsigned i, bool read, bool write)
            {
                if(read)
                    bitset_set(live, i);
                else if(write) // Only occurs if 'read' is false.
                    bitset_clear(live, i);
            });
        }
    }
}

