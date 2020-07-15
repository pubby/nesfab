#include "code_gen.hpp"

#include <cassert>

#include <boost/container/small_vector.hpp>

#include "alloca.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "ir.hpp"
#include "o.hpp"
#include "toposort.hpp"

namespace bc = ::boost::container;

namespace // anonymous
{
    // bm = bytemap
    using bm_t = std::array<ssa_value_t, type_t::max_total_bytes>;
    static_assert(type_t::max_total_bytes == 6); // To match 'zero_bm' below.
    constexpr bm_t zero_bm = { 0u, 0u, 0u, 0u, 0u, 0u };

    struct ssa_byteify_data_t
    {
        bm_t bm;
    };

    bm_t get_bm(ssa_value_t value)
    {
        assert(value);

        if(value.is_const())
        {
            bm_t bm;
            fixed_int_t f = value.fixed().value;
            for(unsigned i = 0; i < bm.size(); ++i)
            {
                bm[i] = f & 0xFF;
                f >>= 8;
            }
            return bm;
        }
        else
        {
            assert(is_arithmetic(value->type().name));
            return value.handle().data<ssa_byteify_data_t>().bm;
        }
    }
}

// Used in 'byteify' to remove casts.
static void _split_cast(ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;

    // Check if the input has outputs:
    ssa_value_t input = ssa_node.input(0);
    if(input.holds_ref() && input->op() == SSA_cast && !input->flags)
        _split_cast(input.handle());

    type_t const type = ssa_node.type();
    auto& data = ssa_h.data<ssa_byteify_data_t>();
    data.bm = zero_bm;
    bm_t input_bm = get_bm(input);
    unsigned const end = end_byte(type.name);
    for(unsigned i = begin_byte(type.name); i < end; ++i)
        data.bm[i] = input_bm[i];

    ssa_node.flags = 1;
}

// Converts all operations with non-BYTE types to only use BYTE.
void byteify(ir_t& ir, global_manager_t& globals, global_t& global)
{
    assert(global.type.name == TYPE_FN);

    ssa_data_pool::scope_guard_t<ssa_byteify_data_t> sg(
        ssa_pool::array_size());

    ssa_workvec.clear();
    static std::vector<ssa_ht> prune_nodes;
    prune_nodes.clear();

    // Split nodes that have multi-byte results into N nodes, where N
    // is the number of bytes its result takees.

    // Create the nodes here, leaving the original node to keep track:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;
            ssa_node.flags = 0; // Zero the flags for the _split_cast step.
            type_t const type = ssa_node.type();

            if(type.name == TYPE_BYTE)
            {
                auto& ssa_data = ssa_it.data<ssa_byteify_data_t>(); 
                ssa_data.bm = zero_bm;
                ssa_data.bm[type_t::max_frac_bytes] = ssa_it;
                continue;
            }

            if(!is_arithmetic(type.name))
                continue;

            if(ssa_node.op() == SSA_cast)
            {
                // Casts will just forward their input(s) to their output(s),
                // and will be removed entirely.
                // The forwarding will happen after all other nodes that
                // need splitting have been split.
                prune_nodes.push_back(ssa_it);
                continue;
            }

            // TODO:
            ssa_op_t split_op;
            switch(ssa_node.op())
            {
            default:
                split_op = ssa_node.op();
                break;
            case SSA_fn_call:
                split_op = SSA_get_return;
                break;
            }

            bm_t bm = zero_bm;
            unsigned const end = end_byte(type.name);
            for(unsigned i = begin_byte(type.name); i < end; ++i)
                bm[i] = cfg_node.emplace_ssa(split_op, type_t{TYPE_BYTE});
            // !!IMPORTANT: 'ssa_node' is invalidated after this!!

            // TODO:
            ssa_data_pool::resize<ssa_byteify_data_t>(ssa_pool::array_size());

            auto& ssa_data = ssa_it.data<ssa_byteify_data_t>();
            ssa_data.bm = std::move(bm);

            // workvec will hold nodes that have been split:
            ssa_workvec.push_back(ssa_it);
        }
    }

    // Split cast operations now.
    // 'prune_nodes' should hold all the casts at the moment and nothing else.
    for(ssa_ht ssa_h : prune_nodes)
    {
        assert(ssa_h->op() == SSA_cast);
        _split_cast(ssa_h);
    }

    // Rewrite the inputs of certain nodes to use multi-byte
    bc::small_vector<ssa_value_t, 24> new_input;
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;

            switch(ssa_node.op())
            {
            case SSA_fn_call:
                {
                    // Functions have a specific calling convention defined
                    // by their function type.
                    // Because SSA constants lack types, it's necessary
                    // to look up the function's global definition first:

                    global_t& global = globals.global(ssa_node);
                    assert(global.gclass == GLOBAL_FN);
                    unsigned const argn = global.type.num_params();

                    assert(argn == ssa_node.input_size()+2);

                    new_input.clear();
                    for(unsigned i = 0; i < argn; ++i)
                    {
                        bm_t bm = get_bm(ssa_node.input(i+2));
                        type_name_t t = global.type[i].name;
                        assert(is_arithmetic(t));
                        unsigned const end = end_byte(t);
                        for(unsigned j = begin_byte(t); j < end; ++j)
                            new_input.push_back(bm[j]);
                    }

                    // Keep the first two arguments
                    // (the fence and the fn index),
                    // but replace the fn parameters.
                    ssa_node.link_shrink_inputs(2);
                    ssa_node.link_append_input(&*new_input.begin(),
                                               &*new_input.end());

                }
                break;

            case SSA_fence:
                {
                    // Fences just expand to hold every new multi-byte value,
                    // in lieu of their originals.

                    new_input.clear();
                    unsigned const input_size = ssa_node.input_size();
                    for(unsigned i = 0; i < input_size; ++i)
                    {
                        ssa_value_t input = ssa_node.input(i);
                        
                        // Const nodes do nothing as fence params and can
                        // be ignored.
                        if(input.is_const())
                            continue;

                        type_name_t t = input->type().name;
                        bm_t bm = get_bm(input);
                        unsigned const end = end_byte(t);
                        for(unsigned j = begin_byte(t); j < end; ++j)
                        {
                            // Again, const can be ignored.
                            if(!bm[j].is_const())
                                new_input.push_back(bm[j]);
                        }
                    }

                    ssa_node.link_clear_inputs();
                    ssa_node.link_append_input(&*new_input.begin(),
                                               &*new_input.end());
                }
                break;

            case SSA_eq:
            case SSA_not_eq:
            case SSA_lt:
            case SSA_lte:
                {
                    // Comparisons convert to N parallel comparisons,
                    // where N is the size in bytes of the largest argument.

                    // The format is interleaved (ababab instead of aaabbb)
                    // to make it easier to remove arguments in optimization.

                    new_input.clear();
                    assert(ssa_node.input_size() == 2);

                    std::array<bm_t, 2> bms;
                    unsigned begin = ~0;
                    unsigned end = 0;
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        ssa_value_t input = ssa_node.input(i);
                        type_name_t t = input.type().name;
                        bms[i] = get_bm(input);
                        begin = std::min(begin, begin_byte(t));
                        end = std::max(end, end_byte(t));
                        std::printf("types %s\n", type_string(input.type()).c_str());
                    }

                    std::printf("span %i\n", end - begin);

                    for(unsigned j = begin; j < end; ++j)
                        for(unsigned i = 0; i < 2; ++i)
                            new_input.push_back(bms[i][j]);

                    ssa_node.link_clear_inputs();
                    ssa_node.link_append_input(&*new_input.begin(),
                                               &*new_input.end());

                    // Should always have an even number of args.
                    assert(ssa_node.input_size() % 2 == 0);
                }
                break;

            default:
                break;
            }

        }
    }

    // Calculate argument offsets to be used later.
    unsigned* arg_offsets = ALLOCA_T(unsigned, global.type.num_params());
    for(unsigned i = 0, offset = 0; i < global.type.num_params(); ++i)
    {
        arg_offsets[i] = offset;
        offset += total_bytes(global.type[i].name);
    }

    // Finish up the split outputs created earlier:
    std::vector<bm_t> bms;
    for(ssa_ht ssa_h : ssa_workvec)
    {
        ssa_node_t& ssa_node = *ssa_h;
        auto& ssa_data = ssa_h.data<ssa_byteify_data_t>(); 
        type_name_t const t = ssa_node.type().name;
        assert(is_arithmetic(t));

        switch(ssa_node.op())
        {
        // These replace the original node with N parallel ops,
        // each with the same amount of arguments as the original.
        case SSA_phi:
        case SSA_cast:
        case SSA_and:
        case SSA_or:
        case SSA_xor:
            {
                unsigned const input_size = ssa_node.input_size();

                bms.resize(input_size);
                for(unsigned i = 0; i < input_size; ++i)
                    bms[i] = get_bm(ssa_node.input(i));

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_value_t split_h = ssa_data.bm[i];
                    assert(split_h.holds_ref());
                    ssa_node_t& split_node = *split_h;

                    split_node.alloc_input(input_size);
                    for(unsigned j = 0; j < input_size; ++j)
                        split_node.build_set_input(j, bms[j][i]);
                }
                prune_nodes.push_back(ssa_h);
            }
            break;

        // These ops are similar to the above, but need a carry:
        case SSA_add:
        case SSA_sub:
            {
                bm_t const lhs_bm = get_bm(ssa_node.input(0));
                bm_t const rhs_bm = get_bm(ssa_node.input(1));

                ssa_value_t carry = ssa_node.input(2);
                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_value_t split_h = ssa_data.bm[i];
                    assert(split_h.holds_ref());
                    ssa_node_t& split_node = *split_h;

                    split_node.alloc_input(3);
                    split_node.build_set_input(0, lhs_bm[i]);
                    split_node.build_set_input(1, rhs_bm[i]);
                    split_node.build_set_input(2, carry);

                    carry = split_h;
                }
                prune_nodes.push_back(ssa_h);
            }
            break;

        // Keeps the original node.
        // All the split nodes created will reference it, using their [1]
        // argument as an index representing which byte they hold.
        case SSA_fn_call:
            {
                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_value_t split_h = ssa_data.bm[i];
                    assert(split_h.holds_ref());
                    ssa_node_t& split_node = *split_h;
                    assert(split_node.op() == SSA_get_return);

                    split_node.alloc_input(2);
                    split_node.build_set_input(0, ssa_h);
                    split_node.build_set_input(1, i);
                }
            }
            break;

        case SSA_argument:
            {
                assert(ssa_node.input_size() == 1);
                ssa_value_t input = ssa_node.input(0);
                assert(input.is_const());
                unsigned const orig_argn = input.whole();
                unsigned offset = arg_offsets[orig_argn];

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_value_t split_h = ssa_data.bm[i];
                    assert(split_h.holds_ref());
                    ssa_node_t& split_node = *split_h;

                    split_node.alloc_input(1);
                    split_node.build_set_input(0, offset);
                    ++offset;
                }

                prune_nodes.push_back(ssa_h);
            }
            break;

        default:
            // Shouldn't ever happen if this was coded correctly...
            throw std::runtime_error(fmt("Unhandled op SSA_% in byteify.", 
                                         ssa_node.op()));
        }
    }

    return;

    // Prune nodes that are now unnecessary:
    for(ssa_ht ssa_h : prune_nodes)
    {
        ssa_node_t& ssa_node = *ssa_h;
        ssa_node.link_clear_inputs();
        ssa_node.unsafe_prune();
    }
}


// THE PLAN:
// - insert copies
// - schedule each CFG node based on some heuristic
// - aggressively coalesce
// - 

// TODO:
// - fix liveness for infintie loops


namespace
{
    using schedule_t = std::vector<ssa_ht>;

    struct ssa_out_data_t
    {
        ssa_value_t set_head = nullptr; // Basically a union-find pointer.
        ssa_ht next = nullptr; // A linked-list to the next node
        unsigned schedule_i = 0;

        // Used to sequentialize parallel copies:
        ssa_ht loc == nullptr;
        ssa_ht pred == nullptr;
    };

    struct cfg_out_data_t
    {
        schedule_t schedule;

        // Bitsets
        unsigned* live_in;
        unsigned* live_out; // Also used to hold the 'KILL' set temporarily.

        // Holds parallel copies introduced to handle phi nodes.
        std::vector<ssa_ht> entry_copies;
        std::vector<ssa_ht> exit_copies;
    };

    // Nodes will be partitioned into congruence classes for coalescing
    // purposes, dubbed "cset" for brevity.
    // These are implemented as union-find on top of a singly-linked list.
    // Below are some helper functions.

    bool _cset_is_first(ssa_ht h)
    {
        return !h.data<ssa_out_data_t>().set_head.holds_ref();
    }

    ssa_ht _cset_get_first(ssa_ht h)
    {
        while(true)
        {
            auto& data = h.data<ssa_out_data_t>();
            if(data.set_head.holds_ref())
                h = data.set_head.handle();
            else
                return h;
        }
    }

    bool _cset_is_pinned(ssa_ht h, addr_t& addr)
    {
        h = _cset_get_first(h);
        auto& data = h.data<ssa_out_data_t>();
        if(data.set_head.is_const())
        {
            addr = data.set_head.whole();
            return true;
        }
        return false;
    }

    // Appends 'h' onto the set of 'last'.
    void _cset_append(ssa_ht last, ssa_ht h)
    {
        assert(last != h);

        auto& last_data = last.data<ssa_out_data_t>();
        auto& h_data = h.data<ssa_out_data_t>();

        assert(!h_data.next);

        if(_cset_is_first(last)
            h_data.set_head = last;
        else
            h_data.set_head = last_data.set_head;

        last_data.next = h;
    }

    // Adds the set starting at 'b_first' to the set of 'a_first' to 'a_last'.
    void _cset_union(ssa_ht a_first, ssa_ht a_last, ssa_ht b_first)
    {
        auto& a_first_data = a_first.data<ssa_out_data_t>();
        auto& a_last_data = a_last.data<ssa_out_data_t>();
        auto& b_first_data = b_first.data<ssa_out_data_t>();

        assert(a_first == _cset_get_first(a_first);
        assert(b_first == _cset_get_first(b_first);
        assert(_cset_get_first(a_last) == _cset_get_first(a_first);
        assert(!a_last_data.next);

        // If 'b' is pinned, make 'a' pinned to the same memory:
        if(b_first_data.set.is_const())
        {
            assert(!a_first_data.set.is_const());
            a_first_data.set = b_first_data.set;
        }

        a_last_data.next = b_first;
        ab_first_data.set = a_first;
    }

    ssa_ht _cset_next(ssa_ht h)
    {
        return h.data<ssa_out_data_t>().next;
    }

    // TODO
    constexpr std::uint32_t FLAG_PROCESSED = 1 << 0;

    enum block_type_t
    {
        BLOCK_PHIS,
        BLOCK_ENTRY_COPIES,
        BLOCK_OPS,
        BLOCK_EXIT_COPIES,
    };

    static block_type_t block_type(ssa_op_t op)
    {
        switch(op)
        {
        case SSA_phi:        return BLOCK_PHIS;
        case SSA_entry_copy: return BLOCK_ENTRY_COPIES;
        case SSA_exit_copy:  return BLOCK_EXIT_COPIES;
        default:             return BLOCK_OPS;
        }
    }

    // Checks if 'ssa_node' occurs after 'h' in the same BLOCK_OPS;
    // i.e. if one of its inputs is also in the same basic block 
    // and it depends on 'h' at some point.
    // TODO!!! Potentially remove
    static bool _op_after(ssa_node_t& ssa_node, ssa_ht h)
    {
        unsigned const input_size = ssa_node.input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t input_v = ssa_node.output(i);
            if(!input_v.holds_ref())
                continue;

            ssa_node_t& input_node = *input_v;

            if(input_node.cfg_node() != ssa_node.cfg_node())
                continue;

            if(block_type(input_node.op()) != BLOCK_OPS)
                continue;

            if(input_v.handle() == h)
                return true;

            if(_op_after(input_node, h))
                return true;
        }
        return false;
    }

    // Checks if any of 'a_h's outputs are in the same block as 'b_h',
    // but occur after 'b_h'.
    // (This is used for interference checks)
    static bool _outputs_after(ssa_ht a_h, ssa_ht b_h)
    {
        ssa_node_t& a_node = *a_h;
        ssa_node_t& b_node = *b_h;

        block_type_t const b_block = block_type(b_node.op());

        unsigned const output_size = a_node.output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_value_t output_v = a_node.output(i);
            if(!output_v.holds_ref())
                continue;
            ssa_node_t& output_node = *output_v;
            if(&output_node == &b_node)
                continue;
            if(output_node.cfg_node() != b.cfg_node())
                continue;

            block_type_t const input_block = block_type(input_node.op());

            if(input_block == b_block)
            {
                if(input_block == BLOCK_OPS)
                {
                    // TODO!!!
                    assert(false);
                    return _op_after(output_node, b_h);
                }
                else
                    continue; // Parallel copies never intersect.
            }
            else if(input_block < b_block)
                continue;
            else
                return true; // A later block will always intersect.
        }
        return false;
    }

    static bool _same_cfg_intersect(ssa_ht a_h, ssa_ht b_h)
    {
        assert(a_h->cfg_node() == b_h->cfg_node());

        cfg_h = a_h->cfg_node();
        cfg_data = cfg_h.data<cfg_out_data_t>();

        // If both are live-out, they intersect
        if(bitset_test(cfg_data.live_out, a_h.index)
           && bitset_test(cfg_data.live_out, b_h.index))
        {
            return true;
        }

        block_type_t a_block = block_type(a_h->op());
        block_type_t b_block = block_type(b_h->op());

        // If both are parallel copies, they don't intersect
        if(a_block == b_block && a_block != BLOCK_OPS)
            return false;

        if(b_block < a_block)
        {
            std::swap(a_h, b_h);
            std::swap(a_block, b_block);
        }


    }

    static bool _live_range_overlap(ssa_ht a_h, ssa_ht b_h)
    {
        assert(a_h != b_h);
        if(b_node->cfg_node()->dominates(a_node->cfg_node()))
            std::swap(a_h, b_h);

        ssa_node_t& a_node = *a_h;
        ssa_node_t& b_node = *b_h;

        auto& a_data = a_node.cfg_data().data<cfg_out_data_t>();
        auto& b_data = b_node.cfg_data()data<cfg_out_data_t>();

        if(a_node.cfg_node() == b_node.cfg_node())
        {
            // If both are live-out, they intersect.
            if(bitset_test(cfg_data.live_out, a_h.index)
               && bitset_test(cfg_data.live_out, b_h.index))
            {
                return true;
            }

            block_type_t a_block = block_type(a_h->op());
            block_type_t b_block = block_type(b_h->op());

            if(a_block == b_block)
            {

                if(a_block == BLOCK_OPS)
                {
                    // If both are non-parallel ops, check if either
                    // occurs after the other.
                    return (_outputs_after(a_h, b_h) 
                            || _outputs_after(b_h, a_h));
                }
                else // If both are parallel copies, they don't intersect
                    return false;
            }

            if(a_block < b_block)
                return _outputs_after(a_h, b_h);
            else
                return _outputs_after(a_h, b_h);

            {
                std::swap(a_h, b_h);
                std::swap(a_block, b_block);
            }


        }
        else
        {
            // If 'a' is in 'b's live-out set, there's interference.
            if(bitset_test(b_data.live_out, a_h.index))
                return true;

            if(_outputs_after(a_h, b_h))
                return true;

            return false;
        }
    }
}


static void _toposort_visit(schedule_t& schedule, ssa_ht node_h)
{
    ssa_node_t& node = *node_h;
    if(node.flags == MARK_PERMANENT)
        return;
    else if(node.flags == MARK_TEMPORARY)
        throw std::runtime_error("Circular SSA dependency.");
    node.flags = MARK_TEMPORARY;

    for(unsigned i = 0; i < node.input_size(); ++i)
    {
        ssa_value_t input = node.input(i);
        if(input.holds_ref() && input->cfg_node() == node.cfg_node())
            _toposort_visit(schedule, input.handle());
    }

    node.flags = MARK_PERMANENT;
    node_h.data<ssa_out_data_t>().schedule_i = schedule.size();
    schedule.push_back(node_h);
}


static schedule_t _schedule_cfg_node(cfg_ht cfg_h)
{
    schedule_t schedule;
    cfg_node_t& cfg_node = *cfg_h;
    assert(cfg_node.exit);

    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_node_t& ssa_node = *ssa_it;
        assert(ssa_node.op() != SSA_phi);
        ssa_node.flags = MARK_NONE;
    }
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_node_t& ssa_node = *ssa_it;
        if(ssa_it != cfg_node.exit && ssa_node.flags == MARK_NONE)
        {
            unsigned const output_size = ssa_node.output_size();
            for(unsigned i = 0; i < output_size; ++i)
            {
                ssa_value_t output_v = ssa_node.output(i);
                if(!output_v.holds_ref())
                    continue;
                ssa_node_t& output = *output_v;
                // Delay nodes as late as possible in the schedule:
                if(output.flags == MARK_NONE
                   && output.cfg_node() == ssa_node.cfg_node())
                    goto skip;
            }
            _toposort_visit(schedule, ssa_it);
        skip:;
        }
    }

    // The exit always comes last
    assert(cfg_node.exit);
    assert(cfg_node.exit->flags == MARK_NONE);
    _toposort_visit(schedule, cfg_node.exit);

    return schedule;
}

void make_conventional(ir_t& ir)
{
    cfg_data_pool::scope_guard_t<cfg_out_data_t> cg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_out_data_t> sg(ssa_pool::array_size());

    static rh::robin_map<int, ssa_ht> pin_map;
    pin_map.clear();

    // TODO
    static std::vector<coalesce_class_t*> coalesce_worklist;
    coalesce_worklist.clear();

    // Deal with conditional nodes that have both edges going to the same node
    // by splitting the edge and inserting a new node.
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        assert(cfg_node.output_size() <= 2);
        if(cfg_node.output_size() == 2 &&
           cfg_node.output(0) == cfg_node.output(1))
        {
            // Introduce a new node as the fix:
            ir.split_edge(cfg_node.output_edge(1));
        }
    }

    // TODO: resize data?

    // Insert copies around phi nodes to make the ir conventional.
    // - One copy per input in each predecessor block
    // - One copy of the phi node itself
    // 
    // Also insert copies around functions, to ensure their vars are pinned to
    // the correct memory locations.
    // - One copy per return byte
    // - One copy per argument byte
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();

        for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
        {
            ssa_ht last = phi_it;

            // Insert copies in predecessor nodes.
            unsigned const input_size = phi_it->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = phi_it->input(i);
                type_t input_type = (input_v.is_const() ? type_t{TYPE_BYTE} 
                                                        : input_v->type());

                assert(!input_v.is_const() 
                       || input_v.whole() == (input_v.whole() & 0xFF));

                cfg_ht pred_h = cfg_node.input(i);
                cfg_node_t& pred_node = *pred_h;

                ssa_ht input_copy_h = pred_node.emplace_ssa(
                    SSA_exit_copy, input_type, input_v);
                phi_it->link_change_input(i, input_copy_h);

                _cset_append(last, input_copy_h);
                last = input_copy_h;

                // Add 
                pred_h.data<cfg_out_data_t>()
                    .exit_copies.push_back(input_copy_h);
            }

            // Create a copy of the phi node.
            ssa_ht phi_copy_h = cfg_node.emplace_ssa(
                SSA_entry_copy, phi_it->type());
            phi_it->replace_with(phi_copy_h);
            phi_copy_h->link_append_input(phi_it);

            cfg_data.entry_copies.push_back(phi_copy_h);

            // Add both coalesce sets to the worklist.
            coalesce_worklist.push_back(phi_it);
            coalesce_worklist.push_back(phi_copy_h);
        }

        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            if(ssa_node.op() == SSA_get_return)
            {
                // Each fn call is pinned to return in specific memory regions.
                // Insert copies for each return point, freeing the pinning.
                // Then, these copies can maybe be coalesced.
                ssa_ht copy_h = cfg_node.emplace_ssa(SSA_copy, ssa_it->type());
                ssa_it->replace_with(copy_h);
                copy_h->link_append_input(ssa_it);

                ssa_data_pool::resize<ssa_out_data_t>(ssa_pool::array_size());
                auto& ssa_data = ssa_it.data<ssa_out_data_t>();
                ssa_node_t& ssa_node = *ssa_it;

                ssa_node_t& fn_node = *ssa_node.input(0);
                global_t& global = globals.global(fn_node);

                int const pin_out = 
                    global.fn->return_bytes[ssa_node.input(1).whole()];

                auto result = pin_map.insert({ pin_out, ssa_it });
                if(result.inserted)
                {
                    ssa_data.set_head.set(pin_out);
                    coalesce_worklist.push_back(ssa_it);
                }
                else
                {
                    _cset_append(*result.mapped, ssa_it);
                    *result.mapped = ssa_it;
                }
            }
            else if(ssa_it->op() == SSA_fn_call)
            {
                // Each fn call has pinned arguments too.
                // Insert copies for each.
                // Then, these copies can maybe be coalesced.
                global_t& global = globals.global(*ssa_it);

                unsigned const input_size = ssa_it->input_size();
                for(unsigned i = 0; i < input_size; ++i)
                {
                    ssa_ht copy_h = cfg_node.emplace_ssa(
                        SSA_copy, global.fn->arg_bytes_types[i],
                        ssa_it->input(i));
                    ssa_node.link_change_input(i, copy_h);

                    ssa_data_pool::resize<ssa_out_data_t>(
                        ssa_pool::array_size());
                    auto& copy_data = copy_h.data<copy_out_data_t>();

                    int const pin_out = 
                        global.fn->return_bytes[global.fn->arg_bytes[i]];

                    auto result = pin_map.insert({ pin_out, copy_h });
                    if(result.inserted)
                    {
                        copy_data.set_head.set(pin_out);
                        coalesce_worklist.push_back(copy_h);
                    }
                    else
                    {
                        _cset_append(*result.mapped, copy_h);
                        *result.mapped = copy_h;
                    }
                }
            }
        }
    }

    // Now schedule the nodes.
    // This is before coalescing, because it's convenient to have a total
    // order of nodes when coalescing to determine interferences.

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& data = cfg_it.data<cfg_cgen_data_t>();
        data.schedule = _schedule_cfg_node(cfg_node);
    }

    // Now calculate liveness sets.

    // TODO: init data?

    static array_pool<unsigned> _bitset_pool;
    _bitset_pool.clear();

    std::size_t const live_set_size = bitset_size(ssa_data_pool::array_size());

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();
        cfg_node.clear_flags(FLAG_IN_WORKLIST | FLAG_PROCESSED);

        cfg_data.live_in  = _bitset_pool.alloc(live_set_size);
        cfg_data.live_out = _bitset_pool.alloc(live_set_size);
        bitset_set_all(live_set_size, cfg_data.live_out);

        assert(bitset_all_reset(live_set_size, cfg_data.live_in));
        assert(bitset_all_set(live_set_size, cfg_data.live_out));
        assert(cfg_data.processed == false);

        // Set 'live_in's initial value to be the set of variables used in
        // this node, minus any variables that were also defined here.
        // (This set is sometimes called 'GEN')
        //
        // Also set 'live_out' to temporarily hold all variables *NOT* defined
        // in this node.
        // (This set is sometimes called 'KILL')
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t const& ssa_node = *ssa_it;

            // Remove all ssa definitions introduced in this cfg node.
            bitset_reset(cfg_data.live_out, ssa_it.index);

            unsigned const input_size = ssa_node.input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input_v = ssa_node.input(i);
                if(!input_v.holds_ref())
                    continue;
                ssa_node_t const& input = *input_v;
                // Only care about nodes defined in other basic blocks.
                if(input.cfg_node() == cfg_h)
                    continue;
                bitset_set(cfg_data.live_in, input_v.handle().index);
            }
        }
    }

    unsigned* temp_set = ALLOCA_T(unsigned, live_set_size);

    assert(ir.exit);
    cfg_worklist::clear();
    cfg_worklist::push(ir.exit);

    while(!cfg_worklist::empty())
    {
    reenter:
        cfg_ht cfg_h = cfg_worklist::pop();
        cfg_node_t& cfg_node = *cfg_h;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();

        // Calculate the real live-out set, storing it in 'temp_set'.
        // The live-out set is the union of the successor's live-in sets.
        bitset_reset_all(live_set_size, temp_set);
        unsigned const output_size = cfg_node.output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht succ_h = cfg_node.output(i);
            auto& succ_data = succ_h.data<cfg_out_data_t>();
            bitset_or(live_set_size, temp_set, succ_data.live_in);
        }

        bitset_and(live_set_size, temp_set, cfg_data.live_out /* (KILL) */);
        bitset_or(live_set_size, temp_set, cfg_data.live_in);

        // If 'live_in' is changing, add all predecessors to the worklist.
        if((cfg_node.flags & FLAG_PROCESSED) == 0
           || !bitset_eq(live_set_size, temp_set, cfg_data.live_in))
        {
            cfg_node.set_flags(FLAG_PROCESSED);
            unsigned const input_size = cfg_node.input_size();
            for(unsigned i = 0; i < input_size; ++i)
                cfg_worklist::push(cfg_node.input(i));
        }

        bitset_copy(live_set_size, cfg_data.live_in, temp_set);
    }

    // Some nodes (infinite loops, etc) might not be reachable travelling
    // backwards from the exit. Handle those now:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
        if(!cfg_it->test_flags(FLAG_PROCESSED)
           cfg_worklist::push(cfg_it);
           
    if(!cfg_worklist::empty())
        goto reenter;

    // Now properly set 'live_out'
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();

        bitset_reset_all(live_set_size, cfg_data.live_out);
        unsigned const output_size = cfg_node.output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht succ_h = cfg_node.output(i);
            auto& succ_data = succ_h.data<cfg_out_data_t>();
            bitset_or(live_set_size, cfg_data.live_out, succ_data.live_in);
        }
    }

    // OK! The liveness sets are now built.
    // Now to perform coalescing.

    // - Pick a candidate copy.
    // - Check if the two classes on each side of the copy interfere.
    // - If they don't coalesce them.

#ifndef NDEBUG
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        ssa_it->clear_flags(FLAG_PROCESSED);
#endif

    // Pick a candidate coalesce class:
    while(coalesce_worklist.size())
    {
        ssa_ht to_start = coalesce_worklist.back();
        ssa_ht to_cset = _cset_get_first(to_start);
        coalesce_worklist.pop_back();

        for(ssa_ht ssa_it = to_start; ssa_it; ssa_it = _cset_next(ssa_it))
        {
            ssa_node_t& ssa_node = *ssa_it;

            // Each SSA node should be checked at most once.
        #ifndef NDEBUG
            assert(!ssa_node.test_flags(FLAG_PROCESSED));
            ssa_node.set_flags(FLAG_PROCESSED);
        #endif

            if(!is_copy(ssa_node.op()))
                continue;

            ssa_value_t input_v = ssa_node.input(0);
            if(!input_v.holds_ref())
                continue;

            ssa_ht from_cset = _cset_get_first(input_v.handle());

            // If both belong to the same coalesce class,
            // they've already been coalesced together.
            if(from_cset == to_cset)
                continue;

            ssa_ht to_h;
            for(to_h = to_cset; to_h; to_h = _cset_next(to_h))
            for(ssa_ht from_h = from_cset; from_h; from_h = _cset_next(from_h))
                if(_interfere_TODO(from_h, to_h))
                    goto cant_coalesce;

            // Can coalesce! Do it now:
            _cset_union(to_cset, to_h, from_cset);
        cant_coalesce:;
        }
    }

    // Schedule parallel copies:


    // - TODO: Remove duplicate copies!

    std::vector<ssa_ht> new_schedule;
    // TODO: reserve

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        auto& cfg_data = cfg_it.data<cfg_out_data_t>();
        _schedule_parallel_copies(cfg_data.entry_copies, new_schedule);

        // TODO Copy old schedule here:

        _schedule_parallel_copies(cfg_data.exit_copies,  new_schedule);
    }


}

bool _is_copy_op(ssa_op_t op)
{
    return ssa_flags(op) & SSAF_COPY;
}

ssa_value_t _value(ssa_value_t v)
{
    if(!v.holds_ref())
        return v;

    ssa_node_t& node = *v;

    if(!_is_copy_op(node.op()))
        return v;

    assert(v.input_size() >= 1);
    return _value(v.input(0));
}


// Based on "Revisiting Out-of-SSA Translation for Correctness, Code
// Quality, and Efficiency", by Boissinot
// (But that paper is terribly written, so read with caution)
//
// Basically, to schedule parallel copies, understand that you can create
// a directed graph of the parallel copies, where a copy (a = b) corresponds
// to an edge from 'b' to 'a'.
// You can schedule starting from the leaf nodes of this graph and working 
// backwords, potentially inserting an extra copy to break cycles.
void _schedule_parallel_copies(std::vector_t<ssa_ht>& copies, 
                               std::vector<ssa_ht>& out)
{
    // The inputted list of copies may need to be pruned, 
    // removing unnecessary copies and such.
    // Do that here:
    {
        fc::small_map<ssa_ht, ssa_ht, 16> dupe_map;
        for(unsigned i = 0; i < copies.size();)
        {
            ssa_ht to_h = copies[i];
            ssa_node_t& to_node = *to_h;
            ssa_ht to_cset = _cset_get_first(to_h);

            assert(_is_copy_op(to_node.op()));

            ssa_value_t input_v = to_node.input(0);

            // If the copy is 'x = x', it can be removed.
            if(input_v.holds_ref())
            {
                ssa_ht from_h = input_v.handle();
                ssa_ht from_cset = _cset_get_first(from_h);

                if(to_cset != from_cset)
                {
                remove_copy:
                    to_node.replace_with(input_v);
                    to_node.link_clear_inputs();
                    to_node.unsafe_prune();

                    copies[i] = copies.back();
                    copies.pop_back();
                    continue;
                }
            }

            // If multiple copies have the same destination, only 1 is needed.
            // ('x = a' and 'x = a')
            auto result = dupe_map.emplace(to_cset, to_h);
            if(!result.second)
            {
                // Make sure the two copy targets are equal:
                assert(_value(result.first->first.input(0)) 
                       == _value(input_v));
                goto remove_copy;
            }

            ++i;
        }
    }

    // Initialize some state to NULL, 
    // and find all copies with constant targets, putting them in 'const_map'.
    {
        fc::small_multimap<std::uint8_t, ssa_ht, 16> const_map;
        for(ssa_ht to_h : copies)
        {
            to_h.data<ssa_out_data_t>().loc = nullptr;

            ssa_value_t input_v = to_h->input(0);
            if(input_v.is_const())
            {
                assert(input_v.whole() & 0xFF == input_v.whole());
                const_map.insert(input_v.whole(), to_h);
                continue;
            }

            assert(input_v.holds_ref());

            ssa_ht from_h = input_v.handle();
            from_h.data<ssa_out_data_t>().pred = nullptr;
        }
        
        // We can now schedule all the copies with constant targets:
        for(auto const& pair : const_map)
            out.push_back(pair.second);
    }

    bc::small_vector<ssa_ht, 16> ready;
    bc::small_vector<ssa_ht, 16> to_do;

    // Initialize more state
    for(ssa_ht to_h : copies)
    {
        ssa_value_t input_v = to_h->input(0);
        if(!input_v.holds_ref())
            continue;

        ssa_ht from_h = input_v.handle();

        from_h.data<ssa_out_data_t>().loc = from_h;
        to_h.data<ssa_out_data_t>().pred = from_h;
        to_do.push_back(from_h);
    }

    // Leaf nodes will be processed first, so add them to the worklist
    // (leaf nodes are nodes that have no successor in 'copies')
    for(ssa_ht to_h : copies)
        if(!to_h.data<ssa_out_data_t>().loc)
            ready.push_back(to_h);

    // Now process and sequentialize each node:
    while(!to_do.empty())
    {
        while(!ready.empty())
        {
            ssa_ht to_h = ready.back();
            ready.pop_back();

            ssa_ht pred_h = to_h.data<ssa_out_data_t>().pred;
            auto& pred_data = pred_h.data<ssa_out_data_t>();
            ssa_ht from_h = pred_h_data.loc;

            from_h->link_change_input(0, to_h);
            out.push_back(from_h);

            pred_data.loc = to_h;

            if(pred_h == from_h && pred_data.pred)
                ready.push_back(pred_h);
        }

        ssa_ht from_h = to_do.back();
        to_do.pop_back();
        
        // The original paper incorrectly had '==' instead of '!=' here.
        if(from_h != from_h.data<ssa_out_data_t>().pred
                           .data<ssa_out_data_t>().loc)
        {
            ssa_ht copy_h = cfg_node.emplace_ssa(
                SSA_copy, from_h->type(), from_h);
            out.push_back(copy_h);

            from_h.data<ssa_out_data_t>().loc = copy_h;

            ready.push_back(from_h);
        }
    }
}





enum
{
    REQ_A,
    REQ_X,
    REQ_Y,
    REQ_X_OR_Y,
    REQ_A_OR_AX;

    OUT_A,
    OUT_X,
    OUT_Y,
    OUT_R,
};

OUT_A,
OUT_A,


REG_A,
REG_X,
REG_Y,
REG_CARRY,
REG_RAM,

inc:
in: RAM
out: RAM

adc:
in: A, RAM
out: A

using regs_t = std::uint8_t;
constexpr regs_t REG_A = 1 << 0;
constexpr regs_t REG_X = 1 << 1;
constexpr regs_t REG_Y = 1 << 2;
constexpr regs_t REG_C = 1 << 3;

struct
{
    asm_op_t asm_op;
    bool flip_args;

    regs_t in_regs() const;
    regs_t out_regs() const;
};


struct asm_op_def_t
{
    char const* name;
    unsigned opcode;
    addr_mode_t addr_mode;
    unsigned size;
    unsigned cycles;
    regs_t implicit_regs;
    regs_t arg_regs;
    regs_t out_regs;
};


// PRIORITIZE:
// 1) Carry ops before carry consumers
// 2) A ops before A consumers


// - Try to identify which ops need X/Y


lda foo1
adc bar1
lda foo2
adc bar2


// As

{
    switch(ssa_node.op())
    {
    case SSA_add:
    case SSA_sub:
        if(ssa_node.input(2).is_const()
           && (ssa_node.input(0).is_const()
               || ssa_node.input(1).is_const()))
        {
            .todo |= REQ_A_OR_AX;
        }
        else
            .todo |= REQ_A;
        break;

    case SSA_and:
    case SSA_or:
    case SSA_xor:
        if(ssa_node.input(0).holds_ref())
            ssa_node.input(0).todo |= REQ_A;
        if(ssa_node.input(1).holds_ref())
            ssa_node.input(1).todo |= REQ_A;

        .todo |= REQ_A;
        break;

    }
}

/*
void alloc_vars(ir_t& ir)
{
}



instr_t make_instr(asm_op_name_t op_name, ssa_value_t value)
{
    if(value.is_const())
    {
        assert((value.whole() & 0xFF) == value.whole());
        return { op_name, MODE_IMMEDIATE, value.whole() };
    }

    std::uint16_t const addr = TODO;
    return { op_name, is_zp(addr) ? MODE_ZERO_PAGE : MODE_ABSOLUTE, addr };
}

instr_t make_store(asm_op_name_t op_name, std::uint16_t addr)
{
    return { OP_NAME, addr < 256 ? MODE_IMMEDIATE : MODE_ABSOLUTE, addr };
}

void load_A(ssa_value_t value)
{
    instr_t instr = ssa_addr(value);
    instr.op_name = LDA;

    block.code.push_back({ LDA, mode });

    if(value.is_const())
        block.code.push_back({ LDA_IMMEDIATE, value.whole() & 0xFF });
    else
    {
        block.code.push_back({ LDA_IMMEDIATE, value.whole() & 0xFF });
    }
}

class code_gen_t
{

    void maybe_lda(ssa_ht ssa_h)
    void delay_sta(ssa_ht ssa_h)

    void maybe_clc();
    void maybe_sec();

};

void maybe_lda(ssa_ht ssa_h)
{
    append_asm(make_instr(LDA, ssa_h));
}

void maybe_clc()
{
    append_asm({ CLC });
}

void maybe_sec()
{
    append_asm({ SEC });
}

void delay_sta(TODO)
{
    append_asm(make_store(STA, todo));
}

void code_gen()
{

    rh::robin_map<ssa_ht, std::uint16_t> vars;


    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        schedule_t schedule = schedule_cfg(cfg_it);
        for(ssa_ht ssa_h : schedule.order)
        {
            ssa_node_t& ssa_node = *ssa_h;
            switch(ssa_node.op())
            {
            case SSA_add:
                maybe_lda(ssa_node.input(0));
                maybe_clc();
                append_asm(make_instr(ADC, ssa_node.input(1)));
                append_asm(make_store(STA, todo));
                break;
            }
        }

    }


    switch(TODO)
    {
    case SSA_add:
        block.code.push_back({ CLC });
        load_A(TODO);
        block.code.push_back({ ADC_IMMEDIATE, });
        store_A(TODO);
    }
}
*/
