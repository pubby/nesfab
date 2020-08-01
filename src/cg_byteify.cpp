#include "cg_byteify.hpp"

#include <array>

namespace bc = ::boost::container;

namespace // anonymous
{
    // bm = bytemap
    // This is used to break up arithmetic types into several bytes.
    using bm_t = std::array<ssa_value_t, type_t::max_total_bytes>;
    static_assert(type_t::max_total_bytes == 6); // To match 'zero_bm' below.
    constexpr bm_t zero_bm = { 0u, 0u, 0u, 0u, 0u, 0u };

    struct ssa_byteify_d
    {
        bm_t bm;
    };

    bm_t _get_bm(ssa_value_t value)
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
            return value.handle().data<ssa_byteify_d>().bm;
        }
    }
}

// Used in 'byteify' to remove casts.
static void _split_cast(ssa_ht ssa_h)
{
    ssa_node_t& ssa_node = *ssa_h;
    assert(ssa_node.op() == SSA_cast);

    // Check if the input is cast that wasn't split yet:
    ssa_value_t input = ssa_node.input(0);
    if(input.holds_ref() && input->op() == SSA_cast 
       && !input->test_flags(FLAG_PROCESSED))
    {
        _split_cast(input.handle());
    }

    type_t const type = ssa_node.type();
    auto& data = ssa_h.data<ssa_byteify_d>();
    data.bm = zero_bm;
    bm_t input_bm = get_bm(input);
    unsigned const end = end_byte(type.name);
    for(unsigned i = begin_byte(type.name); i < end; ++i)
        data.bm[i] = input_bm[i];

    ssa_node.set_flags(FLAG_PROCESSED);
}

// Converts all operations with non-BYTE types to only use BYTE.
void byteify(ir_t& ir, global_manager_t& globals, global_t& global)
{
    assert(global.type.name == TYPE_FN);

    ssa_data_pool::scope_guard_t<ssa_byteify_data_t> sg(
        ssa_pool::array_size());

    ssa_workvec.clear();
    thread_local std::vector<ssa_ht> prune_nodes;
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
            ssa_node.clear_flags(FLAG_PROCESSED); // For '_split_cast'.
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

            SSA_VERSION(1);
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

            // We created nodes, so we have to resize:
            ssa_data_pool::resize<ssa_byteify_d>(ssa_pool::array_size());

            auto& ssa_data = ssa_it.data<ssa_byteify_data_t>();
            ssa_data.bm = std::move(bm);

            // workvec will hold nodes that have been split:
            ssa_workvec.push_back(ssa_it);
        }
    }

    // Split cast operations now.
    // 'prune_nodes' should hold all the casts at the moment and nothing else.
    for(ssa_ht ssa_h : prune_nodes)
        _split_cast(ssa_h);

    // Rewrite the inputs of certain nodes to use multi-byte
    bc::small_vector<ssa_value_t, 24> new_input;
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_node_t& ssa_node = *ssa_it;

            SSA_VERSION(1);
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
                    }

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

    // Calculate this function's argument offsets to be used later.
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

        SSA_VERSION(1);
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
            assert(false);
            break;
        }
    }

    // Prune nodes that are now unnecessary:
    for(ssa_ht ssa_h : prune_nodes)
    {
        ssa_node_t& ssa_node = *ssa_h;
        ssa_node.link_clear_inputs();
        ssa_node.unsafe_prune();
    }
}


