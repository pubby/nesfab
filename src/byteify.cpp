#include "byteify.hpp"

#include <array>

#include <boost/container/small_vector.hpp>

#include "globals.hpp"
#include "ir.hpp"
#include "worklist.hpp"

SSA_VERSION(1);

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
}

static type_t _bm_type(type_t t)
{
    if(t.name() == TYPE_ARRAY)
        return t.elem_type();
    return t;
}

static bm_t _get_bm(ssa_value_t value)
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
        assert(is_arithmetic(_bm_type(value->type())));
        return value.handle().data<ssa_byteify_d>().bm;
    }
}

// Used in 'byteify' to remove casts.
static void _split_cast(ssa_ht ssa_node)
{
    assert(ssa_node->op() == SSA_cast);

    // Check if the input is a cast that wasn't split yet:
    ssa_value_t input = ssa_node->input(0);
    if(input.holds_ref() && input->op() == SSA_cast 
       && !input->test_flags(FLAG_PROCESSED))
    {
        _split_cast(input.handle());
    }

    type_t const type = ssa_node->type();
    auto& data = ssa_node.data<ssa_byteify_d>();
    data.bm = zero_bm;
    bm_t input_bm = _get_bm(input);
    unsigned const end = end_byte(type.name());
    for(unsigned i = begin_byte(type.name()); i < end; ++i)
        data.bm[i] = input_bm[i];

    ssa_node->set_flags(FLAG_PROCESSED);
}

// Converts all operations with non-BYTE types to only use BYTE.
void byteify(ir_t& ir, fn_t const& fn)
{
    ssa_data_pool::scope_guard_t<ssa_byteify_d> sg(
        ssa_pool::array_size());

    ssa_workvec.clear();
    bc::small_vector<ssa_ht, 32> prune_nodes;

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

            type_t const type = _bm_type(ssa_node.type());

            if(ssa_node.op() == SSA_cast)
            {
                // Casts will just forward their input(s) to their output(s),
                // and will be removed entirely.
                // The forwarding will happen after all other nodes that
                // need splitting have been split.
                assert(is_arithmetic(type));
                prune_nodes.push_back(ssa_it);
                continue;
            }

            if(type == TYPE_BYTE)
            {
                auto& ssa_data = ssa_it.data<ssa_byteify_d>(); 
                ssa_data.bm = zero_bm;
                ssa_data.bm[type_t::max_frac_bytes] = ssa_it;
#ifndef NDEBUG
                for(ssa_value_t v : ssa_data.bm)
                    assert(v);
#endif
                continue;
            }

            if(!is_arithmetic(type))
                continue;

            SSA_VERSION(1);
            ssa_op_t split_op;
            switch(ssa_node.op())
            {
            default:
                split_op = ssa_node.op();
                break;
            case SSA_fn_call:
                split_op = SSA_read_global;
                break;
            }

            bm_t bm = zero_bm;
            unsigned const end = end_byte(type.name());
            for(unsigned i = begin_byte(type.name()); i < end; ++i)
                bm[i] = cfg_node.emplace_ssa(split_op, type_t{TYPE_BYTE});
            // !!IMPORTANT: 'ssa_node' is invalidated after this!!

            // We created nodes, so we have to resize:
            ssa_data_pool::resize<ssa_byteify_d>(ssa_pool::array_size());

            auto& ssa_data = ssa_it.data<ssa_byteify_d>();
            ssa_data.bm = std::move(bm);

#ifndef NDEBUG
            for(ssa_value_t v : ssa_data.bm)
                assert(v);
#endif

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
    fn_t const* called_fn = nullptr;
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            SSA_VERSION(1);

            switch(ssa_it->op())
            {
            case SSA_fn_call:
                // Because SSA constants lack types, it's necessary
                // to look up the function's global definition first:
                called_fn = &*get_fn(*ssa_it);
                // fall-through
            case SSA_return:
                {
                    new_input.clear();

                    for_each_written_global(ssa_it, 
                    [&](ssa_value_t v, locator_t loc)
                    {
                        type_t t;

                        if(loc.lclass() == LOC_CALL_ARG)
                        {
                            assert(ssa_it->op() == SSA_fn_call);
                            t = called_fn->type.types()[loc.arg()];
                        }
                        else if(loc.lclass() == LOC_RETURN)
                        {
                            assert(ssa_it->op() == SSA_return);
                            t = fn.type.return_type();
                        }
                        else if(loc.lclass() == LOC_THIS_ARG)
                            t = fn.type.types()[loc.arg()];
                        else if(loc.lclass() == LOC_GVAR)
                            t = loc.gvar()->type;
                        else
                        {
                            assert(loc.lclass() == LOC_GVAR_SET);
                            new_input.push_back(v);
                            new_input.push_back(loc);
                            return;
                        }

                        t = _bm_type(t);

                        bm_t bm = _get_bm(v);

                        unsigned const start = begin_byte(t.name());
                        unsigned const end = end_byte(t.name());
                        for(unsigned j = start; j < end; ++j)
                        {
                            locator_t new_loc = loc;
                            new_loc.set_field(j - start);

                            new_input.push_back(bm[j]);
                            new_input.push_back(std::move(new_loc));
                        }
                    });

                    assert(new_input.size() % 2 == 0);

                    ssa_it->link_shrink_inputs(write_globals_begin(ssa_it->op()));
                    ssa_it->link_append_input(&*new_input.begin(), &*new_input.end());
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
                    assert(ssa_it->input_size() == 2);

                    std::array<bm_t, 2> bms;
                    unsigned begin = ~0;
                    unsigned end = 0;
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        ssa_value_t input = ssa_it->input(i);
                        type_name_t t = input.type().name();
                        assert(is_arithmetic(t));
                        bms[i] = _get_bm(input);
                        begin = std::min(begin, begin_byte(t));
                        end = std::max(end, end_byte(t));
                    }

                    for(unsigned j = begin; j < end; ++j)
                        for(unsigned i = 0; i < 2; ++i)
                            new_input.push_back(bms[i][j]);

                    ssa_it->link_clear_inputs();
                    ssa_it->link_append_input(&*new_input.begin(),
                                              &*new_input.end());

                    // Should always have an even number of args.
                    assert(ssa_it->input_size() % 2 == 0);
                }
                break;

            default:
                break;
            }

        }
    }

    // Finish up the split outputs created earlier:
    bc::small_vector<bm_t, 16> bms;
    for(ssa_ht ssa_node : ssa_workvec)
    {
        auto& ssa_data = ssa_node.data<ssa_byteify_d>(); 
        type_name_t const t = _bm_type(ssa_node->type()).name();
        assert(is_arithmetic(t));

        SSA_VERSION(1);
        switch(ssa_node->op())
        {
        // These replace the original node with N parallel ops,
        // each with the same amount of arguments as the original.
        case SSA_phi:
        case SSA_cast:
        case SSA_and:
        case SSA_or:
        case SSA_xor:
            {
                unsigned const input_size = ssa_node->input_size();

                bms.resize(input_size);
                for(unsigned i = 0; i < input_size; ++i)
                    bms[i] = _get_bm(ssa_node->input(i));

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_ht split = ssa_data.bm[i].handle();
                    split->alloc_input(input_size);
                    for(unsigned j = 0; j < input_size; ++j)
                        split->build_set_input(j, bms[j][i]);
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        // These ops are similar to the above, but need a carry:
        case SSA_add:
        case SSA_sub:
            {
                bm_t const lhs_bm = _get_bm(ssa_node->input(0));
                bm_t const rhs_bm = _get_bm(ssa_node->input(1));

                ssa_value_t carry = ssa_node->input(2);

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_ht split = ssa_data.bm[i].handle();

                    split->alloc_input(3);
                    split->build_set_input(0, lhs_bm[i]);
                    split->build_set_input(1, rhs_bm[i]);
                    split->build_set_input(2, carry);

                    carry = ssa_node->cfg_node()->emplace_ssa(
                        SSA_carry, TYPE_CARRY, split);
                    ssa_data_pool::resize<ssa_byteify_d>(
                        ssa_pool::array_size());
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        case SSA_read_array:
            {
                bm_t const array_bm = _get_bm(ssa_node->input(0));

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_ht split = ssa_data.bm[i].handle();
                    split->alloc_input(2);
                    split->build_set_input(0, array_bm[i]);
                    split->build_set_input(1, ssa_node->input(1));
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        case SSA_write_array:
            {
                bm_t const array_bm = _get_bm(ssa_node->input(0));
                bm_t const assign_bm = _get_bm(ssa_node->input(2));

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_ht split = ssa_data.bm[i].handle();
                    split->alloc_input(3);
                    split->build_set_input(0, array_bm[i]);
                    split->build_set_input(1, ssa_node->input(1));
                    split->build_set_input(2, assign_bm[i]);
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        // Keeps the original node.
        // All the split nodes created will reference it as a link.
        case SSA_fn_call:
            {
                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_ht split = ssa_data.bm[i].handle();
                    assert(split->op() == SSA_read_global);

                    split->alloc_input(2);
                    split->build_set_input(0, ssa_node);
                    split->build_set_input(1, locator_t::ret(fn.handle(), i));
                }
            }
            break;

        // Replace original node with N new nodes:
        case SSA_read_global:
            {
                assert(ssa_node->input_size() == 2);
                ssa_ht link = ssa_node->input(0).handle();
                locator_t loc = ssa_node->input(1).locator();

                unsigned const start = begin_byte(t);
                unsigned const end = end_byte(t);
                for(unsigned i = start; i < end; ++i)
                {
                    ssa_value_t split = ssa_data.bm[i];
                    assert(split.holds_ref());

                    loc.set_field(i - start);

                    split->alloc_input(2);
                    split->build_set_input(0, link);
                    split->build_set_input(1, loc);
                }

                prune_nodes.push_back(ssa_node);
            }
            break;

        case SSA_uninitialized:
            // TODO: split the node up?
            break;

        default:
            // Shouldn't ever happen if this was coded correctly...
            std::fprintf(stderr, "Unhandled op in byteify: %s\n", to_string(ssa_node->op()).data());
            assert(false);
            break;
        }
    }

    // Prune nodes that are now unnecessary:
    for(ssa_ht h : prune_nodes)
    {
        if(h->type() == TYPE_BYTE)
            h->replace_with(
                h.data<ssa_byteify_d>().bm[type_t::max_frac_bytes]);
        h->prune();
    }
}


