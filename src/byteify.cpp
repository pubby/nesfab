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
    using bm_t = std::array<ssa_value_t, max_total_bytes>;
    static_assert(max_total_bytes == 7); // To match 'zero_bm' below.
    constexpr ssa_value_t sv = ssa_value_t(0u, TYPE_U);
    constexpr bm_t zero_bm = { sv, sv, sv, sv, sv, sv, sv };

    struct ssa_byteify_d
    {
        bm_t bm;
    };
}

static type_t _bm_type(type_t t)
{
    if(t.name() == TYPE_TEA)
        return t.elem_type();
    return t;
}

static bm_t _get_bm(ssa_value_t value)
{
    assert(value);

    if(value.is_num())
    {
        bm_t bm;
        fixed_uint_t f = value.fixed().value;
        for(unsigned i = 0; i < bm.size(); ++i)
        {
            bm[i] = ssa_value_t(f & 0xFF, TYPE_U);
            f >>= 8;
        }
        return bm;
    }
    else if(value.is_locator())
    {
        bm_t bm = zero_bm;

        locator_t const loc = value.locator();

        if(loc.byteified())
        {
            bm[max_frac_bytes] = loc;
            return bm;
        }

        type_t const t = loc.type();
        assert(is_scalar(t.name()));

        unsigned const start = begin_byte(t.name());
        unsigned const end = end_byte(t.name());

        for(unsigned j = start; j < end; ++j)
        {
            locator_t new_loc = loc;
            new_loc.set_atom(j - start);
            new_loc.set_byteified(true);
            bm[j] = new_loc;
        }

        return bm;
    }
    else
    {
        assert(value.holds_ref());
        assert(is_scalar(_bm_type(value->type()).name()));
        return value.handle().data<ssa_byteify_d>().bm;
    }
}

// Used in 'byteify' to remove casts.
static void _split_cast(ssa_ht ssa_node)
{
    assert(ssa_node->op() == SSA_cast);

    // Check if the input is a cast that wasn't split yet:
    ssa_value_t input = ssa_node->input(0);
    if(input.holds_ref() && input->op() == SSA_cast && !input->test_flags(FLAG_PROCESSED))
        _split_cast(input.handle());

    type_t const type = ssa_node->type();
    auto& data = ssa_node.data<ssa_byteify_d>();

    type_t const input_type = input.type();

    data.bm = zero_bm;
    bm_t input_bm = _get_bm(input);
    unsigned const end = end_byte(type.name());
    for(unsigned i = begin_byte(type.name()); i < end; ++i)
        data.bm[i] = input_bm[i];

    if(is_signed(input_type.name()))// && whole_bytes(input_type.name()) < whole_bytes(type.name()))
    {
        // We have to sign extend!
        unsigned i = end_byte(input_type.name());
        assert(i > 0);
        ssa_value_t const extension = ssa_node->cfg_node()->emplace_ssa(SSA_sign_extend, TYPE_U, data.bm[i - 1]);
        for(; i < end; ++i)
            data.bm[i] = extension;
    }

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
                assert(is_scalar(type.name()));
                prune_nodes.push_back(ssa_it);
                continue;
            }

            if(ssa_flags(ssa_node.op()) & SSAF_INDEXES_PTR)
            {
                // Pointer accesses may create an 'SSA_make_ptr' node.
                if(ssa_it->input(0).holds_ref())
                {
                    ssa_ht const h = cfg_node.emplace_ssa(
                        SSA_make_ptr, ssa_it->input(0)->type(), ssa_it->input(0));
                    ssa_it->link_change_input(0, h);
                }
                continue;
            }

            if(ssa_node.op() == SSA_make_ptr)
                continue;

            if(type == TYPE_U || type == TYPE_S || type == TYPE_BOOL)
            {
                auto& d = ssa_it.data<ssa_byteify_d>(); 
                d.bm = zero_bm;
                d.bm[max_frac_bytes] = ssa_it;
#ifndef NDEBUG
                for(ssa_value_t v : d.bm)
                    assert(v);
#endif
                continue;
            }

            if(!is_scalar(type.name()))
                continue;
            
            SSA_VERSION(1);

            type_t split_type = TYPE_U;
            if(ssa_node.type().name() == TYPE_TEA)
                split_type = type_t::tea(TYPE_U, ssa_node.type().size());

            bm_t bm = zero_bm;
            unsigned const end = end_byte(type.name());
            for(unsigned i = begin_byte(type.name()); i < end; ++i)
                bm[i] = cfg_node.emplace_ssa(ssa_node.op(), split_type);
            // !!IMPORTANT: 'ssa_node' is invalidated after this!!

            // We created nodes, so we have to resize:
            ssa_data_pool::resize<ssa_byteify_d>(ssa_pool::array_size());

            auto& d = ssa_it.data<ssa_byteify_d>();
            d.bm = std::move(bm);

#ifndef NDEBUG
            for(ssa_value_t v : d.bm)
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
            case SSA_goto_mode:
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

                        if(loc.lclass() == LOC_ARG)
                            t = loc.fn()->type().types()[loc.arg()];
                        else if(loc.lclass() == LOC_RETURN)
                        {
                            assert(ssa_it->op() == SSA_return);
                            t = fn.type().return_type();
                        }
                        else if(loc.lclass() == LOC_GMEMBER)
                            t = loc.gmember()->type();
                        else
                        {
                            assert(loc.lclass() == LOC_GMEMBER_SET);
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
                            new_loc.set_atom(j - start);
                            new_loc.set_byteified(true);

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
                {
                    // Equality comparisons convert to N parallel comparisons,
                    // where N is the size in bytes of the largest argument.

                    // The format is interleaved (ababab instead of aaabbb)
                    // to make it easier to remove arguments in optimization.

                    // The last comparison assumes one argument is signed, the other isn't.

                    new_input.clear();
                    assert(ssa_it->input_size() == 2);

                    ssa_value_t const l = ssa_it->input(0);
                    ssa_value_t const r = ssa_it->input(1);

                    type_name_t const lt = l.type().name();
                    type_name_t const rt = r.type().name();

                    bm_t sbm = _get_bm(is_signed(lt) ? l : r);
                    bm_t ubm = _get_bm(is_signed(lt) ? r : l);

                    int const begin = begin_byte(lt);
                    int const end = end_byte(lt);

                    assert((unsigned)begin == begin_byte(rt));
                    assert((unsigned)end == end_byte(rt));
                    assert(begin != end);

                    // If the signs differ, 
                    if(is_signed(lt) != is_signed(rt))
                    {
                        if(sbm[end-1].holds_ref() && sbm[end-1]->op() == SSA_sign_extend)
                        {
                            // If the most significant signed comparison(s) are sign-extended,
                            // we can eliminate the sign-extension's use inside the comparison.

                            ssa_value_t const extend = sbm[end-1]->input(0);

                            for(int i = end-2; i >= begin; --i)
                            {
                                if(sbm[i].holds_ref() && sbm[i]->op() == SSA_sign_extend)
                                {
                                    if(sbm[i]->input(0) != extend)
                                        break;
                                }
                                else
                                {
                                    if(sbm[i] != extend)
                                        break;

                                    // We can simplify!

                                    // Comparisons above the sign can be simplified:
                                    for(int j = i+1; j < end; ++j) 
                                        sbm[j] = ssa_value_t(0u, TYPE_U);

                                    // Make 'i' the most significant:
                                    std::swap(sbm[i], sbm[end-1]);
                                    std::swap(ubm[i], ubm[end-1]);

                                    break;
                                }
                            }
                        }
                    }

                    for(int i = begin; i < end; ++i)
                    {
                        new_input.push_back(ubm[i]);
                        new_input.push_back(sbm[i]);
                    }

                    // Last two arguments compare signs.
                    // If we don't have to do that, insert trivially true dummy args:
                    if(is_signed(lt) == is_signed(rt))
                    {
                        new_input.push_back(ssa_value_t(0u, TYPE_U));
                        new_input.push_back(ssa_value_t(0u, TYPE_U));
                    }

                    ssa_it->link_clear_inputs();
                    ssa_it->link_append_input(&*new_input.begin(), &*new_input.end());

                    ssa_it->unsafe_set_op(ssa_it->op() == SSA_eq ? SSA_multi_eq : SSA_multi_not_eq);

                    // Should always have an even number of args.
                    assert(ssa_it->input_size() % 2 == 0);
                    assert(ssa_it->input_size() > 0);
                }
                break;

            case SSA_lt:
            case SSA_lte:
                {
                    // Comparisons convert to N parallel comparisons,
                    // where N is the size in bytes of the largest argument.

                    // The format is interleaved (ababab instead of aaabbb)
                    // to make it easier to remove arguments in optimization.

                    new_input.clear();
                    assert(ssa_it->input_size() == 2);

                    ssa_value_t const l = ssa_it->input(0);
                    ssa_value_t const r = ssa_it->input(1);

                    type_name_t const lt = l.type().name();
                    type_name_t const rt = r.type().name();

                    bool const both_signed = is_signed(lt) && is_signed(rt);
                    unsigned const lwhole = whole_bytes(lt);
                    unsigned const rwhole = whole_bytes(rt);

                    // First two args are the types:
                    if(both_signed && lwhole != rwhole)
                    {
                        // When both are signed, the types must have an equal number of whole bytes.
                        // (This simplifies code gen)
                        unsigned const w = std::max(lwhole, rwhole);
                        new_input.push_back(ssa_value_t(type_s(w, frac_bytes(lt)), TYPE_INT));
                        new_input.push_back(ssa_value_t(type_s(w, frac_bytes(rt)), TYPE_INT));
                    }
                    else
                    {
                        new_input.push_back(ssa_value_t((unsigned)lt, TYPE_INT));
                        new_input.push_back(ssa_value_t((unsigned)rt, TYPE_INT));
                    }

                    bm_t lbm = _get_bm(l);
                    int lbegin = begin_byte(lt);
                    int lend = end_byte(lt);
                    for(int i = lbegin; i < lend; ++i)
                        new_input.push_back(lbm[i]);

                    // If both are signed, sign-extend.
                    if(both_signed && lwhole < rwhole)
                    {
                        ssa_value_t const extension = ssa_it->cfg_node()->emplace_ssa(SSA_sign_extend, TYPE_U, new_input.back());
                        for(unsigned i = 0; i < rwhole - lwhole; ++i)
                            new_input.push_back(extension);
                    }

                    bm_t rbm = _get_bm(r);
                    int rbegin = begin_byte(rt);
                    int rend = end_byte(rt);
                    for(int i = rbegin; i < rend; ++i)
                        new_input.push_back(rbm[i]);

                    // If both are signed, sign-extend.
                    if(both_signed && rwhole < lwhole)
                    {
                        ssa_value_t const extension = ssa_it->cfg_node()->emplace_ssa(SSA_sign_extend, TYPE_U, new_input.back());
                        for(unsigned i = 0; i < lwhole - rwhole; ++i)
                            new_input.push_back(extension);
                    }

                    ssa_it->link_clear_inputs();
                    ssa_it->link_append_input(&*new_input.begin(), &*new_input.end());

                    ssa_it->unsafe_set_op(ssa_it->op() == SSA_lt ? SSA_multi_lt : SSA_multi_lte);

                    assert(ssa_it->input_size() >= 4); // 2 type args, plus at least one input per argument
                }
                break;

            case SSA_make_ptr:
                {
                    // Convert 'SSA_make_ptr' nodes created earlier,
                    // expanding their single argument into 2.

                    assert(ssa_it->input_size() == 1);
                    ssa_value_t const input = ssa_it->input(0);
                    assert(is_ptr(input.type().name()));

                    bm_t bm = _get_bm(input);
                    unsigned const begin = begin_byte(input.type().name());

                    ssa_it->link_change_input(0, bm[begin]);
                    ssa_it->link_append_input(bm[begin+1]);
                }
                break;

            default:
                assert(!fn_like(ssa_it->op()));
                break;
            }
        }
    }

    // Finish up the split outputs created earlier:
    bc::small_vector<bm_t, 16> bms;
    for(ssa_ht ssa_node : ssa_workvec)
    {
        auto& d = ssa_node.data<ssa_byteify_d>(); 
        type_name_t const t = _bm_type(ssa_node->type()).name();
        assert(is_scalar(t));

        if(ssa_node->type() == TYPE_S)
            ssa_node->set_type(TYPE_U);

        SSA_VERSION(1);
        switch(ssa_node->op())
        {
        // Shifts convert to multiple rotations (rol/ror)
        case SSA_shl:
        case SSA_shr:
            if(ssa_node->input(1).whole())
            {
                int const shifts = ssa_node->input(1).whole();
                int const byte_shifts = shifts / 8;
                int const bit_shifts = shifts % 8;

                bm_t values = _get_bm(ssa_node->input(0));

                int const begin = begin_byte(t);
                int const end = end_byte(t);

                if(ssa_node->op() == SSA_shl)
                {
                    for(int i = end - 1; i >= begin; --i)
                    {
                        if(i - byte_shifts >= 0)
                            values[i] = values[i - byte_shifts];
                        else
                            values[i] = ssa_value_t(0u, TYPE_U);;
                    }

                    for(int s = 0; s < bit_shifts; ++s)
                    {
                        ssa_value_t prev_carry(0u, TYPE_BOOL);
                        for(int i = begin + byte_shifts; i < end; ++i)
                        {
                            values[i] = ssa_node->cfg_node()->emplace_ssa(
                                SSA_rol, TYPE_U, values[i], prev_carry);
                            prev_carry = ssa_node->cfg_node()->emplace_ssa(
                                SSA_carry, TYPE_BOOL, values[i]);
                        }
                    }
                }
                else
                {
                    assert(ssa_node->op() == SSA_shr);

                    for(int i = begin; i < end; ++i)
                    {
                        if(i + byte_shifts < end)
                            values[i] = values[i + byte_shifts];
                        else
                            values[i] = ssa_value_t(0u, TYPE_U);;
                    }

                    for(int s = 0; s < bit_shifts; ++s)
                    {
                        ssa_value_t prev_carry(0u, TYPE_BOOL);
                        if(is_signed(t))
                           prev_carry = ssa_node->cfg_node()->emplace_ssa(
                               SSA_sign_to_carry, TYPE_BOOL, values[end - 1]);

                        for(int i = end - 1 - byte_shifts; i >= int(begin); --i)
                        {
                            values[i] = ssa_node->cfg_node()->emplace_ssa(
                                SSA_ror, TYPE_U, values[i], prev_carry);
                            prev_carry = ssa_node->cfg_node()->emplace_ssa(
                                SSA_carry, TYPE_BOOL, values[i]);
                        }
                    }
                }

                for(int i = begin; i < end; ++i)
                {
                    ssa_ht const split = d.bm[i].handle();
                    split->alloc_input(1);
                    split->build_set_input(0, values[i]);
                    split->unsafe_set_op(SSA_cast);
                }

                prune_nodes.push_back(ssa_node);
            }
            else
                assert(false); // TODO
            break;

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
                    ssa_ht split = d.bm[i].handle();
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

                assert(ssa_node->input_size() == 3);
                ssa_value_t carry = ssa_node->input(2);

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_ht split = d.bm[i].handle();

                    split->alloc_input(3);
                    split->build_set_input(0, lhs_bm[i]);
                    split->build_set_input(1, rhs_bm[i]);
                    split->build_set_input(2, carry);

                    carry = ssa_node->cfg_node()->emplace_ssa(
                        SSA_carry, TYPE_BOOL, split);
                    ssa_data_pool::resize<ssa_byteify_d>(
                        ssa_pool::array_size());
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        case SSA_read_array:
            {
                bm_t const array_bm = _get_bm(ssa_node->input(0));

                unsigned const start = begin_byte(t);
                unsigned const end = end_byte(t);
                for(unsigned i = start; i < end; ++i)
                {
                    ssa_ht split = d.bm[i].handle();

                    locator_t loc = ssa_node->input(1).locator();
                    //loc.set_atom(i - start);

                    assert(ssa_argn(SSA_read_array) == 3);
                    split->alloc_input(3);
                    split->build_set_input(0, array_bm[i]);
                    split->build_set_input(1, loc);
                    split->build_set_input(2, ssa_node->input(2));
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        case SSA_write_array:
            {
                bm_t const array_bm = _get_bm(ssa_node->input(0));
                bm_t const assign_bm = _get_bm(ssa_node->input(3));

                unsigned const start = begin_byte(t);
                unsigned const end = end_byte(t);
                for(unsigned i = start; i < end; ++i)
                {
                    ssa_ht split = d.bm[i].handle();

                    locator_t loc = ssa_node->input(1).locator();
                    //loc.set_atom(i - start);

                    assert(ssa_argn(SSA_write_array) == 4);
                    split->alloc_input(4);
                    split->build_set_input(0, array_bm[i]);
                    split->build_set_input(1, loc);
                    split->build_set_input(2, ssa_node->input(2));
                    split->build_set_input(3, assign_bm[i]);
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        // Replace original node with N new nodes:
        case SSA_read_global:
            {
                assert(ssa_node->input_size() == 2);
                ssa_ht link = ssa_node->input(0).handle();
                locator_t loc = ssa_node->input(1).locator();
                loc.set_byteified(true);

                unsigned const start = begin_byte(t);
                unsigned const end = end_byte(t);
                for(unsigned i = start; i < end; ++i)
                {
                    ssa_value_t split = d.bm[i];
                    assert(split.holds_ref());

                    loc.set_atom(i - start);

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
        if(h->type() == TYPE_U)
            h->replace_with(h.data<ssa_byteify_d>().bm[max_frac_bytes]);
        h->prune();
    }
}


