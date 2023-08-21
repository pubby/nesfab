#include "byteify.hpp"

#include <array>

#include <boost/container/small_vector.hpp>

#include "globals.hpp"
#include "ir.hpp"
#include "ir_util.hpp"
#include "worklist.hpp"
#include "format.hpp"

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

        type_name_t num_type = value.num_type_name();
        //if(!is_byteified(num_type)) TODO
        if(num_type != TYPE_BOOL)
            num_type = TYPE_U;

        for(unsigned i = 0; i < bm.size(); ++i)
        {
            bm[i] = ssa_value_t(f & 0xFF, num_type);
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

        switch(loc.is())
        {
        case IS_DEREF:
            {
                type_t const t = loc.type();
                passert(is_scalar(t.name()), t, loc);

                unsigned const start = begin_byte(t.name());
                unsigned const end = end_byte(t.name());

                for(unsigned j = start; j < end; ++j)
                {
                    locator_t new_loc = loc;
                    if(has_arg_member_atom(new_loc.lclass()))
                        new_loc.set_atom(j - start);
                    else if(is_ptr(t.name()))
                        new_loc.set_is(locator_is_t(j - start + IS_PTR));
                    else
                        assert(false);
                    new_loc.set_byteified(true);
                    bm[j] = new_loc;
                }
            }
            break;

        case IS_PTR:
            bm[max_frac_bytes+1] = loc.with_is(IS_PTR_HI).with_byteified(true);
            // fall-through
        case IS_PTR_HI:
        case IS_BANK:
            bm[max_frac_bytes] = loc.with_byteified(true);
        }

        return bm;
    }
    else
    {
        assert(value.holds_ref());
        passert(is_scalar(_bm_type(value->type()).name()), value->type(), _bm_type(value->type().name()));
        return value.handle().data<ssa_byteify_d>().bm;
    }
}

// If the op gets entirely removed during byteify.
bool _vanishes(ssa_ht h)
{
    ssa_op_t const op = h->op();

    switch(op)
    {
    case SSA_cast:
        return true;
        //return !(is_byteified(h->type().name()) && is_byteified(h->input(0).type().name()));

    case SSA_get_byte:
    case SSA_array_get_byte:
    case SSA_replace_byte:
    case SSA_array_replace_byte:
        return true;

    default:
        return false;
    }
}

// Used in 'byteify' to remove casts, etc.
static void _split_vanishing(ssa_ht ssa_node)
{
    assert(ssa_node);
    assert(_vanishes(ssa_node));

    auto const split_input = [](ssa_value_t input)
    {
        if(input.holds_ref() && !input->test_flags(FLAG_PROCESSED) && _vanishes(input.handle()))
            _split_vanishing(input.handle());
    };

    passert(ssa_node->input_size() >= 1, ssa_node->op());
    ssa_value_t input = ssa_node->input(0);
    split_input(input);

    type_t const type = ssa_node->type();
    type_t const input_type = input.type();

    auto& data = ssa_node.data<ssa_byteify_d>();
    data.bm = zero_bm;
    bm_t const input_bm = _get_bm(input);

    if(ssa_node->op() == SSA_cast)
    {
        bool from_sign = is_signed(input_type.name());
        bool const to_sign = is_signed(type.name());

        unsigned const end = end_byte(type.name());
        for(unsigned i = begin_byte(type.name()); i < end; ++i)
        {
            if(true || to_sign)
            {
                data.bm[i] = ssa_node->cfg_node()->emplace_ssa(SSA_cast, TYPE_U, input_bm[i]);
                data.bm[i]->set_flags(FLAG_PROCESSED);
            }
            else
                data.bm[i] = input_bm[i];
        }

        if(from_sign)
        {
            // We have to sign extend!
            unsigned i = end_byte(input_type.name());
            assert(i > 0);
            assert(data.bm[i - 1]);
            ssa_value_t const extension = ssa_node->cfg_node()->emplace_ssa(SSA_sign_extend, TYPE_U, data.bm[i - 1]);
            for(; i < end; ++i)
                data.bm[i] = extension;

            extension->set_flags(FLAG_PROCESSED);
        }

        // Created a node, so we have to resize:
        ssa_data_pool::resize<ssa_byteify_d>(ssa_pool::array_size());
    }
    else if(ssa_node->op() == SSA_get_byte)
    {
        unsigned const atom = ssa_node->input(1).whole();
        data.bm = zero_bm;
        data.bm[max_frac_bytes] = input_bm[atom + begin_byte(input_type.name())];
    }
    else if(ssa_node->op() == SSA_array_get_byte)
    {
        unsigned const atom = ssa_node->input(1).whole();
        data.bm = zero_bm;
        data.bm[max_frac_bytes] = input_bm[atom + begin_byte(input_type.elem_type().name())];
    }
    else if(ssa_node->op() == SSA_replace_byte)
    {
        ssa_value_t with = ssa_node->input(2);
        assert(with.type().name() == TYPE_U);
        split_input(with);

        bm_t const with_bm = _get_bm(with);

        unsigned const atom = ssa_node->input(1).whole();
        data.bm = input_bm;
        data.bm[atom + begin_byte(type.name())] = with_bm[max_frac_bytes];
    }
    else if(ssa_node->op() == SSA_array_replace_byte)
    {
        ssa_value_t with = ssa_node->input(2);
        assert(with->type().name() == TYPE_TEA);
        assert(with->type().elem_type().name() == TYPE_U);
        split_input(with);

        bm_t const with_bm = _get_bm(with);

        unsigned const atom = ssa_node->input(1).whole();
        data.bm = input_bm;
        data.bm[atom + begin_byte(type.name())] = with_bm[max_frac_bytes];
    }
    else
        assert(false);

    ssa_node->set_flags(FLAG_PROCESSED);
}

// Converts all operations with non-BYTE types to only use BYTE.
void byteify(ir_t& ir, fn_t const& fn)
{
    // First prepare the IR with some transformations:
    insert_signed_mul_subtractions(ir);
    shifts_to_rotates(ir, false);
    // OK! IR prepared.

    ssa_data_pool::scope_guard_t<ssa_byteify_d> sg(ssa_pool::array_size());

    ssa_workvec.clear();
    bc::small_vector<ssa_ht, 32> prune_nodes;
    bc::small_vector<ssa_ht, 32> original_s;

    // Split nodes that have multi-byte results into N nodes, where N
    // is the number of bytes its result takees.

    // Create the nodes here, leaving the original node to keep track:
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;
        for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        {
            ssa_it->clear_flags(FLAG_PROCESSED); // For '_split_cast'.

            type_t const type = _bm_type(ssa_it->type());

            if(_vanishes(ssa_it))
            {
                // Casts will just forward their input(s) to their output(s),
                // and will be removed entirely.
                // The forwarding will happen after all other nodes that
                // need splitting have been split.
                //
                // Likewise, 'get_byte' and 'replace_byte' will be removed 
                // entirely too.

                assert(ssa_it->op() != SSA_cast || is_scalar(type.name()));
                prune_nodes.push_back(ssa_it);
                continue;
            }

            if(ssa_flags(ssa_it->op()) & SSAF_INDEXES_PTR)
            {
                using namespace ssai::rw_ptr;

                // Pointer accesses may create 'SSA_make_ptr' nodes.
                if(ssa_it->input(PTR).holds_ref())
                {
                    assert(!ssa_it->input(PTR_HI).holds_ref());

                    ssa_ht const lo = cfg_node.emplace_ssa(
                        SSA_make_ptr_lo, TYPE_U, ssa_it->input(PTR));
                    ssa_ht const hi = cfg_node.emplace_ssa(
                        SSA_make_ptr_hi, TYPE_U, ssa_it->input(PTR));

                    ssa_it->link_change_input(PTR, lo);
                    ssa_it->link_change_input(PTR_HI, hi);

                    // We created nodes, so we have to resize:
                    ssa_data_pool::resize<ssa_byteify_d>(ssa_pool::array_size());
                }
                else if(ssa_it->input(PTR).is_num())
                {
                    assert(is_ptr(ssa_it->input(PTR).type().name()));
                    ssa_it->link_change_input(PTR, locator_t::addr(ssa_it->input(PTR).whole()));
                }
            }

            if(ssa_it->op() == SSA_read_array16 || ssa_it->op() == SSA_write_array16)
            {
                // We'll convert these to the '_b' version of their ops,
                // which lack support for the 'OFFSET' input.
                // Thus, we'll extract the offset here and turn it into an add.

                using namespace ssai::array;

                if(!ssa_it->input(OFFSET).eq_whole(0))
                {
                    ssa_ht const add = cfg_node.emplace_ssa(
                        SSA_add, TYPE_U20, 
                        ssa_it->input(OFFSET), ssa_it->input(INDEX), ssa_value_t(0u, TYPE_BOOL));

                    ssa_it->link_change_input(INDEX, add);
                    ssa_it->link_change_input(OFFSET, ssa_value_t(0u, TYPE_U20));

                    // We created nodes, so we have to resize:
                    ssa_data_pool::resize<ssa_byteify_d>(ssa_pool::array_size());
                }
            }

            if(is_byteified(type.name()))
            {
                auto& d = ssa_it.data<ssa_byteify_d>(); 
                if(type == TYPE_S)
                    original_s.push_back(ssa_it);
                d.bm = zero_bm;
                d.bm[max_frac_bytes] = ssa_it;
                continue;
            } 
            /* TODO
            else if(type == TYPE_S)
            {
                auto& d = ssa_it.data<ssa_byteify_d>(); 
                d.bm = zero_bm;
                d.bm[max_frac_bytes] = cfg_node.emplace_ssa(SSA_cast, TYPE_U, ssa_it);

                // We created nodes, so we have to resize:
                ssa_data_pool::resize<ssa_byteify_d>(ssa_pool::array_size());

                continue;
            }
            */

            if(!is_scalar(type.name()))
                continue;

            type_t split_type = TYPE_U;
            if(ssa_it->type().name() == TYPE_TEA)
                split_type = type_t::tea(TYPE_U, ssa_it->type().size());

            bm_t bm = zero_bm;
            unsigned const end = end_byte(type.name());
            for(unsigned i = begin_byte(type.name()); i < end; ++i)
                bm[i] = cfg_node.emplace_ssa(ssa_it->op(), split_type);

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

    assert(ssa_data_pool::array_size() >= ssa_pool::array_size());

    // Split cast operations now.
    // 'prune_nodes' should hold all the casts at the moment and nothing else.
    for(ssa_ht ssa_h : prune_nodes)
        _split_vanishing(ssa_h);

    // Rewrite the inputs of certain nodes to use multi-byte
    bc::small_vector<ssa_value_t, 24> new_input;
    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        switch(ssa_it->op())
        {
        case SSA_fn_call:
            {
                ssa_value_t bank = ssa_it->input(1);

                if(bank.holds_ref())
                {
                    bm_t const bm = _get_bm(bank);
                    passert(bm[max_frac_bytes], ssa_it, bank);
                    ssa_it->link_change_input(1, bm[max_frac_bytes]);
                }
            }
            // fall-through
        case SSA_goto_mode:
        case SSA_return:
        case SSA_fence:
        case SSA_wait_nmi:
        case SSA_cli:
            {
                new_input.clear();

                for_each_written_global(ssa_it, 
                [&](ssa_value_t v, locator_t loc)
                {
                    type_t t;

                    if(loc.lclass() == LOC_ARG)
                        t = member_type(loc.fn()->type().types()[loc.arg()], loc.member());
                    else if(loc.lclass() == LOC_RETURN)
                    {
                        assert(ssa_it->op() == SSA_return);
                        t = member_type(fn.type().return_type(), loc.member());
                    }
                    else if(loc.lclass() == LOC_GMEMBER)
                        t = loc.gmember()->type();
                    else
                    {
                        assert(loc.lclass() == LOC_GMEMBER_SET);
                    keep:
                        new_input.push_back(v);
                        new_input.push_back(loc);
                        return;
                    }

                    t = _bm_type(t);

                    if(t.name() == TYPE_PAA)
                        goto keep;

                    bm_t bm = _get_bm(v);

                    unsigned const start = begin_byte(t.name());
                    unsigned const end = end_byte(t.name());
                    assert(end - start > 0);
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
                passert(ssa_it->input_size() == 2, ssa_it, ssa_it->input_size());

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
                    new_input.push_back(ssa_value_t(0u, TYPE_S));
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

        case SSA_make_ptr_lo:
        case SSA_make_ptr_hi:
            {
                // Convert 'SSA_make_ptr' nodes created earlier,
                // expanding their single argument into 2.

                assert(ssa_it->input_size() == 1);
                ssa_value_t const input = ssa_it->input(0);
                assert(is_ptr(input.type().name()));

                bm_t bm = _get_bm(input);
                unsigned const begin = begin_byte(input.type().name());

                // The relevant input is held in [1],
                // while a dummy input representing the other half 
                // of the pointer is held in [0].
                if(ssa_it->op() == SSA_make_ptr_lo)
                {
                    ssa_it->link_change_input(0, bm[begin+1]);
                    ssa_it->link_append_input(bm[begin]);
                }
                else
                {
                    ssa_it->link_change_input(0, bm[begin]);
                    ssa_it->link_append_input(bm[begin+1]);
                }
            }
            break;

        case SSA_read_array16:
        case SSA_write_array16:
            {
                using namespace ssai::array;
                
                bool const is_read = ssa_it->op() == SSA_read_array16;

                // Offset should have been zero'd earlier:
                assert(ssa_it->input(OFFSET).eq_whole(0));

                bm_t bm = _get_bm(ssa_it->input(INDEX));
                ssa_it->link_change_input(INDEX,    bm[max_frac_bytes]);
                ssa_it->link_change_input(INDEX_HI, bm[max_frac_bytes+1]);
                
                ssa_it->unsafe_set_op(is_read ? SSA_read_array16_b : SSA_write_array16_b);
            }
            break;

        case SSA_sign:
            {
                assert(ssa_it->input_size() == 1);
                ssa_value_t const input = ssa_it->input(0);
                bm_t bm = _get_bm(input);

                unsigned const end = end_byte(input.type().name());

                ssa_it->link_change_input(0, bm[end-1]);
            }
            break;

        default:
            assert(!(ssa_flags(ssa_it->op()) & SSAF_WRITE_GLOBALS));
            assert(!fn_like(ssa_it->op()));

            if(ssa_it->test_flags(FLAG_PROCESSED))
                break;

            assert(!_vanishes(ssa_it));

            type_t const type = ssa_it->type();
            if(type.name() == TYPE_VOID || is_byteified(type.name()))
            {
                unsigned const input_size = ssa_it->input_size();
                for(unsigned i = 0; i < input_size; ++i)
                {
                    ssa_value_t const input = ssa_it->input(i);
                    //passert(is_byteified(input.type().name()), ssa_it, ssa_it->op(), i, input.type());

                    if(input.holds_ref() && input.type() != TYPE_VOID)
                    {
                        bm_t const bm = _get_bm(input);
                        //passert(bm[max_frac_bytes], ssa_it, input, ssa_it->op(), input.type());
                        if(bm[max_frac_bytes])
                            ssa_it->link_change_input(i, bm[max_frac_bytes]);
                    }
                }
            }

            break;
        }
    }

    // Finish up the split outputs created earlier:
    bc::small_vector<bm_t, 16> bms;
    for(ssa_ht ssa_node : ssa_workvec)
    {
        auto& d = ssa_node.data<ssa_byteify_d>(); 
        type_name_t const t = _bm_type(ssa_node->type()).name();
        assert(is_scalar(t));

        switch(ssa_node->op())
        {
        case SSA_rol:
        case SSA_ror:
            {
                bm_t const lhs_bm = _get_bm(ssa_node->input(0));
                bm_t const rhs_bm = _get_bm(ssa_node->input(1));
                ssa_ht prev_carry = {};

                int begin, end, incr;

                if(ssa_node->op() == SSA_rol)
                {
                    begin = begin_byte(t);
                    end = end_byte(t);
                    incr = 1;
                }
                else // ror
                {
                    begin = end_byte(t) - 1;
                    end = begin_byte(t) - 1;
                    incr = -1;
                }

                ssa_value_t carry = rhs_bm[max_frac_bytes];

                for(int i = begin; i != end; i += incr)
                {
                    ssa_ht split = d.bm[i].handle();
                    split->alloc_input(2);
                    split->build_set_input(0, lhs_bm[i]);
                    split->build_set_input(1, carry);

                    if(ssa_ht c = carry_output(*split))
                        prev_carry = c;

                    carry = ssa_node->cfg_node()->emplace_ssa(
                        SSA_carry, TYPE_BOOL, split);

                }
                prune_nodes.push_back(ssa_node);

                if(prev_carry)
                {
                    prev_carry->replace_with(carry);
                    prune_nodes.push_back(prev_carry);
                }
            }
            break;

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

                ssa_value_t carry(0u, TYPE_BOOL);

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
                        carry = ssa_value_t(0u, TYPE_BOOL);

                        for(int i = begin + byte_shifts; i < end; ++i)
                        {
                            values[i] = ssa_node->cfg_node()->emplace_ssa(
                                SSA_rol, TYPE_U, values[i], carry);
                            carry = ssa_node->cfg_node()->emplace_ssa(
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
                        else if(is_signed(ssa_node->type().name()))
                            values[i] = ssa_node->cfg_node()->emplace_ssa(
                                SSA_sign_extend, TYPE_U, values[end - 1]);
                        else
                            values[i] = ssa_value_t(0u, TYPE_U);
                    }

                    for(int s = 0; s < bit_shifts; ++s)
                    {
                        if(is_signed(t))
                           carry = ssa_node->cfg_node()->emplace_ssa(
                               SSA_sign, TYPE_BOOL, values[end - 1]);
                        else
                            carry = ssa_value_t(0u, TYPE_BOOL);

                        for(int i = end - 1 - byte_shifts; i >= int(begin); --i)
                        {
                            values[i] = ssa_node->cfg_node()->emplace_ssa(
                                SSA_ror, TYPE_U, values[i], carry);
                            carry = ssa_node->cfg_node()->emplace_ssa(
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

                    if(ssa_ht prev_carry = carry_output(*split))
                    {
                        prev_carry->replace_with(carry);
                        prune_nodes.push_back(prev_carry);
                    }
                }

                prune_nodes.push_back(ssa_node);
            }
            else
            {
                // This should have been handled by 'shifts_to_rotates'.
                throw std::runtime_error("Trying to byteify a shift of a non-constant number of bits.");
            }
            break;

        // These replace the original node with N parallel ops,
        // each with the same amount of arguments as the original.
        case SSA_phi:
        case SSA_cast:
        case SSA_and:
        case SSA_or:
        case SSA_xor:
        case SSA_init_array:
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
                ssa_ht prev_carry = {};

                unsigned const end = end_byte(t);
                for(unsigned i = begin_byte(t); i < end; ++i)
                {
                    ssa_ht split = d.bm[i].handle();

                    split->alloc_input(3);
                    split->build_set_input(0, lhs_bm[i]);
                    split->build_set_input(1, rhs_bm[i]);
                    split->build_set_input(2, carry);

                    if(ssa_ht c = carry_output(*split))
                        prev_carry = c;

                    carry = ssa_node->cfg_node()->emplace_ssa(
                        SSA_carry, TYPE_BOOL, split);
                }
                prune_nodes.push_back(ssa_node);

                if(prev_carry)
                {
                    prev_carry->replace_with(carry);
                    prune_nodes.push_back(prev_carry);
                }
            }
            break;

        // Multiplying 
        case SSA_mul:
            {
                ssa_value_t const lhs = ssa_node->input(0);
                ssa_value_t const rhs = ssa_node->input(1);

                bm_t const lhs_bm = _get_bm(lhs);
                bm_t const rhs_bm = _get_bm(rhs);

                type_t const lhs_type = lhs.type();
                type_t const rhs_type = rhs.type();

                cfg_ht const cfg = ssa_node->cfg_node();

                int const lhs_begin = begin_byte(lhs_type.name());
                int const lhs_end = end_byte(lhs_type.name());

                int const rhs_begin = begin_byte(rhs_type.name());
                int const rhs_end = end_byte(rhs_type.name());

                int const result_begin = begin_byte(t);
                int const result_end = end_byte(t);
                int const sum_begin = result_begin * 2 - max_frac_bytes;

                std::array<bc::small_vector<ssa_value_t, max_total_bytes>, max_total_bytes + max_frac_bytes> to_sum;

                for(int li = lhs_begin; li < lhs_end; ++li)
                for(int ri = rhs_begin; ri < rhs_end; ++ri)
                {
                    int const lo_i = li + ri;
                    if(lo_i - int(max_frac_bytes) >= result_end)
                        continue;

                    ssa_ht const lo = cfg->emplace_ssa(SSA_mul8_lo, TYPE_U, lhs_bm[li], rhs_bm[ri]);
                    to_sum[lo_i].push_back(lo);

                    int const hi_i = lo_i + 1;
                    if(hi_i - int(max_frac_bytes) >= result_end)
                        continue;

                    ssa_ht const hi = cfg->emplace_ssa(SSA_mul8_hi, TYPE_U, lo);
                    to_sum[hi_i].push_back(hi);
                }

                // Now sum together all the multiplications:

                for(int i = sum_begin; i < result_end; ++i)
                {
                    unsigned const a = i + max_frac_bytes;

                    if(to_sum[a].empty())
                        to_sum[a].push_back(ssa_value_t(0u, TYPE_U));
                    else while(to_sum[a].size() >= 2)
                    {
                        ssa_value_t prev_carry = ssa_value_t(0u, TYPE_BOOL);

                        for(int j = sum_begin; j < result_end; ++j)
                        {
                            unsigned const b = j + max_frac_bytes;

                            ssa_value_t lhs = ssa_value_t(0u, TYPE_U);
                            ssa_value_t rhs = ssa_value_t(0u, TYPE_U);

                            if(!to_sum[b].empty())
                            {
                                lhs = to_sum[b].back();
                                to_sum[b].pop_back();
                            }

                            if(!to_sum[b].empty())
                            {
                                rhs = to_sum[b].back();
                                to_sum[b].pop_back();
                            }

                            ssa_ht const add = cfg->emplace_ssa(SSA_add, TYPE_U, lhs, rhs, prev_carry);
                            ssa_ht const carry = cfg->emplace_ssa(SSA_carry, TYPE_BOOL, add);

                            to_sum[b].push_back(add);
                            prev_carry = carry;
                        }
                    }

                    assert(to_sum[a].size() == 1);
                }

                // Now generate the final bm:

                for(int i = result_begin; i < result_end; ++i)
                {
                    ssa_ht split = d.bm[i].handle();

                    unsigned const a = i + max_frac_bytes;
                    assert(to_sum[a].size() == 1);

                    split->alloc_input(1);
                    split->build_set_input(0, to_sum[a].back());
                    split->unsafe_set_op(SSA_cast);
                }

                prune_nodes.push_back(ssa_node);
            }
            break;

        case SSA_read_array8:
            {
                using namespace ssai::array;

                bm_t const array_bm = _get_bm(ssa_node->input(ARRAY));

                unsigned const start = begin_byte(t);
                unsigned const end = end_byte(t);
                for(unsigned i = start; i < end; ++i)
                {
                    ssa_ht split = d.bm[i].handle();

                    assert(ssa_argn(SSA_read_array8) == 3);
                    split->alloc_input(3);
                    split->build_set_input(ARRAY, array_bm[i]);
                    split->build_set_input(OFFSET, ssa_node->input(OFFSET));
                    split->build_set_input(INDEX, ssa_node->input(INDEX));
                }
                prune_nodes.push_back(ssa_node);
            }
            break;

        case SSA_write_array8:
            {
                using namespace ssai::array;

                bm_t const array_bm = _get_bm(ssa_node->input(ARRAY));
                bm_t const assign_bm = _get_bm(ssa_node->input(ASSIGNMENT));

                unsigned const start = begin_byte(t);
                unsigned const end = end_byte(t);
                for(unsigned i = start; i < end; ++i)
                {
                    ssa_ht split = d.bm[i].handle();

                    passert(ssa_argn(SSA_write_array8) == 4, ssa_argn(SSA_write_array8));
                    assert(assign_bm[i]);
                    split->alloc_input(4);
                    split->build_set_input(ARRAY, array_bm[i]);
                    split->build_set_input(OFFSET, ssa_node->input(OFFSET));
                    split->build_set_input(INDEX, ssa_node->input(INDEX));
                    split->build_set_input(ASSIGNMENT, assign_bm[i]);
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
                    ssa_ht split = d.bm[i].handle();

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
            throw std::runtime_error(fmt("Unhandled op in byteify: %", ssa_node->op()));
        }
    }

    // Retype
    for(ssa_ht h : original_s)
        h->set_type(TYPE_U);

    // Prune nodes that are now unnecessary:
    for(ssa_ht h : prune_nodes)
    {
        if(is_byteified(h->type().name()))
            h->replace_with(h.data<ssa_byteify_d>().bm[max_frac_bytes]);
        h->prune();
    }
}

// Converts signed multiplies to unsigned,
// which is a necessary prerequisite to byteify.
bool insert_signed_mul_subtractions(ir_t& ir)
{
    bool updated = false;

    bc::small_vector<ssa_ht, 8> muls;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        if(ssa_it->op() == SSA_mul)
            muls.push_back(ssa_it);

    for(ssa_ht const ssa_it : muls)
    {
        assert(ssa_it->op() == SSA_mul);

        cfg_ht const cfg_it = ssa_it->cfg_node();

        ssa_value_t const lhs = ssa_it->input(0);
        ssa_value_t const rhs = ssa_it->input(1);
        
        type_t const lhs_type = lhs.type();
        type_t const rhs_type = rhs.type();

        bool const lhs_signed = is_signed(lhs_type.name());
        bool const rhs_signed = is_signed(rhs_type.name());

        if(!lhs_signed && !rhs_signed)
            continue;

        type_t const type = ssa_it->type();

        unsigned const lhs_whole = whole_bytes(lhs_type.name());
        unsigned const rhs_whole = whole_bytes(rhs_type.name());

        unsigned const lhs_begin = begin_byte(lhs_type.name());
        unsigned const rhs_begin = begin_byte(rhs_type.name());
        unsigned const result_end = end_byte(type.name());

        // Check if negative 'lhs' needs to be handled:
        bool const handle_lhs = lhs_signed && (rhs_begin + lhs_whole < result_end);

        // Check if negative 'rhs' needs to be handled:
        bool const handle_rhs = rhs_signed && (lhs_begin + rhs_whole < result_end);

        if(!handle_lhs && !handle_rhs)
            continue;

        cfg_ht prev_cfg = cfg_it;
        cfg_ht const post_cfg = ir.emplace_cfg();

        ssa_ht prev = ssa_it;
        ssa_ht const result = prev_cfg->emplace_ssa(SSA_cast, type);

        ssa_it->replace_with(INPUT_VALUE, result);

        post_cfg->steal_outputs(*cfg_it);
        steal_ssa_after(ssa_it, post_cfg);
        assert(result->cfg_node() == post_cfg);

        auto const handler = [&](unsigned shift_amount, ssa_value_t self, ssa_value_t other)
        {
            cfg_ht const neg_cfg = ir.emplace_cfg();
            ssa_ht const cast = neg_cfg->emplace_ssa(SSA_cast, type, other);
            ssa_ht const shift = neg_cfg->emplace_ssa(SSA_shl, type, cast, ssa_value_t(shift_amount * 8u, TYPE_U));
            ssa_ht const sub = neg_cfg->emplace_ssa(SSA_sub, type, prev, shift, ssa_value_t(1u, TYPE_BOOL));

            cfg_ht const join_cfg = ir.emplace_cfg();
            ssa_ht const join_phi = join_cfg->emplace_ssa(SSA_phi, type);
            neg_cfg->link_append_output(join_cfg, [&](ssa_ht phi){ assert(phi == join_phi); return sub; });

            assert(prev_cfg->output_size() == 0);
            ssa_ht const condition = prev_cfg->emplace_ssa(SSA_sign, TYPE_BOOL, self);
            ssa_ht const branch = prev_cfg->emplace_ssa(SSA_if, TYPE_VOID, condition);
            branch->append_daisy();
            prev_cfg->link_append_output(join_cfg, [&](ssa_ht phi){ assert(phi == join_phi); return prev; });
            prev_cfg->link_append_output(neg_cfg, [](ssa_ht phi){ assert(false); return ssa_value_t{}; });
            assert(prev_cfg->output_size() == 2);

            prev = join_phi;
            prev_cfg = join_cfg;
        };

        if(handle_lhs)
            handler(lhs_whole, lhs, rhs);
        if(handle_rhs)
            handler(rhs_whole, rhs, lhs);

        result->link_append_input(prev);
        prev_cfg->link_append_output(post_cfg, [](ssa_ht phi){ assert(false); return ssa_value_t{}; });

        updated = true;
        ir.assert_valid();
    }

    return updated;
}

// Expands shifts into rotates.
// Returns true if the IR was modified.
bool shifts_to_rotates(ir_t& ir, bool handle_constant_shifts)
{
    auto& to_prune = ssa_workvec;
    to_prune.clear();

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        bool const shl = ssa_it->op() == SSA_shl;
        bool const shr = ssa_it->op() == SSA_shr;
        if(!shl && !shr)
            continue;

        type_t const type = ssa_it->type();
        type_t const unsigned_type = to_u(type.name());

        assert(ssa_it->input(1).type() == TYPE_U);
        
        ssa_ht const carry_output = ::carry_output(*ssa_it);

        if(ssa_it->input(1).is_num())
        {
            if(!handle_constant_shifts)
                continue;

            unsigned const num_shifts = ssa_it->input(1).whole();

            // Shifting more bits than representable resolves to 0 or -1.
            if(num_shifts > type.size_of_bits())
            {
                ssa_value_t replacement = ssa_value_t(0u, type.name());
                if(shr && is_signed(type.name()))
                   replacement = cfg_it->emplace_ssa(SSA_sign_extend, type, ssa_it->input(0));

                if(carry_output)
                {
                    if(shr && is_signed(type.name()))
                    {
                        ssa_ht const new_carry = cfg_it->emplace_ssa(SSA_sign, TYPE_BOOL, ssa_it->input(0));
                        carry_output->replace_with(new_carry);
                    }
                    else
                        carry_output->replace_with(ssa_value_t(0u, TYPE_BOOL));

                    to_prune.push_back(carry_output);
                }

                ssa_it->replace_with(replacement);
                to_prune.push_back(ssa_it);
                continue;
            }

            // Otherwise replace with N rotates.

            ssa_value_t value = ssa_it->input(0);

            for(unsigned i = 0; i < num_shifts; ++i)
            {
                ssa_value_t carry = ssa_value_t(0u, TYPE_BOOL);
                if(is_signed(type.name()))
                   carry = cfg_it->emplace_ssa(SSA_sign, TYPE_BOOL, value);

                value = cfg_it->emplace_ssa(shl ? SSA_rol : SSA_ror, unsigned_type, value, carry);
            }

            if(carry_output)
            {
                if(num_shifts == 0)
                    carry_output->replace_with(ssa_value_t(0u, TYPE_BOOL));
                else
                {
                    ssa_ht const new_carry = cfg_it->emplace_ssa(SSA_carry, TYPE_BOOL, value);
                    carry_output->replace_with(new_carry);
                }

                to_prune.push_back(carry_output);
            }

            ssa_it->replace_with(value);
            to_prune.push_back(ssa_it);

            continue;
        }

        // Split the cfg node
        cfg_ht const loop_head = ir.emplace_cfg();
        cfg_ht const loop_body = ir.emplace_cfg();
        cfg_ht const post_node = ir.emplace_cfg();
        cfg_ht const pre_node = cfg_it;

        // Transfer nodes occurring after 'ssa_it' to 'post_node':
        steal_ssa_after(ssa_it, post_node);

        // Setup 'loop_head':
        loop_head->alloc_output(2);
        loop_head->build_set_output(0, post_node);
        loop_head->build_set_output(1, loop_body);

        // Setup 'loop_body':
        loop_body->alloc_output(1);
        unsigned const loop_to_head = loop_body->build_set_output(0, loop_head);
        assert(loop_head->input(loop_to_head) == loop_body);

        // Setup 'post_node':
        post_node->steal_outputs(*pre_node);

        // Setup 'pre_node':
        unsigned const pre_to_head = pre_node->link_append_output(
            loop_head, [](ssa_ht phi) -> ssa_value_t { assert(false); return {}; });
        assert(loop_head->input(pre_to_head) == pre_node);

        // Create 'loop_head's SSA nodes:
        assert(ssa_it->input(1).type() == TYPE_U);

        ssa_ht const carry_result = carry_output ? loop_head->emplace_ssa(SSA_phi, TYPE_BOOL) : ssa_ht{};
        ssa_ht const loop_result = loop_head->emplace_ssa(SSA_phi, type);
        ssa_ht const loop_i_phi  = loop_head->emplace_ssa(SSA_phi, TYPE_U);
        ssa_ht const loop_cond   = loop_head->emplace_ssa(
            SSA_multi_lt, TYPE_BOOL, 
            ssa_value_t(TYPE_U, TYPE_INT), ssa_value_t(TYPE_U, TYPE_INT), 
            loop_i_phi, ssa_it->input(1));
        ssa_ht const loop_if     = loop_head->emplace_ssa(SSA_if, TYPE_VOID, loop_cond);
        loop_if->append_daisy();

        // Create 'loop_body's SSA nodes:
        ssa_ht loop_shift;
        if(shl)
            loop_shift = loop_body->emplace_ssa(SSA_rol, type, loop_result, ssa_value_t(0u, TYPE_BOOL));
        else
        {
            assert(shr);
            ssa_value_t carry(0u, TYPE_BOOL);
            if(is_signed(type.name()))
               carry = cfg_it->emplace_ssa(SSA_sign, TYPE_BOOL, loop_result);
            loop_shift = loop_body->emplace_ssa(SSA_ror, type, loop_result, carry);
        }
        ssa_ht const loop_incr = loop_body->emplace_ssa(
            SSA_add, TYPE_U, loop_i_phi, ssa_value_t(1u, TYPE_U), ssa_value_t(0u, TYPE_BOOL));

        // Setup phis using indexes saved earlier.
        
        if(carry_output)
        {
            ssa_ht const new_carry = loop_body->emplace_ssa(
                SSA_carry, TYPE_BOOL, loop_shift);

            carry_result->alloc_input(2);
            carry_result->build_set_input(loop_to_head, new_carry);
            carry_result->build_set_input(pre_to_head, ssa_value_t(0u, TYPE_BOOL));
        }

        loop_result->alloc_input(2);
        loop_result->build_set_input(loop_to_head, loop_shift);
        loop_result->build_set_input(pre_to_head, ssa_it->input(0));

        loop_i_phi->alloc_input(2);
        loop_i_phi->build_set_input(loop_to_head, loop_incr);
        loop_i_phi->build_set_input(pre_to_head, ssa_value_t(0u, TYPE_U));

        // OK! Now replace 'ssa_it':

        if(carry_output)
        {
            carry_output->replace_with(carry_result);
            to_prune.push_back(carry_output);
        }

        assert(ssa_it->cfg_node() == pre_node);
        assert(ssa_it->type() == loop_result->type());
        ssa_it->replace_with(loop_result);
        to_prune.push_back(ssa_it); // Schedule for pruning later.

        ir.assert_valid();
    }

    // Prune replaced nodes:
    for(ssa_ht h : to_prune)
        h->prune();

    return to_prune.size() > 0;
}
