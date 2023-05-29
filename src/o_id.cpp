#include "o_id.hpp"

#include <cstdint>

#include <boost/container/small_vector.hpp>
#include <boost/container/static_vector.hpp>

#include "flat/small_map.hpp"

#include "ir.hpp"
#include "type.hpp"
#include "type_mask.hpp"
#include "alloca.hpp"
#include "worklist.hpp"

namespace bc = ::boost::container;

// Replaces single nodes with one of their inputs.
// (e.g. X + 0 becomes X, or Y & ~0 becomes Y)
static bool o_simple_identity(log_t* log, ir_t& ir)
{
    dprint(log, "SIMPLE_IDENTITY");
    bool updated = false;

    constexpr void* replaceWith[] = { &&replaceWith0, &&replaceWith1 };

    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it;)
    {
        dprint(log, "-SIMPLE_IDENTITY_OP", ssa_it, ssa_it->op());

        ssa_value_t carry_replacement = {};

        auto const replace_carry = [log](ssa_node_t& node, ssa_value_t carry_replacement)
        {
            passert(!carry_output(node) || carry_replacement, node.op());
            if(carry_replacement)
            {
                if(ssa_ht carry = carry_output(node))
                {
                    dprint(log, "-SIMPLE_IDENTITY_REPLACE_CARRY", node.handle(), carry_replacement);
                    carry->replace_with(carry_replacement);
                    carry->prune();
                }
            }
        };


        if(ssa_flags(ssa_it->op()) & SSAF_INDEXES_ARRAY8)
        {
            using namespace ssai::array;

            // If the array is size 1, the index has to be zero:
            if(ssa_it->input(ARRAY).type().array_length() == 1)
                ssa_it->link_change_input(INDEX, ssa_value_t(0u, TYPE_U));
        }

        if(ssa_flags(ssa_it->op()) & SSAF_ARRAY_OFFSET)
        {
            using namespace ssai::array;

            ssa_value_t const index = ssa_it->input(INDEX);

            if(index.is_num() && index.whole() != 0)
            {
                // Turn array indexes of a constant into an offset.
                // e.g. foo[5] turns the '5' into an offset.

                unsigned offset = (ssa_it->input(OFFSET).whole() + index.whole());

                ssa_it->link_change_input(OFFSET, ssa_value_t(offset, TYPE_U20));
                ssa_it->link_change_input(INDEX, ssa_value_t(0u, TYPE_U));

                if(ssa_it->op() == SSA_read_array16)
                    ssa_it->unsafe_set_op(SSA_read_array8);
                else if(ssa_it->op() == SSA_write_array16)
                    ssa_it->unsafe_set_op(SSA_write_array8);
                else
                    assert(!(ssa_flags(ssa_it->op()) & SSAF_INDEXES_ARRAY16));

                updated = true;
            }
            else if(index.holds_ref() && (ssa_flags(ssa_it->op()) & SSAF_INDEXES_ARRAY16))
            {
                if(index->op() == SSA_add && index->input(2).is_num())
                {
                    // Turn array indexes plus a constant into an offset.
                    // e.g. foo[a + 5] turns the '5' into an offset.

                    assert(index->type() == TYPE_U20);

                    for(unsigned i = 0; i < 2; ++i)
                    {
                        if(!index->input(i).is_num())
                            continue;

                        unsigned offset = (ssa_it->input(OFFSET).whole()
                                           + index->input(i).whole()
                                           + index->input(2).whole());

                        ssa_it->link_change_input(OFFSET, ssa_value_t(offset, TYPE_U20));
                        ssa_it->link_change_input(INDEX, index->input(!i));

                        updated = true;
                        break;
                    }
                }
                else if(index->op() == SSA_sub && index->input(2).is_num())
                {
                    // Turn array indexes minus a constant into an offset.
                    // e.g. foo[a - 5] turns the '-5' into an offset.

                    assert(index->type() == TYPE_U20);

                    if(index->input(1).is_num())
                    {
                        unsigned offset = (ssa_it->input(OFFSET).whole()
                                           - index->input(1).whole()
                                           - (1 - index->input(2).whole()));

                        ssa_it->link_change_input(OFFSET, ssa_value_t(offset, TYPE_U20));
                        ssa_it->link_change_input(INDEX, index->input(0));

                        updated = true;
                    }
                }
            }
        }

        switch(ssa_it->op())
        {
        case SSA_if:
            {
                if(ssa_it->input(0).holds_ref() && ssa_it->input(0)->op() == SSA_xor)
                {
                    ssa_ht const x = ssa_it->input(0).handle();

                    for(unsigned i = 0; i < 2; ++i)
                    {
                        if(x->input(i) == ssa_value_t(1u, TYPE_BOOL))
                        {
                            ssa_it->link_change_input(0, x->input(!i));
                            ssa_it->cfg_node()->link_swap_outputs(0, 1);
                            updated = true;
                            break;
                        }
                    }
                }
            }
            break;

        case SSA_get_byte:
            if(ssa_it->input(0).holds_ref())
            {
                ssa_ht const input = ssa_it->input(0).handle();
                if(input->op() == SSA_replace_byte && input->input(1) == ssa_it->input(1))
                {
                    if(ssa_it->output_size())
                    {
                        ssa_it->replace_with(input->input(2));
                        ssa_it->prune();
                        updated = true;
                    }
                }
            }
            break;

        case SSA_write_array8:
        case SSA_write_array16:
            {
                using namespace ssai::array;

                ssa_op_t const read = (ssa_it->op() == SSA_write_array8) ? SSA_read_array8 : SSA_read_array16;
                ssa_value_t const array = ssa_it->input(ARRAY);
                ssa_value_t const assign = ssa_it->input(ASSIGNMENT);

                // Prune pointless writes like foo[5] = foo[5]
                if(assign.holds_ref() && assign->op() == read
                   && assign->input(ARRAY) == ssa_it->input(ARRAY)
                   && assign->input(OFFSET) == ssa_it->input(OFFSET)
                   && assign->input(INDEX) == ssa_it->input(INDEX))
                {
                    goto replaceWith0;
                }

                // Prune pointless writes like foo[5] = X; foo[5] = Y
                if(array.holds_ref() && array->op() == ssa_it->op()
                   && array->output_size() == 1
                   && array->input(OFFSET) == ssa_it->input(OFFSET)
                   && array->input(INDEX) == ssa_it->input(INDEX))
                {
                    ssa_it->link_change_input(ARRAY, array->input(ARRAY));
                    assert(array->output_size() == 0);
                    array->prune();
                    updated = true;
                    break;
                }
            }
            break;

        case SSA_read_array8:
        case SSA_read_array16:
            {
                using namespace ssai::array;

                ssa_op_t const write = (ssa_it->op() == SSA_read_array8) ? SSA_write_array8 : SSA_write_array16;
                ssa_value_t const array = ssa_it->input(ARRAY);

                // Prune pointless reads.
                if(array.holds_ref() && array->op() == write
                   && array->input(OFFSET) == ssa_it->input(OFFSET)
                   && array->input(INDEX) == ssa_it->input(INDEX))
                {
                    ssa_it->replace_with(array->input(ASSIGNMENT));
                    goto prune;
                }

                // Reading a fill replaces with the fill.
                if(array.holds_ref() && array->op() == SSA_fill_array)
                {
                    ssa_it->replace_with(array->input(0));
                    goto prune;
                }

                // Reading an init with a constant index can be replaced.
                if(array.holds_ref() && array->op() == SSA_init_array
                   && ssa_it->input(INDEX).is_num())
                {
                    std::uint16_t const index = ssa_it->input(INDEX).whole() + ssa_it->input(OFFSET).whole();
                    if(index < array->type().array_length())
                    {
                        ssa_it->replace_with(array->input(index));
                        goto prune;
                    }
                }
            }
            break;

        case SSA_cast:
            {
                ssa_value_t const input = ssa_it->input(0);

                type_t const from = input.type();
                type_t const to = ssa_it->type();

                // Prune casts from A -> A
                if(from == to)
                    goto replaceWith0;

                // Simplify chains of casts from similar types A -> B
                if(input.holds_ref() && input->op() == SSA_cast)
                {
                    if(same_scalar_layout(from.name(), to.name())
                       || is_arithmetic_bijection(input->input(0).type().name(), from.name()))
                    {
                        dprint(log, "--SIMPLE_IDENTITY_SIMPLIFY_CAST");
                        ssa_it->link_change_input(0, input->input(0));
                    }
                }
            }
            break;

        case SSA_not_eq:
            for(unsigned i = 0; i < 2; ++i)
            {
                if(ssa_it->input(i) == ssa_value_t(0u, TYPE_BOOL))
                    goto *replaceWith[!i];

                if(ssa_it->input(i) == ssa_value_t(1u, TYPE_BOOL))
                {
                    ssa_it->unsafe_set_op(SSA_xor);
                    updated = true;
                    goto check_xor;
                }
            }
            // fall-through
        check_not_eq:
        case SSA_lt:
            if(ssa_it->input(0) == ssa_it->input(1))
            {
                ssa_it->replace_with(ssa_value_t(0u, TYPE_BOOL));
                goto prune;
            }
            else if(ssa_it->op() == SSA_lt)
            {
                type_name_t const lt = ssa_it->input(0).type().name();
                type_name_t const rt = ssa_it->input(1).type().name();

                // Replace C < X with C+1 <= X
                // (This produces more efficient isel)

                if(ssa_it->input(0).is_num() && !ssa_it->input(1).is_num())
                {
                    fixed_sint_t f = ssa_it->input(0).signed_fixed();
                    f += type_unit(lt);

                    if(f <= type_max(lt))
                    {
                        ssa_it->link_change_input(0, ssa_value_t(fixed_t{f}, lt));
                        ssa_it->unsafe_set_op(SSA_lte);
                        updated = true;
                        break;
                    }
                }

                // Replace X < 0 with SSA_sign
                if(ssa_it->input(1).eq_fixed({0}) && is_signed(ssa_it->input(0).type().name()))
                {
                    ssa_it->link_shrink_inputs(1);
                    ssa_it->unsafe_set_op(SSA_sign);
                    updated = true;
                    break;
                }

                if(!same_scalar_layout(lt, rt))
                    break;

                // Replace expressions like (X < 1) with (X == 0).
                // This aids code generation.

                if(ssa_it->input(1).eq_fixed({ type_min(lt) + type_unit(lt) }))
                {
                    ssa_it->link_change_input(1, ssa_value_t(type_min(lt), lt));
                    ssa_it->unsafe_set_op(SSA_eq);
                    updated = true;
                    break;
                }

            }
            break;

        case SSA_eq:
            for(unsigned i = 0; i < 2; ++i)
            {
                auto ie = ssa_it->input_edge(i);

                if(ie.targets_eq(ssa_value_t(1u, TYPE_BOOL)))
                    goto *replaceWith[!i];

                if(ie.targets_eq(ssa_value_t(0u, TYPE_BOOL)))
                {
                    ssa_it->unsafe_set_op(SSA_xor);
                    ssa_it->link_change_input(i, ssa_value_t(1u, TYPE_BOOL));
                    updated = true;
                    goto check_xor;
                }

                ssa_value_t const other = ssa_it->input(!i);

                // Look for expressions like:
                //     (foo & C) == C
                // Where C is a single, constant bit.
                // We can replace this with the more efficient: (foo & C) != 0
                if(ie.holds_ref() 
                   && ie.handle()->op() == SSA_and 
                   && other.is_num()
                   && builtin::popcount(fixed_uint_t(other.signed_fixed())) == 1
                   && other == ie.handle()->input(!ie.index()))
                {
                    ssa_it->unsafe_set_op(SSA_not_eq);
                    ssa_it->link_change_input(!i, ssa_value_t(0u, other.num_type_name()));
                    updated = true;
                    goto check_not_eq;
                }
            }
            // fall-through
        case SSA_lte:
            if(ssa_it->input(0) == ssa_it->input(1))
            {
                ssa_it->replace_with(ssa_value_t(1u, TYPE_BOOL));
                goto prune;
            }
            else if(ssa_it->op() == SSA_lte)
            {
                type_name_t const lt = ssa_it->input(0).type().name();
                type_name_t const rt = ssa_it->input(1).type().name();

                // Replace X <= C with X < C+1
                // (This produces more efficient isel)

                if(!ssa_it->input(0).is_num() && ssa_it->input(1).is_num())
                {
                    fixed_sint_t f = ssa_it->input(1).signed_fixed();
                    f += type_unit(lt);

                    if(f <= type_max(lt))
                    {
                        ssa_it->link_change_input(1, ssa_value_t(fixed_t{f}, lt));
                        ssa_it->unsafe_set_op(SSA_lt);
                        updated = true;
                        break;
                    }
                }

                // Replace expressions like (1 <= x) with (X != 0).
                // This aids code generation.

                if(!same_scalar_layout(lt, rt))
                    break;

                if(ssa_it->input(0).eq_fixed({ type_min(rt) + type_unit(rt) }))
                {
                    ssa_it->link_change_input(0, ssa_value_t(type_min(rt), rt));
                    ssa_it->unsafe_set_op(SSA_not_eq);
                    updated = true;
                    break;
                }
            }
            break;

        case SSA_multi_eq:
        case SSA_multi_not_eq:
            break; // TODO
            {
                // If we have several inputs comparing to zero,
                // we'll combine them using SSA_or first.

                bc::static_vector<unsigned, max_total_bytes+1> to_or;

                unsigned const input_size = ssa_it->input_size();
                for(unsigned i = 0; i < input_size; i += 2)
                {
                    if(ssa_it->input(i+0).eq_whole(0))
                        to_or.push_back(i+1);
                    else if(ssa_it->input(i+1).eq_whole(0))
                        to_or.push_back(i+0);
                }

                if(to_or.size() <= 1)
                    break;

                // Chain together 'SSA_or' nodes:
                ssa_value_t v = ssa_it->input(to_or[0]);
                for(unsigned i = 1; i < to_or.size(); ++i)
                    v = ssa_it->cfg_node()->emplace_ssa(SSA_or, TYPE_U, v, ssa_it->input(to_or[i]));

                // Replace and remove the multi_eq inputs:
                ssa_it->link_change_input(to_or.back(), v);
                to_or.pop_back();
                for(unsigned i : to_or | std::views::reverse)
                {
                    unsigned const first  = i & ~1;
                    ssa_it->link_remove_input(first+1);
                    ssa_it->link_remove_input(first);
                }

                updated = true;
            }
            break;

        case SSA_xor:
        check_xor:
            for(unsigned i = 0; i < 2; ++i)
            {
                if(ssa_it->input(i) != ssa_value_t(1u, TYPE_BOOL))
                    continue;

                assert(ssa_it->input(!i).type() == TYPE_BOOL);
                assert(ssa_it->type() == TYPE_BOOL);

                if(!ssa_it->input(!i).holds_ref())
                    continue;

                ssa_ht const input = ssa_it->input(!i).handle();

                if(input->op() == SSA_eq)
                    ssa_it->unsafe_set_op(SSA_not_eq);
                else if(input->op() == SSA_not_eq)
                    ssa_it->unsafe_set_op(SSA_eq);
                else
                    continue;

                updated = true;
                ssa_it->link_change_input(0, input->input(0));
                ssa_it->link_change_input(1, input->input(1));
                break;
            }
            break;

        case SSA_ror:
            goto rol_ror_common;
            if(ssa_it->input(1).eq_whole(0)
               && ssa_it->input(0).holds_ref())
            {
                ssa_ht const input = ssa_it->input(0).handle();

                // Handle ROR into ROL, replacing with an AND
                if(input->op() == SSA_rol 
                   && input->input(1).eq_whole(0)
                   && input->output_size() == 1)
                {
                    assert(input->output(0) == ssa_it);

                    type_name_t const tn = ssa_it->type().name();
                    fixed_uint_t const mask = numeric_bitmask(tn);
                    ssa_it->unsafe_set_op(SSA_and);
                    ssa_it->link_change_input(0, input->input(0));
                    ssa_it->link_change_input(1, ssa_value_t(fixed_t{ mask & ~high_bit_only(mask) }, tn));

                    if(ssa_ht carry = carry_output(*ssa_it))
                        carry->replace_with(ssa_value_t(0, TYPE_BOOL));

                    updated = true;
                    goto done_rol_ror;
                }

                // Handle ROR into AND into ROL, replacing with an AND
                if(input->op() == SSA_and && input->output_size() == 1)
                {
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        if(!input->input(i).is_num() || !input->input(!i).holds_ref())
                            continue;

                        ssa_ht const other = input->input(!i).handle();

                        if(other->op() == SSA_rol
                           && other->input(1).eq_whole(0)
                           && other->output_size() == 1)
                        {
                            type_name_t const tn = ssa_it->type().name();
                            fixed_uint_t const mask = numeric_bitmask(tn);
                            input->link_change_input(i, ssa_value_t(fixed_t{ mask & (input->input(i).fixed().value >> 1) }, tn));
                            input->link_change_input(!i, other->input(0));
                            
                            if(ssa_ht carry = carry_output(*ssa_it))
                                carry->replace_with(ssa_value_t(0, TYPE_BOOL));

                            ssa_it->replace_with(input);

                            updated = true;
                            goto done_rol_ror;
                        }
                    }
                }
            }

            goto rol_ror_common;

        case SSA_rol:
            if(ssa_it->input(1).eq_whole(0)
               && ssa_it->input(0).holds_ref())
            {
                ssa_ht const input = ssa_it->input(0).handle();

                // Handle ROL into ROR, replacing with an AND
                if(input->op() == SSA_ror
                   && input->input(1).eq_whole(0)
                   && input->output_size() == 1)
                {
                    assert(input->output(0) == ssa_it);

                    type_name_t const tn = ssa_it->type().name();
                    fixed_uint_t const mask = numeric_bitmask(tn);
                    ssa_it->unsafe_set_op(SSA_and);
                    ssa_it->link_change_input(0, input->input(0));
                    ssa_it->link_change_input(1, ssa_value_t(fixed_t{ mask & ~low_bit_only(mask) }, tn));

                    if(ssa_ht carry = carry_output(*ssa_it))
                        carry->replace_with(ssa_value_t(0, TYPE_BOOL));

                    updated = true;
                    goto done_rol_ror;
                }

                // Handle ROL into AND into ROR, replacing with an AND
                if(input->op() == SSA_and && input->output_size() == 1)
                {
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        if(!input->input(i).is_num() || !input->input(!i).holds_ref())
                            continue;

                        ssa_ht const other = input->input(!i).handle();

                        if(other->op() == SSA_ror
                           && other->input(1).eq_whole(0)
                           && other->output_size() == 1)
                        {
                            type_name_t const tn = ssa_it->type().name();
                            fixed_uint_t const mask = numeric_bitmask(tn);
                            input->link_change_input(i, ssa_value_t(fixed_t{ mask & (input->input(i).fixed().value << 1) }, tn));
                            input->link_change_input(!i, other->input(0));
                            
                            if(ssa_ht carry = carry_output(*ssa_it))
                                carry->replace_with(ssa_value_t(0, TYPE_BOOL));

                            ssa_it->replace_with(input);

                            updated = true;
                            goto done_rol_ror;
                        }
                    }
                }
            }

            // fall-through
        rol_ror_common:
            // If only the carry is used, ignore our carry input.
            if(ssa_it->output_size() == 1 
               && ssa_it->output(0)->op() == SSA_carry
               && !ssa_it->input(1).eq_whole(0))
            {
                updated = true;
                ssa_it->link_change_input(1, ssa_value_t(0u, TYPE_BOOL));
                break;
            }
        done_rol_ror:
            break;

        case SSA_shr:
            {
                ssa_value_t const cast = ssa_it->input(0);
                if(cast.holds_ref() && cast->op() == SSA_cast)
                {
                    ssa_value_t const from = cast->input(0);

                    type_t const from_type = from.type();
                    type_t const to_type = ssa_it->type();

                    unsigned const from_whole = whole_bytes(from_type.name());
                    //unsigned const from_frac  = frac_bytes(from_type.name());
                    unsigned const from_signed = is_signed(from_type.name());

                    unsigned const to_whole = whole_bytes(to_type.name());
                    unsigned const to_frac  = frac_bytes(to_type.name());
                    unsigned const to_signed = is_signed(to_type.name());

                    if(from_whole < to_whole && (!from_signed || to_signed))
                    {
                        type_t const new_from_type = type_s_or_u(from_whole, to_frac, from_signed & to_signed);

                        ssa_value_t new_from = from;
                        if(new_from_type != from_type)
                            new_from = ssa_it->cfg_node()->emplace_ssa(SSA_cast, new_from_type, from);

                        ssa_ht const new_to = ssa_it->cfg_node()->emplace_ssa(SSA_cast, to_type);

                        ssa_it->link_change_input(0, new_from);
                        ssa_it->set_type(new_from_type);
                        ssa_it->replace_with(INPUT_VALUE, new_to);

                        new_to->link_append_input(ssa_it);

                        updated = true;
                    }
                }
            }
            break;

        default:
            break;
        }

        if(is_scalar(ssa_it->type().name()))
        {
            fixed_t const all_set = { numeric_bitmask(ssa_it->type().name()) };

            auto const add_sub_impl = [&all_set](ssa_value_t v, fixed_t carry, bool sub) -> ssa_value_t
            {
                if(!v.is_num())
                    return {};

                // Put the carry in the lowest bit.
                if(!carry.value == sub)
                    carry.value = low_bit_only(all_set.value);

                if((v.fixed().value + carry.value) & all_set.value)
                    return {};

                return ssa_value_t(!!(v.fixed().value + carry.value) != sub, TYPE_BOOL);
            };

            switch(ssa_it->op())
            {
            case SSA_add:
                if(ssa_it->input(2).is_num())
                {
                    for(unsigned i = 0; i < 2; ++i)
                        if((carry_replacement = add_sub_impl(ssa_it->input(i), ssa_it->input(2).fixed(), false)))
                            goto *replaceWith[!i];

                    if(frac_bytes(ssa_it->type().name()) == 0)
                    {
                        // Fold pointer additions 
                        if(!carry_used(*ssa_it))
                        {
                            for(unsigned i = 0; i < 2; ++i)
                            {
                                ssa_value_t const input = ssa_it->input(i);

                                if(!input.is_locator() || !input.is_num())
                                    continue;

                                if(carry_used(*ssa_it))
                                    continue;

                                locator_t const loc = input.locator();

                                if(loc.is() == IS_PTR)
                                {
                                    unsigned const offset = ssa_it->input(!i).whole() + ssa_it->input(2).whole();
                                    ssa_it->link_change_input(i, loc.with_advance_offset(offset));
                                    goto *replaceWith[i];
                                }
                            }
                        }
                    }

                    if(ssa_it->input(2).eq_whole(0))
                    {
                        // When adding a casted carry, we can remove the cast.
                        for(unsigned i = 0; i < 2; ++i)
                        {
                            ssa_value_t const cast = ssa_it->input(i);

                            if(!cast.holds_ref() || cast->op() != SSA_cast)
                                continue;

                            ssa_value_t const carry = cast->input(0);
                            if(!carry.holds_ref() || carry->op() != SSA_carry)
                                continue;

                            ssa_it->link_change_input(i, ssa_value_t(0u, ssa_it->type().name()));
                            ssa_it->link_change_input(2, carry);

                            updated = true;
                            goto done_add;
                        }

                        // Handle chained adds, where zeroes and carries are involved.
                        for(unsigned i = 0; i < 2; ++i)
                        {
                            if(!ssa_it->input(i).holds_ref())
                                continue;

                            ssa_ht const input = ssa_it->input(i).handle();

                            if(input->op() == SSA_add)
                            {
                                for(unsigned j = 0; j < 2; ++j)
                                {
                                    if(input->input(j).eq_whole(0))
                                    {
                                        ssa_it->link_change_input(i, input->input(!j));
                                        ssa_it->link_change_input(2, input->input(2));

                                        updated = true;
                                        goto done_add;
                                    }
                                    else if(input->input(2).eq_whole(0) && input->input(j).eq_low_bit())
                                    {
                                        ssa_it->link_change_input(i, input->input(!j));
                                        ssa_it->link_change_input(2, ssa_value_t(1u, TYPE_BOOL));

                                        updated = true;
                                        goto done_add;
                                    }
                                }
                            }
                        }
                    }
                }
                else if(ssa_it->input(0).eq_whole(0) && ssa_it->input(1).eq_whole(0))
                {
                    // Replace 0 + 0 + C with C
                    if(frac_bytes(ssa_it->type().name()) == 0)
                    {
                        ssa_it->link_remove_input(0);
                        ssa_it->link_remove_input(0);
                        ssa_it->unsafe_set_op(SSA_cast);
                        updated = true;
                        goto done_add;
                    }
                }
            done_add:
                break;

            case SSA_sub:
                if(ssa_it->input(2).is_num())
                {
                    if((carry_replacement = add_sub_impl(ssa_it->input(1), ssa_it->input(2).fixed(), true)))
                        goto *replaceWith[0];

                    if(frac_bytes(ssa_it->type().name()) == 0)
                    {
                        // Fold pointer subtractions
                        if(!carry_used(*ssa_it) && ssa_it->input(0).is_locator() && ssa_it->input(1).is_num())
                        {
                            locator_t const loc = ssa_it->input(0).locator();

                            if(carry_used(*ssa_it))
                                continue;

                            if(loc.is() == IS_PTR)
                            {
                                unsigned const offset = -ssa_it->input(1).whole() - (1 - ssa_it->input(2).whole());
                                ssa_it->link_change_input(0, loc.with_advance_offset(offset));
                                goto replaceWith0;
                            }
                        }
                    }

                    if(ssa_it->input(2).eq_whole(1))
                    {
                        // When subtracting a casted carry, we can remove the cast.
                        auto const cast = ssa_it->input(1);

                        if(cast.holds_ref() && cast->op() == SSA_cast)
                        {
                            ssa_value_t const xor_ = cast->input(0);
                            if(xor_.holds_ref() && xor_->op() == SSA_xor)
                            {
                                for(unsigned i = 0; i < 2; ++i)
                                {
                                    if(!xor_->input(!i).eq_whole(1)) // Looking for XOR as negation.
                                        continue;

                                    ssa_value_t const carry = xor_->input(0);
                                    if(!carry.holds_ref() || carry->op() != SSA_carry)
                                        continue;

                                    ssa_it->link_change_input(1, ssa_value_t(0u, ssa_it->type().name()));
                                    ssa_it->link_change_input(2, carry);

                                    updated = true;
                                    goto done_sub;
                                }
                            }
                            else
                            {
                                ssa_value_t const carry = cast->input(0);
                                if(carry.holds_ref() && carry->op() == SSA_carry)
                                {
                                    // Alternatively, look for SSA_sub outputs
                                    unsigned const output_size = ssa_it->output_size();
                                    for(unsigned i = 0; i < output_size; ++i)
                                    {
                                        auto oe = ssa_it->output_edge(i);
                                        if(oe.handle->op() == SSA_sub 
                                           && oe.index == 1
                                           && oe.handle->input(2).eq_whole(1))
                                        {
                                            oe.handle->link_change_input(1, ssa_it->input(0));
                                            oe.handle->link_change_input(2, carry);
                                            updated = true;
                                        }
                                    }
                                }
                            }
                        }

                        // Handle chained subs and adds, where zeroes and carries are involved.
                        for(unsigned i = 0; i < 2; ++i)
                        {
                            if(!ssa_it->input(i).holds_ref())
                                continue;

                            ssa_ht const input = ssa_it->input(i).handle();

                            if(i == 0 && input->op() == SSA_sub)
                            {
                                for(unsigned j = 0; j < 2; ++j)
                                {
                                    if(input->input(j).eq_whole(0))
                                    {
                                        ssa_it->link_change_input(i, input->input(!j));
                                        ssa_it->link_change_input(2, input->input(2));

                                        updated = true;
                                        goto done_sub;
                                    }
                                }
                            }
                            else if(i == 1 && input->op() == SSA_add)
                            {
                                for(unsigned j = 0; j < 2; ++j)
                                {
                                    if((input->input(2).eq_whole(1) && input->input(j).eq_whole(0))
                                       || (input->input(2).eq_whole(0) && input->input(j).eq_low_bit()))
                                    {
                                        ssa_it->link_change_input(i, input->input(!j));
                                        ssa_it->link_change_input(2, ssa_value_t(0u, TYPE_BOOL));

                                        updated = true;
                                        goto done_sub;
                                    }
                                }
                            }
                        }
                    }

                }
            done_sub:
                break;

            case SSA_or:
                if(ssa_it->input(0) == ssa_it->input(1))
                    goto replaceWith0;
                // fall through
            case SSA_xor:
                if(ssa_it->input(0).eq_fixed({0}))
                    goto replaceWith1;
                if(ssa_it->input(1).eq_fixed({0}))
                    goto replaceWith0;
                break;

            case SSA_shl:
            case SSA_shr:
                if(ssa_it->input(1).eq_fixed({0}))
                {
                    carry_replacement = ssa_value_t(0u, TYPE_BOOL);
                    goto replaceWith0;
                }
                break;

            case SSA_and:
                for(unsigned i = 0; i < 2; ++i)
                    if(ssa_it->input(i).eq_fixed(all_set))
                        goto *replaceWith[!i];
                if(ssa_it->input(0) == ssa_it->input(1))
                    goto replaceWith0;

                if(ssa_it->output_size() == 1)
                {
                    auto oe = ssa_it->output_edge(0);

                    if(oe.handle->cfg_node() == ssa_it->cfg_node()
                       && (oe.handle->op() == SSA_not_eq || (oe.handle->op() == SSA_multi_not_eq && oe.handle->input_size() == 2))
                       && oe.handle->input(oe.index ^ 1).eq_whole(0u))
                    {
                        fixed_uint_t const mask = numeric_bitmask(ssa_it->type().name());
                        fixed_uint_t const one  =  1ull << fixed_t::shift;
                        fixed_uint_t const two  =  2ull << fixed_t::shift;

                        // AND'ing with the lowest bit, or the highest bit,
                        // then converting to Bool,
                        // can be replaced with a rotate.

                        // Don't do this optimization unless we can use the carry:
                        unsigned const output_size = oe.handle->output_size();
                        for(unsigned i = 0; i < output_size; ++i)
                        {
                            auto const ooe = oe.handle->output_edge(i);
                            if(possible_carry_input_i(ooe.handle->op()) != int(ooe.index))
                                goto done_and_to_rotate;
                        }

                        {
                            fixed_uint_t const low  =  low_bit_only(mask);
                            fixed_uint_t const high = high_bit_only(mask);
                            for(unsigned i = 0; i < 2; ++i)
                            {
                                if(ssa_it->input(i).eq_fixed({ low }))
                                {
                                    ssa_it->unsafe_set_op(SSA_ror);
                                    ssa_it->link_change_input(i, ssa_value_t(0u, TYPE_BOOL));
                                    ssa_it->link_swap_inputs(1, i);

                                    oe.handle->unsafe_set_op(SSA_carry);
                                    oe.handle->link_clear_inputs();
                                    oe.handle->link_append_input(ssa_it);

                                    updated = true;
                                    goto done_and;
                                }
                                else if(ssa_it->input(i).eq_fixed({ high }))
                                {
                                    ssa_it->unsafe_set_op(SSA_rol);
                                    ssa_it->link_change_input(i, ssa_value_t(0u, TYPE_BOOL));
                                    ssa_it->link_swap_inputs(1, i);

                                    oe.handle->unsafe_set_op(SSA_carry);
                                    oe.handle->link_clear_inputs();
                                    oe.handle->link_append_input(ssa_it);

                                    updated = true;
                                    goto done_and;
                                }
                            }
                        }

                    done_and_to_rotate:

                        // AND'ing with 1, then converting to bool via 'not_eq',
                        // can be replaced with a cast.
                        // Likewise, AND'ing with 2 can do this, 
                        // but first it must be shifted right 1.
                        // (This often compiles down to a single ALR instruction.)
                        for(unsigned i = 0; i < 2; ++i)
                        {
                            if(ssa_it->input(i).eq_fixed({ one }))
                            {
                                oe.handle->unsafe_set_op(SSA_as_bool);
                                oe.handle->link_clear_inputs();
                                oe.handle->link_append_input(ssa_it);

                                updated = true;
                                goto done_and;
                            }
                            else if(ssa_it->input(i).eq_fixed({ two }))
                            {
                                ssa_ht const ror = oe.handle->cfg_node()->emplace_ssa(
                                    SSA_ror, ssa_it->type(), 
                                    ssa_it, ssa_value_t(0u, TYPE_BOOL));

                                oe.handle->unsafe_set_op(SSA_as_bool);
                                oe.handle->link_clear_inputs();
                                oe.handle->link_append_input(ror);

                                updated = true;
                                goto done_and;
                            }
                        }
                    }
                }
            done_and:
                break;

            case SSA_mul8_lo:
            case SSA_mul:
                for(unsigned i = 0; i < 2; ++i)
                {
                    ssa_value_t const input = ssa_it->input(i);
                    ssa_value_t const other = ssa_it->input(!i);

                    if(input.eq_whole(0))
                    {
                        if(ssa_ht hi = mul8_output(*ssa_it))
                        {
                            hi->replace_with(ssa_value_t(0u, hi->type().name()));
                            hi->prune();
                        }

                        goto *replaceWith[i];
                    }

                    if(input.eq_whole(1))
                    {
                        if(ssa_ht hi = mul8_output(*ssa_it))
                        {
                            if(is_signed(other.type().name()))
                            {
                                ssa_ht const extend = hi->cfg_node()->emplace_ssa(
                                    SSA_sign_extend, hi->type(), other);
                                hi->replace_with(extend);
                            }
                            else
                                hi->replace_with(ssa_value_t(0u, hi->type().name()));
                            hi->prune();
                        }

                        goto *replaceWith[!i];
                    }

                    if(is_signed(input.type().name()) && input.eq_signed_whole(-1))
                    {
                        if(ssa_ht hi = mul8_output(*ssa_it))
                        {
                            if(is_signed(other.type().name()))
                            {
                                ssa_ht const extend = hi->cfg_node()->emplace_ssa(
                                    SSA_sign_extend, hi->type(), ssa_it);
                                hi->replace_with(extend);
                            }
                            else
                                hi->replace_with(ssa_value_t(0xFF, hi->type().name()));
                        }

                        ssa_ht const sub = ssa_it->cfg_node()->emplace_ssa(
                            SSA_sub, other.type(), ssa_value_t(0u, other.type().name()), other);

                        ssa_it->link_remove_input(i);
                        ssa_it->link_change_input(0, sub);
                        ssa_it->unsafe_set_op(SSA_cast);

                        updated = true;
                        goto done_mul;
                    }
                }

                if(ssa_it->op() != SSA_mul)
                {
                done_mul:
                    break;
                }

                // Convert to shifts
                for(unsigned i = 0; i < 2; ++i)
                {
                    ssa_value_t const input = ssa_it->input(i);
                    ssa_value_t const other = ssa_it->input(!i);

                    if(!input.is_num())
                        continue;

                    fixed_sint_t const f = input.signed_fixed();
                    fixed_uint_t const abs = std::abs(f);
                    assert(f); // handled earlier
                    
                    // Multiplications will turn into the sum or difference of several shifts.
                    // The code below determines whether it's more efficient to add or subtract.

                    fixed_uint_t add = abs;
                    fixed_uint_t sub = 0;

                    bitset_for_each(abs, [&](unsigned bit)
                    {
                        fixed_uint_t const new_add = add + (1ull << bit);
                        if(builtin::popcount(new_add) + 1 < builtin::popcount(add))
                        {
                            add = new_add;
                            sub |= (1ull << bit);
                        }
                    });

                    assert(!(add & sub));

                    // Before generating the shifts and adds,
                    // convert the operand to the resulting type,
                    // and change the sign if necessary.

                    cfg_ht const cfg = ssa_it->cfg_node();
                    type_t const type = ssa_it->type();

                    ssa_ht initial = cfg->emplace_ssa(SSA_cast, type, other);

                    if(f < 0)
                    {
                        initial = cfg->emplace_ssa(
                            SSA_sub, type, 
                            ssa_value_t(0, type.name()), initial, ssa_value_t(1, TYPE_BOOL));
                    }

                    // Now generate the other SSA nodes

                    ssa_value_t total = ssa_value_t(0u, type.name());
                    ssa_ht shift = initial;
                    unsigned prev_bit = 0;

                    // Whole component:
                    bitset_for_each((add | sub) >> fixed_t::shift, [&](unsigned bit)
                    {
                        unsigned const shift_amount = bit - prev_bit;

                        bool const is_add = (1ull << bit) & (add >> fixed_t::shift);

                        ssa_ht const next_shift = 
                            shift_amount ? cfg->emplace_ssa(SSA_shl, type, shift, ssa_value_t(shift_amount, TYPE_U)) 
                                         : shift;
                        ssa_ht const next_total = 
                            cfg->emplace_ssa(is_add ? SSA_add : SSA_sub, type, 
                                             total, next_shift, ssa_value_t(!is_add, TYPE_BOOL));

                        shift = next_shift;
                        total = next_total;
                        prev_bit = bit;
                    });

                    // Fractional component:
                    if((add | sub) & ((1ull << fixed_t::shift)-1))
                    {
                        shift = initial;
                        prev_bit = fixed_t::shift;

                        for(unsigned bit = fixed_t::shift-1; bit < fixed_t::shift; --bit)
                        {
                            if(!((add | sub) & (1 << bit)))
                                continue;

                            bool const is_add = (1ull << bit) & add;

                            unsigned const shift_amount = prev_bit - bit;
                            passert(shift_amount, prev_bit, bit);

                            ssa_ht const next_shift = 
                                cfg->emplace_ssa(SSA_shr, type, shift, ssa_value_t(shift_amount, TYPE_U));
                            ssa_ht const next_total = 
                                cfg->emplace_ssa(is_add ? SSA_add : SSA_sub, type, 
                                                 total, next_shift, ssa_value_t(!is_add, TYPE_BOOL));

                            shift = next_shift;
                            total = next_total;
                            prev_bit = bit;
                        }
                    }

                    ssa_it->replace_with(total);
                    goto prune;
                }
                break;

            default:
                break;
            }
        }

        ++ssa_it;
        continue;

    replaceWith0:
        dprint(log, "--SIMPLE_IDENTITY_REPLACE 0", ssa_it, ssa_it->input(0));
        replace_carry(*ssa_it, carry_replacement);
        ssa_it->replace_with(ssa_it->input(0));
        goto prune;

    replaceWith1:
        dprint(log, "--SIMPLE_IDENTITY_REPLACE 1", ssa_it, ssa_it->input(1));
        replace_carry(*ssa_it, carry_replacement);
        ssa_it->replace_with(ssa_it->input(1));
        goto prune;

    prune:
        dprint(log, "--SIMPLE_IDENTITY_PRUNE", ssa_it, ssa_it->op(), ssa_it->handle());
        ssa_it = ssa_it->prune();
        updated = true;
    }

    return updated;
}

namespace // anonymous
{

struct banks_and_indexes_t
{
    std::vector<ssa_value_t> banks;
    std::vector<ssa_value_t> indexes;
    auto operator<=>(banks_and_indexes_t const&) const = default;
};

// 'operands' tracks the operands of our new expression.
struct operand_t 
{ 
    ssa_value_t v; 
    bool negative; 
    banks_and_indexes_t banks_and_indexes;
};

banks_and_indexes_t calc_banks_and_indexes(ssa_ht initial)
{
    fc::small_map<ssa_value_t, unsigned, 8> banks;
    fc::small_map<ssa_value_t, unsigned, 8> indexes;

    cfg_ht const cfg = initial->cfg_node();

    ssa_worklist.push(initial);
    
    unsigned iters = 0;
    constexpr unsigned MAX_ITERS = 8;

    while(!ssa_worklist.empty())
    {
        ++iters;
        ssa_ht h = ssa_worklist.pop();

        bool process_inputs = true;

        if(ssa_banks(h->op()))
        {
            banks[h->input(ssa_bank_input(h->op()))] += 1;
            process_inputs = false;
        }

        if(ssa_indexes8(h->op()))
        {
            indexes[h->input(ssa_index8_input(h->op()))] += 1;
            process_inputs = false;
        }

        if(h->cfg_node() != cfg || h->in_daisy() || h->op() == SSA_phi)
            process_inputs = false;

        // TODO: We could do something better than MAX_ITERS.
        if(process_inputs && iters < MAX_ITERS)
        {
            for(unsigned i = 0; i < h->input_size(); ++i)
            {
                if(h->input(i).holds_ref())
                {
                    assert(h != h->input(i).handle());
                    ssa_worklist.queue(h->input(i).handle());
                }
            }
        }
    }

    // Sort by use count. Most used comes first.
    std::sort(banks.container.begin(), banks.container.end(), [](auto const& l, auto const& r)
        { return l.second > r.second; });
    std::sort(indexes.container.begin(), indexes.container.end(), [](auto const& l, auto const& r)
        { return l.second > r.second; });

    banks_and_indexes_t ret;

    ret.banks.reserve(banks.size());
    ret.indexes.reserve(indexes.size());

    for(auto const& p : banks)
        ret.banks.push_back(p.first);
    for(auto const& p : indexes)
        ret.indexes.push_back(p.first);

    return ret;
}

struct ssa_monoid_d
{
    bitset_uint_t* post_dom = nullptr;
    unsigned total_outputs = 0;
    bool sealed_singleton = false;

    // A 'compatible' node only outputs to the same 'defining_op', of the same type.
    bool compatible = false;

    // Used only for 'dfs_sort':
    operand_t* operand = nullptr;
};

// Sorts nodes based on DFS order (postorder).
// Reset marks before calling.
void dfs_sort(std::vector<operand_t>& result, cfg_ht in_cfg, ssa_ht h)
{
    ssa_node_t& node = *h;

    if(node.get_mark() == MARK_PERMANENT)
        return;

    if(node.op() != SSA_phi)
    {
        unsigned const input_size = node.input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t const input = node.input(i);
            if(input.holds_ref() && input->cfg_node() == in_cfg)
                dfs_sort(result, in_cfg, input.handle());
        }
    }

    if(node.get_mark() == MARK_TEMPORARY)
        result.push_back(std::move(*h.data<ssa_monoid_d>().operand));

    node.set_mark(MARK_PERMANENT);
}

// 'run_monoid_t' searches for connected nodes of similar ops, where the op forms a monoid.
// For example, it searches for expressions like (1 | X | Y | 5 | X) or (X + 10 - Y - 3).
// Then, it optimizes these nodes, pulling out constants and merging duplicated operands.
class run_monoid_t
{
public:
    run_monoid_t(log_t* log, ir_t& ir);

    static ssa_op_t defining_op(ssa_op_t op);

    ssa_monoid_d& data(ssa_ht h) { return h.data<ssa_monoid_d>(); }

    void mark_singleton(ssa_ht h) 
    { 
        assert(!data(h).sealed_singleton);
        data(h).sealed_singleton = true;
        singletons.push_back(h);
    }

    template<bool Initial = true>
    void build(ssa_op_t def_op, ssa_ht h, bool negative);

    bool updated = false;
private:
    struct leaf_count_t
    {
        int pos;
        int neg;

        int total() const { return pos + neg; }
        int diff() const { return pos - neg; }
    };

    void accumulate(ssa_op_t def_op, fixed_t const f, bool negative)
    {
        // Update 'accum':
        switch(def_op)
        {
        case SSA_add: 
            if(negative)
                accum.value -= f.value;
            else
                accum.value += f.value;
            break;

        case SSA_and: 
            accum.value &= f.value;
            break;

        case SSA_or:
            accum.value |= f.value;
            break;

        case SSA_xor: 
            accum.value ^= f.value; 
            break;

        default: 
            assert(0); 
            break;
        }
        dprint(log, "----MONOID_ACCUMULATE", f.value >> fixed_t::shift, accum.value >> fixed_t::shift);
    }

    //////////////////////////
    // Used before 'build': //
    //////////////////////////

    // Bitsets track post dominance in 'ssa_ht's
    std::size_t bs_size = 0;
    array_pool_t<bitset_uint_t> bs_pool;

    // Singletons aren't post-dominated, meaning they're a root of the expression.
    bc::small_vector<ssa_ht, 32> singletons;

    //////////////////////////
    // Used inside 'build': //
    //////////////////////////

    // Tracks the accumulation of all constant nums:
    fixed_t accum;

    // The root of the expression (a singleton)
    ssa_ht root;

    // Counts leaf nodes of our expression.
    // 'compatible_leafs' may or may not convert to 'incompatible_leafs' later on,
    // which is used to optimize unnecessary shared expressions like:
    // a = x + 10
    // b = a + 20
    // c = a + 30
    // ... optimizes to become:
    // b = x + 30
    // c = x + 40
    fc::small_map<ssa_value_t, leaf_count_t, 16> incompatible_leafs;
    fc::small_map<ssa_value_t, leaf_count_t, 16> compatible_leafs;

    // Counts how often a node appears during 'build'.
    using internals_map_t = fc::vector_map<ssa_ht, unsigned>;
    internals_map_t internals;

    log_t* log;
};

// Whatever op defines the monoid, or SSA_null if the op isn't supported.
ssa_op_t run_monoid_t::defining_op(ssa_op_t op)
{
    switch(op)
    {
    case SSA_add:
    case SSA_sub:
        return SSA_add;
    case SSA_and:
    case SSA_or:
    case SSA_xor:
        return op;
    default:
        return SSA_null;
    }
}

run_monoid_t::run_monoid_t(log_t* log, ir_t& ir)
: log(log)
{
    dprint(log, "MONOID");

    bs_size = bitset_size<>(ssa_pool::array_size());
    bitset_uint_t* const temp_bs = ALLOCA_T(bitset_uint_t, bs_size);

#ifndef NDEBUG
    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        assert(ssa_it->test_flags(FLAG_IN_WORKLIST) == false);
#endif

    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        auto& d = data(ssa_it);
        assert(d.post_dom == nullptr);

        ssa_op_t const def_op = defining_op(ssa_it->op());
        
        // Ignore nodes that don't have an op we care about.
        if(!def_op)
            continue;
        
        // Carries must be constant numbers.
        // Likewise, the node's carry can't be used.
        if(def_op == SSA_add && (!ssa_it->input(2).is_num() || carry_used(*ssa_it)))
            continue;

        d.post_dom = bs_pool.alloc(bs_size);
        assert(bitset_all_clear(bs_size, d.post_dom));

        type_t const type = ssa_it->type();

        // Count this node's outputs
        d.total_outputs = 0;
        unsigned compatible_outputs = 0;
        for_each_output_with_links(ssa_it, [&](ssa_ht from, ssa_ht output)
        {
            ++d.total_outputs;
            if(from == ssa_it && def_op == defining_op(output->op()) && type == output->type())
                if(def_op != SSA_add || (output->input(2).is_num() && !carry_used(*output)))
                    ++compatible_outputs;
        });
        assert(compatible_outputs <= d.total_outputs);

        dprint(log, "-MONOID_INITIAL_TEST", ssa_it, d.total_outputs, compatible_outputs);

        // If a node has an output that isn't compatible (or no outputs), 
        // set its post-dom set to only itself.
        if(d.total_outputs == 0 || d.total_outputs != compatible_outputs)
        {
            dprint(log, "-MONOID_INITIAL_SINGLETON", ssa_it);

            // Treat the node as a singleton.
            bitset_set(d.post_dom, ssa_it.id);
            mark_singleton(ssa_it);

            unsigned const input_size = ssa_it->input_size();
            for(unsigned j = 0; j < input_size; ++j)
                if(ssa_it->input(j).holds_ref())
                    ssa_worklist.push(ssa_it->input(j).handle());
        }
        else
        {
            // Otherwise optimistically assume every node is a post dominator.
            // (We'll then prove those which aren't.)
            bitset_set_all(bs_size, d.post_dom);
            d.compatible = true;
        }
    }
    
    // Now run a data-flow equation until reaching a fixed point,
    // calculating the post-dominator sets:
    while(!ssa_worklist.empty())
    {
        ssa_ht const h = ssa_worklist.pop();
        auto& d = data(h);

        if(d.sealed_singleton || !d.post_dom)
            continue;
        assert(d.total_outputs > 0);

        // Calculate post-dominance by intersecting the sets of this node's outputs.

        bitset_set_all(bs_size, temp_bs);

        unsigned const output_size = h->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            auto const oe = h->output_edge(i);

            if(oe.input_class() == INPUT_LINK)
                continue;

            auto const& output_d = data(oe.handle);

            if(!output_d.post_dom)
            {
                bitset_clear_all(bs_size, temp_bs);
                break;
            }

            assert(defining_op(h->op()) == defining_op(oe.handle->op()));
            passert(output_d.post_dom, h, h->op());
            assert(h->type() == oe.handle->type());

            bitset_and(bs_size, temp_bs, output_d.post_dom);
        }

        if(bitset_all_clear(bs_size, temp_bs))
            mark_singleton(h);
        else
            assert(bitset_popcount(bs_size, temp_bs) != 1 || !bitset_test(temp_bs, h.id));

        // Then set the node's own bit:
        bitset_set(temp_bs, h.id);

        // Continue if 'd.post_dom' isn't changing.
        if(bitset_eq(bs_size, d.post_dom, temp_bs))
            continue;
        bitset_copy(bs_size, d.post_dom, temp_bs);

        // Next, consider inputs, pushing them onto the worklist:
        unsigned const input_size = h->input_size();
        for(unsigned i = 0; i < input_size; ++i)
            if(h->input(i).holds_ref())
                ssa_worklist.push(h->input(i).handle());
    }

    std::vector<operand_t> operands;

    // These are used at the end to replace the expression's old nodes with the new ones:
    std::vector<ssa_value_t> replacements;
    std::vector<internals_map_t> to_prune;

    for(ssa_ht h : singletons)
    {
        dprint(log, "-MONOID_BUILD", h);

        // Build the monoid

        ssa_op_t const def_op = defining_op(h->op());
        cfg_ht const cfg = h->cfg_node();
        type_t const type = h->type();
        fixed_uint_t const mask = numeric_bitmask(type.name());

        root = h;
        incompatible_leafs.clear();
        compatible_leafs.clear();
        internals.clear();
        accum = { (def_op == SSA_and) ? ~0ull : 0ull };
        build<>(def_op, h, false);

        bool uses_accum; // Tracks if we'll use 'accum'
        int carry_req; // Tracks if we need a carry.

    accum_loop:
        {
            // Check 'accum', seeing if it's necessary.
            // Also set 'carry_req' when needed.

            uses_accum = true;
            carry_req = 0;

            accum.value &= mask;

            switch(def_op)
            {
            case SSA_add:
                if(accum.value == low_bit_only(mask))
                {
                    carry_req = 1;
                    uses_accum = false;
                    break;
                }
                else if(accum.value == mask)
                {
                    carry_req = -1;
                    uses_accum = false;
                    break;
                }
                // fall through
            case SSA_or:
            case SSA_xor:
                uses_accum = accum.value != 0ull;
                break;
            case SSA_and:
                uses_accum = accum.value != mask;
                break;
            default:
                assert(0);
                break;
            }

            while(compatible_leafs.size() > 0)
            {
                auto pair = std::move(compatible_leafs.container.back());
                compatible_leafs.container.pop_back();

                assert(pair.first.holds_ref());
                assert(pair.second.total() > 0);

                if(uses_accum && pair.second.total() == 1)
                {
                    build<false>(def_op, pair.first.handle(), pair.second.neg);
                    goto accum_loop;
                }
                else
                {
                    // Note: this breaks sorted property, which we shouldn't need.
                    incompatible_leafs.container.push_back(std::move(pair));
                }
            }
        }

        // Build 'operands' from 'incompatible_leafs':
        operands.clear();
        for(auto const& p : incompatible_leafs)
        {
            ssa_value_t const v = p.first;
            leaf_count_t const count = p.second;
            assert(!v.is_num());
            assert(count.total());

            switch(def_op)
            {
            case SSA_add:
                {
                    // Convert multiple adds into shifts.

                    int const diff = count.diff();

                    ssa_value_t cur = v;
                    for(unsigned bits = std::abs(diff), pow = 0; bits; bits >>= 1, ++pow)
                    {
                        if(!(bits & 1))
                            continue;

                        if(pow)
                        {
                            cur = cfg->emplace_ssa(SSA_shl, type, cur, ssa_value_t(pow, TYPE_U));
                            pow = 0;
                        }

                        operands.push_back({ cur, diff < 0 });
                    }
                }
                break;

            case SSA_and:
            case SSA_or:
                // Ignore duplicates.
                operands.push_back({ p.first });
                break;

            case SSA_xor:
                if(count.pos % 2)
                    operands.push_back({ p.first });
                break;

            default: 
                assert(0); 
                break;
            }
        }

        dprint(log, "--MONOID_ACCUM", accum.value >> fixed_t::shift, uses_accum, carry_req);

        if(operands.size() > 1)
        {
            // Calc banks and indexes for our operands:
            for(operand_t& operand : operands)
                if(operand.v.holds_ref())
                    operand.banks_and_indexes = calc_banks_and_indexes(operand.v.handle());

            // Sort operands, ordering their bank accesses and array indexes.
            std::sort(operands.begin(), operands.end(), [](auto const& l, auto const& r)
                { return l.banks_and_indexes < r.banks_and_indexes; });

            // Further sort:
            std::vector<operand_t> final_sort;
            final_sort.reserve(operands.size());

            for(ssa_ht ssa = cfg->ssa_begin(); ssa; ++ssa)
                ssa->clear_mark();
            for(operand_t& op : operands)
            {
                if(!op.v.holds_ref())
                    continue;
                op.v->set_mark(MARK_TEMPORARY);
                data(op.v.handle()).operand = &op;
            }
            for(operand_t& op : operands)
            {
                if(op.v.holds_ref())
                    dfs_sort(final_sort, cfg, op.v.handle());
                else
                    final_sort.push_back(std::move(op));
            }

            assert(operands.size() == final_sort.size());
                
            unsigned const size = operands.size();
            for(unsigned i = 0; i < size; ++i)
                operands[i] = std::move(final_sort[size - i - 1]);
        }

        dprint(log, "--MONOID_SORTED_BEGIN");
        for(operand_t const& op : operands)
        {
            dprint(log, "---MONOID_SORTED", op.v);
            for(auto const& bank : op.banks_and_indexes.banks)
                dprint(log, "----MONOID_SORTED_BANK", bank);
        }

        // Now use 'operands' to build a replacement for 'h':

        if(def_op == SSA_add)
        {
            auto const step = [&]
            {
                assert(operands.size() >= 2);
                auto& a0 = operands.rbegin()[0];
                auto& a1 = operands.rbegin()[1];

                if(a0.negative)
                {
                    if(a1.negative)
                        goto do_add;
                    else
                    {
                        // We don't have to swap, but doing so ensures that our resulting operand is positive.
                        // This is desirable, to make it likely that our final result will be positive.
                        std::swap(a0, a1);
                        goto do_sub;
                    }
                }
                else
                {
                    if(a1.negative)
                    {
                    do_sub:
                        bool const carry = carry_req != (a0.negative ? 1 : -1);
                        ssa_ht const ssa = cfg->emplace_ssa(SSA_sub, type, a0.v, a1.v, ssa_value_t(carry, TYPE_BOOL));
                        if(!carry)
                            carry_req = 0;
                        operands.pop_back();
                        operands.back() = { ssa, a0.negative };
                    }
                    else
                    {
                    do_add:
                        bool const carry = carry_req == (a0.negative ? -1 : 1);
                        ssa_ht const ssa = cfg->emplace_ssa(SSA_add, type, a0.v, a1.v, ssa_value_t(carry, TYPE_BOOL));
                        if(carry)
                            carry_req = 0;
                        operands.pop_back();
                        operands.back() = { ssa, a0.negative };
                    }
                }
            };

            while(operands.size() > 1)
                step();

            if(operands.size() > 0 && operands[0].negative)
            {
                operands[0].v = cfg->emplace_ssa(SSA_sub, type, ssa_value_t(0u, type.name()), operands[0].v, 
                                                 ssa_value_t(carry_req != -1, TYPE_BOOL));
                operands[0].negative = false;
                if(carry_req == -1)
                    carry_req = 0;
            }

            if(carry_req != 0 || uses_accum)
            {
                operands.push_back({ ssa_value_t(accum, type.name()) });
                carry_req = 0;
                if(operands.size() == 2)
                    step();
            }

            assert(!operands[0].negative);
        }
        else // Not 'SSA_add':
        {
            auto const step = [&]
            {
                ssa_ht const ssa = cfg->emplace_ssa(def_op, type, operands.rbegin()[0].v, operands.rbegin()[1].v);
                operands.pop_back();
                operands.back().v = ssa;
            };

            while(operands.size() > 1)
                step();

            if(uses_accum)
            {
                operands.push_back({ ssa_value_t(accum, type.name()) });
                if(operands.size() == 2)
                    step();
            }
        }

        assert(operands.size() == 1);
        replacements.push_back(operands[0].v);
        to_prune.push_back(std::move(internals));
    }

    // Now replace all singletons and prune:

    assert(singletons.size() == replacements.size());
    assert(singletons.size() == to_prune.size());

    for(unsigned i = 0; i < singletons.size(); ++i)
    {
        if(singletons[i] == replacements[i])
            continue;

        singletons[i]->replace_with(replacements[i]);

        for(auto const& p : to_prune[i])
            p.first->prune();
    }
}

// Builds a monoid
template<bool Initial>
void run_monoid_t::build(ssa_op_t def_op, ssa_ht h, bool negative)
{
    assert(h->type() == root->type());
    assert(defining_op(h->op()) == def_op);

    dprint(log, "---MONOID_BUILD_STEP", h);
    if(Initial)
        internals[h] += 1;

    unsigned const input_size = h->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t input = h->input(i);

        bool const next_negative = (h->op() == SSA_sub && i >= 1) ? !negative : negative;

        if(Initial && input.holds_ref() && data(input.handle()).post_dom && bitset_test(data(input.handle()).post_dom, root.id))
        {
            assert(i != 2);
            assert(defining_op(input->op()) == defining_op(h->op()));
            assert(input->type() == h->type());

            build<Initial>(def_op, h->input(i).handle(), next_negative);
        }
        else 
        {
            if(i == 2)
            {
                assert(input.is_num());
                assert(input.num_type_name() == TYPE_BOOL);
                assert(input.whole() <= 1);

                type_name_t const type_name = h->type().name();

                // Convert the carry into the h's type.
                if(!!input.whole() != (h->op() == SSA_sub))
                    accumulate(def_op, fixed_t{ low_bit_only(numeric_bitmask(type_name)) }, next_negative);

                continue;
            }
            else if(Initial && input.holds_ref() && defining_op(input->op()) == def_op)
            {
                if(data(input.handle()).compatible 
                   && (input->input(0).is_num() || input->input(1).is_num()))
                {
                    dprint(log, "----MONOID_COMPATIBLE_LEAF", input.handle());

                    if(next_negative)
                        compatible_leafs[input].neg += 1;
                    else
                        compatible_leafs[input].pos += 1;

                    continue;
                }
            }

            if(input.is_num())
                accumulate(def_op, input.fixed(), next_negative);
            else if(next_negative)
                incompatible_leafs[input].neg += 1;
            else
                incompatible_leafs[input].pos += 1;
        }
    }
}

} // end anonymous namespace

bool o_identities(log_t* log, ir_t& ir)
{
    auto const simple_repeated = [&]
    {
        bool updated = false;
        while(o_simple_identity(log, ir))
            updated = true;
        return updated;
    };

    bool updated = false;

    updated |= simple_repeated();

    {
        ssa_data_pool::scope_guard_t<ssa_monoid_d> sg(ssa_pool::array_size());
        run_monoid_t run(log, ir);
        updated |= run.updated;

        if(run.updated)
            simple_repeated();
    }

    return updated;
}

