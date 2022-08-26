#include "o_id.hpp"

#include <cstdint>

#include <boost/container/small_vector.hpp>

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

    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it;)
    {
        dprint(log, "-SIMPLE_IDENTITY_OP", ssa_it, ssa_it->op());

        ssa_node_t& node = *ssa_it;
        ssa_value_t carry_replacement = {};

        auto const replace_carry = [log](ssa_node_t& node, ssa_value_t carry_replacement)
        {
            assert(!carry_output(node) || carry_replacement);
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

        switch(node.op())
        {
        case SSA_write_array:
            {
                using namespace ssai::array;

                // Prune pointless writes like foo[5] = foo[5]

                ssa_value_t const assign = node.input(ASSIGNMENT);

                if(assign.holds_ref() && assign->op() == SSA_read_array 
                   && assign->input(ARRAY) == node.input(ARRAY)
                   && assign->input(INDEX) == node.input(INDEX))
                {
                    goto replaceWith0;
                }
            }
            break;

        case SSA_cast:
            {
                ssa_value_t const input = node.input(0);

                type_t const from = input.type();
                type_t const to = node.type();

                // Prune casts from A -> A
                if(from == to)
                    goto replaceWith0;

                // Simplify chains of casts from similar types A -> B
                if(input.holds_ref() && input->op() == SSA_cast
                   && same_scalar_layout(from.name(), to.name()))
                {
                    dprint(log, "--SIMPLE_IDENTITY_SIMPLIFY_CAST");
                    node.link_change_input(0, input->input(0));
                }
            }
            break;

        case SSA_lt:
        case SSA_not_eq:
            if(node.input(0) == node.input(1))
            {
                node.replace_with(ssa_value_t(0u, TYPE_BOOL));
                goto prune;
            }
            else if(node.op() == SSA_lt)
            {
                // Replace expressions like (X < 1) with (X == 0).
                // This aids code generation.

                type_name_t const lt = node.input(0).type().name();
                type_name_t const rt = node.input(1).type().name();

                if(!same_scalar_layout(lt, rt))
                    break;

                if(node.input(1).eq_fixed({ type_min(lt) + type_unit(lt) }))
                {
                    node.link_change_input(1, ssa_value_t(type_min(rt), rt));
                    node.unsafe_set_op(SSA_eq);
                }
            }
            break;

        case SSA_lte:
        case SSA_eq:
            if(node.input(0) == node.input(1))
            {
                node.replace_with(ssa_value_t(1u, TYPE_BOOL));
                goto prune;
            }
            else if(node.op() == SSA_lte)
            {
                // Replace expressions like (1 <= x) with (X != 0).
                // This aids code generation.

                type_name_t const lt = node.input(0).type().name();
                type_name_t const rt = node.input(1).type().name();

                if(!same_scalar_layout(lt, rt))
                    break;

                if(node.input(0).eq_fixed({ type_min(rt) + type_unit(rt) }))
                {
                    node.link_change_input(0, ssa_value_t(type_min(lt), lt));
                    node.unsafe_set_op(SSA_not_eq);
                }
            }
            break;

        default:
            break;
        }

        if(is_arithmetic(ssa_it->type().name()))
        {
            fixed_t const all_set = { numeric_bitmask(node.type().name()) };

            auto const add_sub_impl = [&all_set](ssa_value_t v, fixed_t carry, bool sub) -> ssa_value_t
            {
                if(!v.is_num())
                    return {};

                // Put the carry in the lowest bit.
                if(carry.value)
                    carry.value = low_bit_only(all_set.value);

                if((v.fixed().value + carry.value) & all_set.value)
                    return {};

                return ssa_value_t(!!(v.fixed().value + carry.value) != sub, TYPE_BOOL);
            };

            switch(node.op())
            {
            case SSA_add:
                {
                    if(!node.input(2).is_num())
                        break;

                    if((carry_replacement = add_sub_impl(node.input(0), node.input(2).fixed(), false)))
                        goto replaceWith1;
                    if((carry_replacement = add_sub_impl(node.input(1), node.input(2).fixed(), false)))
                        goto replaceWith0;
                }
                break;

            case SSA_sub:
                {
                    if(!node.input(2).is_num())
                        break;
                    if((carry_replacement = add_sub_impl(node.input(1), node.input(2).fixed(), true)))
                        goto replaceWith0;
                }
                break;

            case SSA_or:
                if(node.input(0) == node.input(1))
                    goto replaceWith0;
                // fall through
            case SSA_xor:
                if(node.input(0).eq_fixed({0}))
                    goto replaceWith1;
                // fall through
            case SSA_shl:
            case SSA_shr:
                if(node.input(1).eq_fixed({0}))
                    goto replaceWith0;
                break;

            case SSA_and:
                if(node.input(0).eq_fixed(all_set))
                    goto replaceWith1;
                if(node.input(1).eq_fixed(all_set))
                    goto replaceWith0;
                if(node.input(0) == ssa_it->input(1))
                    goto replaceWith0;
                break;

            default:
                break;
            }
        }

        ++ssa_it;
        continue;

    replaceWith0:
        dprint(log, "--SIMPLE_IDENTITY_REPLACE 0", ssa_it, node.input(0));
        replace_carry(node, carry_replacement);
        node.replace_with(node.input(0));
        goto prune;

    replaceWith1:
        dprint(log, "--SIMPLE_IDENTITY_REPLACE 1", ssa_it, node.input(1));
        replace_carry(node, carry_replacement);
        node.replace_with(node.input(1));
        goto prune;

    prune:
        dprint(log, "--SIMPLE_IDENTITY_PRUNE", ssa_it);
        ssa_it = node.prune();
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

banks_and_indexes_t calc_banks_and_indexes(ssa_ht initial)
{
    fc::small_map<ssa_value_t, unsigned, 8> banks;
    fc::small_map<ssa_value_t, unsigned, 8> indexes;

    cfg_ht const cfg = initial->cfg_node();

    ssa_worklist.push(initial);
    
    while(!ssa_worklist.empty())
    {
        ssa_ht h = ssa_worklist.pop();

        bool process_inputs = true;

        if(ssa_banks(h->op()))
        {
            banks[h->input(ssa_bank_input(h->op()))] += 1;
            process_inputs = false;
        }

        if(ssa_indexes(h->op()))
        {
            indexes[h->input(ssa_index_input(h->op()))] += 1;
            process_inputs = false;
        }

        if(h->cfg_node() != cfg || h->in_daisy() || h->op() == SSA_phi)
            process_inputs = false;

        if(process_inputs)
            for(unsigned i = 0; i < h->input_size(); ++i)
                if(h->input(i).holds_ref())
                    ssa_worklist.push(h->input(i).handle());
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
};

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

    void build(ssa_ht h, bool negative);

    bool updated = false;
private:
    struct leaf_count_t
    {
        int pos;
        int neg;

        int total() const { return pos + neg; }
        int diff() const { return pos - neg; }
    };

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

    // The root of the expression (a singleton)
    ssa_ht root;

    // Counts leaf nodes of our expression.
    fc::small_map<ssa_value_t, leaf_count_t, 16> leafs;

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
        
        // Carries must be constant numbers:
        if(def_op == SSA_add && !ssa_it->input(2).is_num())
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
                ++compatible_outputs;
        });
        assert(compatible_outputs <= d.total_outputs);

        // If a node has an output that isn't compatible (or no outputs), 
        // set its post-dom set to only itself.
        if(d.total_outputs == 0 || d.total_outputs != compatible_outputs)
        {
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

            assert(defining_op(h->op()) == defining_op(oe.handle->op()));
            assert(output_d.post_dom);
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

    // Sanity checks
#ifndef NDEBUG
    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        assert(ssa_it->test_flags(FLAG_IN_WORKLIST) == false);
        
        auto& d = data(ssa_it);
        if(!d.post_dom)
            continue;

        assert(d.sealed_singleton == (bitset_popcount(bs_size, d.post_dom) == 1));

        unsigned count = 0;
        bitset_for_each(bs_size, d.post_dom, [&](unsigned bit)
        {
            if(data(ssa_ht{bit}).sealed_singleton)
                ++count;
        });

        assert(count == 1);
    }
#endif

    // 'operands' tracks the operands of our new expression.
    struct operand_t 
    { 
        ssa_value_t v; 
        bool negative; 
        banks_and_indexes_t banks_and_indexes;
    };
    std::vector<operand_t> operands;

    // These are used at the end to replace the expression's old nodes with the new ones:
    std::vector<ssa_value_t> replacements;
    std::vector<internals_map_t> to_prune;

    for(ssa_ht h : singletons)
    {
        dprint(log, "-MONOID_BUILD", h);

        // Build the monoid

        root = h;
        leafs.clear();
        internals.clear();
        build(h, false);

        ssa_op_t const def_op = defining_op(h->op());
        type_t const type = h->type();
        cfg_ht const cfg = h->cfg_node();

        // Calculate the constant while building 'operands':

        operands.clear();

        unsigned num_nums = 0; // Counts how many constant numbers are in leafs.
        fixed_t accum = { (def_op == SSA_and) ? ~0ull : 0ull }; // Combination of constant numbers in leafs

        for(auto const& p : leafs)
        {
            ssa_value_t const v = p.first;
            leaf_count_t const count = p.second;
            assert(count.total());

            if(v.is_num())
            {
                // Update 'accum':

                fixed_t const f = v.fixed();

                switch(def_op)
                {
                case SSA_add: accum.value += f.value * count.pos - f.value * count.neg; break;
                case SSA_and: accum.value &= f.value; break;
                case SSA_or:  accum.value |= f.value; break;
                case SSA_xor: accum.value ^= f.value; break;
                default: assert(0); break;
                }

                ++num_nums;
            }
            else
            {
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
                    // An even number of XORs cancels out.
                    if(count.pos % 2)
                        operands.push_back({ p.first });
                    else
                        ++num_nums; // Treat it as a 0.
                    break;

                default: 
                    assert(0); 
                    break;
                }
            }
        }

        int carry_req = 0; // Tracks if we need a carry.
        if(num_nums)
        {
            // Check 'accum', seeing if it's necessary.
            // Also set 'carry_req' when needed.

            fixed_uint_t const mask = numeric_bitmask(type.name());

            accum.value &= mask;

            if(!operands.empty())
            {
                switch(def_op)
                {
                case SSA_add:
                    if(accum.value == low_bit_only(mask))
                    {
                        carry_req = 1;
                        goto skip_num;
                    }
                    else if(accum.value == mask)
                    {
                        carry_req = -1;
                        goto skip_num;
                    }
                    // fall through
                case SSA_or:
                case SSA_xor:
                    if(accum.value == 0ull)
                        goto skip_num;
                    break;
                case SSA_and:
                    if(accum.value == mask)
                        goto skip_num;
                    break;
                default:
                    assert(0);
                    break;
                }
            }

            // It's needed; add it to 'operands'.
            operands.push_back({ ssa_value_t(accum, type.name()) });
        skip_num:;
        }

        dprint(log, "--MONOID_ACCUM", accum.value, num_nums);

        // Calc banks and indexes for our operands:
        for(operand_t& operand : operands)
        {
            if(!operand.v.holds_ref())
                continue;
            operand.banks_and_indexes = calc_banks_and_indexes(operand.v.handle());
        }

        // Sort operands, ordering their bank accesses and array indexes.
        std::sort(operands.begin(), operands.end(), [](auto const& l, auto const& r)
            { return l.banks_and_indexes < r.banks_and_indexes; });

        dprint(log, "--MONOID_SORTED_BEGIN");
        for(operand_t const& op : operands)
        {
            dprint(log, "---MONOID_SORTED", op.v);
            for(auto const& bank : op.banks_and_indexes.banks)
                dprint(log, "----MONOID_SORTED_BANK", bank);
        }

        // Now use 'operands' to build a replacement for 'h':

        assert(!operands.empty());

        if(def_op == SSA_add)
        {
        loop:
            while(operands.size() > 1)
            {
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
            }

            if(operands[0].negative)
            {
                operands[0].v = cfg->emplace_ssa(SSA_sub, type, ssa_value_t(0u, type.name()), operands[0].v, 
                                                 ssa_value_t(carry_req != -1, TYPE_BOOL));
                operands[0].negative = false;
                if(carry_req == -1)
                    carry_req = 0;
            }

            if(carry_req != 0)
            {
                operands.push_back({ ssa_value_t(accum, type.name()) });
                carry_req = 0;
                goto loop;
            }
        }
        else // Not 'SSA_add':
        {
            while(operands.size() > 1)
            {
                ssa_ht const ssa = cfg->emplace_ssa(def_op, type, operands.rbegin()[0].v, operands.rbegin()[1].v);
                operands.pop_back();
                operands.back().v = ssa;
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
void run_monoid_t::build(ssa_ht h, bool negative)
{
    dprint(log, "---MONOID_BUILD_STEP", h);
    internals[h] += 1;

    unsigned const input_size = h->input_size();
    for(unsigned i = 0; i < input_size; ++i)
    {
        ssa_value_t input = h->input(i);

        bool const next_negative = (h->op() == SSA_sub && i >= 1) ? !negative : negative;

        if(input.holds_ref() && data(input.handle()).post_dom && bitset_test(data(input.handle()).post_dom, root.id))
        {
            assert(i != 2);
            assert(defining_op(input->op()) == defining_op(h->op()));
            assert(input->type() == h->type());

            build(h->input(i).handle(), next_negative);
        }
        else
        {
            if(i == 2)
            {
                assert(input.is_num());
                assert(input.num_type_name() == TYPE_BOOL);
                assert(input.whole() <= 1);

                type_name_t const type_name = h->type().name();

                // Convert the carry into h's type.
                if(!!input.whole() != (h->op() == SSA_sub))
                {
                    fixed_uint_t const carry_bit = low_bit_only(numeric_bitmask(type_name));
                    input = ssa_value_t(fixed_t{ carry_bit }, type_name);
                }
                else
                    input = ssa_value_t(0, type_name);
            }

            if(next_negative)
                leafs[input].neg += 1;
            else
                leafs[input].pos += 1;
        }
    }
}

} // end anonymous namespace

bool o_identities(log_t* log, ir_t& ir)
{
    bool updated = o_simple_identity(log, ir);

    {
        ssa_data_pool::scope_guard_t<ssa_monoid_d> sg(ssa_pool::array_size());
        run_monoid_t run(log, ir);
        updated |= run.updated;
    }

    return updated;
}

