#include "o_loop.hpp"

#include <cstdint>
#include <vector>
#include <iostream> // TODO

#include <boost/container/small_vector.hpp>

#include "robin/map.hpp"
#include "robin/hash.hpp"

#include "ir.hpp"
#include "ir_algo.hpp"
#include "ir_util.hpp"
#include "worklist.hpp"
#include "guard.hpp"

namespace bc = ::boost::container;

/*
for each value inside loop
{
    - if an output is used outside of the loop, the loop order matters
    - if an input is used inside the loop, 


    - PHIs mark the point of re-use
}

// 1. tag induction variables
// 2. check outputs o i

////////////////////////////
// TODO
////////////////////////////

// TODO
enum loop_dep_type_t : std::uint8_t
{
    LD_NONE,
    LD_RAW,
    LD_WAW,
    LD_WAR,
};

// TODO
enum loop_dir_t : std::uint8_t
{
    DIR_EQ,
    DIR_NEG,
    DIR_POS,
};
*/ 

namespace // anonymous
{

// "iv" means "induction variable".

struct iv_t;

struct iv_base_t
{
    virtual ~iv_base_t() {};

    virtual ssa_ht ssa(bool phi) const = 0;
    virtual bool has_ssa(ssa_ht match) const = 0;
    virtual ssa_value_t make_init(cfg_ht cfg) const = 0;
    virtual iv_t& root() = 0;
    virtual bool const_init(fixed_sint_t& result) const = 0;
    virtual bool transform(fixed_sint_t& result) const = 0;

    bool value_dep = false;
    bool order_dep = false;

    std::vector<iv_base_t*> children;
};

struct iv_t : public iv_base_t
{
    ssa_value_t init = {};
    ssa_ht phi = {};
    ssa_ht arith = {};
    ssa_value_t operand = {};
    bool arith_to_phi_input = 0;
    std::uint32_t entry_inputs = 0;

    virtual ssa_ht ssa(bool phi) const { return phi ? this->phi : arith; }
    virtual bool has_ssa(ssa_ht match) const { return phi == match || arith == match; }
    virtual ssa_value_t make_init(cfg_ht cfg) const { return init; }

    virtual iv_t& root() { return *this; }

    virtual bool const_init(fixed_sint_t& result) const 
    {
        if(init.is_num())
        {
            result = init.signed_fixed();
            return true;
        }
        return false;
    }

    virtual bool transform(fixed_sint_t& result) const { return true; }

    bool plus() const { return arith->op() == SSA_add; }
    int sign() const { return arith->op() == SSA_add ? 1 : -1; }
    ssa_value_t non_neg() const 
    { 
        if(!operand.is_num())
            return {};
        fixed_sint_t const f = to_signed(operand.fixed().value, operand.num_type_name()) * sign();
        if(f >= 0)
            return ssa_value_t(fixed_t{f}, operand.num_type_name());
        return {};
    }
};

struct mutual_iv_t : public iv_base_t
{
    iv_base_t* parent = nullptr;
    ssa_ht arith = {};
    unsigned arith_to_parent_input = 0;

    virtual ssa_ht ssa(bool phi) const { return arith; }
    virtual bool has_ssa(ssa_ht match) const { return arith == match; }
    virtual ssa_value_t make_init(cfg_ht cfg) const
    {
        std::cout << "MAKE INIT" << arith << std::endl;
        assert(cfg);

        ssa_value_t const parent_init = parent->make_init(cfg);

        ssa_ht init = cfg->emplace_ssa(arith->op(), arith->type());
        for(unsigned i = 0; i < arith->input_size(); ++i)
        {
            if(i == arith_to_parent_input)
                init->link_append_input(parent_init);
            else
                init->link_append_input(arith->input(i));
        }

        return init;
    }

    virtual bool const_init(fixed_sint_t& result) const 
    {
        if(!parent->const_init(result))
            return false;
        return transform_step(result);
    }

    virtual bool transform(fixed_sint_t& result) const 
    {
        if(!parent->transform(result))
            return false;
        return transform_step(result);
    }

    bool transform_step(fixed_sint_t& result) const 
    {
        switch(arith->op())
        {
        case SSA_add:
        case SSA_sub:
            {
                if(!arith->input(2).eq_whole(arith->op() == SSA_sub))
                    return false;
                ssa_value_t const operand = arith->input(!arith_to_parent_input);
                if(!operand.is_num())
                    return false;

                if(arith->op() == SSA_add)
                    result += operand.signed_fixed();
                else if(arith_to_parent_input == 0)
                    result -= operand.signed_fixed();
                else
                    result = operand.signed_fixed() - result;
            }
            // fall-through
        case SSA_cast:
            result &= numeric_bitmask(arith->type().name());
            result = to_signed(result, arith->type().name());
            return true;

        default:
            assert(false);
            return false;
        }
    }

    virtual iv_t& root() { return parent->root(); }
};

/* TODO
struct bounded_loop_t
{
    iv_base_t* iv;
    ssa_ht comparison;
    ssa_ht branch;
    std::uint64_t iterations;
};
    */

struct header_d
{
    ssa_ht simple_condition = {};
    ssa_ht simple_branch = {};
    unsigned simple_condition_iv_i = 0;

    iv_base_t* iv = nullptr;

    bool is_do = false;

    fixed_sint_t init = 0;
    fixed_sint_t compare_with = 0;
    fixed_sint_t end = 0;
    fixed_sint_t increment = 0;
    fixed_sint_t iterations = 0;
};

// iv = induction variable
thread_local std::deque<iv_t> ivs;
thread_local std::deque<mutual_iv_t> mutual_ivs;
//thread_local std::vector<bounded_loop_t> bounded_loops;
thread_local std::vector<header_d> header_data_vec;

struct cfg_loop_d
{
    // If the iterations can occur in any order:
    // TODO
    //bool parallel;

    // 

    // TODO: is this used?
    //bc::small_vector<unsigned, 1> ivs; // indexes into global 'ivs' vector.
    //bc::small_vector<iv_t, 1> mutual_ivs;

    // For headers only:
};

struct ssa_loop_d
{
    bc::small_vector<iv_base_t*, 1> ivs;
};

cfg_loop_d& data(cfg_ht cfg) { return cfg.data<cfg_loop_d>(); }
ssa_loop_d& data(ssa_ht ssa) { return ssa.data<ssa_loop_d>(); }
header_d& header_data(cfg_ht cfg) 
{ 
    assert(algo(cfg).is_loop_header);
    return header_data_vec[algo(cfg).header_i];
}

// Includes nested loops - this isn't always what you want.
bool def_in_loop(cfg_ht loop_header, ssa_value_t v)
{
    return v.holds_ref() && loop_is_parent_of(loop_header, v->cfg_node());
};

struct step_t
{
    ssa_ht init;
    ssa_ht phi;
    ssa_ht arith;
};

step_t reduce(cfg_ht header, cfg_ht step_cfg, iv_t& root, type_t type, ssa_op_t init_op, ssa_op_t arith_op)
{
    unsigned const input_size = header->input_size();
    for(unsigned j = 0; j < input_size; ++j)
        if(!(root.entry_inputs & (1 << j)))
            step_cfg = dom_intersect(step_cfg, header->input(j));

    if(this_loop_header(step_cfg) != header)
        step_cfg = header;

    cfg_ht const step_init_cfg = algo(header).idom;
    assert(step_init_cfg);
    // TODO: remove?
    //assert(!init.holds_ref() || dominates(step_init_cfg, init->cfg_node()));
    //assert(!iv.init.holds_ref() || dominates(step_init_cfg, iv.init->cfg_node()));

    step_t step = {};
    step.init = step_init_cfg->emplace_ssa(init_op, type);
    step.phi = header->emplace_ssa(SSA_phi, type);
    step.arith = step_cfg->emplace_ssa(arith_op, type);

    // Setup 'step.phi':
    for(unsigned j = 0; j < input_size; ++j)
    {
        if(root.entry_inputs & (1 << j))
            step.phi->link_append_input(step.init);
        else
            step.phi->link_append_input(step.arith);
    }

    return step;
}

bool has_order_dep(cfg_ht header, iv_base_t& def, ssa_ht ssa)
{
        std::cout << "HAS ORDER" << ssa << ssa->op() << std::endl;
    if(ssa->test_flags(FLAG_PROCESSED) || ssa->in_daisy())
        return true;

    assert(!(ssa_flags(ssa->op()) & SSAF_CONDITIONAL));
       
    if(!def_in_loop(header, ssa))
        return true;

    ssa->set_flags(FLAG_PROCESSED);
    auto guard = make_scope_guard([&]{ ssa->clear_flags(FLAG_PROCESSED); });

    unsigned const output_size = ssa->output_size();
    for(unsigned i = 0; i < output_size; ++i)
        if(has_order_dep(header, def, ssa->output(i)))
            return true;

    return false;
}

bool try_reduce(cfg_ht header, iv_base_t& def, bool is_phi, unsigned depth = 1)
{
    constexpr unsigned MAX_DEPTH = 4;
    if(depth >= MAX_DEPTH)
        return false;

    bool updated = false;

    ssa_ht const def_ssa = def.ssa(is_phi);
    type_t const type = def_ssa->type();

    iv_t& root = def.root();

    for(unsigned i = 0; i < def_ssa->output_size();)
    {
        auto oe = def_ssa->output_edge(i);

        if(oe.handle == root.ssa(!is_phi))
            goto next_iter;

        if(!def_in_loop(header, oe.handle))
        {
            def.value_dep = true;
            def.order_dep = true;
            goto next_iter;
        }

        switch(oe.handle->op())
        {
        default:
            if(oe.handle == header_data(header).simple_condition)
                goto next_iter;

            break;

            /* TODO
        //case SSA_read_array8:
        case SSA_write_array8:
            {
                using namespace ssai::array;

                if(oe.index == INDEX)
                {
                    // Pattern match.

                    // - input is phi in loop header
                    // - output is same phi in loop header
                    // - all read arrays in loop use same index

                    ssa_value_t const array = oe.handle->input(ARRAY);
                    if(!array.holds_ref() || array->cfg_node() != header || array->op() != SSA_phi)
                        break;
                }
            }
            break;
            */

        case SSA_cast:
            if(is_arithmetic_bijection(type.name(), oe.handle->type().name()))
            {
            make_new_iv:
                auto& new_iv = mutual_ivs.emplace_back(mutual_iv_t{});
                new_iv.parent = &def;
                new_iv.arith = oe.handle;
                new_iv.arith_to_parent_input = oe.index;
                def.children.push_back(&new_iv);

                data(oe.handle).ivs.push_back(&new_iv);

                updated |= try_reduce(header, new_iv, is_phi, depth + 1);
                def.value_dep |= new_iv.value_dep;
                def.order_dep |= new_iv.order_dep;
                goto next_iter;
            }
            break;

        case SSA_add:
        case SSA_sub:
            {
                // The carry must not be used:
                ssa_value_t const carry = oe.handle->input(2);
                if(!carry.is_num() || !!carry.whole() != (oe.handle->op() == SSA_sub))
                    break;

                // The other argument must be defined outside the loop:
                ssa_value_t const operand = oe.handle->input(!oe.index);
                if(def_in_loop(header, operand))
                    break;

                goto make_new_iv;
            }
            break;

        case SSA_shl:
            if(oe.index == 0 && !def_in_loop(header, oe.handle->input(1)))
            {
                // IV << C

                if(ssa_value_t const inc = def.root().non_neg())
                {
                    step_t step = reduce(header, oe.handle->cfg_node(), def.root(), type, SSA_shl, SSA_add);

                    ssa_value_t cast_inc = inc;
                    if(type.name() != inc.num_type_name())
                        cast_inc = step.init->cfg_node()->emplace_ssa(SSA_cast, type, inc);
                    ssa_ht const add_operand = step.init->cfg_node()->emplace_ssa(SSA_shl, type, cast_inc, oe.handle->input(1));

                    step.init->link_append_input(def.make_init(step.init->cfg_node()));
                    step.init->link_append_input(oe.handle->input(1));

                    step.arith->link_append_input(step.phi);
                    step.arith->link_append_input(add_operand);
                    step.arith->link_append_input(ssa_value_t(0u, TYPE_BOOL));

                    oe.handle->replace_with(is_phi ? step.phi : step.arith);
                    oe.handle->prune(); // TODO
                    updated = true;
                    continue;
                }
            }
            else if(oe.index == 1 && !def_in_loop(header, oe.handle->input(0)))
            {
                // C << IV

                if(ssa_value_t const inc = def.root().non_neg())
                {
                    step_t step = reduce(header, oe.handle->cfg_node(), def.root(), type, SSA_shl, SSA_shl);

                    step.init->link_append_input(oe.handle->input(0));
                    step.init->link_append_input(def.make_init(step.init->cfg_node()));

                    step.arith->link_append_input(step.phi);
                    step.arith->link_append_input(inc);

                    oe.handle->replace_with(is_phi ? step.phi : step.arith);
                    oe.handle->prune(); // TODO
                    updated = true;
                    continue;
                }
            }
            break;
        }

        def.value_dep = true;
        def.order_dep |= has_order_dep(header, def, oe.handle);
    next_iter:
        ++i;
    }

    return updated;
}

bool o_loop_index_rewrite(cfg_ht header, bool is_byteified)
{
    auto& d = header_data(header);
    assert(d.simple_condition);
    assert(d.iterations > 0);

    if(d.iv->order_dep)
        return false; // Can't rewrite if the order matters.

    iv_t& root = d.iv->root();

    if(d.iv->value_dep)
    {
        // We have to keep the IV values the same, 
        // but we can reverse the order and count to 0.

        if(&root != d.iv)
            return false;

        if(d.init != 0)
            return false;

        type_name_t const iv_type = root.phi->type().name();

        // Rewrite 'iv.phi':
        assert(root.entry_inputs);
        bitset_for_each(root.entry_inputs, [&](unsigned input_i)
        {
            assert(root.phi->input(input_i) == root.init);
            root.phi->link_change_input(input_i, ssa_value_t(fixed_t{d.end}, iv_type));
        });

        // Rewrite 'iv.arith':
        root.arith->unsafe_set_op(SSA_sub);
        root.arith->link_change_input(!root.arith_to_phi_input, ssa_value_t(fixed_t{d.increment}, iv_type));
        root.arith->link_change_input(2, ssa_value_t(1, TYPE_BOOL)); // carry
        if(root.arith_to_phi_input)
        {
            root.arith_to_phi_input = 0;
            root.arith->link_swap_inputs(0, 1);
        }

        // Rewrite 'condition':
        d.simple_condition->link_change_input(!d.simple_condition_iv_i, ssa_value_t(0, iv_type));
        d.simple_condition->unsafe_set_op(SSA_not_eq);

        return true;
    }
    else
    {
        // Re-write the loop.

        if(is_byteified && d.iterations >= 256)
            return false; // Not worth the complexity of implementing.

        // Determine the type:
        auto const calc_whole = [](unsigned iterations, bool is_do)
        {
            return 1 + (builtin::rclz((iterations - is_do) | 1) - 1) / 8;
        };
        assert(calc_whole(1, 0) == 1);
        assert(calc_whole(255, 0) == 1);
        assert(calc_whole(255, 1) == 1);
        assert(calc_whole(256, 0) == 2);
        assert(calc_whole(256, 1) == 1);
        assert(calc_whole(257, 0) == 2);
        assert(calc_whole(257, 1) == 2);
        unsigned const whole_bytes = calc_whole(d.iterations, d.is_do);
        assert(whole_bytes > 0);
        type_name_t const iv_type = type_u(whole_bytes, 0);

        // It's stupid to replace with a larger type.
        if(whole_bytes > root.arith->type().size_of())
            return false;

        // No point in optimizing an already good loop:
        if(iv_type == root.phi->type().name() && d.end == 0)
            return false;

        // Rewrite 'iv.phi':
        root.phi->set_type(iv_type);
        assert(root.entry_inputs);
        bitset_for_each(root.entry_inputs, [&](unsigned input_i)
        {
            assert(root.phi->input(input_i) == root.init);
            root.phi->link_change_input(input_i, ssa_value_t(d.iterations & ((1 << whole_bytes * 8) - 1), iv_type));
        });

        // Rewrite 'iv.arith':
        root.arith->set_type(iv_type);
        root.arith->unsafe_set_op(SSA_sub);
        root.arith->link_change_input(!root.arith_to_phi_input, ssa_value_t(1, iv_type));
        root.arith->link_change_input(2, ssa_value_t(1, TYPE_BOOL)); // carry
        if(root.arith_to_phi_input)
        {
            root.arith_to_phi_input = 0;
            root.arith->link_swap_inputs(0, 1);
        }

        // Rewrite 'condition':
        d.simple_condition->link_change_input(!d.simple_condition_iv_i, ssa_value_t(0, iv_type));
        d.simple_condition->unsafe_set_op(SSA_not_eq);

        return true;
    }
}

bool initial_loop_processing(log_t* log, ir_t& ir, bool is_byteified)
{
    bool updated = false;

    ivs.clear();
    mutual_ivs.clear();
    for(cfg_ht header : loop_headers)
    {
        auto const& a = ::algo(header);

        // Don't bother with irreducible headers.
        if(a.is_irreducible)
            continue;

        unsigned const cfg_input_size = header->input_size();
        assert(cfg_input_size >= 2);

        constexpr unsigned MAX_LOOP_INPUT_SIZE = 8;
        if(cfg_input_size > MAX_LOOP_INPUT_SIZE) // Yeah, let's not handle huge input lists.
            continue;

        std::uint32_t entry_inputs = 0;
        std::uint32_t reentry_inputs = 0;
        for(unsigned i = 0; i < cfg_input_size; ++i)
        {
            cfg_ht const input = header->input(i);
            if(loop_is_parent_of(header, input))
                reentry_inputs |= 1 << i;
            else
                entry_inputs |= 1 << i;
        }

        passert(entry_inputs, header, entry_inputs, cfg_input_size);
        passert(reentry_inputs, header, reentry_inputs, cfg_input_size);
        passert(builtin::popcount(entry_inputs) != cfg_input_size, entry_inputs, cfg_input_size);

        // Look for a simple loop condition:
        if(builtin::popcount(reentry_inputs) == 1)
        {
            unsigned const reentry_i = builtin::ctz(reentry_inputs);
            cfg_ht const reentry = header->input(reentry_i);
            cfg_ht branch_cfg;

            if(header->output_size() == 2 && reentry->output_size() == 1)
                branch_cfg = header; // simple 'while' loop
            else if(header->output_size() == 1 && reentry->output_size() == 2)
                branch_cfg = reentry; // simple 'do' loop

            if(branch_cfg)
            {
                ssa_ht const branch = branch_cfg->last_daisy();
                if(branch && branch->op() == SSA_if && header == this_loop_header(branch->cfg_node()))
                {
                    ssa_value_t const condition = branch->input(0);
                    if(condition.holds_ref() 
                       && condition->output_size() == 1 
                       && (condition->op() == SSA_not_eq
                           || condition->op() == SSA_lt
                           || condition->op() == SSA_lte)
                       && header == this_loop_header(condition->cfg_node()))
                    {
                        assert(condition->input_size() == 2);
                        bool const iv_index = def_in_loop(header, condition->input(1));
                        if(def_in_loop(header, condition->input(0)) != iv_index)
                        {
                            // Found a simple loop condition:
                            header_data(header).simple_condition = condition.handle();
                            header_data(header).simple_branch = branch;
                            header_data(header).simple_condition_iv_i = iv_index;
                        }
                    }
                }
            }
        }

        // Look for induction variables:
        for(ssa_ht phi = header->phi_begin(); phi; ++phi)
        {
            ssa_value_t init = {};
            ssa_value_t arith = {};

            unsigned const input_size = phi->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t const input = phi->input(i);
                assert(input);

                if(entry_inputs & (1 << i))
                {
                    if(init && init != input)
                        continue;
                    init = input;
                }
                else
                {
                    if(arith && arith != input)
                        continue;
                    arith = input;
                }
            }

            // OK! We have 'init' and 'arith'.
            // Now pattern match to see if they are suitable.

            // 'init' must be defined outside the loop.
            if(def_in_loop(header, init))
                continue;

            // 'arith' must be a SSA handle:
            if(!arith.holds_ref())
                continue;

            // 'arith' should exist inside the loop:
            if(this_loop_header(arith->cfg_node()) != header)
                continue;

            // For now, 'arith' must be + or -.
            if(arith->op() != SSA_add && arith->op() != SSA_sub)
                continue;

            // The carry must not be used:
            ssa_value_t const carry = arith->input(2);
            if(!carry.is_num() || !!carry.whole() != (arith->op() == SSA_sub))
                continue;

            // 'arith' must use 'phi' as an argument:
            unsigned phi_input = 0;
            unsigned const search_to = arith->op() == SSA_add ? 2 : 1;
            for(; phi_input < search_to; ++phi_input)
                if(arith->input(phi_input) == phi)
                   goto found_phi_input;
            continue;
        found_phi_input:

            // The other argument must be defined outside the loop:
            ssa_value_t const operand = arith->input(!phi_input);
            if(def_in_loop(header, operand))
                continue;

            // TODO
            //data(header).ivs.push_back(ivs.size());

            auto& new_iv = ivs.emplace_back(iv_t{});
            new_iv.init = init;
            new_iv.phi = phi;
            new_iv.arith = arith.handle();
            new_iv.operand = operand;
            new_iv.arith_to_phi_input = phi_input;
            new_iv.entry_inputs = entry_inputs;

            data(phi).ivs.push_back(&new_iv);
            data(arith.handle()).ivs.push_back(&new_iv);
        }
    }

    // Perform strength reductions and collect mutual_ivs:
    for(iv_t& iv : ivs)
    {
        cfg_ht const header = iv.phi->cfg_node();

        updated |= try_reduce(header, iv, false);
        updated |= try_reduce(header, iv, true);
    }

    // Now finish the identification of simple loops:
    for(cfg_ht header : loop_headers)
    {
        auto& d = header_data(header);
        {
            if(!d.simple_condition)
                continue;

            // Find the 'iv' going into the condition.

            ssa_value_t const iv_ssa = d.simple_condition->input(d.simple_condition_iv_i);
            assert(iv_ssa.holds_ref());

            iv_t* root;
            for(iv_base_t* iv : data(iv_ssa.handle()).ivs)
            {
                assert(iv->has_ssa(iv_ssa.handle()));
                root = &iv->root();
                if(root->phi->cfg_node() == header)
                {
                    d.iv = iv;
                    goto found_iv;
                }
            }
            goto fail;
        found_iv:

            // Needs a constant starting point:
            if(!d.iv->const_init(d.init))
                goto fail;

            // Need a constant increment:
            if(!root->operand.is_num())
                goto fail;
            d.increment = root->operand.signed_fixed();

            ssa_value_t const compare_with = d.simple_condition->input(!d.simple_condition_iv_i);

            // Must be comparing with a constant:
            if(!compare_with.is_num())
                goto fail;
            d.compare_with = compare_with.signed_fixed();

            d.is_do = (d.simple_branch->cfg_node() == root->arith->cfg_node() 
                       || !dominates(d.simple_branch->cfg_node(), root->arith->cfg_node()));

            fixed_sint_t const span = d.compare_with - d.init;
            d.iterations = span / d.increment;
            fixed_sint_t const remainder = span % d.increment;

            if(d.iterations <= 0)
                goto fail;

            switch(d.simple_condition->op())
            {
            default: 
                goto fail;
            case SSA_lte:
                if(remainder == 0)
                    ++d.iterations;
                // fall-through
            case SSA_lt:
                if(d.simple_condition_iv_i == 0) // IV < C
                {
                    if(d.increment < 0 || d.iterations * d.increment + d.init > type_max(compare_with.num_type_name()))
                        goto fail;
                }
                else // C < IV
                {
                    if(d.increment > 0 || d.iterations * d.increment + d.init < type_min(compare_with.num_type_name()))
                        goto fail;
                }
                break;

            case SSA_not_eq:
                // Require increment reaches 'd.compare_with' exactly, without passing it.
                // Technically the value could wrap around and become equivalent eventually,
                // but it's simpler to ignore that and assume no wrap around.
                if(remainder != 0)
                    goto fail;
                break;
            }

            d.end = d.iterations * d.increment + d.init;

            updated |= o_loop_index_rewrite(header, is_byteified);
            continue;
        }
    fail:
        d.simple_condition = d.simple_branch = {};
    }

    return updated;
}

/*
unroll
{
    // we need to know the iteration bound
    // divide that bound evenly
    // estimate code cost

    // - duplicate every node in the loop
    // - attach inputs to old header to new header

    // what order should we process?
    // - innermost to outermost

    // do we need to recalc?
    // - probably not, just set iloopheader appropriately
}
*/

/*
static bool o_loop_fusion(log_t* log, ir_t& ir)
{
    struct header_d
    {
        bool can_fuse = true;
        cfg_ht exit = {};
    };

    std::vector<header_d> header_data(loop_headers.size());

    // 1. identify loops with a single entry and exit
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_ht const header = this_loop_header(cfg_it);

        if(!header)
            continue;

        unsigned const output_size = cfg_it->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht const output = cfg_it->output(i);

            if(loop_is_parent_of(header, output))
                continue;

            cfg_ht const output_header = this_loop_header(cfg_it->output(i));

            while(cfg_ht h = header; h && h != output_header; h = algo(h).iloop_header)
            {
                auto& d = TODO;
                if(d.exit)
                    d.can_fuse = false;
                else
                    d.exit = cfg_it;
            }
        }
    }

    // 2. for those loops, check if they are adjacent

    // 3. for those loops, check if they have equivalent IVs

    // 4. check if dependencies exist


    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        if(!this_loop_header(cfg_it))
            continue;

        for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
        {
            if(ssa_it->in_daisy())
            {
                goto fail;
            }

            if(


        }


    fail:
    }

    for(cfg_ht header : loop_headers)
    {

    }

    for(iv_t& iv : ivs)
    {

    }
}
*/

} // end anonymous namespace

//////////
// LOOP //
//////////

bool o_loop(log_t* log, ir_t& ir, bool is_byteified)
{
    build_loops_and_order(ir);
    build_dominators_from_order(ir);

    header_data_vec.clear();
    header_data_vec.resize(loop_headers.size());

    for(cfg_node_t& cfg : ir)
    for(ssa_ht ssa = cfg.ssa_begin(); ssa; ++ssa)
        ssa->clear_flags(FLAG_PROCESSED);

    bool updated = false;

    cfg_data_pool::scope_guard_t<cfg_loop_d> cfg_sg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_loop_d> ssa_sg(ssa_pool::array_size());

    updated |= initial_loop_processing(log, ir, is_byteified);

    //updated |= o_strength_reduction(log, ir);
    //updated |= o_loop_index_rewrite(log, ir, false);

    return updated;
}
