#include "o_loop.hpp"

#include <cstdint>
#include <cmath>
#include <vector>
#include <iostream> // TODO
#include <fstream> // TODO
#include "graphviz.hpp" // TODO

#include <boost/container/small_vector.hpp>

#include "robin/map.hpp"
#include "robin/hash.hpp"

#include "ir.hpp"
#include "ir_algo.hpp"
#include "ir_util.hpp"
#include "worklist.hpp"
#include "guard.hpp"
#include "unroll_divisor.hpp"
#include "constraints.hpp"
#include "o_ai.hpp"

namespace bc = ::boost::container;

namespace // anonymous
{

// "iv" means "induction variable".

struct iv_t;
void new_ssa(ssa_ht ssa);

using iv_deps_t = std::uint32_t;
constexpr iv_deps_t VALUE_DEP = 1 << 0;
constexpr iv_deps_t INDEX_DEP = 1 << 1;
constexpr iv_deps_t ORDER_DEP = 1 << 2;

struct iv_base_t
{
    virtual ~iv_base_t() {};

    virtual ssa_ht ssa(bool phi) const = 0;
    virtual bool has_ssa(ssa_ht match) const = 0;
    virtual ssa_value_t make_init(cfg_ht cfg) const = 0;
    virtual iv_t& root() = 0;
    virtual bool const_init(fixed_sint_t& result) const = 0;
    virtual bool transform(fixed_sint_t& result) const = 0;
    virtual void propagate_dep(iv_deps_t deps) = 0;

    iv_deps_t deps = 0;

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

    virtual void propagate_dep(iv_deps_t new_deps) { deps |= new_deps; }

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

        ssa_ht const init = cfg->emplace_ssa(arith->op(), arith->type());
        new_ssa(init);
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

    virtual void propagate_dep(iv_deps_t new_deps) { deps |= new_deps; parent->propagate_dep(new_deps); }

    virtual iv_t& root() { return parent->root(); }
};

struct header_d
{
    // Number of exits from this loop.
    unsigned loop_exits = 0;

    ssa_ht simple_condition = {};
    ssa_ht simple_branch = {};
    unsigned simple_condition_iv_i = 0;
    unsigned simple_reentry_i = 0;
    bool simple_do = false;

    // For unrolling:
    cfg_ht simple_unroll_body = {};
};

// iv = induction variable
thread_local std::deque<iv_t> ivs;
thread_local std::deque<mutual_iv_t> mutual_ivs;
thread_local std::vector<header_d> header_data_vec;

template<typename Fn>
void for_each_iv(Fn const& fn)
{
    for(iv_base_t& iv : ivs)
        fn(iv);
    for(iv_base_t& iv : mutual_ivs)
        fn(iv);
}

/* TODO: remove?
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
*/

struct ssa_loop_d
{
    unsigned unroll_i = 0;

    bc::small_vector<iv_base_t*, 1> ivs;
};

// TODO
//cfg_loop_d& data(cfg_ht cfg) { return cfg.data<cfg_loop_d>(); }
ssa_loop_d& data(ssa_ht ssa) { return ssa.data<ssa_loop_d>(); }
header_d& header_data(cfg_ht cfg) 
{ 
    assert(algo(cfg).is_loop_header);
    return header_data_vec[algo(cfg).header_i];
}

void new_ssa(ssa_ht ssa)
{
    ssa_data_pool::resize<ssa_loop_d>(ssa_pool::array_size());
    resize_ai_prep();
    data(ssa) = {};
    ai_prep(ssa) = {};
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
    new_ssa(step.init = step_init_cfg->emplace_ssa(init_op, type));
    new_ssa(step.phi = header->emplace_ssa(SSA_phi, type));
    new_ssa(step.arith = step_cfg->emplace_ssa(arith_op, type));

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
    if(ssa->test_flags(FLAG_PROCESSED) || ssa->in_daisy())
        return true;

    assert(!(ssa_flags(ssa->op()) & SSAF_CONDITIONAL));
       
    if(!def_in_loop(header, ssa))
        return true;

    // If we reach a flagged array write,
    // we know the order has already been handled.
    {
        using namespace ssai::array;

        if(ssa->test_flags(FLAG_ARRAY) 
           && ssa->input(ARRAY)->cfg_node() == header
           && ssa->input(INDEX) == def.root().ssa(true))
        {
            return false;
        }
    }

    ssa->set_flags(FLAG_PROCESSED);
    auto guard = make_scope_guard([&]{ ssa->clear_flags(FLAG_PROCESSED); });

    unsigned const output_size = ssa->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        ssa_ht const output = ssa->output(i);
        if(has_order_dep(header, def, output))
            return true;
    }

    return false;
}

struct to_calc_order_dep_t
{
    cfg_ht header;
    iv_base_t* def;
    ssa_ht ssa;
};

using to_calc_order_dep_vec_t = bc::small_vector<to_calc_order_dep_t, 16>;

bool try_reduce(to_calc_order_dep_vec_t& to_calc_order_dep, cfg_ht header, iv_base_t& def, bool is_phi, unsigned depth = 1)
{
    constexpr unsigned MAX_DEPTH = 4;
    if(depth >= MAX_DEPTH)
        return false;

    bool updated = false;

    // TODO
    //auto const& hd = header_data(header);

    ssa_ht const def_ssa = def.ssa(is_phi);

    iv_t& root = def.root();

    for(unsigned i = 0; i < def_ssa->output_size();)
    {
        auto oe = def_ssa->output_edge(i);
        type_t const type = oe.handle->type();

        if(oe.handle == root.ssa(!is_phi))
            goto next_iter;

        if(ssa_flags(oe.handle->op()) & SSAF_ARRAY_OFFSET)
        {
            using namespace ssai::array;
            if(oe.index == INDEX)
            {
                def.deps |= INDEX_DEP;
                goto calc_order;
            }
        }

        if(!def_in_loop(header, oe.handle))
        {
            def.deps |= VALUE_DEP | ORDER_DEP;
            goto next_iter;
        }

        switch(oe.handle->op())
        {
        default:
            if(oe.handle == header_data(header).simple_condition)
                goto next_iter;
            break;

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

                updated |= try_reduce(to_calc_order_dep, header, new_iv, is_phi, depth + 1);
                def.deps |= new_iv.deps;
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
                    {
                        cast_inc = step.init->cfg_node()->emplace_ssa(SSA_cast, type, inc);
                        new_ssa(cast_inc.handle());
                    }
                    ssa_ht const add_operand = step.init->cfg_node()->emplace_ssa(SSA_shl, type, cast_inc, oe.handle->input(1));
                    new_ssa(add_operand);

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

        def.deps |= VALUE_DEP;
    calc_order:
        to_calc_order_dep.push_back({ header, &def, oe.handle });
    next_iter:
        ++i;
    }

    return updated;
}

void increment_array_offsets(iv_base_t& iv, std::uint16_t amount)
{
    using namespace ssai::array;

    ssa_ht const ssa = iv.ssa(true);

    unsigned const output_size = ssa->output_size();
    for(unsigned i = 0; i < output_size; ++i)
    {
        auto oe = ssa->output_edge(i);

        if(oe.index == INDEX && (ssa_flags(oe.handle->op()) & SSAF_ARRAY_OFFSET))
        {
            std::uint16_t const offset = oe.handle->input(OFFSET).whole();
            oe.handle->link_change_input(OFFSET, ssa_value_t(offset + amount, TYPE_U20));
        }
    }

    for(iv_base_t* child : iv.children)
        increment_array_offsets(*child, amount);
}

bool reverse_loop(cfg_ht header, iv_t& root, fixed_sint_t init, fixed_sint_t increment, ssa_ht condition, bool condition_root_i, cfg_ht branch)
{
    if(root.deps & ORDER_DEP)
        return false;

    // Counting down to 0 is efficient.
    if(init != 0)
        return false;

    // We can only handle != and <
    if(condition->op() != SSA_not_eq && (condition->op() != SSA_lt || condition_root_i != 0))
        return false;

    type_name_t const root_type = root.phi->type().name();

    // For now, only handle the smallest increment.
    // TODO: handle other increments
    if(fixed_uint_t(increment) != low_bit_only(numeric_bitmask(root_type)))
        return false;

    // Can't index with fractional.
    if((root.deps & INDEX_DEP) && frac_bytes(root_type) > 0)
        return false;

    ssa_value_t end = condition->input(!condition_root_i);
    if(def_in_loop(header, end))
        return false;
    assert(end);

    if(end.is_num() && !(end.fixed().value & high_bit_only(numeric_bitmask(end.num_type_name()))))
    {
        end = ssa_value_t(fixed_t{end.signed_fixed() - increment}, end.num_type_name());

        // Rewrite 'condition':
        assert(condition->output_size() == 1);
        condition->link_remove_input(!condition_root_i);
        condition->unsafe_set_op(SSA_sign);

        branch->link_swap_outputs(0, 1);
    }
    else
    {
        if(root.deps & VALUE_DEP)
            return false;

        // Rewrite array outputs:
        if(root.deps & INDEX_DEP)
            increment_array_offsets(root, -1);

        // Rewrite 'condition':
        condition->link_change_input(!condition_root_i, ssa_value_t(0, root_type));
        condition->unsafe_set_op(SSA_not_eq);
    }

    // Rewrite 'iv.phi':
    assert(root.entry_inputs);
    assert(end.type().name() == root_type);
    bitset_for_each(root.entry_inputs, [&](unsigned input_i)
    {
        assert(root.phi->input(input_i) == root.init);
        root.phi->link_change_input(input_i, end);
    });

    // Rewrite 'iv.arith':
    root.arith->unsafe_set_op(SSA_sub);
    root.arith->link_change_input(!root.arith_to_phi_input, ssa_value_t(fixed_t{increment}, root_type));
    root.arith->link_change_input(2, ssa_value_t(1, TYPE_BOOL)); // carry
    if(root.arith_to_phi_input)
    {
        root.arith_to_phi_input = 0;
        root.arith->link_swap_inputs(0, 1);
    }

    return true;
}

bool rewrite_loop(bool is_do, bool is_byteified, iv_t& root, 
                  fixed_sint_t init, fixed_sint_t increment, fixed_sint_t iterations, fixed_sint_t end,
                  ssa_ht condition, bool condition_root_i)
{
    assert(iterations > 0);

    if(root.deps & (ORDER_DEP | VALUE_DEP | INDEX_DEP))
        return false; // Can't rewrite if the order or value matters.

    // Re-write the loop.

    if(is_byteified && iterations >= 256)
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
    unsigned const whole_bytes = calc_whole(iterations, is_do);
    assert(whole_bytes > 0);
    if(whole_bytes > max_whole_bytes)
        return false;
    type_name_t const iv_type = type_u(whole_bytes, 0);

    // It's stupid to replace with a larger type.
    if(whole_bytes > root.arith->type().size_of())
        return false;

    // No point in optimizing an already good loop:
    if(iv_type == root.phi->type().name() && end == 0)
        return false;

    // Rewrite 'root.phi':
    root.phi->set_type(iv_type);
    assert(root.entry_inputs);
    bitset_for_each(root.entry_inputs, [&](unsigned input_i)
    {
        assert(root.phi->input(input_i) == root.init);
        root.phi->link_change_input(input_i, ssa_value_t(iterations & ((1 << whole_bytes * 8) - 1), iv_type));
    });

    // Rewrite 'root.arith':
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
    condition->link_change_input(!condition_root_i, ssa_value_t(0, iv_type));
    condition->unsafe_set_op(SSA_not_eq);

    return true;
}

// Returns times unrolled, or 0 if nothing happened.
fixed_sint_t unroll_loop(cfg_ht header, fixed_sint_t iterations)
{
    std::cout << "UNROLL ENTER\n";

    auto const& hd = header_data(header);

    if(!hd.simple_unroll_body)
        return 0;
    cfg_ht const body = hd.simple_unroll_body;

    // Estimate the cost of each loop iteration.

    constexpr unsigned MAX_COST = 64;
    unsigned cost_per_iter = 0;

    auto const calc_cost_per_iter = [&](cfg_ht cfg)
    {
        for(ssa_ht ssa = cfg->ssa_begin(); ssa; ++ssa)
        {
            if(ssa != hd.simple_condition && ssa != hd.simple_branch)
            {
                cost_per_iter += estimate_cost(*ssa);
                if(cost_per_iter > MAX_COST / 2)
                    return false;
            }
        }
        return true;
    };

    if(!calc_cost_per_iter(header))
        return 0;

    if(body != header)
        if(!calc_cost_per_iter(body))
            return 0;

    std::cout << "UNROLL COST " << cost_per_iter << std::endl;

    if(cost_per_iter == 0)
        return 0;

    unsigned const unroll_amount = estimate_unroll_divisor(iterations, MAX_COST / cost_per_iter);
    passert(iterations % unroll_amount == 0, iterations, unroll_amount);

    std::cout << "UNROLL " << header << ' ' << iterations << " / " << unroll_amount << std::endl;

    if(unroll_amount <= 1)
        return 0;

    auto const in_unroll = [&](cfg_ht cfg) { return cfg == header || cfg == body; };

    // We'll use these vectors to track SSA nodes while we're unrolling.

    std::vector<ssa_ht> orig_map;
    std::vector<ssa_value_t> map, next_map;

    unsigned reserve_size = header->ssa_size();
    if(header != body)
        reserve_size += body->ssa_size();
    orig_map.reserve(reserve_size);
    map.reserve(reserve_size);
    
    auto const init_map = [&](cfg_ht cfg)
    {
        for(ssa_ht ssa = cfg->ssa_begin(); ssa; ++ssa)
        {
            if(ssa != hd.simple_branch)
            {
                data(ssa).unroll_i = map.size();
                orig_map.push_back(ssa);
                map.push_back(ssa->op() == SSA_phi ? ssa->input(!hd.simple_reentry_i) : ssa);
            }
        }
    };

    init_map(header);
    if(body != header)
        init_map(body);

    next_map.resize(map.size());
    
    // Add the new SSA nodes:
    for(unsigned u = 1; u < unroll_amount; ++u)
    {
        for(unsigned i = 0; i < map.size(); ++i)
        {
            if(orig_map[i]->op() == SSA_phi)
            {
                ssa_value_t const input = orig_map[i]->input(hd.simple_reentry_i);

                if(input.holds_ref() && in_unroll(input->cfg_node()))
                    next_map[i] = map[data(input.handle()).unroll_i];
                else
                    next_map[i] = input;
            }
            else
            {
                // Create new nodes, but don't fill their inputs yet.
                ssa_ht const orig = orig_map[i];
                ssa_ht const next = body->emplace_ssa(orig->op(), orig->type());
                new_ssa(next);

                if(orig->in_daisy())
                    next->append_daisy();

                next_map[i] = next;
            }
        }

        // Finish new nodes:
        for(unsigned i = 0; i < map.size(); ++i)
        {
            if(orig_map[i]->op() == SSA_phi)
                continue;

            ssa_ht const orig = orig_map[i];
            ssa_ht const next = next_map[i].handle();

            unsigned const input_size = orig->input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t input = orig->input(i);

                if(input.holds_ref() && in_unroll(input->cfg_node()))
                    input = next_map[data(input.handle()).unroll_i];

                next->link_append_input(input);
            }
        }

        std::swap(map, next_map);
    }

    // Fix up the phis:
    for(ssa_ht phi = header->phi_begin(); phi; ++phi)
    {
        ssa_value_t const input = phi->input(hd.simple_reentry_i);
        if(input.holds_ref() && in_unroll(input->cfg_node()))
            phi->link_change_input(hd.simple_reentry_i, map[data(input.handle()).unroll_i]);
    }

    // Fix up the branch:
    assert(hd.simple_branch->op() == SSA_if);
    assert(hd.simple_branch->input(0).handle() == hd.simple_condition);
    assert(hd.simple_branch->in_daisy());

    if(hd.simple_do)
    {
        hd.simple_branch->erase_daisy();
        hd.simple_branch->append_daisy();
        hd.simple_branch->link_change_input(0, map[data(hd.simple_condition).unroll_i]);

        // Fix up uses outside the loop:
        for(unsigned i = 0; i < orig_map.size(); ++i)
        {
            ssa_ht const orig = orig_map[i];
            for(unsigned j = 0; j < orig->output_size();)
            {
                auto const oe = orig->output_edge(j);
                if(in_unroll(oe.handle->cfg_node()))
                    ++j;
                else
                    oe.handle->link_change_input(oe.index, map[i]);
            }
        }
    }

    return unroll_amount;
}

bool initial_loop_processing(log_t* log, ir_t& ir, bool is_byteified)
{
    bool updated = false;

    ivs.clear();
    mutual_ivs.clear();

    // Count how many exits each loop has.
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        cfg_ht const this_header = this_loop_header(cfg);
        if(!this_header)
            continue;

        unsigned const output_size = cfg->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht const output = cfg->output(i);
            for(cfg_ht header = this_header; header && !loop_is_parent_of(header, output); header = algo(header).iloop_header)
                header_data(header).loop_exits += 1;
        }
    }

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
            bool is_do = false;

            if(header->output_size() == 2 && reentry->output_size() == 1)
                branch_cfg = header; // simple 'while' loop
            else if(reentry == header || (header->output_size() == 1 && reentry->output_size() == 2))
            {
                branch_cfg = reentry; // simple 'do' loop
                is_do = true;
            }

            if(branch_cfg)
            {
                bool const branch_output_i = loop_is_parent_of(header, branch_cfg->output(1));

                ssa_ht const branch = branch_cfg->last_daisy();
                if(branch && branch->op() == SSA_if && header == this_loop_header(branch->cfg_node()))
                {
                    ssa_value_t const condition = branch->input(0);
                    if(condition.holds_ref() 
                       && condition->output_size() == 1 
                       && header == this_loop_header(condition->cfg_node()))
                    {
                        bool iv_index;

                        switch(condition->op())
                        {
                        default:
                            break;

                        case SSA_not_eq:
                        case SSA_lt:
                        case SSA_lte:
                            if(branch_output_i == 0)
                                break;
                            if(condition->input(0).type() == condition->input(1).type())
                            {
                                iv_index = def_in_loop(header, condition->input(1));
                                if(def_in_loop(header, condition->input(0)) != iv_index)
                                    goto is_simple;
                            }
                            break;

                        case SSA_sign:
                            if(branch_output_i == 1)
                                break;
                            iv_index = 0;
                            // fall-through
                        is_simple:
                            // Found a simple loop condition:
                            auto& hd = header_data(header);
                            hd.simple_condition = condition.handle();
                            hd.simple_branch = branch;
                            hd.simple_condition_iv_i = iv_index;
                            hd.simple_reentry_i = reentry_i;
                            hd.simple_do = is_do;

                            // Now look for a simple unroll body:
                            if(is_do)
                            {
                                if(header == branch_cfg
                                   || (header->output_size() == 1
                                       && header->output(0) == branch_cfg))
                                {
                                    hd.simple_unroll_body = branch_cfg;
                                }
                            }
                            else
                            {
                                assert(branch_cfg == header);
                                if(reentry->input_size() == 1
                                   && reentry->input(0) == header)
                                {
                                    hd.simple_unroll_body = reentry;
                                }
                            }

                            assert(!hd.simple_unroll_body || this_loop_header(hd.simple_unroll_body) == header);
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
    to_calc_order_dep_vec_t to_calc_order_dep;
    for(iv_t& iv : ivs)
    {
        cfg_ht const header = iv.phi->cfg_node();

        updated |= try_reduce(to_calc_order_dep, header, iv, false);
        updated |= try_reduce(to_calc_order_dep, header, iv, true);
    }

    // Identify some simple looping array writes.
    for_each_iv([&](iv_base_t& iv)
    {
        iv_t& root = iv.root();
        cfg_ht const header = root.phi->cfg_node();
        auto const& hd = header_data(header);

        // We can only handle arrays if they're in a simple loop with one exit.
        if(!hd.simple_branch || hd.loop_exits != 1)
            return;

        ssa_ht const def_ssa = iv.ssa(true);

        for(unsigned i = 0; i < def_ssa->output_size(); ++i)
        {
            auto oe = def_ssa->output_edge(i);

            if(!def_in_loop(header, oe.handle))
                continue;

            if(oe.handle->op() == SSA_write_array8 || oe.handle->op() == SSA_write_array16)
            {
                using namespace ssai::array;

                // We'll tentatively flag this node with FLAG_ARRAY,
                // which is needed for 'has_order_dep' to work.
                oe.handle->set_flags(FLAG_ARRAY);

                ssa_op_t const read = oe.handle->op() == SSA_write_array8 ? SSA_read_array8 : SSA_read_array16;

                if(oe.index == INDEX)
                {
                    // Pattern match.

                    ssa_value_t const offset = oe.handle->input(OFFSET);

                    // We're looking for a simple loop of phi and an array write.
                    ssa_value_t const array_phi = oe.handle->input(ARRAY);
                    if(!array_phi.holds_ref() || array_phi->cfg_node() != header || array_phi->op() != SSA_phi)
                        goto next_iter;

                    unsigned const input_size = header->input_size();
                    for(unsigned i = 0; i < input_size; ++i)
                    {
                        if(root.entry_inputs & (1 << i))
                            continue;
                        if(array_phi->input(i) != oe.handle)
                            goto next_iter;
                    }

                    // Check each output, pattern matching.

                    // Check the write's outputs:
                    for(unsigned i = 0; i < oe.handle->output_size(); ++i)
                    {
                        ssa_ht const output = oe.handle->output(i);
                        if(output == array_phi)
                            continue;
                        if(!hd.simple_do || def_in_loop(header, output))
                            goto next_iter;
                    }

                    // Check the phi's outputs:
                    for(unsigned i = 0; i < array_phi->output_size(); ++i)
                    {
                        ssa_ht const output = array_phi->output(i);
                        if(output == oe.handle)
                            continue;

                        if(def_in_loop(header, output))
                        {
                            // It's fine to read values before writing,
                            // so long as the index and offset match.
                            if(output->op() != read
                               || output->input(INDEX) != def_ssa
                               || output->input(OFFSET) != offset)
                            {
                                goto next_iter;
                            }

                            // The output could still impose an ORDER_DEP.
                            // Check for that here:

                            assert(oe.handle->test_flags(FLAG_ARRAY));
                            if(has_order_dep(header, iv, output))
                                goto next_iter;

                        }
                        else if(hd.simple_do)
                            goto next_iter;
                    }

                    // Alright! This write_array looks good.
                    // We'll keep the flag set.
                    assert(oe.handle->test_flags(FLAG_ARRAY));
                    continue;
                }
            }
        next_iter:;
            oe.handle->clear_flags(FLAG_ARRAY); // remove the flag
        }
    });

    // Calculate ORDER_DEP:
    for(auto const& c : to_calc_order_dep)
        if(has_order_dep(c.header, *c.def, c.ssa))
            c.def->propagate_dep(ORDER_DEP);

    // Now finish the identification of simple loops:
    bool this_iter_updated;
    for(cfg_ht header : loop_headers)
    {
        this_iter_updated = false;
        auto& d = header_data(header);

        {
            if(!d.simple_condition)
                continue;

            // Find the 'iv' going into the condition.

            ssa_value_t const iv_ssa = d.simple_condition->input(d.simple_condition_iv_i);
            assert(iv_ssa.holds_ref());

            iv_t* root;
            for(iv_base_t* base : data(iv_ssa.handle()).ivs)
            {
                if((root = dynamic_cast<iv_t*>(base)))
                {
                    assert(root->has_ssa(iv_ssa.handle()));
                    if(root->phi->cfg_node() == header)
                        goto found_iv;
                }
            }
            goto fail;
        found_iv:

            // Needs a constant starting point:
            fixed_sint_t init;
            if(!root->const_init(init))
                goto fail;

            // Need a constant increment:
            if(!root->operand.is_num())
                goto fail;
            fixed_sint_t increment = root->operand.signed_fixed() * root->sign();

            // First, try to reverse the loop:
            if(reverse_loop(header, *root, init, increment, d.simple_condition, d.simple_condition_iv_i, d.simple_branch->cfg_node()))
            {
                updated = this_iter_updated = true;
                goto did_reverse;
            }

            fixed_sint_t iterations = 0;

            // TODO
            if(d.simple_condition->op() == SSA_sign)
            {
                fixed_sint_t signed_init = sign_extend(init, numeric_bitmask(root->ssa(true)->type().name()));

                std::cout << "SHREK " << (init >> 24) << ' ' << (increment >> 24) << std::endl;

                if(signed_init >= 0 && increment >= 0)
                    increment = sign_extend(increment, numeric_bitmask(root->operand.num_type_name()));

                if(std::signbit(signed_init) == std::signbit(increment))
                    goto fail;

                if(signed_init >= 0)
                    signed_init += 1;

                iterations = -signed_init / increment;

                if(signed_init % increment != 0)
                    ++iterations;

                if(iterations <= 0)
                    goto fail;

                std::cout << "SHREK " << iterations << std::endl;
            }
            else
            {
                ssa_value_t const compare_with = d.simple_condition->input(!d.simple_condition_iv_i);
                assert(d.simple_condition->input(0).type() == d.simple_condition->input(1).type());

                // Must be comparing with a constant:
                if(!compare_with.is_num())
                    goto fail;
                fixed_sint_t compare_with_value = compare_with.signed_fixed();

                fixed_sint_t const span = compare_with_value - init;

                if(d.simple_condition->op() == SSA_not_eq && span < 0 && increment >= 0)
                    increment = sign_extend(increment, numeric_bitmask(root->operand.num_type_name()));

                iterations = span / increment;
                fixed_sint_t const remainder = span % increment;

                if(iterations <= 0)
                    goto fail;

                switch(d.simple_condition->op())
                {
                default: 
                    goto fail;

                case SSA_lt:
                    if(remainder != 0)
                    {
                case SSA_lte:
                        ++iterations;
                    }

                    if(d.simple_condition_iv_i == 0) // IV < C
                    {
                        if(increment < 0 || iterations * increment + init > type_max(compare_with.num_type_name()))
                            goto fail;
                    }
                    else // C < IV
                    {
                        if(increment > 0 || iterations * increment + init < type_min(compare_with.num_type_name()))
                            goto fail;
                    }
                    break;

                case SSA_not_eq:
                    // Require increment reaches 'compare_with' exactly, without passing it.
                    // Technically the value could wrap around and become equivalent eventually,
                    // but it's simpler to ignore that and assume no wrap around.
                    if(remainder != 0)
                        goto fail;
                    break;
                }

                fixed_sint_t const end = init + iterations * increment;

                if(rewrite_loop(
                    d.simple_do, is_byteified, *root,
                    init, increment, iterations, end,
                    d.simple_condition, d.simple_condition_iv_i))
                {
                    init = 0;
                    increment = -1ull << fixed_t::shift;
                    updated = this_iter_updated = true;
                }
            }

            if(fixed_sint_t unroll_amount = unroll_loop(header, iterations))
            {
                iterations /= unroll_amount;
                increment *= unroll_amount;
                updated = this_iter_updated = true;
            }

            // Prepare constraints:
            {
                ssa_ht const phi = root->ssa(true);

                assert(iterations > 0);
                fixed_sint_t last = init + increment * iterations;

                if(d.simple_do)
                    last -= increment;

                constraints_t c = {};
                c.bounds.min = std::min(init, last);
                c.bounds.max = std::max(init, last);
                std::cout << "SHREK " << d.simple_do << ' ' << (last >> 24) << ' ' << iterations << c << std::endl;

                assert(increment);
                fixed_uint_t const b = (1ull << builtin::ctz(fixed_uint_t(increment))) - 1ull;
                c.bits.known0 = ~init & b;
                c.bits.known1 = init & b;

                c.normalize(type_constraints_mask(phi->type().name()));


                resize_ai_prep();
                auto& prep = ai_prep(phi);
                prep.constraints.reset(new constraints_t(std::move(c)));
            }

            continue;
        }
    fail:
        d.simple_condition = d.simple_branch = {};
        continue;

    did_reverse:;
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
        ssa->clear_flags(FLAG_PROCESSED | FLAG_ARRAY);

    bool updated = false;

    // TODO
    //cfg_data_pool::scope_guard_t<cfg_loop_d> cfg_sg(cfg_pool::array_size());
    ssa_data_pool::scope_guard_t<ssa_loop_d> ssa_sg(ssa_pool::array_size());

    updated |= initial_loop_processing(log, ir, is_byteified);

    //updated |= o_strength_reduction(log, ir);
    //updated |= o_loop_index_rewrite(log, ir, false);

    return updated;
}
