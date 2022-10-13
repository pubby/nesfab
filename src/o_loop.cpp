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
    virtual ssa_value_t make_init(cfg_ht cfg) const = 0;
    virtual iv_t& root() = 0;
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
    virtual ssa_value_t make_init(cfg_ht cfg) const { return init; }

    virtual iv_t& root() { return *this; }

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

    virtual iv_t& root() { return parent->root(); }
};

// iv = induction variable
thread_local std::deque<iv_t> ivs;
thread_local std::deque<mutual_iv_t> mutual_ivs;

struct cfg_loop_d
{
    // If the iterations can occur in any order:
    // TODO
    //bool parallel;

    // 

    // TODO: is this used?
    bc::small_vector<unsigned, 1> ivs; // indexes into global 'ivs' vector.
    //bc::small_vector<iv_t, 1> mutual_ivs;
};

struct ssa_loop_d
{
    ssa_ht iv = {};
};

static cfg_loop_d& data(cfg_ht cfg) { return cfg.data<cfg_loop_d>(); }

// Includes nested loops - this isn't always what you want.
bool def_in_loop(cfg_ht loop_header, ssa_value_t v)
{
    return v.holds_ref() && loop_is_parent_of(loop_header, v->cfg_node());
};

// Returns number of ivs found.
void identify_induction_variables(log_t* log, ir_t& ir)
{
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
        for(unsigned i = 0; i < cfg_input_size; ++i)
        {
            cfg_ht const input = header->input(i);
            if(!loop_is_parent_of(header, input))
                entry_inputs |= 1 << i;
        }

        passert(entry_inputs, header, entry_inputs, cfg_input_size);
        passert(builtin::popcount(entry_inputs) != cfg_input_size, entry_inputs, cfg_input_size);

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

            data(header).ivs.push_back(ivs.size());

            auto& new_iv = ivs.emplace_back(iv_t{});
            new_iv.init = init;
            new_iv.phi = phi;
            new_iv.arith = arith.handle();
            new_iv.operand = operand;
            new_iv.arith_to_phi_input = phi_input;
            new_iv.entry_inputs = entry_inputs;
        }
    }
}

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

bool try_reduce(cfg_ht header, iv_base_t& def, bool is_phi, unsigned depth = 1)
{
    constexpr unsigned MAX_DEPTH = 4;
    if(depth >= MAX_DEPTH)
        return false;

    bool updated = false;

    ssa_ht const def_ssa = def.ssa(is_phi);
    type_t const type = def_ssa->type();

    for(unsigned i = 0; i < def_ssa->output_size();)
    {
        auto oe = def_ssa->output_edge(i);

        if(!def_in_loop(header, oe.handle))
            goto next_iter;

        switch(oe.handle->op())
        {
        default:
            break;

        case SSA_read_array8:
            {
                using namespace ssai::array;

                if(oe.index == INDEX)
                {
                    // Pattern match.

                    // - input is phi in loop header
                    // - output is same phi in loop header
                    // - all read arrays in loop use same index
                }
            }
            break;

        case SSA_cast:
            if(is_arithmetic_bijection(type.name(), oe.handle->type().name()))
            {
                auto& new_iv = mutual_ivs.emplace_back(mutual_iv_t{});
                new_iv.parent = &def;
                new_iv.arith = oe.handle;
                new_iv.arith_to_parent_input = oe.index;

                updated |= try_reduce(header, new_iv, is_phi, depth + 1);
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

                auto& new_iv = mutual_ivs.emplace_back(mutual_iv_t{});
                new_iv.parent = &def;
                new_iv.arith = oe.handle;
                new_iv.arith_to_parent_input = oe.index;

                updated |= try_reduce(header, new_iv, is_phi, depth + 1);
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

    next_iter:
        ++i;
    }

    return updated;
}

static bool o_strength_reduction(log_t* log, ir_t& ir)
{
    bool updated = false;

    for(iv_t& iv : ivs)
    {
        cfg_ht const header = iv.phi->cfg_node();

        updated |= try_reduce(header, iv, false);
        updated |= try_reduce(header, iv, true);
    }

    return updated;
}

static bool o_loop_index_rewrite(log_t* log, ir_t& ir, bool is_byteified)
{
    bool updated = false;

    for(iv_t& iv : ivs)
    {
        // TODO: properly handle mutual IVs


        // First, identify the loop condition.


        // Needs a constant starting point:
        if(!iv.init.is_num())
            continue;

        passert(iv.arith->op() == SSA_add || iv.arith->op() == SSA_sub, iv.arith->op(), iv.arith);
        passert(iv.arith->input_size() == 3, iv.arith->input_size());

        ssa_value_t const operand = iv.arith->input(!iv.arith_to_phi_input);
        if(!operand.is_num())
            continue;

        // We're looking for IVs that are not used for anything besides control flow.
        if(iv.phi->output_size() != 1 || iv.arith->output_size() != 2)
            continue;
        assert(iv.phi->output(0) == iv.arith);

        // Find the other output:
        unsigned const output_size = iv.arith->output_size();
        unsigned output_i = 0;
        for(; output_i < output_size; ++output_i)
            if(iv.arith->output(output_i) != iv.phi)
                goto found_output;
        continue;

    found_output:
        assert(output_size == 2);
        assert(iv.arith->output(!output_i) == iv.phi);

        ssa_ht const condition = iv.arith->output(output_i);

        if(condition->op() == SSA_if)
        {
            // TODO
        }

        if(condition->output_size() != 1)
            continue;

        cfg_ht const header = this_loop_header(iv.phi->cfg_node());

        if(this_loop_header(condition->cfg_node()) != header)
            continue;

        ssa_ht const branch = condition->output(0);
        if(branch->op() != SSA_if)
            continue;

        if(this_loop_header(branch->cfg_node()) != header)
            continue;

        bool const is_do = branch->cfg_node() == iv.arith->cfg_node() || !dominates(branch->cfg_node(), iv.arith->cfg_node());

        // Only a few conditions are supported.
        switch(condition->op())
        {
        default: 
            continue;
        case SSA_lt:
        case SSA_lte:
        case SSA_eq:
        case SSA_not_eq:
            break;
        }

        unsigned compare_input = !iv.arith->output_edge(output_i).index;
        ssa_value_t const compare = condition->input(compare_input);

        // Must be comparing with a constant:
        if(!compare.is_num())
            continue;

        // For now, expect the same types.
        // TODO: Handle differing types.
        if(compare.type() != iv.arith->type())
            continue;

        fixed_sint_t const start = iv.init.signed_fixed();
        fixed_sint_t const end = compare.signed_fixed();
        fixed_sint_t const span = end - start;
        fixed_sint_t accum = operand.signed_fixed();
        if(iv.arith->op() == SSA_sub)
            accum = -accum;

        if(accum == 0)
            continue;

        fixed_sint_t const iterations = span / accum;
        fixed_sint_t const remainder = span % accum;

        if(iterations <= 0)
            continue;

        // TODO: identify already optimized

        switch(condition->op())
        {
        default: 
            continue;
        case SSA_lt:
        case SSA_lte:
        case SSA_eq:
            continue; // TODO

        case SSA_not_eq:
            // Require increment reaches 'end' exactly, without passing it.
            // Technically the value could wrap around and become equivalent eventually,
            // but it's simpler to ignore that and assume no wrap around.
            if(remainder != 0)
                continue;

            // Don't bother handling 0 iterations; the AI pass can instead.
            if(iterations == 0)
                continue; 

            // Re-write the loop.

            if(is_byteified && iterations >= 256)
                continue; // Not worth the complexity of implementing.

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
            type_name_t const iv_type = type_u(whole_bytes, 0);

            // It's stupid to replace with a larger type.
            if(whole_bytes > iv.arith->type().size_of())
                continue;

            // Rewrite 'iv.phi':
            iv.phi->set_type(iv_type);
            assert(iv.entry_inputs);
            bitset_for_each(iv.entry_inputs, [&](unsigned input_i)
            {
                assert(iv.phi->input(input_i) == iv.init);
                iv.phi->link_change_input(input_i, ssa_value_t(iterations & ((1 << whole_bytes * 8) - 1), iv_type));
            });

            // Rewrite 'iv.arith':
            iv.arith->set_type(iv_type);
            iv.arith->unsafe_set_op(SSA_sub);
            iv.arith->link_change_input(!iv.arith_to_phi_input, ssa_value_t(1, iv_type));
            iv.arith->link_change_input(2, ssa_value_t(1, TYPE_BOOL)); // carry
            if(iv.arith_to_phi_input)
            {
                iv.arith_to_phi_input = 0;
                iv.arith->link_swap_inputs(0, 1);
            }

            // Rewrite 'condition':
            condition->link_change_input(compare_input, ssa_value_t(0, iv_type));
            updated = true;
            // TODO;

            std::cout << "REWRITE " << iv.phi << std::endl;
            //std::cout << "LOOP INDEX " << iv.phi << to_double(start) << ' ' << to_double(end) << ' ' << to_double(accum) << std::endl;

            assert(iv.arith->op() == SSA_sub);


            break;
        }
    }

    return updated;
}

} // end anonymous namespace

//////////
// LOOP //
//////////

bool o_loop(log_t* log, ir_t& ir)
{
    build_loops_and_order(ir);
    build_dominators_from_order(ir);

    bool updated = false;

    cfg_data_pool::scope_guard_t<cfg_loop_d> cfg_sg(cfg_pool::array_size());

    identify_induction_variables(log, ir);
    if(ivs.empty())
        return updated;

    updated |= o_strength_reduction(log, ir);
    updated |= o_loop_index_rewrite(log, ir, false); // TODO: byteified

    return updated;
}
