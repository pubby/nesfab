#include "o_id.hpp"

// What do we want to do?

/*
struct math_monoid_t
{
    ssa_op_t op;
    type_t type;
    ssa_ht root;
    std::vector<ssa_value_t> values;
};

struct math_group_t
{
    ssa_op_t op;
    type_t type;
    ssa_ht root;
    std::vector<ssa_value_t> values;
    std::vector<ssa_value_t> inverts;

    fixed_t extract_fixed() const
    {
        fixed_t accum = {};

        for(ssa_value_t v : values)
        {
            if(!v.is_num())
                continue;
            fixed_t f = v.fixed();

            switch(op)
            {
            default: assert(false);
            case SSA_add: acum.value += f.value; break;
            case SSA_sub: acum.value -= f.value; break;
            }

            f &= TODO;


        }
    }
};
*/

// add x x -> shift

// OPERATIONS:
// - swap lhs and rhs
// - tree rotate

ssa_ht tree_rotate_left(ssa_ht root)
{
    assert(root.input_size() >= 2);

    ssa_ht new_root = root->input(0);
    assert(new_root->input_size() >= 2);

    root->link_change_input(0, new_root->input(1));
    new_root->link_change_input(1, root);
}

ssa_ht tree_rotate_right(ssa_ht root)
{
    assert(root.input_size() >= 2);

    ssa_ht new_root = root->input(1);
    assert(new_root->input_size() >= 2);

    root->link_change_input(1, new_root->input(0));
    new_root->link_change_input(0, root);
}


bool o_remove_identity_elements(ir_t& ir)
{
    bool updated = false;

    for(cfg_node_t const& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        if(!is_numeric(ssa_it->type()))
            continue;

        ssa_node_t const& node = *ssa_it;
        fixed_t const all_set = numeric_bitset(node.type());

        switch(node.op())
        {
        default:
            break;
        case SSA_add:
            {
                if(!node.input(2).is_num())
                    break;
                std::uint64_t const carry = node.input(2).fixed().value;
                if(node.input(0).is_num() && !((node.input(0).fixed().value + carry) & all_set.value))
                    goto replaceWith1;
                if(node.input(1).is_num() && !((node.input(1).fixed().value + carry) & all_set.value))
                    goto replaceWith0;
                break;
            }
        case SSA_sub:
            {
                if(!node.input(2).is_num())
                    break;
                std::uint64_t const carry = node.input(2).fixed().value;
                if(node.input(1).is_num() && !((~node.input(1).fixed().value + carry) & all_set.value))
                    goto replaceWith0;
                break;
            }
        case SSA_or:
        case SSA_xor:
            if(ssa_it->input(0).eq_fixed({}))
                goto replaceWith1;
            if(ssa_it->input(1).eq_fixed({}))
                goto replaceWith0;
            break;
        case SSA_and:
            if(ssa_it->input(0).eq_fixed(all_set))
                goto replaceWith1;
            if(ssa_it->input(1).eq_fixed(all_set))
                goto replaceWith0;
            break;
        }

        continue;

    replaceWith0:
        node.replace_with(node.input(0));
        goto prune;

    replaceWith1:
        node.replace_with(node.input(1));
        goto prune;

    prune:
        node.prune();
        updated = true;
    }

    return updated;
}

bool o_(ir_t& ir)
{
    fc::small_multiset<ssa_ht, 32> set;
    fc::small_multiset<ssa_ht, 32> inverse_set;

    {
        if(node.output_size() == 0)
            continue;

        if(node.output_size() == 1 && node.output(0).op() == node.op())
            continue;

        build_group(node);

        // node has inputs of same op
        // node has no outputs of same op

    }
}

void build_set(ssa_ht h, ssa_op_t op, ssa_op_t inverse)
{
    for_each_node_input(h, [&, op, inverse, h](ssa_ht input)
    {
        if(input->type() != h->type())
            return;

        if(input->output_size() > 1)
        {
            // Every output to our input must be a member of our sets.
            for(unsigned j = 0; j < input->output_size(); ++j)
            {
                ssa_ht const output = input->output(j);
                if(set.count(output) == 0 && inverse_set.count(output) == 0)
                    return;
            }
        }

        if(input->op() == op)
        {
            set[input] += 1;
            build_set(input, op, inverse);
        }
        else if(input->op() == inverse)
        {
            inverse_set[input] += 1;
            build_set(input, inverse, op); // Swap 'op' and 'inverse'!
        }
    });
}

bool o_identities(ir_t& ir)
{
    bool updated = false;
    updated |= o_remove_identity_elements(ir);
}
