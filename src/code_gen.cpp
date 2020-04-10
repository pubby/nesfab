#include "code_gen.hpp"

void make_conventional(ir_t& ir)
{
    // Insert copies
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    {
        cfg_node_t& cfg_node = *cfg_it;

        for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
        {
            ssa_node_t& phi_node = *phi_it;

            // Insert copies in predecessor nodes.
            unsigned const input_size = phi_node.input_size();
            for(unsigned i = 0; i < input_size; ++i)
            {
                if(phi_node.input(i).is_const())
                    continue;

                ssa_ht input_h = *phi_node.input(i);
                ssa_node_t& input_node = *input_h;

                cfg_ht pred_h = cfg_node.input(i);
                cfg_node_t& pred_node = *pred_h;

                ssa_ht input_copy_h = pred_node.emplace_ssa(
                    SSA_exit_copy, input_node.type(), input_h);

                phi_node.link_change_input(i, input_copy_h);
            }

            // Create a copy of the phi node.
            ssa_ht phi_copy_h = cfg_node.emplace_ssa(
                SSA_entry_copy, phi_node.type());

            phi_node.replace_with(phi_copy_h);

            phi_copy_h->link_append_input(phi_it);
        }
    }

}

struct schedule_t
{
    std::vector<ssa_ht> order;
};

void schedule(cfg_ht cfg_h)
{
    cfg_node_t& cfg_node = *cfg_h;

    // Phi nodes first.
    for(ssa_ht phi_it = cfg_node.phi_begin(); phi_it; ++phi_it)
        schedule.order.push_back(phi_it);

    // Then entry_copy nodes.
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
        if(ssa_it->op() == SSA_entry_copy)
            schedule.order.push_back(ssa_it);

    // Then any
    
    // Then all remaining nodes.

}

addr_mode_t ssa_addr_mode(ssa_value_t value)
{
    if(value.is_const())
    {
        assert((value.whole() & 0xFF) == value.whole());
        return { MODE_IMMEDIATE, value.whole() };
    }

    std::uint16_t const addr = TODO;
    return { is_zp(addr) ? MODE_ZERO_PAGE : MODE_ABSOLUTE, addr };
}

void load_A(ssa_value_t value)
{
    ssa_addr_mode(value);

    if(value.is_const())
        block.code.push_back({ LDA_IMMEDIATE, value.whole() & 0xFF });
    else
    {
        block.code.push_back({ LDA_IMMEDIATE, value.whole() & 0xFF });
    }
}

void code_gen()
{

    rh::robin_map<ssa_ht, std::uint16_t> vars;


    switch(TODO)
    {
    case SSA_add:
        block.code.push_back({ CLC });
        load_A(TODO);
        block.code.push_back({ ADC_IMMEDIATE, });
        store_A(TODO);
    }
}
