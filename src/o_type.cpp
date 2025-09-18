#include "o_type.hpp"

#include "globals.hpp"
#include "ir.hpp"
#include "ir_util.hpp"

bool o_remove_index_types(log_t* log, ir_t& ir)
{
    bool updated = false;

    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        ssa_node_t& ssa_node = *ssa_it;

        if(is_index(ssa_node.type().name()))
        {
            ssa_it->set_type(to_u(ssa_node.type().name()));
            updated = true;
        }
    }

    for(cfg_node_t& cfg_node : ir)
    for(ssa_ht ssa_it = cfg_node.ssa_begin(); ssa_it; ++ssa_it)
    {
        unsigned const size = ssa_it->input_size();
        for(unsigned i = 0; i < size; i += 1)
        {
            ssa_value_t input = ssa_it->input(i); 
            type_name_t const tn = input.type().name();

            if(is_index(tn) && input.is_num())
            {
                input.set_num_type_name(to_u(tn));
                ssa_it->link_change_input(i, input);
                updated = true;
            }
        }
    }

    return updated;
}
