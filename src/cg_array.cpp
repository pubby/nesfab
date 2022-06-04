#include "cg_array.hpp"

std::mutex rom_array_set_mutex;
rom_array_set_type rom_array_set;

static locator_t lookup_rom_array(rom_array_t&& rom_array, std::uint16_t offset=0)
{
    rh::apair<rom_array_set_type::value_type const*, bool> result;
    {
        std::lock_guard<std::mutex> lock(rom_array_set_mutex);
        result = rom_array_set.insert(std::move(rom_array));
    }

    return locator_t::rom_array(result.first - rom_array_set.begin(), offset);
}

void build_rom_arrays(ir_t& ir)
{
    std::puts("building rom");
    for(cfg_node_t const& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it;)
    {
        unsigned const input_size = ssa_it->input_size();

        if(ssa_it->op() != SSA_init_array)
            goto next_iter;
        
        // We're looking for SSA_init_arrays of all constants
        for(unsigned i = 0; i < input_size; ++i)
            if(!ssa_it->input(i).is_const())
                goto next_iter;

        // Now build the rom_array_t
        {
            rom_array_t rom_array;
            rom_array.data.resize(input_size);

            for(unsigned i = 0; i < input_size; ++i)
            {
                ssa_value_t const input = ssa_it->input(i);

                if(input.is_locator())
                    rom_array.data[i] = input.locator();
                else if(input.is_num())
                {
                    assert(input.whole() % 0xFF == input.whole());
                    rom_array.data[i] = locator_t::const_byte(input.whole());
                }
                else
                    assert(false);
            }

            ssa_it->replace_with(lookup_rom_array(std::move(rom_array)));
            ssa_it = ssa_it->prune();
            continue;
        }
    next_iter:
        ++ssa_it;
    }
    std::puts("done building rom");
}
