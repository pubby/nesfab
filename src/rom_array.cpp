#include "rom_array.hpp"

#include "flat/small_set.hpp"

#include "group.hpp"
#include "globals.hpp"
#include "ir.hpp"

std::mutex rom_array_map_mutex;
rom_array_map_t rom_array_map;

rom_array_meta_t::rom_array_meta_t()
: used_by_fns(impl_bitset_size<fn_t>())
, used_by_group_data(impl_bitset_size<group_data_t>())
{}

void rom_array_meta_t::mark_used_by(fn_ht fn)
{
    assert(fn);
    std::lock_guard<std::mutex> lock(mutex);
    used_by_fns.set(fn.value);
}

void rom_array_meta_t::mark_used_by(group_data_ht group_data)
{
    assert(group_data);
    std::lock_guard<std::mutex> lock(mutex);
    used_by_group_data.set(group_data.value);
}

rom_array_meta_t& rom_array_meta_t::get(rom_array_ht h)
{
    assert(h);
    std::lock_guard<std::mutex> lock(rom_array_map_mutex);
    assert(h.value < rom_array_map.size());
    return rom_array_map.begin()[h.value].second;
}

locator_t lookup_rom_array(fn_ht fn, group_data_ht group_data, rom_array_t&& rom_array, std::uint16_t offset)
{
    // Just to be safe, we'll strip byteify information:
    for(locator_t& loc : rom_array.data)
        loc = loc.strip_byteify();

    rom_array_ht h;
    rom_array_meta_t* meta;

    rom_array_map_t::insertion result;
    {
        std::lock_guard<std::mutex> lock(rom_array_map_mutex);
        result = rom_array_map.emplace(std::move(rom_array), 
            []() -> rom_array_meta_t { return rom_array_meta_t(); });
        h = { result.first - rom_array_map.begin() };
        // Get address while locked, for thread safety:
        meta = &result.first->second;
    }

    if(fn)
        meta->mark_used_by(fn);

    if(group_data)
        meta->mark_used_by(group_data);

    return locator_t::rom_array(h);
}

void build_rom_arrays(fn_ht fn, ir_t& ir)
{
    // Handle existing 'LOC_ROM_ARRAY's, marking them as used by 'fn':
    fc::small_set<rom_array_ht, 16> used;
    for(cfg_node_t const& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it; ++ssa_it)
    {
        unsigned const input_size = ssa_it->input_size();
        for(unsigned i = 0; i < input_size; ++i)
        {
            ssa_value_t const input = ssa_it->input(i);
            if(!input.is_locator())
                continue;

            locator_t const loc = input.locator();

            if(loc.lclass() == LOC_ROM_ARRAY)
                used.insert(loc.rom_array());
        }
    }

    for(rom_array_ht h : used)
        rom_array_meta_t::get(h).mark_used_by(fn);

    // Then convert SSA_init_arrays
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

            ssa_it->replace_with(lookup_rom_array(fn, {}, std::move(rom_array)));
            ssa_it = ssa_it->prune();
            continue;
        }
    next_iter:
        ++ssa_it;
    }

}
