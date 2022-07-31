#include "rom_array.hpp"

#include "flat/small_set.hpp"

#include "group.hpp"
#include "globals.hpp"
#include "ir.hpp"

////////////////
// rom_arrays //
////////////////

std::mutex rom_array_map_mutex;
rom_array_map_t rom_array_map;

rom_array_meta_t::rom_array_meta_t()
: m_used_in_group_data(impl_bitset_size<group_data_t>())
{
    assert(compiler_phase() > PHASE_PARSE);
}

void rom_array_meta_t::mark_used_by(rom_proc_ht rom_proc)
{
    assert(rom_proc);
    std::lock_guard<std::mutex> lock(m_mutex);
    m_used_in_procs.insert(rom_proc);
}

void rom_array_meta_t::mark_used_by(group_data_ht gd)
{
    assert(gd);
    std::lock_guard<std::mutex> lock(m_mutex);
    m_used_in_group_data.set(gd.id);
}

rom_array_t& rom_array_t::get(rom_array_ht h)
{
    assert(h);
    std::lock_guard<std::mutex> lock(rom_array_map_mutex);
    assert(h.id < rom_array_map.size());
    return rom_array_map.begin()[h.id].first;
}

rom_array_meta_t& rom_array_meta_t::get(rom_array_ht h)
{
    assert(h);
    std::lock_guard<std::mutex> lock(rom_array_map_mutex);
    assert(h.id < rom_array_map.size());
    return rom_array_map.begin()[h.id].second;
}

rom_array_ht lookup_rom_array(rom_array_t&& rom_array, rom_proc_ht rp, group_data_ht gd)
{
    assert(compiler_phase() < PHASE_ALLOC_ROM);

    // Just to be safe, we'll strip byteify information:
    for(locator_t& loc : rom_array.data)
        loc.set_byteified(false);

    rom_array_ht ret;
    rom_array_meta_t* meta;

    {
        std::lock_guard<std::mutex> lock(rom_array_map_mutex);
        auto result = rom_array_map.emplace(std::move(rom_array), 
            []() -> rom_array_meta_t { return rom_array_meta_t(); });
        ret = { result.first - rom_array_map.begin() };
        // Get address while locked, for thread safety:
        meta = &result.first->second;
    }

    assert(meta);
    assert(ret);

    if(rp)
        meta->mark_used_by(rp);

    if(gd)
        meta->mark_used_by(gd);

    return ret;
}

void locate_rom_arrays(fn_ht fn, ir_t& ir)
{
    assert(compiler_phase() < PHASE_ALLOC_ROM);

    for(cfg_node_t const& cfg : ir)
    for(ssa_ht ssa_it = cfg.ssa_begin(); ssa_it;)
    {
        unsigned const input_size = ssa_it->input_size();

        if(ssa_it->op() != SSA_init_array)
            goto next_iter;
        
        // We're looking for SSA_init_arrays of all constants
        // TODO: Also handle arrays of *mostly* constants.
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

            ssa_it->replace_with(locator_t::rom_array(lookup_rom_array(std::move(rom_array), fn->rom_proc())));
            ssa_it = ssa_it->prune();
            continue;
        }
    next_iter:
        ++ssa_it;
    }

}

rom_array_meta_t& get_meta(rom_array_ht h)
{
    assert(compiler_phase() >= PHASE_ALLOC_ROM);
    assert(h);
    // Don't have to lock mutex at this phase.
    return (rom_array_map.begin() + h.id)->second;
}

///////////////
// rom_procs //
///////////////

void rom_proc_t::assign(asm_proc_t&& asm_proc)
{
    // Reset the old state:
    m_uses_rom_arrays.clear();

    // Move it:
    m_asm_proc = std::move(asm_proc);

    // Update state:

    for(asm_inst_t const& inst : m_asm_proc.code)
    {
        if(inst.arg.lclass() == LOC_ROM_ARRAY)
            m_uses_rom_arrays.insert(inst.arg.rom_array());
        if(inst.ptr_hi.lclass() == LOC_ROM_ARRAY)
            m_uses_rom_arrays.insert(inst.ptr_hi.rom_array());
    }

    m_max_size = m_asm_proc.size();
}

bitset_t const* rom_proc_t::uses_groups() const 
{ 
    return m_asm_proc.fn ? &m_asm_proc.fn->ir_ptr_groups() : nullptr; 
}

bool rom_proc_t::for_each_group_test(std::function<bool(group_ht)> const& fn)
{
    if(bitset_t const* bs = uses_groups())
        return bs->for_each_test([&](unsigned i){ return fn(group_ht{i}); });
    return true;
}

rom_proc_ht rom_proc_t::make()
{
    assert(compiler_phase() < PHASE_ALLOC_ROM);
    std::lock_guard<std::mutex> lock(rom_proc_deque_mutex);
    rom_proc_ht const ret = { rom_proc_deque.size() };
    rom_proc_deque.emplace_back();
    return ret;
}

rom_proc_ht rom_proc_t::make(asm_proc_t&& asm_proc)
{
    assert(compiler_phase() < PHASE_ALLOC_ROM);
    std::lock_guard<std::mutex> lock(rom_proc_deque_mutex);
    rom_proc_ht const ret = { rom_proc_deque.size() };
    rom_proc_deque.emplace_back(std::move(asm_proc));
    return ret;
}

//////////////////////
// rom data generic //
//////////////////////

rom_data_ht to_rom_data(rom_array_t&& rom_array)
{
    return lookup_rom_array(std::move(rom_array));
}

rom_data_ht to_rom_data(asm_proc_t&& asm_proc)
{
    return rom_proc_t::make(std::move(asm_proc));
}


