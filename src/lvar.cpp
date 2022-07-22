#include "lvar.hpp"

#include "ir.hpp"
#include "cg.hpp"
#include "globals.hpp"

lvars_manager_t::lvars_manager_t(fn_ht fn, ir_t const& ir)
{
    auto const insert_this_lvar = [&](locator_t loc) -> int
    {
        assert(is_this_lvar(fn, loc));
        auto result = m_map.insert(loc.mem_head());

        if(result.second)
        {
            m_this_lvar_info.push_back({ .size = loc.mem_size(), .zp_only = loc.mem_zp_only() });

            if(loc.lclass() == LOC_ARG)
            {
                assert(loc.fn() == fn);
                assert(loc.arg() < sizeof_bits<decltype(m_seen_args)>);

                m_seen_args |= 1ull << loc.arg();
            }
        }

        return result.first - m_map.begin(); // Return an index
    };

    auto const mark_ptr = [this](unsigned index, int ptr_alt, bool ptr_hi)
    {
        if(m_this_lvar_info[index].ptr_hi)
            assert(ptr_hi);

        if(m_this_lvar_info[index].ptr_alt >= 0)
            assert(m_this_lvar_info[index].ptr_alt == ptr_alt);

        assert(m_this_lvar_info[index].size <= 2);

        m_this_lvar_info[index].size = 2;
        m_this_lvar_info[index].zp_only = true;
        m_this_lvar_info[index].ptr_hi = ptr_hi;
        m_this_lvar_info[index].ptr_alt = ptr_alt;
    };

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(asm_inst_t inst : cg_data(cfg_it).code)
    {
        int arg_index = -1;
        if(is_this_lvar(fn, inst.arg))
            arg_index = insert_this_lvar(inst.arg);

        if(inst.ptr_hi)
        {
            int const ptr_hi_index = insert_this_lvar(inst.ptr_hi);
            assert(arg_index != ptr_hi_index);

            mark_ptr(ptr_hi_index, arg_index, true);

            if(arg_index >= 0)
                mark_ptr(arg_index, ptr_hi_index, false);
        }
    }

    m_num_this_lvars = m_map.size();
    assert(m_this_lvar_info.size() == m_map.size());

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(asm_inst_t inst : cg_data(cfg_it).code)
    {
        if(is_call_lvar(fn, inst.arg))
            m_map.insert(inst.arg.mem_head());
    }

    m_num_lvars = m_map.size();

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(asm_inst_t inst : cg_data(cfg_it).code)
        if(is_tracked_non_lvar(fn, inst.arg))
            m_map.insert(inst.arg.mem_head());

    m_bitset_size = ::bitset_size<>(m_map.size());

    m_lvar_interferences.resize(m_map.size() * m_bitset_size, 0);
    m_fn_interferences.resize(m_map.size());
}

bool lvars_manager_t::is_this_lvar(fn_ht fn, locator_t arg)
{
    auto const l = arg.lclass();
    return ((l == LOC_ARG && fn == arg.fn()) 
            || (l == LOC_RETURN && fn == arg.fn()) 
            || l == LOC_PHI 
            || l == LOC_SSA);
}

bool lvars_manager_t::is_call_lvar(fn_ht fn, locator_t arg)
{
    auto const l = arg.lclass();
    return ((l == LOC_ARG && arg.fn() != fn) 
            || (l == LOC_RETURN && arg.fn() != fn));
}

bool lvars_manager_t::is_tracked_non_lvar(fn_ht fn, locator_t arg)
{
    auto const l = arg.lclass();
    return l == LOC_GMEMBER;
}

bool lvars_manager_t::is_tracked(fn_ht fn, locator_t arg)
{
    return is_lvar(fn, arg) || is_tracked_non_lvar(fn, arg);
}
