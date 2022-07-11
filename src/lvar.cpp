#include "lvar.hpp"

#include "ir.hpp"
#include "cg.hpp"

lvars_manager_t::lvars_manager_t(fn_ht fn, ir_t const& ir)
{
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(asm_inst_t inst : cg_data(cfg_it).code)
    {
        if(is_this_lvar(fn, inst.arg))
        {
            auto result = m_map.insert(inst.arg.mem_head());

            if(result.second)
                m_sizes_and_zp_only.push_back((inst.arg.mem_size() << 1) | inst.arg.mem_zp_only());

            if(inst.arg.lclass() == LOC_ARG)
            {
                assert(inst.arg.fn() == fn);
                assert(inst.arg.arg() < sizeof_bits<decltype(m_seen_args)>);

                m_seen_args |= 1ull << inst.arg.arg();
            }
        }
    }

    m_num_this_lvars = m_map.size();

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(asm_inst_t inst : cg_data(cfg_it).code)
        if(is_call_lvar(fn, inst.arg))
            m_map.insert(inst.arg.mem_head());

    m_bitset_size = ::bitset_size<>(m_map.size());

    m_lvar_interferences.resize(m_map.size() * m_bitset_size, 0);
    m_fn_interferences.resize(m_map.size());
}

bool lvars_manager_t::is_this_lvar(fn_ht fn, locator_t arg)
{
    auto const l = arg.lclass();
    return (l == LOC_ARG && fn == arg.fn()) || l == LOC_PHI || l == LOC_SSA;
}

bool lvars_manager_t::is_call_lvar(fn_ht fn, locator_t arg)
{
    auto const l = arg.lclass();
    return (l == LOC_ARG && arg.fn() != fn);
}

