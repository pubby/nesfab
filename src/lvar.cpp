#include "lvar.hpp"

#include "ir.hpp"
#include "cg.hpp"

lvars_manager_t::lvars_manager_t(fn_ht fn, ir_t const& ir)
{
    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(asm_inst_t inst : cg_data(cfg_it).code)
    {
        if(is_this_lvar(inst.arg))
        {
            auto result = m_map.insert(inst.arg.mem_head());

            if(result.second)
                m_sizes_and_zp_only.push_back((inst.arg.mem_size() << 1) | inst.arg.mem_zp_only());

            // TODO: remove
            //inst.arg = locator_t::lvar(fn, result.first - m_map.begin());
        }
    }

    m_num_this_lvars = m_map.size();

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(asm_inst_t inst : cg_data(cfg_it).code)
        if(is_call_lvar(inst.arg))
            m_map.insert(inst.arg.mem_head());

    m_bitset_size = ::bitset_size<>(m_map.size());

    m_lvar_interferences.resize(m_map.size() * m_bitset_size, 0);
    m_fn_interferences.resize(m_map.size());
}

bool lvars_manager_t::is_this_lvar(locator_t arg)
{
    auto const l = arg.lclass();
    return l == LOC_THIS_ARG || l == LOC_PHI || l == LOC_SSA;
}

bool lvars_manager_t::is_call_lvar(locator_t arg)
{
    auto const l = arg.lclass();
    return l == LOC_CALL_ARG;
}

