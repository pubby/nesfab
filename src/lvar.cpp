#include "lvar.hpp"

#include "asm.hpp"
#include "ir.hpp"
#include "cg.hpp"
#include "globals.hpp"
#include "asm_proc.hpp"
#include "asm_graph.hpp"

lvars_manager_t::lvars_manager_t(fn_ht fn, asm_graph_t const& graph)
{
    assert(fn);

    auto const insert_this_lvar = [&](locator_t loc) -> int
    {
        assert(is_this_lvar(fn, loc));
        auto result = m_map.insert(loc.mem_head());

        if(result.second)
        {
            passert(loc.mem_size(), loc, loc.type());
            m_this_lvar_info.push_back({ 
                .size = loc.mem_size(), 
                .zp_only = loc.mem_zp_only(), 
                .zp_valid = loc.mem_zp_valid() 
            });

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
        m_this_lvar_info[index].zp_valid = true;
        m_this_lvar_info[index].ptr_hi = ptr_hi;
        m_this_lvar_info[index].ptr_alt = ptr_alt;
    };

    // Add 'this_lvar's
    graph.for_each_inst([&](asm_inst_t const& inst)
    {
        int arg_index = -1;
        if(is_this_lvar(fn, inst.arg))
            arg_index = insert_this_lvar(inst.arg);

        if(inst.alt && is_this_lvar(fn, inst.alt))
        {
            int const ptr_hi_index = insert_this_lvar(inst.alt);
            assert(arg_index != ptr_hi_index);

            mark_ptr(ptr_hi_index, arg_index, true);

            if(arg_index >= 0)
                mark_ptr(arg_index, ptr_hi_index, false);
        }
    });

    // For modes, every arg is seen, as compilation may be done out of order:
    if(fn->fclass == FN_MODE)
    {
        m_seen_args = ~0ull;

        for(unsigned i = 0; i < fn->def().num_params; ++i)
        {
            var_decl_t const& decl = fn->def().local_vars[i].decl;
            type_t const param_type = decl.src_type.type;
            unsigned const num_members = ::num_members(param_type);

            for(unsigned j = 0; j < num_members; ++j)
            {
                type_t const member_type = ::member_type(param_type, j);
                unsigned const num_atoms = ::num_atoms(member_type, 0);

                for(unsigned k = 0; k < num_atoms; ++k)
                    insert_this_lvar(locator_t::arg(fn, i, j, k));
            }
        }
    }

    // Also add every argument / return that has been referenced:
    fn->for_each_referenced_locator([&](locator_t loc){ insert_this_lvar(loc); });

    m_num_this_lvars = m_map.size();
    assert(m_this_lvar_info.size() == m_map.size());

    // Add 'call_lvar's
    graph.for_each_inst([&](asm_inst_t const& inst)
    {
        if(mem_inst(inst) && is_call_lvar(fn, inst.arg))
            m_map.insert(inst.arg.mem_head());
    });

    m_bitset_size = ::bitset_size<>(m_map.size());
    m_lvar_interferences.resize(m_map.size() * m_bitset_size, 0);
    m_fn_interferences.resize(m_map.size());

#ifndef NDEBUG
    for(auto const& loc : m_map)
        assert(loc == loc.mem_head());
#endif
}

// For iasm fns:
lvars_manager_t::lvars_manager_t(fn_t const& fn)
{
    assert(fn.iasm);

    auto const insert_lvar = [&](locator_t loc, bool is_this) -> int
    {
        auto result = m_map.insert(loc.mem_head());

        if(is_this && result.second)
        {
            assert(loc.mem_size());
            m_this_lvar_info.push_back({ 
                .size = loc.mem_size(), 
                .zp_only = loc.mem_zp_only(), 
                .zp_valid = loc.mem_zp_valid() 
            });
        }

        return result.first - m_map.begin(); // Return an index
    };

    auto const add_fn = [&](fn_ht fn, bool is_this)
    {
        // Local vars:
        for(unsigned i = 0; i < fn->def().local_vars.size(); ++i)
        {
            var_decl_t const& decl = fn->def().local_vars[i].decl;
            type_t const param_type = decl.src_type.type;
            unsigned const num_members = ::num_members(param_type);

            for(unsigned j = 0; j < num_members; ++j)
            {
                type_t const member_type = ::member_type(param_type, j);
                unsigned const num_atoms = ::num_atoms(member_type, 0);

                for(unsigned k = 0; k < num_atoms; ++k)
                {
                    if(i < fn->def().num_params)
                        insert_lvar(locator_t::arg(fn, i, j, k), is_this);
                    else
                        insert_lvar(locator_t::asm_local_var(fn, i, j, k), is_this);
                }
            }
        }

        // Returns:
        type_t const return_type = fn->type().return_type();
        unsigned const num_members = ::num_members(return_type);

        for(unsigned j = 0; j < num_members; ++j)
        {
            type_t const member_type = ::member_type(return_type, j);
            unsigned const num_atoms = ::num_atoms(member_type, 0);

            for(unsigned k = 0; k < num_atoms; ++k)
                insert_lvar(locator_t::ret(fn, j, k), is_this);
        }
    };

    // Add 'this_lvar's
    add_fn(fn.handle(), true);
    m_num_this_lvars = m_map.size();

    fc::vector_set<fn_ht> called_fns;

    // Add 'call_lvars':
    for(auto const& pair : fn.precheck_tracked().calls)
    {
        add_fn(pair.first, false);
        called_fns.insert(pair.first);
    }

    m_bitset_size = ::bitset_size<>(m_map.size());
    m_lvar_interferences.resize(m_map.size() * m_bitset_size, 0);
    m_fn_interferences.resize(m_map.size(), called_fns);

    // Every lvar interferes with every other lvar:
    bitset_uint_t* bs = CALLOCA_T(bitset_uint_t, m_bitset_size);
    bitset_set_n(m_bitset_size, bs, m_map.size());
    add_lvar_interferences(bs);

    // Every arg is seen:
    m_seen_args = ~0ull;
}

bool lvars_manager_t::is_this_lvar(fn_ht fn, locator_t arg)
{
    auto const l = arg.lclass();
    return ((l == LOC_ARG && fn == arg.fn()) 
            || (l == LOC_RETURN && fn == arg.fn()) 
            || l == LOC_PHI 
            || l == LOC_SSA
            || l == LOC_MINOR_VAR);
}

bool lvars_manager_t::is_call_lvar(fn_ht fn, locator_t arg)
{
    auto const l = arg.lclass();
    return ((l == LOC_ARG && arg.fn() != fn) 
            || (l == LOC_RETURN && arg.fn() != fn));
}

