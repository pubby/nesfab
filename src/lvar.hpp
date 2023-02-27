#ifndef LVAR_HPP
#define LVAR_HPP

#include <vector>

#include "robin/set.hpp"
#include "flat/flat_set.hpp"

#include "bitset.hpp"
#include "ram.hpp"
#include "ir_decl.hpp"
#include "locator.hpp"
#include "span.hpp"

class asm_graph_t;
struct asm_inst_t;

// Tracks all vars used in assembly code, assigning them an index.
class lvars_manager_t
{
public:
    lvars_manager_t() = default;
    lvars_manager_t(fn_ht fn, asm_graph_t const& graph);
    explicit lvars_manager_t(fn_t const& fn); // For iasm

    rh::batman_set<locator_t> const& map() const { return m_map; }

    bool seen_arg(unsigned argn) const { return m_seen_args & (1ull << argn); }

    int index(locator_t var) const
    {
        if(auto result = m_map.lookup(var.mem_head()))
        {
            assert(result - m_map.begin() >= 0);
            return result - m_map.begin();
        }
        return -1;
    }

    bool is_lvar(locator_t loc) const { return is_lvar(index(loc)); }
    bool is_lvar(int i) const { return i >= 0 && unsigned(i) < num_all_lvars(); }

    locator_t locator(unsigned i) const 
    { 
        assert(i < m_map.size());
        return m_map.begin()[i];
    }

    std::size_t num_this_lvars() const { return m_num_this_lvars; }
    std::size_t num_all_lvars() const { return m_map.size(); }
    std::size_t bitset_size() const { return m_bitset_size; }

    bitset_uint_t const* lvar_interferences(unsigned i) const 
    { 
        assert(i < m_lvar_interferences.size());
        return &m_lvar_interferences[i * bitset_size()]; 
    }

    fc::vector_set<fn_ht> const& fn_interferences(unsigned i) const
    {
        assert(i < m_fn_interferences.size());
        return m_fn_interferences[i];
    }

    void add_lvar_interferences(bitset_uint_t const* bs)
    {
        bitset_for_each(bitset_size(), bs, [this, bs](unsigned i)
        {
            bitset_or(bitset_size(), lvar_interferences(i), bs);
        });
    }

    void add_fn_interference(unsigned i, fn_ht fn)
    { 
        assert(i < m_fn_interferences.size());
        m_fn_interferences[i].insert(fn); 
    }

    static bool is_this_lvar(fn_ht fn, locator_t arg);
    static bool is_call_lvar(fn_ht fn, locator_t arg);
    static bool is_lvar(fn_ht fn, locator_t arg) { return is_this_lvar(fn, arg) || is_call_lvar(fn, arg); }

    template<typename Fn>
    void for_each_lvar(bool this_lvars_only, Fn const& fn) const
    {
        unsigned i = 0;
        for(locator_t const& loc : m_map)
        {
            if(this_lvars_only && i >= num_this_lvars())
                return;
            if(i >= num_all_lvars())
                return;
            assert(i == unsigned(index(loc)));
            fn(loc, i);
            ++i;
        }
    }

    template<typename Fn>
    void for_each_locator(Fn const& fn) const
    {
        unsigned i = 0;
        for(locator_t const& loc : m_map)
        {
            assert(i == index(loc));
            fn(loc, i);
            ++i;
        }
    }

    struct loc_info_t
    {
        std::uint16_t size;
        bool zp_only;
        bool zp_valid;
        bool ptr_hi;
        int ptr_alt = -1;
    };

    loc_info_t const& this_lvar_info(unsigned index) const 
    { 
        assert(index < m_this_lvar_info.size());
        return m_this_lvar_info[index]; 
    }

private:
    bitset_uint_t* lvar_interferences(unsigned i) 
    { 
        passert(i < m_lvar_interferences.size(), i, m_lvar_interferences.size());
        return &m_lvar_interferences[i * bitset_size()]; 

    }

    fc::vector_set<fn_ht>& fn_interferences(unsigned i) 
    { 
        assert(i < m_fn_interferences.size());
        return m_fn_interferences[i]; 
    }

    std::uint64_t m_seen_args = 0;
    rh::batman_set<locator_t> m_map;

    // We'll have to cache the size of each locator, as this information
    // must persist after the 'ir_t' destructs.
    std::vector<loc_info_t> m_this_lvar_info;

    std::vector<bitset_uint_t> m_lvar_interferences;
    std::vector<fc::vector_set<fn_ht>> m_fn_interferences;

    unsigned m_num_this_lvars = 0;
    unsigned m_bitset_size = 0;
};

#endif
