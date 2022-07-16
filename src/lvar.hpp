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

// Tracks all vars used in assembly code, assigning them an index.
class lvars_manager_t
{
public:
    lvars_manager_t() = default;
    lvars_manager_t(fn_ht fn, ir_t const& ir);
    
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

    unsigned mem_size(unsigned i) const
    { 
        assert(i < m_sizes_and_zp_only.size());
        return m_sizes_and_zp_only[i] >> 1;
    }
    bool mem_zp_only(unsigned i) const
    { 
        assert(i < m_sizes_and_zp_only.size());
        return m_sizes_and_zp_only[i] & 1;
    }

    std::size_t num_this_lvars() const { return m_num_this_lvars; }
    std::size_t num_all_lvars() const { return m_num_lvars; }
    std::size_t num_locators() const { return m_map.size(); }
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
    static bool is_tracked_non_lvar(fn_ht fn, locator_t arg);
    static bool is_tracked(fn_ht fn, locator_t arg);
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

    template<typename Fn>
    void for_each_non_lvar(Fn const& fn) const
    {
        unsigned i = num_all_lvars();
        for(auto it = m_map.begin() + i; it != m_map.end(); ++it, ++i)
        {
            assert(i == unsigned(index(*it)));
            fn(*it, i);
        }
    }

private:
    bitset_uint_t* lvar_interferences(unsigned i) 
    { 
        assert(i < m_lvar_interferences.size());
        return &m_lvar_interferences[i * bitset_size()]; 

    }

    fc::vector_set<fn_ht>& fn_interferences(unsigned i) 
    { 
        assert(i < m_fn_interferences.size());
        return m_fn_interferences[i]; 
    }

    std::uint64_t m_seen_args = 0;
    rh::batman_set<locator_t> m_map;
    std::vector<unsigned> m_sizes_and_zp_only;
    std::vector<bitset_uint_t> m_lvar_interferences;
    std::vector<fc::vector_set<fn_ht>> m_fn_interferences;
    unsigned m_num_this_lvars = 0;
    unsigned m_num_lvars = 0;
    unsigned m_bitset_size = 0;
};

#endif
