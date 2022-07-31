#ifndef ROM_ARRAY_HPP
#define ROM_ARRAY_HPP

#include <cstdint>
#include <functional>
#include <mutex>
#include <vector>

#include "robin/map.hpp"
#include "robin/set.hpp"

#include "rom_decl.hpp"
#include "bitset.hpp"
#include "decl.hpp"
#include "ir_decl.hpp"
#include "locator.hpp"
#include "asm_proc.hpp"
#include "phase.hpp"

// Passkey pattern
class rom_key_t
{
    friend class rom_allocator_t;
    rom_key_t() {}
    rom_key_t(rom_key_t const&) {}
};

class rom_data_t
{
public:
    rom_alloc_ht alloc() const { return m_alloc; }
    void set_alloc(rom_alloc_ht alloc, rom_key_t) { m_alloc = alloc; }

    bool emits() const { return true; } // TODO: implement
protected:
    // These are used later on, when the rom is actually allocated.
    rom_alloc_ht m_alloc;
};

struct rom_array_t
{
    std::vector<locator_t> data;

    auto operator<=>(rom_array_t const&) const = default;

    static rom_array_t& get(rom_array_ht);
};

class rom_array_meta_t : public rom_data_t
{
public:
    rom_array_meta_t();

    void mark_used_by(rom_proc_ht rom_proc);
    void mark_used_by(group_data_ht group);

    // Don't have to lock the mutex if we're in PHASE_ALLOC_ROM.
    auto const& used_in_procs() const { assert(compiler_phase() == PHASE_ALLOC_ROM); return m_used_in_procs; }
    auto const& used_in_group_data() const { assert(compiler_phase() == PHASE_ALLOC_ROM); return m_used_in_group_data; }

    static rom_array_meta_t& get(rom_array_ht);
private:
    std::mutex m_mutex; // Protects the members below
    rh::batman_set<rom_proc_ht> m_used_in_procs;
    bitset_t m_used_in_group_data;
    // End mutex protected

};

class rom_proc_t : public rom_data_t
{
public:
    rom_proc_t() = default;
    rom_proc_t(asm_proc_t&& asm_proc) { assign(std::move(asm_proc)); }

    // Call from a single thread only!
    void assign(asm_proc_t&& asm_proc);
    
    asm_proc_t const& asm_proc() const { return m_asm_proc; }

    rh::batman_set<rom_array_ht> const& uses_rom_arrays() const { return m_uses_rom_arrays; }
    std::size_t max_size() const { return m_max_size; }

    bitset_t const* uses_groups() const;
    bool for_each_group_test(std::function<bool(group_ht)> const& fn);

    static rom_proc_ht make();
    static rom_proc_ht make(asm_proc_t&& asm_proc);
private:
    asm_proc_t m_asm_proc;
    rh::batman_set<rom_array_ht> m_uses_rom_arrays;

    // An estimate.
    std::size_t m_max_size = 0;

    // These are used later on, when the rom is actually allocated.
    rom_alloc_ht m_alloc;
};

template<>
struct std::hash<rom_array_t>
{
    std::size_t operator()(rom_array_t const& a) const noexcept
    {
        std::hash<locator_t> lh;

        // Include array size in the hash:
        std::size_t h = a.data.size();

        // Only hash the first 4 locators, for speed:
        unsigned const n = std::min<unsigned>(a.data.size(), 4);
        for(unsigned i = 0; i < n; ++i)
            h = rh::hash_combine(h, lh(a.data[i]));

        // Also hash the last locator:
        if(!a.data.empty())
            h = rh::hash_combine(h, lh(a.data.back()));

        return h;
    }
};

using rom_array_map_t = rh::joker_map<rom_array_t, rom_array_meta_t>;
extern std::mutex rom_array_map_mutex;
extern rom_array_map_t rom_array_map;

extern std::mutex rom_proc_deque_mutex;
extern std::deque<rom_proc_t> rom_proc_deque;

// Creates a new rom_array, or returns an existing one matching 'rom_array'.
// Modifies 'result_h' with the looked up handle.
rom_array_ht lookup_rom_array(rom_array_t&& rom_array, rom_proc_ht={}, group_data_ht={});

// Orders CFG basic blocks, trying to find an optimal layout for branches.
void locate_rom_arrays(fn_ht fn, ir_t& ir);

rom_array_meta_t& get_meta(rom_array_ht h);

// Generic construction functions:
rom_data_ht to_rom_data(rom_array_t&& rom_array);
rom_data_ht to_rom_data(asm_proc_t&& asm_proc);

#endif
