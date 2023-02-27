#ifndef ROM_HPP
#define ROM_HPP

#include <ranges>
#include <vector>

#include "robin/map.hpp"
#include "robin/set.hpp"

#include "rom_decl.hpp"
#include "handle.hpp"
#include "bitset.hpp"
#include "phase.hpp"
#include "span_allocator.hpp"
#include "decl.hpp"
#include "locator.hpp"
#include "asm_proc.hpp"
#include "options.hpp"

class locator_t;
class asm_proc_t;

//////////////
// ROM data //
//////////////

// Passkey pattern
class rom_key_t
{
    friend class rom_allocator_t;
    friend class rom_array_t;
    rom_key_t() {}
    rom_key_t(rom_key_t const&) {}
};

class rom_data_t
{
public:
    rom_data_t(romv_allocs_t const& a, romv_flags_t desired_romv, bool align) 
    : m_allocs(a)
    , m_align(align)
    , m_desired_romv(desired_romv)
    {}

    rom_alloc_ht get_alloc(romv_t romv) const 
    {
        assert(romv < m_allocs.size()); 
        return m_allocs[romv];
    }

    rom_alloc_ht find_alloc(romv_t romv) const 
    { 
        assert(romv < m_allocs.size()); 
        rom_alloc_ht h = m_allocs[romv]; 
        for(unsigned i = 0; !h && i < NUM_ROMV; ++i)
            h = m_allocs[i];
        return h;
    }

    void set_alloc(romv_t romv, rom_alloc_ht alloc, rom_key_t) 
        { assert(compiler_phase() == PHASE_PREPARE_ALLOC_ROM); m_allocs[romv] = alloc; }

    romv_flags_t desired_romv() const { assert(compiler_phase() >= PHASE_PREPARE_ALLOC_ROM); return m_desired_romv; }
    bool align() const { assert(compiler_phase() >= PHASE_PREPARE_ALLOC_ROM); return m_align; }
    bool dpcm() const { assert(compiler_phase() >= PHASE_PREPARE_ALLOC_ROM); return m_dpcm; }
    bool emits() const { assert(compiler_phase() >= PHASE_PREPARE_ALLOC_ROM); return m_emits; }
    void mark_emits() { assert(compiler_phase() >= PHASE_PREPARE_ALLOC_ROM); m_emits.store(true); }
    void mark_aligned() { m_align.store(true); }
    void mark_dpcm() { m_dpcm.store(true); }

protected:
    // These are used later on, when the rom is actually allocated.
    romv_allocs_t m_allocs = {};

    std::atomic<bool> m_align = false;
    std::atomic<bool> m_dpcm = false;
    std::atomic<bool> m_emits = false;
    std::atomic<romv_flags_t> m_desired_romv = 0;
};

// Tracks a non-code segment of data that is represented as a loc_vec_t,
// which will end up in ROM.
class rom_array_t : public rom_data_t
{
public:
    rom_array_t(loc_vec_t&& vec, romv_allocs_t const& a, rom_key_t const&, bool align);

    void mark_used_by(group_data_ht group);

    auto const& data() const { return m_data; } // 'data' is immutable.

    // Don't have to lock the mutex if we're in PHASE_ALLOC_ROM.
    auto const& used_in_group_data() const { assert(compiler_phase() > rom_array_ht::phase); return m_used_in_group_data; }

    // Use this to construct globally:
    static rom_array_ht make(loc_vec_t&& vec, bool align, bool dpcm, group_data_ht={}, romv_allocs_t const& a={});

    void for_each_locator(std::function<void(locator_t)> const& fn) const;
private:
    std::vector<locator_t> m_data;

    std::mutex m_mutex; // Protects the members below
    bitset_t m_used_in_group_data;
    // End mutex protected

    inline static rh::robin_auto_table<rom_array_ht> m_pool_map;
};

// Converts SSA_make_arrays into rom_array locators.
void locate_rom_arrays(ir_t& ir, rom_proc_ht rom_proc);

// Tracks a code segment of data that will end up in ROM.
class rom_proc_t : public rom_data_t
{
public:
    rom_proc_t(romv_allocs_t const& a, romv_flags_t desired_romv, bool align) 
    : rom_data_t(a, desired_romv, align) 
    {}

    rom_proc_t(asm_proc_t&& asm_proc, romv_allocs_t const& a, romv_flags_t desired_romv, bool align)
    : rom_data_t(a, desired_romv, align)
    { assign(std::move(asm_proc)); }

    // Sets the proc's state.
    // BE CAREFUL. NO SYNCHRONIZATION!
    void assign(asm_proc_t&& asm_proc);

    // BE CAREFUL. NO SYNCHRONIZATION!
    asm_proc_t const& asm_proc() const { return m_asm_proc; }
    unsigned max_size() const { assert(m_max_size < 1 << 16); return m_max_size; }

    xbitset_t<group_ht> const* uses_groups() const;
    bool for_each_group_test(std::function<bool(group_ht)> const& fn);

    void for_each_locator(std::function<void(locator_t)> const& fn) const;
private:
    // BE CAREFUL. NO SYNCHRONIZATION!
    asm_proc_t m_asm_proc;
    unsigned m_max_size = 0; // Upper-bound on allocation size
};

// Generic construction functions:
rom_data_ht to_rom_data(loc_vec_t&& rom_array, bool align, romv_allocs_t const& a={});
rom_data_ht to_rom_data(asm_proc_t&& asm_proc, bool align, romv_allocs_t const& a={}, romv_flags_t desired_romv = 0);

///////////////
// ROM alloc //
///////////////

static constexpr unsigned max_banks = 256;
using bank_bitset_t = static_bitset_t<max_banks>;

struct rom_static_ht : pool_handle_t<rom_static_ht, std::deque<rom_static_t>, PHASE_PREPARE_ALLOC_ROM> {};
struct rom_many_ht : pool_handle_t<rom_many_ht, std::deque<rom_many_t>, PHASE_PREPARE_ALLOC_ROM> {};
struct rom_once_ht : pool_handle_t<rom_once_ht, std::deque<rom_once_t>, PHASE_PREPARE_ALLOC_ROM> {};

DEF_HANDLE_HASH(rom_static_ht);
DEF_HANDLE_HASH(rom_many_ht);
DEF_HANDLE_HASH(rom_once_ht);

struct rom_alloc_t
{
    std::uint16_t desired_alignment = 0;
    romv_t romv = {};
    span_t span = {};
    rom_data_ht data = {};
}; 

struct rom_static_t : public rom_alloc_t
{
    rom_static_t(romv_t romv, span_t span, rom_data_ht data = {}) 
        { this->romv = romv; this->span = span; this->data = data; }

    int only_bank() const { return mapper().num_32k_banks == 1 ? 0 : -1; }

    template<typename Fn>
    void for_each_bank(Fn const& fn) const
    {
        for(unsigned bank = 0; bank < mapper().num_32k_banks; ++bank)
            fn(bank);
    }
};

struct rom_many_t : public rom_alloc_t
{
    rom_many_t(romv_t romv, rom_data_ht data, std::uint16_t desired_alignment);

    bank_bitset_t in_banks = {};

    int only_bank() const { return in_banks.popcount() == 1 ? in_banks.lowest_bit_set() : -1; }

    template<typename Fn>
    void for_each_bank(Fn const& fn) const { in_banks.for_each(fn); }
};

struct rom_once_t : public rom_alloc_t
{
    rom_once_t(romv_t romv, rom_data_ht data, std::uint16_t desired_alignment);

    // Set of MANYs that this node depends upon.
    bitset_uint_t* required_manys = nullptr;

    // Points to a set of ONCEs that ideally belong in the same bank(s)
    // It's a pointer because it's duplicated across all related onces.
    bitset_uint_t const* related_onces = nullptr;

    // Which bank we're allocated in
    unsigned bank = ~0;

    int only_bank() const { return bank; }

    template<typename Fn>
    void for_each_bank(Fn const& fn) const { fn(bank); }
};

class rom_bank_t
{
public:
    rom_bank_t(span_allocator_t const& allocator, unsigned many_bs_size, unsigned once_bs_size)
    : allocator(allocator)
    , allocated_manys(many_bs_size)
    , allocated_onces(once_bs_size)
    {}

    span_allocator_t allocator;
    rh::robin_map<rom_many_ht, span_t> many_spans;
    bitset_t allocated_manys;
    bitset_t allocated_onces;
};

#endif
