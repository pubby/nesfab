#ifndef RAM_HPP
#define RAM_HPP

#include <cstdint>

#include "bitset.hpp"
#include "options.hpp"

constexpr std::uint16_t ram_size = 0x800;
constexpr std::uint16_t sram_size = 0x2000;
constexpr std::uint16_t ram_addr  = 0x0000;
constexpr std::uint16_t sram_addr = 0x6000;
using ram_bitset_t = static_bitset_t<ram_size>;
using sram_bitset_t = static_bitset_t<sram_size>;
using page_bitset_t = static_bitset_t<256>;
constexpr ram_bitset_t zp_bitset = { .array = { ~0ull, ~0ull, ~0ull, ~0ull }};
constexpr ram_bitset_t stack_bitset = { .array = { 0ull, 0ull, 0ull, 0ull, ~0ull, ~0ull, ~0ull, ~0ull }};

class ram_sets_t
{
public:
    ram_sets_t() 
    : ram()
    {
        if(mapper().sram)
            sram.reset(new sram_bitset_t());
    }

    explicit ram_sets_t(ram_bitset_t const& ram) 
    : ram(ram)
    , sram()
    {
        if(mapper().sram)
            sram.reset(new sram_bitset_t());
    }

    ram_sets_t(ram_sets_t const& o) 
    : ram(o.ram)
    , sram(o.sram ? new sram_bitset_t(*o.sram) : nullptr)
    {}

    ram_sets_t(ram_sets_t&&) = default;

    ram_sets_t& operator=(ram_sets_t const& o)
    {
        ram = o.ram;
        if(o.sram)
            sram.reset(new sram_bitset_t(*o.sram));
        return *this;
    }

    ram_sets_t& operator=(ram_sets_t&& o) = default;

    void clear_all()
    {
        ram.clear_all();
        if(sram)
            sram->clear_all();
    }

    void set_all()
    {
        ram.set_all();
        if(sram)
            sram->set_all();
    }

    bool all_clear() const
    {
        if(sram)
            return ram.all_clear() && sram->all_clear();
        return ram.all_clear();
    }

    void flip_all()
    {
        ram.flip_all();
        if(sram)
            sram->flip_all();
    }

    std::size_t popcount() const
    {
        std::size_t count = ram.popcount();
        if(sram)
            count += sram->popcount();
        return count;
    }

    ram_sets_t& operator&=(ram_sets_t const& o)
    {
        ram &= o.ram;
        if(sram && o.sram)
            *sram &= *o.sram;
        return *this;
    }

    ram_sets_t operator&(ram_sets_t const& o) const
    {
        ram_sets_t copy = *this;
        copy &= o;
        return copy;
    }

    ram_sets_t& operator|=(ram_sets_t const& o)
    {
        ram |= o.ram;
        if(sram && o.sram)
            *sram |= *o.sram;
        return *this;
    }

    ram_sets_t operator|(ram_sets_t const& o) const
    {
        ram_sets_t copy = *this;
        copy |= o;
        return copy;
    }

    ram_sets_t& operator^=(ram_sets_t const& o)
    {
        ram ^= o.ram;
        if(sram && o.sram)
            *sram ^= *o.sram;
        return *this;
    }

    ram_sets_t operator^(ram_sets_t const& o) const
    {
        ram_sets_t copy = *this;
        copy ^= o;
        return copy;
    }

    ram_sets_t& operator-=(ram_sets_t const& o)
    {
        ram -= o.ram;
        if(sram && o.sram)
            *sram -= *o.sram;
        return *this;
    }

    ram_sets_t operator-(ram_sets_t const& o) const
    {
        ram_sets_t copy = *this;
        copy -= o;
        return copy;
    }

    ram_bitset_t ram = {};
    std::unique_ptr<sram_bitset_t> sram;
};

inline ram_bitset_t& get_ram(ram_sets_t& sets)   { return sets.ram; }
inline sram_bitset_t& get_sram(ram_sets_t& sets) { assert(sets.sram); return *sets.sram; }

#endif
