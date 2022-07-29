#ifndef ROM_HPP
#define ROM_HPP

#include <variant>
#include <vector>

#include "robin/map.hpp"

#include "rom_decl.hpp"
#include "handle.hpp"
#include "bitset.hpp"
#include "phase.hpp"
#include "span_allocator.hpp"
#include "decl.hpp"

class locator_t;
class asm_proc_t;

template<typename T> 
inline std::vector<T> rom_vector;

template<typename T>
struct rom_impl_ht : handle_t<unsigned, T, ~0>
{
    T* operator->() const { return &operator*(); }
    T& operator*() const
    { 
        assert(compiler_phase() >= PHASE_ALLOC_ROM);
        assert(this->id < rom_vector<T>.size());
        return rom_vector<T>[this->id];
    }
};

struct rom_static_ht : rom_impl_ht<rom_static_t> {};
struct rom_many_ht : rom_impl_ht<rom_many_t> {};
struct rom_once_ht : rom_impl_ht<rom_once_t> {};

namespace std
{
    template<> struct hash<rom_static_ht> : handle_hash_t<rom_static_ht> {};
    template<> struct hash<rom_many_ht> : handle_hash_t<rom_many_ht> {};
    template<> struct hash<rom_once_ht> : handle_hash_t<rom_once_ht> {};
}

static constexpr unsigned max_banks = 256;
using bank_bitset_t = static_bitset_t<max_banks>;

using rom_data_t = std::variant
    < std::monostate
    , std::vector<locator_t> const*
    , asm_proc_t*
    >;

std::size_t rom_data_size(rom_data_t const& data);

struct rom_alloc_t
{
    std::uint16_t desired_alignment = 0;
    std::uint16_t desired_size = 0;

    span_t span = {};
    rom_data_t data;
}; 

struct rom_static_t : public rom_alloc_t
{
    // TODO
};

struct rom_many_t : public rom_alloc_t
{

    bank_bitset_t in_banks;
};

struct rom_once_t : public rom_alloc_t
{
    std::uint16_t desired_alignment = 0;
    std::uint16_t desired_size = 0;

    // Set of MANYs that this node depends upon.
    bitset_uint_t* required_manys;

    // Points to a set of ONCEs that ideally belong in the same bank(s)
    // It's a pointer because it's duplicated across all related onces.
    bitset_uint_t const* related_onces;

    // Which bank we're allocated in
    unsigned bank;
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
    //std::vector<span_t> many_spans;
    rh::robin_map<rom_many_ht, span_t> many_spans;
    bitset_t allocated_manys;
    bitset_t allocated_onces;
};

#endif
