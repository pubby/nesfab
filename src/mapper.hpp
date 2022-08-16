#ifndef MAPPER_HPP
#define MAPPER_HPP

#include <cstdint>

#include "span.hpp"

enum mapper_type_t : std::uint16_t // Values are ines mapper numbers
{
    MAPPER_NROM = 0,
    MAPPER_BNROM = 34,
    MAPPER_GTROM = 111,
};

constexpr std::uint16_t bankswitch_addr(mapper_type_t mt)
{
    // Try to keep this page-aligned, as the iota table will get allocated here.
    switch(mt)
    {
    case MAPPER_GTROM: return 0x5000;
    default: return 0x8000;
    }
}

constexpr bool has_bus_conflicts(mapper_type_t mt)
{
    switch(mt)
    {
    case MAPPER_NROM: 
    case MAPPER_GTROM: 
        return false;
    default:
        return true;
    }
}

constexpr std::uint16_t state_size(mapper_type_t mt)
{
    switch(mt)
    {
    case MAPPER_GTROM: return 1;
    default: return 0;
    }
}

enum mapper_mirroring_t : std::uint8_t
{
    MIRROR_H,
    MIRROR_V,
    MIRROR_4,
};

struct mapper_t
{
    mapper_type_t type;
    mapper_mirroring_t mirroring;
    std::uint16_t num_32k_banks;
    std::uint16_t num_8k_chr_rom;
    std::uint16_t num_8k_chr_ram;

    unsigned num_16k_banks() const { return num_32k_banks * 2; }

    static mapper_t nrom(mapper_mirroring_t mirroring);
    static mapper_t bnrom(mapper_mirroring_t mirroring, unsigned banks_32k);
    static mapper_t gtrom(unsigned banks_32k);

    span_t rom_span() const { return { 0x8000, 0x8000 }; }
    std::size_t ines_header_size() const { return 16; }
    bool bankswitches() const { return num_32k_banks > 1; }
};

void write_ines_header(std::uint8_t* at, mapper_t const& mapper);

#endif
