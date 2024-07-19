#ifndef MAPPER_HPP
#define MAPPER_HPP

#include <cassert>
#include <cstdint>
#include <string_view>

#include "span.hpp"

#define MAPPER_XENUM \
MAPPER(NROM, 0) \
MAPPER(MMC1, 1) \
MAPPER(UNROM, 2) \
MAPPER(CNROM, 3) \
MAPPER(MMC3, 4) \
MAPPER(MMC5, 5) \
MAPPER(ANROM, 7) \
MAPPER(BNROM, 34) \
MAPPER(GNROM, 66) \
MAPPER(COLORDREAMS, 11) \
MAPPER(GTROM, 111) \
MAPPER(189, 189) \
MAPPER(30, 30) \
MAPPER(RAINBOW, 682) \

struct mapper_t;

enum mapper_type_t : std::uint16_t // Values are ines mapper numbers
{
#define MAPPER(name, value) MAPPER_##name = value,
    MAPPER_XENUM
#undef MAPPER
};

inline mapper_t const& mapper();

std::string_view mapper_name(mapper_type_t mt);

enum mapper_mirroring_t : std::uint8_t
{
    MIRROR_NONE,
    MIRROR_H,
    MIRROR_V,
    MIRROR_4,
    MIRROR_1,
};

enum mapper_bus_conflicts_t : std::uint8_t
{
    BUSC_DEFAULT,
    BUSC_NEVER,
    BUSC_ALWAYS,
};

enum mapper_sram_t : std::uint8_t
{
    SRAM_DEFAULT,
    SRAM_OFF,
    SRAM_ON_DEFAULT,
    SRAM_FIRST_ON = SRAM_ON_DEFAULT,
    SRAM_ON_VOLATILE,
    SRAM_ON_PERSISTENT,
};

struct mapper_params_t
{
    mapper_mirroring_t mirroring;
    unsigned prg_size; // in KiB
    unsigned chr_size; // in KiB
    mapper_bus_conflicts_t bus_conflicts;
    mapper_sram_t sram;
    unsigned sector_size;

    mapper_mirroring_t mirroring_none(mapper_type_t mt) const;
    mapper_mirroring_t mirroring_HV(mapper_type_t mt) const;
    mapper_mirroring_t mirroring_4(mapper_type_t mt) const;
    mapper_mirroring_t mirroring_1(mapper_type_t mt) const;

    bool conflicts(mapper_type_t mt, bool default_) const;
    bool conflicts(mapper_type_t mt) const;
    bool no_conflicts(mapper_type_t mt) const;
    bool has_sram(mapper_type_t mt, bool default_) const;
    bool sram_persistent(mapper_type_t mt, bool default_) const;
    bool no_sram(mapper_type_t) const;

    unsigned num_32k_banks(mapper_type_t mt, unsigned min, unsigned max, unsigned default_) const;
    unsigned num_16k_banks(mapper_type_t mt, unsigned min, unsigned max, unsigned default_) const;
    unsigned num_8k_chr(mapper_type_t mt, unsigned min, unsigned max, unsigned default_) const;

    unsigned default_sector_size(unsigned default_) const;
};

struct mapper_t
{
    mapper_type_t type;
    mapper_mirroring_t mirroring;
    std::uint16_t num_banks;
    std::uint16_t num_8k_chr_rom;
    std::uint16_t num_8k_chr_ram;
    bool fixed_16k;
    bool bus_conflicts;
    bool sram;
    bool sram_persistent;
    bool force_battery;
    unsigned sector_size = 4096;

    unsigned num_16k_banks() const { return fixed_16k ? num_banks : num_banks * 2; }
    unsigned bank_size() const { return fixed_16k ? 0x4000 : 0x8000; }
    unsigned prg_size() const { return bank_size() * num_banks; }
    unsigned num_switched_prg_banks() const { return fixed_16k ? num_banks - 1 : num_banks; }

    static mapper_t nrom(mapper_params_t const& params);
    static mapper_t cnrom(mapper_params_t const& params);
    static mapper_t anrom(mapper_params_t const& params);
    static mapper_t bnrom(mapper_params_t const& params);
    static mapper_t gnrom(mapper_params_t const& params);
    static mapper_t colordreams(mapper_params_t const& params);
    static mapper_t gtrom(mapper_params_t const& params);
    static mapper_t ines_189(mapper_params_t const& params);
    static mapper_t mmc1(mapper_params_t const& params);
    static mapper_t unrom(mapper_params_t const& params);
    static mapper_t mmc3(mapper_params_t const& params);
    static mapper_t ines_30(mapper_params_t const& params);
    static mapper_t mmc5(mapper_params_t const& params);
    static mapper_t rainbow(mapper_params_t const& params);

    std::string_view name() const { return mapper_name(type); }
    span_t rom_span() const { return { 0x8000, 0x8000 }; }
    span_t fixed_rom_span() const { return fixed_16k ? span_t{ 0xC000, 0x4000 } : span_t{ 0x8000, 0x8000 }; }
    span_t switched_rom_span() const { return fixed_16k ? span_t{ 0x8000, 0x4000 } : span_t{ 0x8000, 0x8000 }; }
    std::uint16_t this_bank_addr() const { return fixed_16k ? 0xBFFF : 0; }
    span_t bank_span(unsigned bank) const 
    { 
        if(fixed_16k)
            return bank + 1 == num_banks ? fixed_rom_span() : switched_rom_span();
        return rom_span();
    }
    std::size_t ines_header_size() const { return 16; }
    bool bankswitches() const { return num_banks > 1; }

    unsigned submapper() const;
};


void write_ines_header(std::uint8_t* at, mapper_t const& mapper);

#include "options.hpp" // Define mapper().

constexpr std::uint16_t bankswitch_addr(mapper_type_t mt = mapper().type)
{
    // Try to keep this page-aligned, as the iota table will often get allocated here.
    switch(mt)
    {
    case MAPPER_GTROM:
        return 0x5000;
    case MAPPER_189:
        return 0x4120;
    case MAPPER_MMC1:  
        return 0xE000;
    case MAPPER_UNROM: 
    case MAPPER_30: 
        return 0xC000;
    case MAPPER_MMC5: 
        return 0x5117;
    case MAPPER_RAINBOW:
        return 0x4118;
    default: 
        return 0x8000;
    }
}

constexpr std::uint16_t vectors_after_addr(mapper_type_t mt = mapper().type)
{
    // Try to keep this page-aligned, as the iota table will often get allocated here.
    switch(mt)
    {
    case MAPPER_MMC3:
    case MAPPER_MMC5:
    case MAPPER_189:
        return 0xE000;
    default: 
        return 0;
    }
}

constexpr std::uint16_t iota_addr(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    default: 
        break;
    case MAPPER_RAINBOW:
        return 0x8000;
    }

    if(std::uint16_t addr = vectors_after_addr(mt))
        return addr;

    // Try to keep this page-aligned.
    return bankswitch_addr(mt);
}

constexpr std::uint16_t state_size(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    case MAPPER_ANROM: 
    case MAPPER_CNROM: 
    case MAPPER_GNROM: 
    case MAPPER_COLORDREAMS: 
    case MAPPER_GTROM: 
    case MAPPER_MMC1: 
    case MAPPER_30: 
        return 1;
    default: 
        return 0;
    }
}

constexpr std::uint16_t detail_size(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    case MAPPER_MMC1: 
    case MAPPER_MMC3: 
    case MAPPER_189: 
        return 1;
    default: 
        return 0;
    }
}


constexpr std::uint16_t has_mapper_reset(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    case MAPPER_MMC1: 
        return true;
    default: 
        return false;
    }
}

constexpr int bank_shift(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    case MAPPER_MMC1: 
    case MAPPER_MMC3: 
        return 1;
    case MAPPER_MMC5: 
        return 2;
    case MAPPER_GNROM:
        return 4;
    default: 
        return 0;
    }
}

constexpr int bank_add(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    default:
        return 0;
    case MAPPER_MMC3: 
        return 1;
    }
}

constexpr bool bankswitch_use_x(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    default:
        return false; // use Y
    case MAPPER_MMC3: 
        return true;
    }
}

constexpr bool mmc3_variant(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    default:
        return false;
    case MAPPER_MMC3: 
    case MAPPER_189: 
        return true;
    }
}

/////////////////////
// Expansion Audio //
/////////////////////

enum expansion_audio_t : std::uint8_t
{
    EXP_AUDIO_NONE,
    EXP_AUDIO_MMC5,
    EXP_AUDIO_RNBW,
};

constexpr expansion_audio_t expansion_audio(mapper_type_t mt = mapper().type)
{
    switch(mt)
    {
    default:
        return EXP_AUDIO_NONE;
    case MAPPER_MMC5: 
        return EXP_AUDIO_MMC5;
    case MAPPER_RAINBOW: 
        return EXP_AUDIO_RNBW;
    }
}

#endif
