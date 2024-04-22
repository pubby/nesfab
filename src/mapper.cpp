#include "mapper.hpp"

#include <stdexcept>
#include <cstring>
#include <charconv>

#include "builtin.hpp"
#include "format.hpp"

mapper_mirroring_t mapper_params_t::mirroring_none(mapper_type_t mt) const
{
    if(mirroring == MIRROR_NONE)
        return MIRROR_NONE;
    throw std::runtime_error(fmt("Unsupported % mirroring.", mapper_name(mt)));
}

mapper_mirroring_t mapper_params_t::mirroring_HV(mapper_type_t mt) const
{
    if(mirroring == MIRROR_NONE)
        return MIRROR_V;
    else if(mirroring != MIRROR_H && mirroring != MIRROR_V)
        throw std::runtime_error(fmt("Unsupported % mirroring. Expecting H or V.", mapper_name(mt)));
    return mirroring;
}

mapper_mirroring_t mapper_params_t::mirroring_4(mapper_type_t mt) const
{
    if(mirroring == MIRROR_NONE || mirroring == MIRROR_4)
        return MIRROR_4;
    throw std::runtime_error(fmt("Unsupported % mirroring. Expecting 4.", mapper_name(mt)));
}

mapper_mirroring_t mapper_params_t::mirroring_1(mapper_type_t mt) const
{
    if(mirroring == MIRROR_NONE || mirroring == MIRROR_1)
        return MIRROR_1;
    throw std::runtime_error(fmt("Unsupported % mirroring. Expecting 1.", mapper_name(mt)));
}

unsigned mapper_params_t::num_32k_banks(mapper_type_t mt, unsigned min, unsigned max, unsigned default_) const
{
    if(!prg_size)
        return default_;
    if(prg_size < min)
        throw std::runtime_error(fmt("Invalid % PRG size: %. Minimum accepted: %.", mapper_name(mt), prg_size, min));
    if(prg_size > max)
        throw std::runtime_error(fmt("Invalid % PRG size: %. Maximum accepted: %.", mapper_name(mt), prg_size, max));
    if((prg_size % 32) != 0)
        throw std::runtime_error(fmt("Invalid % PRG size: %. Expecting a multiple of 32.", mapper_name(mt), prg_size));
    return prg_size / 32;
}

unsigned mapper_params_t::num_16k_banks(mapper_type_t mt, unsigned min, unsigned max, unsigned default_) const
{
    if(!prg_size)
        return default_;
    if(prg_size < min)
        throw std::runtime_error(fmt("Invalid % PRG size: %. Minimum accepted: %.", mapper_name(mt), prg_size, min));
    if(prg_size > max)
        throw std::runtime_error(fmt("Invalid % PRG size: %. Maximum accepted: %.", mapper_name(mt), prg_size, max));
    if((prg_size % 16) != 0)
        throw std::runtime_error(fmt("Invalid % PRG size: %. Expecting a multiple of 16.", mapper_name(mt), prg_size));
    return prg_size / 16;
}

unsigned mapper_params_t::num_8k_chr(mapper_type_t mt, unsigned min, unsigned max, unsigned default_) const
{
    if(!chr_size)
        return default_;
    if(chr_size < min)
        throw std::runtime_error(fmt("Invalid % CHR size: %. Minimum accepted: %.", mapper_name(mt), prg_size, min));
    if(chr_size > max)
        throw std::runtime_error(fmt("Invalid % CHR size: %. Maximum accepted: %.", mapper_name(mt), prg_size, max));
    if((chr_size % 8) != 0)
        throw std::runtime_error(fmt("Invalid % CHR size: %. Expecting a multiple of 8.", mapper_name(mt), chr_size));
    return chr_size / 8;
}

bool mapper_params_t::conflicts(mapper_type_t mt, bool default_) const
{
    if(bus_conflicts == BUSC_DEFAULT)
        return default_;
    return bus_conflicts == BUSC_ALWAYS;
}

bool mapper_params_t::conflicts(mapper_type_t mt) const
{
    if(bus_conflicts == BUSC_NEVER)
        throw std::runtime_error(fmt("Invalid %: Mapper always has bus conflicts."));
    return true;
}

bool mapper_params_t::no_conflicts(mapper_type_t mt) const
{
    if(bus_conflicts == BUSC_ALWAYS)
        throw std::runtime_error(fmt("Invalid %: Mapper never has bus conflicts."));
    return false;
}

bool mapper_params_t::has_sram(mapper_type_t mt, bool default_) const
{
    if(sram == SRAM_DEFAULT)
        return default_;
    return sram >= SRAM_FIRST_ON;
}

bool mapper_params_t::no_sram(mapper_type_t) const
{
    if(sram >= SRAM_FIRST_ON)
        throw std::runtime_error(fmt("Invalid %: Mapper does not support SRAM."));
    return false;
}

bool mapper_params_t::sram_persistent(mapper_type_t mt, bool default_) const
{
    switch(sram)
    {
    default:
        return default_;
    case SRAM_OFF:
    case SRAM_ON_VOLATILE:
        return false;
    case SRAM_ON_PERSISTENT:
        return true;
    }
}

mapper_t mapper_t::nrom(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_NROM;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_HV(mt),
        .num_banks = params.num_32k_banks(mt, 32, 32, 1),
        .num_8k_chr_rom = params.num_8k_chr(mt, 8, 8, 1),
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::anrom(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_ANROM;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_1(mt),
        .num_banks = params.num_32k_banks(mt, 32, 512, 8),
        .num_8k_chr_ram = params.num_8k_chr(mt, 8, 8, 1),
        .bus_conflicts = params.conflicts(mt, false),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::bnrom(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_BNROM;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_HV(mt),
        .num_banks = params.num_32k_banks(mt, 32, 8192, 4),
        .num_8k_chr_ram = params.num_8k_chr(mt, 8, 8, 1),
        .bus_conflicts = params.conflicts(mt, true),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::cnrom(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_CNROM;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_HV(mt),
        .num_banks = params.num_32k_banks(mt, 32, 32, 1),
        .num_8k_chr_rom = params.num_8k_chr(mt, 8, 2048, 4),
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::gnrom(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_GNROM;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_HV(mt),
        .num_banks = params.num_32k_banks(mt, 32, 512, 4),
        .num_8k_chr_rom = params.num_8k_chr(mt, 8, 128, 4),
        .bus_conflicts = params.conflicts(mt, true),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::colordreams(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_COLORDREAMS;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_HV(mt),
        .num_banks = params.num_32k_banks(mt, 32, 512, 4),
        .num_8k_chr_rom = params.num_8k_chr(mt, 8, 128, 16),
        .bus_conflicts = params.conflicts(mt, true),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::gtrom(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_GTROM;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_4(mt),
        .num_banks = params.num_32k_banks(mt, 32, 512, 16),
        .num_8k_chr_ram = params.num_8k_chr(mt, 8, 16, 2),
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::ines_189(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_189;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_none(mt),
        .num_banks = params.num_32k_banks(mt, 32, 512, 4),
        .num_8k_chr_rom = params.num_8k_chr(mt, 256, 256, 32),
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.no_sram(mt),
        .sram_persistent = false,
    };
}

mapper_t mapper_t::mmc1(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_MMC1;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_none(mt),
        .num_banks = params.num_32k_banks(mt, 256, 256, 8),
        .num_8k_chr_rom = params.num_8k_chr(mt, 128, 128, 16),
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::unrom(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_UNROM;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_HV(mt),
        .num_banks = params.num_16k_banks(mt, 32, 4096, 4),
        .num_8k_chr_ram = params.num_8k_chr(mt, 8, 8, 1),
        .fixed_16k = true,
        .bus_conflicts = params.conflicts(mt, true),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::mmc3(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_MMC3;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_none(mt),
        .num_banks = params.num_16k_banks(mt, 512, 2048, 32),
        .num_8k_chr_rom = params.num_8k_chr(mt, 256, 256, 32),
        .fixed_16k = true,
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::ines_30(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_30;
    bool bus_conflicts = params.conflicts(mt, false);
    return 
    {
        .type = mt,
        .mirroring = params.mirroring,
        .num_banks = params.num_16k_banks(mt, 32, 512, 32),
        .num_8k_chr_ram = params.num_8k_chr(mt, 8, 32, 4),
        .fixed_16k = true,
        .bus_conflicts = bus_conflicts,
        .sram = params.has_sram(mt, false),
        .sram_persistent = params.sram_persistent(mt, false),
        .force_battery = !bus_conflicts,
    };
}

mapper_t mapper_t::mmc5(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_MMC5;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_none(mt),
        .num_banks = params.num_16k_banks(mt, 1024, 1024, 32),
        .num_8k_chr_rom = params.num_8k_chr(mt, 256, 256, 32),
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.has_sram(mt, true),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

mapper_t mapper_t::rainbow(mapper_params_t const& params)
{
    constexpr mapper_type_t mt = MAPPER_RAINBOW;
    return 
    {
        .type = mt,
        .mirroring = params.mirroring_none(mt),
        .num_banks = params.num_32k_banks(mt, 32, 8192, 256),
        .num_8k_chr_rom = params.num_8k_chr(mt, 8, 8192, 1024),
        .bus_conflicts = params.no_conflicts(mt),
        .sram = params.has_sram(mt, true),
        .sram_persistent = params.sram_persistent(mt, false),
    };
}

unsigned mapper_t::submapper() const
{
    switch(type)
    {
    default:
        return 0;

    case MAPPER_BNROM:
        return 2;
    }
}

void write_ines_header(std::uint8_t* at, mapper_t const& mapper)
{
    // https://www.nesdev.org/wiki/NES_2.0

    // 0-3
    char const magic_header[4] = { 0x4E, 0x45, 0x53, 0x1A };
    std::memcpy(at, magic_header, 4);

    // 4
    at[4] = std::uint8_t(mapper.num_16k_banks()); // Banks in 16k units, low byte.

    // 5
    at[5] = std::uint8_t(mapper.num_8k_chr_rom); // Banks in 16k units.

    // 6
    std::uint8_t flags6 = 0;
    flags6 |= unsigned(mapper.type) << 4;
    switch(mapper.mirroring)
    {
    default: break;
    case MIRROR_1: if(mapper.type == MAPPER_30) flags6 |= 1 << 3; break;
    case MIRROR_4: flags6 |= 1 << 3; // fall-through
    case MIRROR_V: flags6 |= 1 << 0; break;
    }
    if((mapper.sram && mapper.sram_persistent) || mapper.force_battery)
        flags6 |= 1 << 1; // Battery-backed RAM.
    at[6] = flags6;

    // 7
    std::uint8_t flags7 = 0b00001000; // NES 2.0 format
    flags7 |= unsigned(mapper.type) & 0b11110000;
    at[7] = flags7;

    // 8
    std::uint8_t flags8 = 0;
    flags8 |= (unsigned(mapper.type) >> 8) & 0b1111;
    flags8 |= (mapper.submapper() << 4);
    at[8] = flags8;

    // 9
    if(((mapper.num_16k_banks()) >> 8) > 0b1111)
        throw std::runtime_error("Too many ROM banks.");

    if(((mapper.num_8k_chr_rom) >> 8) > 0b1111)
        throw std::runtime_error("Too many CHR RAM banks.");

    std::uint8_t hi = 0;
    hi |= ((mapper.num_16k_banks()) >> 8) & 0b1111;
    hi |= ((mapper.num_8k_chr_rom) >> 4) & 0b11110000;
    at[9] = hi;

    // 10
    if(mapper.sram)
    {
        if(mapper.sram_persistent)
            at[10] = 7 << 4;
        else
            at[10] = 7;
    }
    else
        at[10] = 0;

    // 11
    unsigned const chr_ram_chunks = mapper.num_8k_chr_ram * 0x2000 / 64;
    if(chr_ram_chunks && builtin::popcount(chr_ram_chunks) != 1)
        throw std::runtime_error("Invalid CHR RAM size.");
    unsigned const chr_shift = chr_ram_chunks ? builtin::rclz(chr_ram_chunks)-1 : 0;
    assert(!chr_ram_chunks || 64 << chr_shift == mapper.num_8k_chr_ram * 0x2000);

    if(chr_shift > 0b1111)
        throw std::runtime_error("CHR RAM is too large.");

    at[11] = chr_shift & 0b1111;

    // 12
    at[12] = 0;

    // 13
    at[13] = 0;

    // 14
    at[14] = 0;

    // 15
    at[15] = 0;
}

std::string_view mapper_name(mapper_type_t mt)
{
    using namespace std::literals;

    switch(mt)
    {
    default: return "unknown mapper"sv;
#define MAPPER(name, value) case MAPPER_##name: return #name ""sv;
    MAPPER_XENUM
#undef MAPPER
    }
}
