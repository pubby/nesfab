#include "mapper.hpp"

#include <stdexcept>
#include <cstring>
#include <charconv>

#include "builtin.hpp"

mapper_t mapper_t::nrom(mapper_mirroring_t mirroring)
{
    if(mirroring == MIRROR_NONE)
        mirroring = MIRROR_V;
    else if(mirroring != MIRROR_H && mirroring != MIRROR_V)
        throw std::runtime_error("Unsupported NROM mirroring.");

    return 
    {
        .type = MAPPER_NROM,
        .mirroring = mirroring,
        .num_32k_banks = 1,
        .num_8k_chr_rom = 1,
        .num_8k_chr_ram = 0,
    };
}

mapper_t mapper_t::cnrom(mapper_mirroring_t mirroring, unsigned banks_8k)
{
    if(mirroring == MIRROR_NONE)
        mirroring = MIRROR_V;
    else if(mirroring != MIRROR_H && mirroring != MIRROR_V)
        throw std::runtime_error("Unsupported CNROM mirroring.");

    if(!banks_8k)
        banks_8k = 4;
    else if(banks_8k > 256)
        throw std::runtime_error("Unsupported CNROM CHR ROM size.");

    return 
    {
        .type = MAPPER_NROM,
        .mirroring = mirroring,
        .num_32k_banks = 1,
        .num_8k_chr_rom = banks_8k,
        .num_8k_chr_ram = 0,
    };
}

mapper_t mapper_t::anrom(unsigned banks_32k)
{
    if(!banks_32k)
        banks_32k = 8;
    else if(banks_32k > 16)
        throw std::runtime_error("Unsupported BxROM PRG ROM size.");

    return 
    {
        .type = MAPPER_ANROM,
        .mirroring = MIRROR_NONE,
        .num_32k_banks = banks_32k,
        .num_8k_chr_rom = 0,
        .num_8k_chr_ram = 1,
    };
}

mapper_t mapper_t::bnrom(mapper_mirroring_t mirroring, unsigned banks_32k)
{
    if(mirroring == MIRROR_NONE)
        mirroring = MIRROR_V;
    else if(mirroring != MIRROR_H && mirroring != MIRROR_V)
        throw std::runtime_error("Unsupported BxROM mirroring.");

    if(!banks_32k)
        banks_32k = 4;
    else if(banks_32k > 256)
        throw std::runtime_error("Unsupported BxROM PRG ROM size.");

    return 
    {
        .type = MAPPER_BNROM,
        .mirroring = mirroring,
        .num_32k_banks = banks_32k,
        .num_8k_chr_rom = 0,
        .num_8k_chr_ram = 1,
    };
}

mapper_t mapper_t::gnrom(mapper_mirroring_t mirroring, unsigned banks_32k, unsigned banks_8k)
{
    if(mirroring == MIRROR_NONE)
        mirroring = MIRROR_V;
    else if(mirroring != MIRROR_H && mirroring != MIRROR_V)
        throw std::runtime_error("Unsupported GxROM mirroring.");

    if(!banks_32k)
        banks_32k = 4;
    else if(banks_32k > 16)
        throw std::runtime_error("Unsupported GxROM PRG ROM size.");

    if(!banks_8k)
        banks_8k = 4;
    else if(banks_8k > 16)
        throw std::runtime_error("Unsupported GxROM CHR ROM size.");

    return 
    {
        .type = MAPPER_GNROM,
        .mirroring = mirroring,
        .num_32k_banks = banks_32k,
        .num_8k_chr_rom = banks_8k,
        .num_8k_chr_ram = 0,
    };
}

mapper_t mapper_t::gtrom()
{
    return 
    {
        .type = MAPPER_GTROM,
        .mirroring = MIRROR_4,
        .num_32k_banks = 16,
        .num_8k_chr_rom = 0,
        .num_8k_chr_ram = 2,
    };
}

/* TODO remove
static mapper_t mapper_t::from_string(std::string const& str)
{
    using std::literals;

    char const* ptr = str.data();
    char const* const end = str.data() + str.size();

    constexpr char delim = '_';

    auto const parse = [&](char const* expecting)
    {
        if(ptr != end && *ptr == delim)
            ++ptr;
        char const* begin = ptr;
        while(ptr != end && *ptr != delim)
            ++ptr;
        if(begin == ptr)
            throw std::runtime_error(fmt("Invalid mapper description: \"%\". Expecting %.", 
                                         str, expecting));
        return std::string(begin, ptr);
    };

    auto const parse_mirroring = [&]() - > mapper_mirroring_t
    {
        std::string const mirroring = parse("mirroring option");
        if(mirroring == "H"sv)
            return MIRROR_H;
        if(mirroring == "V"sv)
            return MIRROR_V;
        if(mirroring == "4"sv)
            return MIRROR_4;
        throw std::runtime_error(fmt("Unknown mapper mirroring: \"%\".", mirroring));
    };

    auto const parse_uint = [&](char const* expecting = "integer") -> unsigned
    {
        std::string const int_str = parse(expecting);
        unsigned u;
        auto result = std::from_chars(&*int_str.begin(), &*int_str.end(), u);
        if(result.ptr != &*int_str.end() || result.ec != std::errc())
            throw std::runtime_error(fmt("Invalid mapper description: \"%\". Expecting %.", 
                                         str, expecting));
        return u;
    };

    auto const parse_size = [&](bool ROM) -> unsigned
    {
        char const* expecting = ROM ? "mapper PRG ROM size" : "mapper CHR ROM size";
        unsigned size = parse_uint(expecting);
        if((result % 32) != 0)
            throw std::runtime_error(fmt("Invalid %: \"%\". Expecting a multiple of 32.", expecting, size));
        return result / 32;
    };

    auto const end_parse = [&]
    {
        if(ptr != end)
            throw std::runtime_error(fmt("Invalid mapper description: \"%\". Trailing characters: %\"%\".", 
                                         str, std::string_view(str, end)));
    };

    std::string const name = parse("mapper name");

    if(name == "NROM"sv)
    {
        mapper_mirror_t const mirroring = parse_mirroring();
        end_parse();
        return nrom(mirroring);
    }

    if(name == "AxROM"sv)
    {
        unsigned const prg_size = parse_size(true);
        end_parse();
        return anrom(prg_size);
    }

    if(name == "BxROM"sv)
    {
        mapper_mirror_t const mirroring = parse_mirroring();
        unsigned const prg_size = parse_size(true);
        end_parse();
        return bnrom(mirroring, prg_size);
    }

    if(name == "GxROM"sv)
    {
        mapper_mirror_t const mirroring = parse_mirroring();
        unsigned const prg_size = parse_size(true);
        unsigned const chr_size = parse_size(false);
        end_parse();
        return bnrom(mirroring, size);
    }

    if(name == "GTROM"sv)
    {
        mapper_mirror_t const mirroring = parse_mirroring();
        unsigned const prg_size = parse_size(true);
        unsigned const chr_size = parse_size(false);
        end_parse();
        return bnrom(mirroring, size);
    }

    -mapper BxROM_H_3260

    ANROM_H

    if(str == "NROM:H"sv)
    if(str == "NROM:V"sv)
        return nrom(MIRROR_V);

    if(str == "ANROM:128"sv)
        return anrom(4);
    if(str == "ANROM:256"sv)
        return anrom(8);
    if(str == "ANROM:512"sv)
        return anrom(16);

    if(str == "BNROM-128"sv)
        return bnrom(4);
    if(str == "BNROM-256"sv)
        return bnrom(8);
    if(str == "BNROM-512"sv)
        return bnrom(16);

    MAPPER_NROM = 0,
    MAPPER_ANROM = 7,
    MAPPER_BNROM = 34,
    MAPPER_GNROM = 66,
    MAPPER_GTROM = 111,

}
*/

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
    case MIRROR_V: flags6 |= 1 << 0; break;
    case MIRROR_4: flags6 |= 1 << 3; break;
    }
    at[6] = flags6;

    // 7
    std::uint8_t flags7 = 0b00001000; // NES 2.0 format
    flags7 |= unsigned(mapper.type) & 0b11110000;
    at[7] = flags7;

    // 8
    std::uint8_t flags8 = 0;
    flags8 |= (unsigned(mapper.type) >> 8) & 0b1111;
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
