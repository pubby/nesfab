#ifndef MAPFAB_HPP
#define MAPFAB_HPP

#include <cstdint>

#include "pstring.hpp"

// MapFab level files

struct mapfab_macros_t
{
    std::string chr;
    std::string palette;
    std::string metatiles;
    std::string level;
};

void convert_mapfab(std::uint8_t const* const begin, std::size_t size, pstring_t at, 
                    fs::path mapfab_path, mapfab_macros_t const& macros);

#endif
