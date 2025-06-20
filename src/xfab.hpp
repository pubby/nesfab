#ifndef XFAB_HPP
#define XFAB_HPP

#include <cstdint>

#include "pstring.hpp"

// 8x8Fab level files

struct xfab_macros_t
{
    std::string chr;
    std::string palette;
    std::string level;
};

enum xfab_convert_type_t
{
    XFAB_INVALID,
    XFAB_RAW,
    XFAB_RLZ,
    XFAB_PBZ,
};

template<typename Handle>
class ident_map_t;
struct global_ht;
struct group_ht;

void convert_xfab(xfab_convert_type_t ct, std::uint8_t const* const begin, std::size_t size, 
                  lpstring_t at, fs::path xfab_path, xfab_macros_t const& macros,
                  ident_map_t<global_ht>* private_globals,
                  ident_map_t<group_ht>* private_groups);

#endif
