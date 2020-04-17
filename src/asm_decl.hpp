#ifndef ASM_DEF_HPP
#define ASM_DEF_HPP

#include <boost/preprocessor/stringize.hpp>

enum addr_mode_t
{
#define ADDR_MODE(name) MODE_##name,
#include "addr_mode.inc"
#undef ADDR_MODE
};

inline char const* addr_mode_name(addr_mode_t addr_mode)
{
    switch(addr_mode)
    {
#define ADDR_MODE(name) case MODE_##name: return "MODE_" BOOST_PP_STRINGIZE(name);
#include "addr_mode.inc"
#undef ADDR_MODE
    }
    return nullptr;
}

#endif
