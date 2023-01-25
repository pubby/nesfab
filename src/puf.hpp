#ifndef PUF_HPP
#define PUF_HPP

#include <cstdint>

#include "pstring.hpp"

// PUF music engine

void convert_puf_music(char const* const begin, std::size_t size, pstring_t at);

void convert_puf_sfx(char const* const txt_data, std::size_t txt_size, 
                     std::uint8_t const* const nsf_data, std::size_t nsf_size,
                     pstring_t at);

#endif
