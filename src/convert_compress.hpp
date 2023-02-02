#ifndef CONVERT_COMPRESS_HPP
#define CONVERT_COMPRESS_HPP

#include <cstdint>
#include <vector>

#include "convert.hpp"

std::vector<std::uint8_t> compress_pbz(std::uint8_t* begin, std::uint8_t* end);
conversion_t convert_pbz(std::uint8_t* begin, std::uint8_t* end);

// GBA RLUnComp
// See: https://www.nesdev.org/wiki/Tile_compression#GBA_RLUnComp
std::vector<std::uint8_t> compress_rlz(std::uint8_t* begin, std::uint8_t* end, bool terminate);
conversion_t convert_rlz(std::uint8_t* begin, std::uint8_t* end, bool terminate);

#endif
