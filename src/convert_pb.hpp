#ifndef CONVERT_PB_HPP
#define CONVERT_PB_HPP

#include <cstdint>
#include <vector>

#include "convert.hpp"

std::vector<std::uint8_t> compress_pbz(std::uint8_t* begin, std::uint8_t* end);
conversion_t convert_pbz(std::uint8_t* begin, std::uint8_t* end);

#endif
