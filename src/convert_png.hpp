#ifndef CONVERT_PNG_HPP
#define CONVERT_PNG_HPP

#include <cstdint>
#include <vector>

#include "convert.hpp"

std::vector<std::uint8_t> png_to_chr(std::uint8_t const* png, std::size_t size);

#endif
