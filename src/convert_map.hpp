#ifndef CONVERT_MAP_HPP
#define CONVERT_MAP_HPP

#include <cstdint>
#include <vector>

#include "convert.hpp"

std::vector<std::uint8_t> map_to_nt(std::uint8_t const* map, std::size_t size);

#endif
