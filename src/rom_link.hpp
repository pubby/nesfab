#ifndef ROM_LINK_HPP
#define ROM_LINK_HPP

#include <cstdint>
#include <vector>

class locator_t;

std::vector<std::uint8_t> write_rom(std::uint8_t default_fill = 0x00);

#endif
