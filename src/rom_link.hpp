#ifndef ROM_LINK_HPP
#define ROM_LINK_HPP

#include <cstdint>
#include <vector>

class locator_t;

struct ines_file_t
{
    std::size_t header_size;
    std::size_t prg_rom_size;
    std::size_t chr_rom_size;

    std::size_t header_start;
    std::size_t prg_rom_start;
    std::size_t chr_rom_start;

    std::vector<std::uint8_t> rom;
};

void link_variables_optimize();
ines_file_t write_rom(std::uint8_t default_fill = 0x00);

double estimate_rom_usage(std::uint8_t const* data, std::size_t size, std::size_t min_span = 8);

#endif
