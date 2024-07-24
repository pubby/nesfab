#include "convert_png.hpp"

#include <array>
#include <stdexcept>
#include <cassert>

#include "format.hpp"
#include "lodepng/lodepng.h"

std::vector<std::uint8_t> map_to_nt(std::uint8_t const* map, std::size_t size)
{
    if(size < 4)
        throw convert_error_t(".map file contains no footer.");

    // Extract the width and height:
    unsigned const width  = map[size-4] | (map[size-3] << 8);
    unsigned const height = map[size-2] | (map[size-1] << 8);

    if(width == 0 || height == 0 || width % 32 != 0 || height % 30 != 0)
        throw convert_error_t(fmt(".map file must be divisible into 32x30 nametables. Specified size is %x%.", width, height));

    unsigned const expected_size = (width * height) + (width*height)/16 + 8;
    if(expected_size != size)
        throw convert_error_t(fmt("Invalid %x% .map file. Size of % does not match expected size of %.", 
                                  width, height, size, expected_size));

    std::uint8_t const* tiles = map;
    std::uint8_t const* attrs = map + (width * height);
    unsigned const nt_w = width / 32;
    unsigned const nt_h = height / 30;

    std::vector<std::uint8_t> result;

    for(unsigned ny = 0; ny < nt_h; ++ny)
    for(unsigned nx = 0; nx < nt_w; ++nx)
    {
        for(unsigned y = 0; y < 30; ++y)
        for(unsigned x = 0; x < 32; ++x)
            result.push_back(tiles[(nx*32 + x) + (ny*30 + y)*width]);

        for(unsigned y = 0; y < 8; ++y)
        for(unsigned x = 0; x < 8; ++x)
        {
            // Man... fuck attributes.
            unsigned const i = (nx*8 + x) + y*width/4 + (ny*7*width)/4 + (ny/2)*width/4;
            unsigned attr = attrs[i];

            if(ny % 2)
            {
                attr <<= 4;
                attr |= attrs[i + width/4] >> 4;
            }

            if(y == 7)
                attr &= 0xF;

            result.push_back(attr);
        }
    }

    return result;
}
