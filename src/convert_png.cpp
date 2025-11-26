#include "convert_png.hpp"

#include <array>
#include <stdexcept>
#include <cassert>

#include "format.hpp"
#include "lodepng/lodepng.h"

static std::uint8_t map_grey_alpha(std::uint8_t grey, std::uint8_t alpha, std::uint8_t& transparent)
{
    transparent = alpha < 128;
    return grey >> 6;
}

namespace
{
    class palette_map_t
    {
    public:
        palette_map_t(unsigned char* palette, unsigned num_colors)
        {
            for(unsigned i = 0; i < num_colors; ++i) 
            {
                std::uint8_t const a = palette[4 * i + 3];
                if(a >= 128)
                    map.push_back(i);
            }
        }

        std::uint8_t lookup(std::uint8_t palette) const 
        { 
            for(unsigned i = 0; i < map.size(); i += 1)
                if(palette == map[i])
                    return i;
            return 0;
        }

        std::uint8_t is_alpha(std::uint8_t palette) const 
        {
            for(unsigned i = 0; i < map.size(); i += 1)
                if(palette == map[i])
                    return false;
            return true;
        }

    private:
        std::vector<std::uint8_t> map;
    };
}

std::vector<std::uint8_t> png_to_chr(std::uint8_t const* png, std::size_t size, bool chr16, std::vector<unsigned>* indices)
{
    unsigned width, height;
    std::vector<std::uint8_t> image; //the raw pixels
    std::vector<std::uint8_t> transparent;
    lodepng::State state;
    unsigned error;

    if((error = lodepng_inspect(&width, &height, &state, png, size)))
        goto fail;

    if(width % 8 != 0)
        throw convert_error_t("Image width is not a multiple of 8.");
    if(chr16 && height % 16 != 0)
        throw convert_error_t("Image height is not a multiple of 16.");
    else if(!chr16 && height % 8 != 0)
        throw convert_error_t("Image height is not a multiple of 8.");

    switch(state.info_png.color.colortype)
    {
    case LCT_PALETTE:
        {
            state.info_raw.colortype = LCT_PALETTE;
            if((error = lodepng::decode(image, width, height, state, png, size)))
                goto fail;
            LodePNGColorMode& color = state.info_png.color;
            palette_map_t map(color.palette, color.palettesize);
            unsigned const n = width * height;
            transparent.resize(n);
            for(unsigned i = 0; i < n; i += 1)
            {
                transparent[i] = map.is_alpha(image[i]);
                image[i] = map.lookup(image[i]);
            }
        }
        break;

    case LCT_GREY:
    case LCT_RGB:
        state.info_raw.colortype = LCT_GREY;
        if((error = lodepng::decode(image, width, height, state, png, size)))
            goto fail;
        transparent.resize(width * height, 0);
        for(std::uint8_t& c : image)
            c >>= 6;
        break;

    default:
        state.info_raw.colortype = LCT_GREY_ALPHA;
        if((error = lodepng::decode(image, width, height, state, png, size)))
            goto fail;
        assert(image.size() == width * height * 2);
        unsigned const n = width * height;
        transparent.resize(n);
        for(unsigned i = 0; i < n; ++i)
            image[i] = map_grey_alpha(image[i*2], image[i*2 + 1], transparent[i]);
        image.resize(n);
        break;
    }

    // Now convert to CHR
    {
        std::vector<std::uint8_t> result;
        result.reserve(image.size() / 4);

        unsigned index = 0;
        if(indices)
            indices->reserve(image.size() / 64);

        if(chr16)
        {
            for(unsigned ty = 0; ty < height; ty += 16)
            for(unsigned tx = 0; tx < width; tx += 8)
            {
                if(indices)
                    indices->push_back(index);

                bool any_transparent = false;
                bool any_opaque = false;

                for(unsigned y = 0; y < 16; ++y)
                for(unsigned x = 0; x < 8; ++x)
                {
                    bool t = transparent[tx + x + (ty + y)*width];
                    any_transparent |= t;
                    any_opaque      |= !t;
                }

                if(any_transparent && any_opaque)
                    throw convert_error_t(fmt("Tile contains partial transparency. (X = %, Y = %)", tx, ty));

                if(any_transparent)
                    continue;

                for(unsigned y = 0; y < 8; ++y)
                {
                    std::uint8_t v = 0;
                    for(unsigned x = 0; x < 8; ++x)
                        v |= (image[tx + x + (ty + y)*width] & 1) << (7-x);
                    result.push_back(v);
                }

                for(unsigned y = 0; y < 8; ++y)
                {
                    std::uint8_t v = 0;
                    for(unsigned x = 0; x < 8; ++x)
                        v |= (image[tx + x + (ty + y)*width] >> 1) << (7-x);
                    result.push_back(v);
                }

                for(unsigned y = 8; y < 16; ++y)
                {
                    std::uint8_t v = 0;
                    for(unsigned x = 0; x < 8; ++x)
                        v |= (image[tx + x + (ty + y)*width] & 1) << (7-x);
                    result.push_back(v);
                }

                for(unsigned y = 8; y < 16; ++y)
                {
                    std::uint8_t v = 0;
                    for(unsigned x = 0; x < 8; ++x)
                        v |= (image[tx + x + (ty + y)*width] >> 1) << (7-x);
                    result.push_back(v);
                }

                index += 1;
            }
        }
        else
        {
            for(unsigned ty = 0; ty < height; ty += 8)
            for(unsigned tx = 0; tx < width; tx += 8)
            {
                if(indices)
                    indices->push_back(index);

                bool any_transparent = false;
                bool any_opaque = false;

                for(unsigned y = 0; y < 8; ++y)
                for(unsigned x = 0; x < 8; ++x)
                {
                    bool t = transparent[tx + x + (ty + y)*width];
                    any_transparent |= t;
                    any_opaque      |= !t;
                }

                if(any_transparent && any_opaque)
                    throw convert_error_t(fmt("Tile contains partial transparency. (X = %, Y = %)", tx, ty));

                if(any_transparent)
                    continue;

                for(unsigned y = 0; y < 8; ++y)
                {
                    std::uint8_t v = 0;
                    for(unsigned x = 0; x < 8; ++x)
                        v |= (image[tx + x + (ty + y)*width] & 1) << (7-x);
                    result.push_back(v);
                }

                for(unsigned y = 0; y < 8; ++y)
                {
                    std::uint8_t v = 0;
                    for(unsigned x = 0; x < 8; ++x)
                        v |= (image[tx + x + (ty + y)*width] >> 1) << (7-x);
                    result.push_back(v);
                }

                index += 1;
            }
        }

        return result;
    }
fail:
    throw convert_error_t(fmt("png decoder error: %", lodepng_error_text(error)));
}
