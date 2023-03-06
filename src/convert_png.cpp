#include "convert_png.hpp"

#include <array>
#include <stdexcept>
#include <cassert>

#include "format.hpp"
#include "lodepng/lodepng.h"

std::uint8_t map_grey_alpha(std::uint8_t grey, std::uint8_t alpha)
{
    return (grey * alpha) >> (6 + 8);
}

std::vector<std::uint8_t> png_to_chr(std::uint8_t const* png, std::size_t size, bool chr16)
{
    unsigned width, height;
    std::vector<std::uint8_t> image; //the raw pixels
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
        state.info_raw.colortype = LCT_PALETTE;
        if((error = lodepng::decode(image, width, height, state, png, size)))
            goto fail;
        for(std::uint8_t& c : image)
            c &= 0b11;
        break;

    case LCT_GREY:
    case LCT_RGB:
        state.info_raw.colortype = LCT_GREY;
        if((error = lodepng::decode(image, width, height, state, png, size)))
            goto fail;
        for(std::uint8_t& c : image)
            c >>= 6;
        break;

    default:
        state.info_raw.colortype = LCT_GREY_ALPHA;
        if((error = lodepng::decode(image, width, height, state, png, size)))
            goto fail;
        assert(image.size() == width * height * 2);
        unsigned const n = image.size() / 2;
        for(unsigned i = 0; i < n; ++i)
            image[i] = map_grey_alpha(image[i*2], image[i*2 + 1]);
        image.resize(n);
        break;
    }

    // Now convert to CHR
    {
        std::vector<std::uint8_t> result;
        result.resize(image.size() / 4);

        unsigned i = 0;

        if(chr16)
        {
            for(unsigned ty = 0; ty < height; ty += 16)
            for(unsigned tx = 0; tx < width; tx += 8)
            {
                for(unsigned y = 0; y < 8; ++y, ++i)
                for(unsigned x = 0; x < 8; ++x)
                    result[i] |= (image[tx + x + (ty + y)*width] & 1) << (7-x);

                for(unsigned y = 0; y < 8; ++y, ++i)
                for(unsigned x = 0; x < 8; ++x)
                    result[i] |= (image[tx + x + (ty + y)*width] >> 1) << (7-x);

                for(unsigned y = 8; y < 16; ++y, ++i)
                for(unsigned x = 0; x < 8; ++x)
                    result[i] |= (image[tx + x + (ty + y)*width] & 1) << (7-x);

                for(unsigned y = 8; y < 16; ++y, ++i)
                for(unsigned x = 0; x < 8; ++x)
                    result[i] |= (image[tx + x + (ty + y)*width] >> 1) << (7-x);
            }
        }
        else
        {
            for(unsigned ty = 0; ty < height; ty += 8)
            for(unsigned tx = 0; tx < width; tx += 8)
            {
                for(unsigned y = 0; y < 8; ++y, ++i)
                for(unsigned x = 0; x < 8; ++x)
                    result[i] |= (image[tx + x + (ty + y)*width] & 1) << (7-x);

                for(unsigned y = 0; y < 8; ++y, ++i)
                for(unsigned x = 0; x < 8; ++x)
                    result[i] |= (image[tx + x + (ty + y)*width] >> 1) << (7-x);
            }
        }

        assert(i == result.size());

        return result;
    }
fail:
    throw convert_error_t(fmt("png decoder error: %", lodepng_error_text(error)));
}
