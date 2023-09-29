#include "convert_compress.hpp"

#include "donut.hpp"

std::vector<std::uint8_t> compress_pbz(std::uint8_t* begin, std::uint8_t* end)
{
    using plane_t = std::array<std::uint8_t, 8>;

    std::size_t const input_size = end - begin;
    if(input_size % 8 != 0)
        throw convert_error_t("PBZ conversion error. Expecting size to be a multiple of 8.");

    std::vector<std::uint8_t> result;
    result.reserve(input_size);

    auto const read_plane = [&](std::uint8_t* at)
    {
        plane_t plane = {};
        for(unsigned i = 0; i < 8 && at < end; ++i)
            plane[i] = at[i];
        return plane;
    };

    auto const write_pbz = [&](plane_t const& plane)
    {
        result.reserve(result.size() + 9);
        std::uint8_t& header = result.emplace_back(0);
        std::uint8_t prev = 0;
        for(std::uint8_t byte : plane)
        {
            header <<= 1;
            if(byte == prev)
                header |= 1;
            else
                result.push_back(byte);
            prev = byte;
        }
    };

    for(std::uint8_t* it = begin; it != end; it += 8)
        write_pbz(read_plane(it));

    return result;
}

conversion_t convert_pbz(std::uint8_t* begin, std::uint8_t* end)
{
    std::size_t const size = end - begin;
    conversion_t c = { .data = compress_pbz(begin, end) };
    c.named_values.push_back({ "chunks", ssa_value_t(size / 8, TYPE_INT) });
    if(size % 16 == 0)
        c.named_values.push_back({ "tiles", ssa_value_t(size / 16, TYPE_INT) });
    return c;
}

std::vector<std::uint8_t> compress_rlz(std::uint8_t* begin, std::uint8_t* end, bool terminate)
{
    std::vector<std::uint8_t> result;
    std::size_t const span = end - begin;

    for(std::size_t i = 0; i < span;)
    {
        std::uint8_t c = begin[i];

        // Count a run of identical characters.

        unsigned run = 1;
        while(run < 129 && i + run < span && begin[i + run] == c)
            ++run;

        if(run > 2)
        {
            result.push_back(run - 2);
            result.push_back(c);
            i += run;
            continue;
        }

        // Count a run of different characters.

        unsigned match = 1;
        run = 1;
        while(match < 128 && i + match < span)
        {
            assert(i + match < span);
            if(begin[i + match] == c)
            {
                ++run;
                if(run == 3)
                {
                    match -= 2;
                    assert(match > 0);
                    break;
                }
            }
            else
            {
                run = 1;
                c = begin[i + match];
            }
            
            ++match;
        }
        
        result.push_back(match + 0x80 - 1);
        for(unsigned j = 0; j < match; ++j)
        {
            assert(i + j < span);
            result.push_back(begin[i + j]);
        }
        i += match;
    }

    if(terminate)
        result.push_back(0);
    
    return result;
}

conversion_t convert_rlz(std::uint8_t* begin, std::uint8_t* end, bool terminate)
{
    conversion_t c = { .data = compress_rlz(begin, end, terminate) };
    return c;
}

///////////////////////////////////////////////////////////////////////////////
// DONUT //////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

conversion_t convert_donut(std::uint8_t* begin, std::uint8_t* end)
{
    std::size_t const size = end - begin;
    conversion_t c = { .data = compress_donut(begin, end) };
    c.named_values.push_back({ "chunks", ssa_value_t(size / 64, TYPE_INT) });
    c.named_values.push_back({ "tiles", ssa_value_t(size / 16, TYPE_INT) });
    return c;
}

