#include "convert_pb.hpp"

std::vector<std::uint8_t> compress_pbz(std::uint8_t* begin, std::uint8_t* end)
{
    using plane_t = std::array<std::uint8_t, 8>;

    std::size_t const input_size = end - begin;
    if(input_size % 16 != 0)
        throw convert_error_t("Expecting size to be a multiple of 16.");

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
    conversion_t c = { .data = compress_pbz(begin, end) };
    c.named_values.push_back({ "chunks", ssa_value_t((end - begin) / 16, TYPE_INT) });
    return c;
}
