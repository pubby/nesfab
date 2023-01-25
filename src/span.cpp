#include "span.hpp"

#include <algorithm>
#include <cassert>
#include <sstream>

#include "format.hpp"

std::ostream& operator<<(std::ostream& o, span_t span)
{
    o << "span_t{ $" << to_hex_string(span.addr) << ", " << span.size << " }";
    return o;
}

span_t aligned(span_t span, std::uint16_t size, std::uint16_t alignment)
{
    if(!alignment)
        alignment = 1;
    // Must be power of 2.
    assert((alignment & (alignment - 1)) == 0);

    std::uint16_t const add = (alignment - span.addr) & (alignment - 1);
    span_t const ret = { .addr = span.addr + add, .size = size };
    assert(ret.addr % alignment == 0);
    return span.contains(ret) ? ret : span_t{};
}

span_t aligned_reverse(span_t span, std::uint16_t size, std::uint16_t alignment)
{
    if(!alignment)
        alignment = 1;
    // Must be power of 2.
    assert((alignment & (alignment - 1)) == 0);

    std::uint16_t const addr = span.end() - size;
    std::uint16_t const sub = addr & (alignment - 1);
    span_t const ret = { .addr = addr - sub, .size = size };
    assert(ret.addr % alignment == 0);
    return span.contains(ret) ? ret : span_t{};
}

