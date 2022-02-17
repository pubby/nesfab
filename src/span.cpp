#include "span.hpp"

#include <cassert>

std::ostream& operator<<(std::ostream& o, span_t span)
{
    o << "span_t{ " << span.addr << ", " << span.size << " }";
    return o;
}

span_t aligned(span_t span, std::uint16_t size, std::uint16_t alignment)
{
    // Must be power of 2.
    assert(alignment);
    assert((alignment & (alignment - 1)) == 0);

    std::uint16_t const add = (alignment - span.addr) & (alignment - 1);
    span_t const ret = { .addr = span.addr + add, .size = size };
    return span.contains(ret) ? ret : span_t{};
}

span_t aligned_reverse(span_t span, std::uint16_t size, std::uint16_t alignment)
{
    // Must be power of 2.
    assert(alignment);
    assert((alignment & (alignment - 1)) == 0);

    std::uint16_t const addr = span.end() - size;
    std::uint16_t const sub = addr & (alignment - 1);
    span_t const ret = { .addr = addr - sub, .size = size };
    return span.contains(ret) ? ret : span_t{};
}
