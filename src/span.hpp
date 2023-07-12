#ifndef SPAN_HPP
#define SPAN_HPP

#include <cstdint>
#include <ostream>
#include <vector>

// Spans are intrevals representing memory regions.

template<typename T>
struct generic_span_t
{
    T addr;
    T size;

    using value_type = T;
    
    constexpr std::uint64_t end() const { return addr + size; }
    constexpr explicit operator bool() const { return size; }
    constexpr auto operator<=>(generic_span_t const&) const = default;

    constexpr bool contains(T addr) const { return addr >= this->addr && end() > addr; }
    constexpr bool contains(generic_span_t const& o) const { return o.addr >= addr && o.end() <= end(); }
    constexpr bool intersects(generic_span_t const& o) const { return o.end() > addr && end() > o.addr; }
};

using span_t = generic_span_t<std::uint16_t>;

template<typename T>
constexpr generic_span_t<T> offset_span(generic_span_t<T> span, int amount) { span.addr += amount; return span; }

std::ostream& operator<<(std::ostream& o, span_t span);

span_t aligned(span_t span, std::uint16_t size, std::uint16_t alignment);
span_t aligned_reverse(span_t span, std::uint16_t size, std::uint16_t alignment);

#endif
