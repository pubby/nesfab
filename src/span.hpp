#ifndef SPAN_HPP
#define SPAN_HPP

#include <cstdint>
#include <ostream>
#include <vector>

// Spans are intrevals representing memory regions.

struct span_t
{
    std::uint16_t addr;
    std::uint16_t size;
    
    constexpr std::uint32_t end() const { return addr + size; }
    constexpr explicit operator bool() const { return size; }
    constexpr auto operator<=>(span_t const&) const = default;

    constexpr bool contains(std::uint16_t addr) const { return addr >= this->addr && end() > addr; }
    constexpr bool contains(span_t const& o) const { return o.addr >= addr && o.end() <= end(); }
    constexpr bool intersects(span_t const& o) const { return o.end() > addr && end() > o.addr; }
};

constexpr span_t offset_span(span_t span, int amount) { span.addr += amount; return span; }

std::ostream& operator<<(std::ostream& o, span_t span);

span_t aligned(span_t span, std::uint16_t size, std::uint16_t alignment);
span_t aligned_reverse(span_t span, std::uint16_t size, std::uint16_t alignment);

#endif
