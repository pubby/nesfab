#ifndef PSTRING_HPP
#define PSTRING_HPP

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <string>
#include <string_view>

#include "file.hpp"

// Holds a slice of the parser's input buffer.
// Convertible to std::string_view, and you'll probably want to do that.
struct pstring_t
{
    std::uint32_t offset;
    std::uint16_t size;
    std::uint16_t file_i;

    std::string_view view(char const* buffer) const 
        { return std::string_view(buffer + offset, size); }

    std::string string(char const* buffer) const 
        { return std::string(view(buffer)); }

    constexpr std::uint32_t end() const { return offset + size; }

    constexpr explicit operator bool() const { return size; }
};

// Combines two pstrings into one. 
// Requires that 'lo' comes before 'hi', and lo.end() comes before hi.end().
constexpr pstring_t fast_concat(pstring_t lo, pstring_t hi)
{
    assert(lo.file_i == hi.file_i);
    assert(lo.offset <= hi.offset);
    assert(lo.end() <= hi.end());
    return { lo.offset, hi.end() - lo.offset, lo.file_i };
}

constexpr pstring_t concat(pstring_t lo, pstring_t hi)
{
    assert(lo.file_i == hi.file_i);
    auto min_offset = std::min(lo.offset, hi.offset);
    auto max_end = std::max(lo.end(), hi.end());
    return { min_offset, max_end - min_offset, lo.file_i };
}

// Useful for associative containers like std::map.
struct pstring_less_t
{
    char const* source;
    bool operator()(pstring_t lhs, pstring_t rhs) const
        { return lhs.view(source) < rhs.view(source); }
};

#endif
