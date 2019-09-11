#ifndef PSTRING_HPP
#define PSTRING_HPP

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <string_view>

#include "file.hpp"

// Holds a slice of the parser's input buffer.
// Convertible to std::string_view, and you'll probably want to do that.
struct pstring_t
{
    std::uint32_t offset;
    std::uint16_t size;
    std::uint16_t file_i;

    std::string_view view() const 
        { return std::string_view(files[file_i].source() + offset, size); }

    constexpr std::uint32_t end() { return offset + size; }
};

// Combines two pstrings into one. 
// Requires that 'lo' comes before 'hi', and lo.end() comes before hi.end().
constexpr pstring_t concat(pstring_t lo, pstring_t hi)
{
    assert(lo.file_i == hi.file_i);
    assert(lo.offset <= hi.offset);
    assert(lo.end() <= hi.end());
    return { lo.offset, hi.end() - lo.offset, lo.file_i };
}

// Useful for associative containers like std::map.
struct pstring_less_t
{
    bool operator()(pstring_t lhs, pstring_t rhs) const
        { return lhs.view() < rhs.view(); }
};

#endif
