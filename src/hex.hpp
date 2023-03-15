#ifndef HEX_HPP
#define HEX_HPP

#include <cstdint>
#include <array>
#include <string>
#include <string_view>

inline int char_to_int(char ch)
{
    constexpr auto lookup_table = []
    {
        std::array<std::int8_t, 256> table;

        table.fill(-1);

        for(unsigned i = 0; i < 10; ++i)
            table['0'+i] = i;

        table['a'] = table['A'] = 10;
        table['b'] = table['B'] = 11;
        table['c'] = table['C'] = 12;
        table['d'] = table['D'] = 13;
        table['e'] = table['E'] = 14;
        table['f'] = table['F'] = 15;

        return table;
    }();

    return lookup_table[ch];
}

inline char int_to_char(int i)
{
    switch(i)
    {
    default:
    case 0: return '0';
    case 1: return '1';
    case 2: return '2';
    case 3: return '3';
    case 4: return '4';
    case 5: return '5';
    case 6: return '6';
    case 7: return '7';
    case 8: return '8';
    case 9: return '9';
    case 10: return 'A';
    case 11: return 'B';
    case 12: return 'C';
    case 13: return 'D';
    case 14: return 'E';
    case 15: return 'F';
    }
}

inline std::string hex_string(unsigned v, unsigned digits)
{
    std::string ret;
    ret.resize(digits);
    for(unsigned i = 0; i < digits; ++i)
    {
        ret.rbegin()[i] = int_to_char(v & 0xF);
        v >>= 4;
    }
    return ret;
}

#endif
