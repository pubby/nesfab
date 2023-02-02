#ifndef HEX_HPP
#define HEX_HPP

inline int char_to_int(char ch)
{
    constexpr auto lookup_table = []
    {
        std::array<std::uint8_t, 256> table;

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

#endif
