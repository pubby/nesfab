#include <array>
#include <cstdint>
#include <cstdio>

// Used in constraints.cpp to implement addition of constraints.

// Dedicated to the Daily WTF
// https://thedailywtf.com/articles/What_Is_Truth_0x3f_
enum wtf_bool_t
{
    WTF_UNKNOWN = 0b00,
    WTF_0 = 0b01,
    WTF_1 = 0b10,
    WTF_TOP = 0b11,
};

std::uint64_t wtf_min(wtf_bool_t b) { return (b == WTF_1); }
std::uint64_t wtf_max(wtf_bool_t b) { return (b != WTF_0); }

wtf_bool_t wtf_add(wtf_bool_t l, wtf_bool_t r, wtf_bool_t& c)
{
    if(c == WTF_TOP)
        return WTF_TOP;

    std::uint64_t min = wtf_min(l) + wtf_min(r) + wtf_min(c);
    std::uint64_t max = wtf_max(l) + wtf_max(r) + wtf_max(c);
    if(max < 2)
        c = WTF_0;
    else if(min > 1)
        c = WTF_1;
    else
        c = WTF_UNKNOWN;
    if(min == max)
        return (min & 1) ? WTF_1 : WTF_0;
    return WTF_UNKNOWN;
}

wtf_bool_t wtf(bool known0, bool known1)
{
    if(known0 && known1)
        return WTF_TOP;
    if(known0)
        return WTF_0;
    if(known1)
        return WTF_1;
    return WTF_UNKNOWN;
}

int main()
{
    std::array<std::uint8_t, 1 << (2*4+2)> table = {};
    for(std::uint64_t i = 0; i < table.size(); ++i)
    {
        table[i] = 0;
        wtf_bool_t carry = (wtf_bool_t)(i & 0b11ull);

        std::uint64_t l0 = (i >> 2ull) & 0b11ull;
        std::uint64_t l1 = (i >> 4ull) & 0b11ull;

        std::uint64_t r0 = (i >> 6ull) & 0b11ull;
        std::uint64_t r1 = (i >> 8ull) & 0b11ull;

        for(std::uint64_t j = 0; j < 2; ++j)
        {
            std::uint64_t bit = 1ull << j;
            wtf_bool_t lwtf = wtf(l0 & bit, l1 & bit);
            wtf_bool_t rwtf = wtf(r0 & bit, r1 & bit);
            wtf_bool_t result = wtf_add(lwtf, rwtf, carry);
            if(result == WTF_0)
                table[i] |= bit << 2ull;
            if(result == WTF_1)
                table[i] |= bit << 4ull;
        }
        table[i] |= (std::uint64_t)carry & 0b11ull;
    }

    std::printf("#include <cstdint>\n");
    std::printf("extern std::uint8_t const add_constraints_table[1024] =\n{\n");
    for(std::uint8_t v : table)
        std::printf("%u,\n", (unsigned)v);
    std::printf("};\n");

}
