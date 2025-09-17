#include "text.hpp"

#include <charconv>

#include "compiler_error.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "rom.hpp"
#include "hex.hpp"
#include "assert.hpp"

string_literal_manager_t sl_manager;

char32_t utf8_to_utf32(char const*& str)
{
    if(!str)
    fail: return str = nullptr, 0;

    std::uint8_t const ch = *str++;
    char32_t wide_ch = 0;
    unsigned utf8_char_size = 0;

    if(ch < 0x80)
        wide_ch = ch;
    else if((ch & 0b11100000) == 0b11000000)
    {
        wide_ch = ch & 0b11111;
        utf8_char_size = 1;
    }
    else if((ch & 0b11110000) == 0b11100000)
    {
        wide_ch = ch & 0b1111;
        utf8_char_size = 2;
    }
    else if((ch & 0b11111000) == 0b11110000)
    {
        wide_ch = ch & 0b111;
        utf8_char_size = 3;
    }
    else
        goto fail;
    
    while(utf8_char_size--)
    {
        std::uint8_t const ch = *str++;
        if((ch & 0b11000000) != 0b10000000)
            goto fail;
        wide_ch <<= 6;
        wide_ch |= ch & 0b111111;
    }

    return wide_ch;
}

char32_t utf8_to_utf32(pstring_t pstring, char const*& str)
{
    char32_t const result = utf8_to_utf32(str);
    if(!str)
        compiler_error(pstring, "Invalid utf8 sequence.");
    return result;
}

#define SIMPLE_XMACRO \
    SIMPLE('\'', '\'') \
    SIMPLE('\"', '\"') \
    SIMPLE('`', '`') \
    SIMPLE('\\', '\\') \
    SIMPLE('a', '\a') \
    SIMPLE('b', '\b') \
    SIMPLE('f', '\f') \
    SIMPLE('n', '\n') \
    SIMPLE('r', '\r') \
    SIMPLE('t', '\t') \
    SIMPLE('v', '\v') \
    SIMPLE('0', '\0') \
    SIMPLE('/', SPECIAL_SLASH)

char32_t escaped_utf8_to_utf32(char const*& str, char const** error)
{
    char32_t const first = utf8_to_utf32(str);
    if(first != '\\')
        return first;

    char32_t const escape = utf8_to_utf32(str);
    unsigned xsize = 0;
    switch(escape)
    {
#define SIMPLE(from, to) case from: return to;
    SIMPLE_XMACRO
#undef SIMPLE
    case 'x':
        xsize = 2;
        goto hex;
    case 'u':
        xsize = 4;
        goto hex;
    case 'U':
        xsize = 8;
        goto hex;
    hex:
        {
            std::uint32_t wide_ch;
            auto result = std::from_chars(str, str+xsize, wide_ch, 16);
            if(result.ptr != str+xsize || result.ec != std::errc())
                goto fail;
            str = result.ptr;
            return static_cast<char32_t>(wide_ch);
        }
        break;
    case '(':
        {
            std::size_t size = 0;
            while(true)
            {
                if(str[size] == '\0')
                {
                    if(error)
                        *error = "Hash sequence was not terminated.";
                    goto fail;
                }
                if(str[size] == ')')
                    break;
                if((std::uint8_t(str[size]) & 0x80) || str[size] == '\\' || (!std::isalnum(str[size]) && !std::ispunct(str[size])))
                {
                    if(error)
                        *error = "Only alphanumeric characters and punctuation are allowed inside of a hash sequence.";
                    goto fail;
                }
                size += 1;
            }

            fnv1a<std::uint32_t> hash;
            std::uint32_t result = fnv1a<std::uint32_t>::hash(str, size);
            result |= 1 << 31;

            str += size + 1;
            return static_cast<char32_t>(result);
        }
    default:
        break;
    }

fail:
    str = nullptr;
    return 0;
}

char32_t escaped_utf8_to_utf32(pstring_t pstring, char const*& str)
{
    char const* error = nullptr;
    char32_t const result = escaped_utf8_to_utf32(str, &error);
    if(!str)
        compiler_error(pstring, fmt("Invalid character sequence. %", error ? error : ""));
    return result;
}

std::size_t normalize_line_endings(char* const data, std::size_t size)
{
    std::size_t o = 0;

    for(std::size_t i = 0; i < size; ++i, ++o)
    {
        char c = data[i];

        if(c == '\r')
        {
            c = '\n';
            if(i+1 != size && data[i+1] == '\n')
                ++i;
        }
        else if(c == '\n' && i+1 != size && data[i+1] == '\r')
            ++i;

        data[i] = c;
    }

    return o;
}

std::string escape(std::string const& str)
{
    std::string ret;
    char32_t utf32;

    for(char const* ptr = str.data(); (utf32 = utf8_to_utf32(ptr));)
    {
        switch(utf32)
        {
#define SIMPLE(from, to) case to: ret.push_back('\\'); ret.push_back(from); break;
    SIMPLE_XMACRO
#undef SIMPLE
        default:
            if(unsigned(utf32) < 128)
                ret.push_back(static_cast<char>(utf32));
            else
            {
                ret += "\\U";
                ret += hex_string(utf32, 8);
            }
            break;
        }
    }

    return ret;
}

//////////////////////////////
// string_literal_manager_t //
//////////////////////////////

unsigned string_literal_manager_t::add_string(global_t const* charmap, pstring_t at, std::string string, bool compressed)
{
    assert(compiler_phase() < PHASE_COMPRESS_STRINGS);
    assert(charmap);

    std::lock_guard<std::mutex> lock(mutex);

    auto& info = m_map[charmap];
    info.pstring = at;
    auto& map = info.map(compressed);
    auto result = map.emplace(std::move(string));
    result.first->second.pstring = at;
    return result.first - map.begin();
}

std::string const& string_literal_manager_t::get_string(global_t const* charmap, unsigned index, bool compressed) const
{
    assert(compiler_phase() > PHASE_COMPRESS_STRINGS);
    assert(charmap);

    if(auto const* result = m_map.mapped(charmap))
    {
        auto& map = result->map(compressed);
        return map.begin()[index].first;
    }

    throw std::runtime_error("Bad or unknown charmap. Cannot get string size.");
}

rom_array_ht string_literal_manager_t::get_rom_array(global_t const* charmap, unsigned index, bool compressed)
{
    assert(compiler_phase() > PHASE_COMPRESS_STRINGS);
    assert(charmap);

    if(auto* result = m_map.mapped(charmap))
    {
        auto& map = result->map(compressed);
        std::string const& str = map.begin()[index].first;
        data_t& data = map.begin()[index].second;

        group_data_ht const gd = charmap->impl<charmap_t>().group_data();

        if(!gd)
            compiler_error(data.pstring, fmt("Invalid use of string literal. % has no stows modifier.", charmap->name));

        std::lock_guard<std::mutex> lock(data.mutex);
        if(!data.rom_array)
        {
            // Allocate a rom array:
            loc_vec_t vec;
            vec.resize(str.size());
            for(unsigned i = 0; i < str.size(); ++i)
                vec[i] = locator_t::const_byte(str[i]);

            assert(charmap->gclass() == GLOBAL_CHARMAP);

            data.rom_array = rom_array_t::make(std::move(vec), false, charmap->impl<charmap_t>().stows_omni(), ROMR_NORMAL, gd);
        }
        else
            data.rom_array.safe().mark_used_by(gd);

        assert(data.rom_array);
        return data.rom_array;
    }

    throw std::runtime_error("Bad or unknown charmap. Cannot get rom array.");
}

std::pair<ct_array_t, unsigned> string_literal_manager_t::get_byte_pairs(global_t const* charmap)
{
    assert(compiler_phase() > PHASE_COMPRESS_STRINGS);
    assert(charmap);

    if(auto* result = m_map.mapped(charmap))
    {
        std::size_t const size = result->byte_pairs.size();

        if(size > 0)
        {
            ct_array_t array = make_ct_array(size);
            for(unsigned i = 0; i < size; ++i)
            {
                unsigned bp;
                bp  = result->byte_pairs[i][0];
                bp |= result->byte_pairs[i][1] << 8;
                array[i] = ssa_value_t(bp, TYPE_U20);
            }

            return { std::move(array), size };
        }
    }

    ct_array_t array = make_ct_array(1);
    array[0] = ssa_value_t(0, TYPE_U20);
    return { std::move(array), 1 };
}

// Single-threaded
void string_literal_manager_t::convert_all()
{
    assert(compiler_phase() == PHASE_CONVERT_STRINGS);

    for(auto& i : m_map)
    {
        if(i.first->gclass() != GLOBAL_CHARMAP)
            compiler_error(i.second.pstring, fmt("% is not a charmap.", i.first->name));
        convert(i.first->impl<charmap_t>(), i.second);
    }
}

// Single-threaded
void string_literal_manager_t::compress_all()
{
    assert(compiler_phase() == PHASE_COMPRESS_STRINGS);

    for(auto& i : m_map)
        compress(i.first->impl<charmap_t>(), i.second);
}

void string_literal_manager_t::convert(charmap_t const& charmap, charmap_info_t& info)
{
    assert(compiler_phase() == PHASE_CONVERT_STRINGS);

    auto const do_convert = [&](std::string& str, pstring_t pstring)
    {
        char const* ptr = str.data();
        char const* end = str.data() + str.size();
        unsigned new_size = 0;

        while(ptr < end)
        {
            char const* pre = ptr;
            char32_t const utf32 = escaped_utf8_to_utf32(pstring, ptr);

            int const new_char = charmap.convert(utf32);
            if(new_char < 0)
                compiler_error(pstring, fmt("Character '%' isn't in %.", std::string_view(pre, ptr), charmap.global.name));
            assert(new_char < 256);

            str[new_size++] = static_cast<char>(new_char);
        }

        assert(new_size <= str.size());
        str.resize(new_size);

        int const sentinel = charmap.sentinel();
        assert(sentinel < 256);
        if(sentinel >= 0)
            str.push_back(static_cast<char>(sentinel));
    };

    for(auto& p : info.compressed)
        do_convert(p.first, p.second.pstring);
    for(auto& p : info.uncompressed)
        do_convert(p.first, p.second.pstring);
}

void string_literal_manager_t::compress(charmap_t const& charmap, charmap_info_t& info)
{
    assert(compiler_phase() == PHASE_COMPRESS_STRINGS);

    assert(info.byte_pairs.empty());
    assert(charmap.size() <= 256);
    unsigned const offset = charmap.size() + charmap.offset();
    unsigned const max_byte_pairs = 256 - charmap.size();

    // Counts how often each byte_pair appears:
    rh::batman_map<byte_pair_t, unsigned> pair_map;

    // Counts how deep each byte pair goes.
    // (Maintain same size as 'info.byte_pairs'.)
    std::vector<unsigned> depths;
    
    auto const depth = [&](std::uint8_t c) -> unsigned
    {
        c -= charmap.offset();
        if(c < charmap.size())
            return 0;
        assert((c - charmap.size()) < depths.size());
        return depths[c - charmap.size()];
    };

    auto const pair_depth = [&](byte_pair_t const& bp) -> unsigned
    {
        return std::max(depth(bp[0]), depth(bp[1]));
    };

    // As the assembly decompressor uses recursion, 
    // we should limit the depth to prevent stack overflows.
    constexpr unsigned MAX_DEPTH = 32;

    while(info.byte_pairs.size() < max_byte_pairs)
    {
        pair_map.clear();

        // Count how often each pair appears:
        for(auto const& p : info.compressed)
        {
            std::string const& str = p.first;

            int const to = int(str.size()) - 1;
            for(int i = 0; i < to; ++i)
                pair_map[{ str[i], str[i+1] }] += 1;
        }

        // If there's no pairs, abort!
        if(pair_map.empty())
            break;

        rh::apair<byte_pair_t, unsigned> const* most_common = nullptr;
        auto it = pair_map.begin();
        for(; it != pair_map.end(); ++it)
            if(pair_depth(it->first) < MAX_DEPTH)
                goto new_most_common;
        for(; it != pair_map.end(); ++it)
            if(it->second > most_common->second && pair_depth(it->first) < MAX_DEPTH)
                new_most_common: most_common = &*it;

        // No point in replacing if it hardly occurs:
        if(!most_common || most_common->second <= 2)
            break;

        assert(pair_depth(most_common->first) < MAX_DEPTH);

        // Do the replacement
        char const replacement = static_cast<char>(offset + info.byte_pairs.size());
        for(auto& p : info.compressed)
        {
            std::string& str = p.first;

            unsigned const size = str.size();
            unsigned j = 0;
            for(unsigned i = 0; i < size; ++i, ++j)
            {
                if(i+1 < size
                   && std::uint8_t(str[i+0]) == most_common->first[0]
                   && std::uint8_t(str[i+1]) == most_common->first[1])
                {
                    str[j] = replacement;
                    ++i;
                }
                else
                    str[j] = str[i];
            }
            str.resize(j);
        }

        info.byte_pairs.push_back(most_common->first);
        depths.push_back(pair_depth(most_common->first));
    }
}

char const* parse_string_literal(string_literal_t& literal, char const* source, char const* next_char, char last, unsigned file_i)
{
    passert(*next_char == last, *next_char);
    char const* begin = ++next_char;

    while(true)
    {
        char const ch = *next_char++;

        if(ch == last)
            break;
        else if(std::iscntrl(ch))
            throw std::runtime_error("Unexpected character in string literal.");
        else if(ch == '\\')
        {
            literal.string.push_back(ch);
            char const e = *next_char++;
            if(std::iscntrl(e))
                throw std::runtime_error("Unexpected character in string literal.");
            literal.string.push_back(e);
        }
        else
            literal.string.push_back(ch);
    }

    pstring_t const pstring = { begin - source, next_char - begin, file_i };
    if(literal.pstring)
        literal.pstring = fast_concat(literal.pstring, pstring);
    else
        literal.pstring = pstring;

    return next_char;
}
