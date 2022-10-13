#ifndef TEXT_HPP
#define TEXT_HPP

#include <array>
#include <cstdint>
#include <mutex>
#include <functional>

#include "robin/map.hpp"

#include "pstring.hpp"
#include "rom_decl.hpp"

class global_t;
class charmap_t;

constexpr char32_t SPECIAL_SLASH = char32_t(~0u);

// Reads one utf32 character from a utf8 string, advancing 'data'.
// Sets 'data' to nullptr on failure.
char32_t utf8_to_utf32(char const*& str);
char32_t utf8_to_utf32(pstring_t pstring, char const*& str);

char32_t escaped_utf8_to_utf32(char const*& str);
char32_t escaped_utf8_to_utf32(pstring_t pstring, char const*& str);

using byte_pair_t = std::array<std::uint8_t, 2>;

class string_literal_manager_t
{
public:
    unsigned add_string(global_t const* charmap, pstring_t at, std::string string, bool compressed);
    std::string const& get_string(global_t const* charmap, unsigned index, bool compressed) const;
    rom_array_ht get_rom_array(global_t const* charmap, unsigned index, bool compressed);

    void convert_all();
    void compress_all();

private:
    struct data_t
    {
        pstring_t pstring;

        std::mutex mutex; // protects below
        rom_array_ht rom_array;
    };

    struct charmap_info_t
    {
        charmap_info_t() = default;
        charmap_info_t(charmap_info_t&&) = default;

        auto const& map(bool is_compressed) const { return is_compressed ? compressed : uncompressed; }
        auto& map(bool is_compressed) { return is_compressed ? compressed : uncompressed; }

        rh::joker_map<std::string, data_t> compressed;
        rh::joker_map<std::string, data_t> uncompressed;
        std::vector<byte_pair_t> byte_pairs;
    };

    void convert(charmap_t const& charmap, charmap_info_t& info);
    void compress(charmap_t const& charmap, charmap_info_t& info);

    std::mutex mutex;
    rh::batman_map<global_t const*, charmap_info_t> m_map;
};

extern string_literal_manager_t sl_manager;

template<>
struct std::hash<byte_pair_t>
{
    std::size_t operator()(byte_pair_t const& pair)
    {
        return rh::hash_combine(pair[0], pair[1] << 8);
    }
};

#endif
