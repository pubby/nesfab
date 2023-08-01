#include "mapfab.hpp"

#include "macro.hpp"
#include "define.hpp"
#include "globals.hpp"

#include <iostream> // TODO

void convert_mapfab(std::uint8_t const* const begin, std::size_t size, pstring_t at, 
                    fs::path mapfab_path, mapfab_macros_t const& macros)
{
    using namespace std::literals;

    constexpr std::uint8_t SAVE_VERSION = 1; 

    fs::path base_path = mapfab_path;
    base_path.remove_filename();

    std::cout << "PATH = " << mapfab_path << std::endl;

    std::uint8_t const* ptr = begin;
    std::uint8_t const* const end = begin + size;;

    auto const get8 = [&](bool adjust = false) -> unsigned
    {
        if(ptr >= end)
            throw std::runtime_error("Invalid MapFab file; unexpected EOF.");
        int const got = *(ptr++);
        if(adjust && got == 0)
            return 256;
        return got;
    };

    auto const get16 = [&]() -> std::uint16_t
    {
        unsigned const lo = get8();
        unsigned const hi = get8();
        return (lo & 0xFF) | ((hi & 0xFF) << 8);
    };

    auto const get_str = [&]() -> std::string
    {
        std::string ret;
        while(char c = get8())
            ret.push_back(c);
        return ret;
    };

    if(size < 8)
        throw std::runtime_error("Invalid MapFab file; no magic number.");

    if(memcmp(ptr, "MapFab", 7) != 0)
        throw std::runtime_error("Incorrect magic number.");
    unsigned const save_version = ptr[7];
    if(save_version > SAVE_VERSION)
        throw std::runtime_error("File is from a newer version of MapFab.");
    ptr += 8;

    // Collision file:
    std::string collisions_path = get_str();

    std::cout << "PATH = " << collisions_path << std::endl;

    // CHR:
    unsigned const num_chr = get8(true);
    for(unsigned i = 0; i < num_chr; ++i)
    {
        ident_map_t<global_ht> private_globals;
        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);

        macro_invocation_t m = { macros.chr };
        m.args.push_back(get_str()); // Name

        fs::path path = get_str();
        std::cout << "CHR PATH " << path << std::endl;
        if(path.is_relative())
            path = base_path / path;
        m.args.push_back(path.string());
        invoke_macro(std::move(m), std::move(private_globals), {});
        std::printf("CHR MACRO %i\n", i);
    }

    // Palettes:
    unsigned const num_palettes = get8(true);
    std::vector<std::uint8_t> palette_data(256*25);
    for(std::uint8_t& data : palette_data)
        data = get8();
    for(unsigned i = 0; i < num_palettes; ++i)
    {
        // Defines:

        ident_map_t<global_ht> private_globals;
        global_t& g = private_globals.lookup(at, "_palette"sv);
        define_ct(g, at, palette_data.data() + 25*i, 25);
        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);

        macro_invocation_t m = { macros.palette };
        m.args.push_back(std::to_string(i));
        invoke_macro(std::move(m), std::move(private_globals), {});
        std::printf("PALETTE MACRO %i\n", i);
    }

    // Metatiles:
    unsigned const num_mt = get8(true);
    std::vector<std::uint8_t> mt_tiles(32*32);
    std::vector<std::uint8_t> mt_attributes(256);
    std::vector<std::uint8_t> mt_collisions(256);
    std::vector<std::uint8_t> mt_combined(256);
    std::vector<std::uint8_t> mt_nw(256);
    std::vector<std::uint8_t> mt_ne(256);
    std::vector<std::uint8_t> mt_sw(256);
    std::vector<std::uint8_t> mt_se(256);
    std::printf("NUM MT %i\n", num_mt);
    for(unsigned i = 0; i < num_mt; ++i)
    {
        std::printf("MT MACRO %i\n", i);
        std::string name = get_str();
        std::string chr_name = get_str();
        std::cout << name << ' ' << chr_name << std::endl;
        unsigned palette = get8();
        unsigned num = get8(true);
        for(std::uint8_t& data : mt_tiles)
            data = get8();
        for(std::uint8_t& data : mt_attributes)
            data = get8();
        for(std::uint8_t& data : mt_collisions)
            data = get8();

        for(unsigned y = 0; y < 16; ++y)
        for(unsigned x = 0; x < 16; ++x)
        {
            mt_nw[x + y*16] = mt_tiles[x*2 + y*64 + 0];
            mt_ne[x + y*16] = mt_tiles[x*2 + y*64 + 1];
            mt_sw[x + y*16] = mt_tiles[x*2 + y*64 + 32];
            mt_se[x + y*16] = mt_tiles[x*2 + y*64 + 33];
        }

        for(unsigned i = 0; i < 256; ++i)
            mt_combined[i] = (mt_attributes[i] & 0b11) | (mt_collisions[i] << 2);

        ident_map_t<global_ht> private_globals;
        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);
        define_ct_int(private_globals.lookup(at, "_num"sv), at, TYPE_INT, num);
        define_ct(private_globals.lookup(at, "_nw"sv), at, mt_nw.data(), num);
        define_ct(private_globals.lookup(at, "_ne"sv), at, mt_ne.data(), num);
        define_ct(private_globals.lookup(at, "_sw"sv), at, mt_sw.data(), num);
        define_ct(private_globals.lookup(at, "_se"sv), at, mt_se.data(), num);
        define_ct(private_globals.lookup(at, "_attributes"sv), at, mt_attributes.data(), num);
        define_ct(private_globals.lookup(at, "_collisions"sv), at, mt_collisions.data(), num);
        define_ct(private_globals.lookup(at, "_combined"sv), at, mt_combined.data(), num);

        macro_invocation_t m = { macros.metatiles };
        m.args.push_back(name);
        m.args.push_back(chr_name);
        m.args.push_back(std::to_string(palette));
        invoke_macro(std::move(m), std::move(private_globals), {});
    }

    struct field_t
    {
        std::string name;
        std::string type;
    };

    fc::vector_map<std::string, std::deque<field_t>> ocs;
    unsigned const num_oc = get8(true);
    std::printf("OC = %i\n", num_oc);
    for(unsigned i = 0; i < num_oc; ++i)
    {
        std::string name = get_str();
        get8(); // R
        get8(); // G
        get8(); // B
        std::deque<field_t> fields;

        unsigned const num_fields = get8();
        for(unsigned i = 0; i < num_fields; ++i)
            fields.emplace_back(get_str(), get_str());

        ocs.emplace(std::move(name), std::move(fields));
    }

    // Levels:
    unsigned const num_levels = get8(true);
    std::printf("LEV = %i\n", num_levels);
    for(unsigned i = 0; i < num_levels; ++i)
    {
        std::printf("LEVEL MACRO %i\n", i);
        std::string name = get_str();
        std::string macro_name = get_str();
        std::string chr_name = get_str();
        std::uint8_t const palette = get8();
        std::string metatiles_name = get_str();
        std::cout << name << ' ' << chr_name << ' ' << metatiles_name << std::endl;
        unsigned const w = get8(true);
        unsigned const h = get8(true);
        std::vector<std::uint8_t> tiles_xy(w*h);
        for(std::uint8_t& data : tiles_xy)
            data = get8();

        std::vector<std::uint8_t> tiles_yx(w*h);
        for(unsigned x = 0; x < w; ++x)
        for(unsigned y = 0; y < h; ++y)
            tiles_yx[x*h + y] = tiles_xy[x + y*w];

        std::vector<std::deque<std::string>> objects(ocs.size());
        std::vector<std::vector<std::int16_t>> objects_x(ocs.size());
        std::vector<std::vector<std::int16_t>> objects_y(ocs.size());

        unsigned const num_objects = get16();
        for(unsigned i = 0; i < num_objects; ++i)
        {
            std::string name = get_str();
            std::string oc = get_str();
            std::int16_t const x = static_cast<std::int16_t>(get16());
            std::int16_t const y = static_cast<std::int16_t>(get16());

            auto it = ocs.find(oc);

            if(it != ocs.end())
            {
                objects_x[it - ocs.begin()].push_back(x);
                objects_y[it - ocs.begin()].push_back(y);
                for(auto const& field : it->second)
                    objects[it - ocs.begin()].push_back(get_str());
            }
        }

        ident_map_t<global_ht> private_globals;
        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);
        define_ct_int(private_globals.lookup(at, "_width"sv), at, TYPE_INT, w);
        define_ct_int(private_globals.lookup(at, "_height"sv), at, TYPE_INT, h);
        define_ct(private_globals.lookup(at, "_row_major"sv), at, tiles_xy.data(), tiles_xy.size());
        define_ct(private_globals.lookup(at, "_column_major"sv), at, tiles_yx.data(), tiles_yx.size());

        std::string append;
        for(unsigned i = 0; i < objects.size(); ++i)
        {
            auto const& oc = *(ocs.begin() + i);

            define_ct(private_globals.lookup(at, fmt("_%_x", oc.first)), at, objects_x[i].data(), objects_x[i].size());
            define_ct(private_globals.lookup(at, fmt("_%_y", oc.first)), at, objects_y[i].data(), objects_y[i].size());

            for(unsigned j = 0; j < oc.second.size(); ++j)
            {
                auto const& field = oc.second[j];

                append += fmt("\n%[%] _%_% = %[%](", 
                    field.type, objects[i].size(), 
                    oc.first, field.name,
                    field.type, objects[i].size());
                bool first = true;
                for(unsigned k = j; k < objects[i].size(); k += oc.second.size())
                {
                    if(!first)
                        append += ", ";
                    first = false;
                    append += objects[i][k];
                }
                append += ")\n";
            }
        }

        macro_invocation_t m = { macros.level };
        m.args.push_back(name);
        m.args.push_back(chr_name);
        m.args.push_back(std::to_string(palette));
        m.args.push_back(metatiles_name);
        invoke_macro(std::move(m), std::move(private_globals), {}, append);
    }
}

