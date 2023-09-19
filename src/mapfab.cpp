#include "mapfab.hpp"

#include <array>
#include <map>

#include "json.hpp"

#include "macro.hpp"
#include "define.hpp"
#include "globals.hpp"
#include "debug_print.hpp"
#include "convert_compress.hpp"

using json = nlohmann::json;

struct mapfab_t
{
    struct chr_t
    {
        std::string name;
        fs::path path;
    };


    struct palette_t
    {
        std::array<std::uint8_t, 25> data;
    };

    struct mt_t
    {
        std::uint8_t nw;
        std::uint8_t ne;
        std::uint8_t sw;
        std::uint8_t se;

        auto operator<=>(mt_t const&) const = default;
    };

    struct mt_set_t
    {
        std::string name;
        std::string chr_name;
        unsigned palette;
        unsigned num;
        std::vector<std::uint8_t> attributes;
        std::vector<std::uint8_t> collisions;
        std::vector<std::uint8_t> combined;
        std::vector<std::uint8_t> combined_alt;
        std::vector<std::uint8_t> nw;
        std::vector<std::uint8_t> ne;
        std::vector<std::uint8_t> sw;
        std::vector<std::uint8_t> se;
        std::vector<std::uint8_t> nw32;
        std::vector<std::uint8_t> ne32;
        std::vector<std::uint8_t> sw32;
        std::vector<std::uint8_t> se32;
        std::vector<std::uint8_t> attributes32;
        std::map<mt_t, unsigned> map32;
    };

    struct field_t
    {
        std::string type;
        std::string name;
    };

    struct level_t
    {
        std::string name;
        std::string macro_name;
        std::string chr_name;
        unsigned palette;
        std::string metatiles_name;
        unsigned w;
        unsigned h;
        std::vector<std::uint8_t> tiles;
        std::vector<std::uint8_t> tiles32;
        std::vector<std::deque<std::string>> objects;
        std::vector<std::vector<std::int16_t>> objects_x;
        std::vector<std::vector<std::int16_t>> objects_y;
    };

    std::vector<chr_t> chrs;
    std::vector<palette_t> palettes;
    std::vector<mt_set_t> mt_sets;
    fc::vector_map<std::string, std::deque<field_t>> ocs;
    std::vector<level_t> levels;

    void load_binary(std::uint8_t const* const begin, std::size_t size, fs::path mapfab_path);
    void load_json(std::uint8_t const* const begin, std::size_t size, fs::path mapfab_path);
    void compute_mmt_32();
};

static constexpr std::uint8_t SAVE_VERSION = 1; 

void mapfab_t::load_binary(std::uint8_t const* const begin, std::size_t size, fs::path mapfab_path)
{
    fs::path base_path = mapfab_path;
    base_path.remove_filename();

    log_t* log = nullptr;

    dprint(log, "MAPFAB_PATH", mapfab_path);

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

    auto const convert_path = [&](std::string const& str) -> std::filesystem::path
    {
        std::filesystem::path path(str, std::filesystem::path::generic_format);
        path.make_preferred();

        if(!path.empty() && path.is_relative())
            path = base_path / path;

        return path;
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

    dprint(log, "MAPFAB_COLLISIONS_PATH", collisions_path);

    // CHR:
    unsigned const num_chr = get8(true);
    chrs.reserve(num_chr);
    for(unsigned i = 0; i < num_chr; ++i)
    {
        chr_t chr = {};
        chr.name = get_str();
        chr.path = convert_path(get_str());

        dprint(log, "MAPFAB_CHR", chr.name, chr.path);
        chrs.push_back(std::move(chr));
    }

    // Palettes:
    unsigned const num_palettes = get8(true);
    palettes.reserve(num_palettes);
    std::vector<std::uint8_t> palette_data(256*25);
    for(std::uint8_t& data : palette_data)
        data = get8();
    for(unsigned i = 0; i < num_palettes; ++i)
    {
        palette_t palette = {};
        std::copy_n(palette_data.begin() + 25 * i, 25, palette.data.data());
        palettes.push_back(std::move(palette));
    }

    // Metatiles:
    unsigned const num_mt = get8(true);
    mt_sets.reserve(num_mt);
    std::vector<std::uint8_t> mt_tiles(32*32);
    dprint(log, "MAPFAB_NUM_MT", num_mt);
    for(unsigned i = 0; i < num_mt; ++i)
    {
        mt_set_t mt_set = {};
        mt_set.name = get_str();
        mt_set.chr_name = get_str();
        dprint(log, "MAPFAB_MT_MACRO", i, mt_set.name, mt_set.chr_name);

        mt_set.palette = get8();

        mt_set.num = get8(true);
        for(std::uint8_t& data : mt_tiles)
            data = get8();
        mt_set.attributes.resize(256);
        for(std::uint8_t& data : mt_set.attributes)
            data = get8();
        mt_set.collisions.resize(256);
        for(std::uint8_t& data : mt_set.collisions)
            data = get8();

        unsigned j = 0;
        for(unsigned y = 0; y < 16; ++y)
        for(unsigned x = 0; x < 16; ++x)
        {
            if(j++ == mt_set.num)
                break;
            mt_set.nw.push_back(mt_tiles[x*2 + y*64 + 0]);
            mt_set.ne.push_back(mt_tiles[x*2 + y*64 + 1]);
            mt_set.sw.push_back(mt_tiles[x*2 + y*64 + 32]);
            mt_set.se.push_back(mt_tiles[x*2 + y*64 + 33]);
        }

        mt_set.combined.resize(256);
        for(unsigned i = 0; i < 256; ++i)
            mt_set.combined[i] = (mt_set.attributes[i] & 0b11) | (mt_set.collisions[i] << 2);

        mt_set.combined_alt.resize(256);
        for(unsigned i = 0; i < 256; ++i)
            mt_set.combined_alt[i] = ((mt_set.attributes[i] & 0b11) << 6) | (mt_set.collisions[i]);

        mt_sets.push_back(std::move(mt_set));
    }

    // Object Classes:
    unsigned const num_oc = get8(true);
    dprint(log, "MAPFAB_NUM_OC", num_oc);
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
    levels.reserve(num_levels);
    dprint(log, "MAPFAB_NUM_LEVELS", num_levels);
    for(unsigned i = 0; i < num_levels; ++i)
    {
        level_t level = {};

        level.name = get_str();
        level.macro_name = get_str();
        level.chr_name = get_str();
        level.palette = get8();
        level.metatiles_name = get_str();
        dprint(log, "MAPFAB_LEVEL_MACRO", i, level.name, level.chr_name, level.metatiles_name);
        level.w = get8(true);
        level.h = get8(true);

        level.tiles.resize(level.w * level.h);
        for(std::uint8_t& data : level.tiles)
            data = get8();

        level.objects.resize(ocs.size());
        level.objects_x.resize(ocs.size());
        level.objects_y.resize(ocs.size());

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
                level.objects_x[it - ocs.begin()].push_back(x);
                level.objects_y[it - ocs.begin()].push_back(y);
                for(unsigned j = 0; j < it->second.size(); ++j)
                    level.objects[it - ocs.begin()].push_back(get_str());
            }
        }

        levels.push_back(std::move(level));
    }
}

void mapfab_t::load_json(std::uint8_t const* const begin, std::size_t size, fs::path mapfab_path)
{
    fs::path base_path = mapfab_path;
    base_path.remove_filename();

    auto const convert_path = [&](std::string const& str) -> std::filesystem::path
    {
        std::filesystem::path path(str, std::filesystem::path::generic_format);
        path.make_preferred();

        if(!path.empty() && path.is_relative())
            path = base_path / path;

        return path;
    };

    log_t* log = nullptr;

    dprint(log, "MAPFAB_PATH", mapfab_path);

    json data = json::parse(begin, begin+size);

    if(data.at("version") > SAVE_VERSION)
        throw std::runtime_error("File is from a newer version of MapFab.");

    // CHR:
    chrs.clear();
    for(auto const& v : data.at("chr").get<json::array_t>())
    {
        chr_t chr = {};
        chr.name = v.at("name").get<std::string>();
        chr.path = convert_path(v.at("path").get<std::string>());
        dprint(log, "MAPFAB_CHR", chr.name, chr.path);
        chrs.push_back(std::move(chr));
    }

    // Palettes:
    {
        auto const& array = data.at("palettes").at("data").get<json::array_t>();
        unsigned const num_palettes = data.at("palettes").at("num").get<int>();
        palettes.reserve(num_palettes);

        std::vector<std::uint8_t> palette_data(256*25);
        unsigned i = 0;
        for(std::uint8_t& v : palette_data)
            v = array.at(i++).get<int>();

        for(unsigned i = 0; i < num_palettes; ++i)
        {
            palette_t palette;
            std::copy_n(palette_data.begin() + 25 * i, 25, palette.data.data());
            palettes.push_back(std::move(palette));
        }
    }

    // Metatile sets:
    {
        auto const& array = data.at("metatile_sets").get<json::array_t>();
        for(auto const& mt_set : array)
        {
            mt_set_t mt = {};
            mt.name = mt_set.at("name").get<std::string>();
            mt.chr_name = mt_set.at("chr").get<std::string>();
            mt.palette = mt_set.at("palette").get<int>();
            mt.num = mt_set.at("num").get<int>();

            auto const& tiles = mt_set.at("tiles").get<json::array_t>();
            auto const& attributes = mt_set.at("attributes").get<json::array_t>();
            auto const& collisions = mt_set.at("collisions").get<json::array_t>();

            std::vector<std::uint8_t> mt_tiles(32*32);

            unsigned i = 0;
            for(std::uint8_t& v : mt_tiles)
                v = tiles.at(i++).get<int>();

            i = 0;
            mt.attributes.resize(256);
            for(std::uint8_t& v : mt.attributes)
                v = attributes.at(i++).get<int>();

            i = 0;
            mt.collisions.resize(256);
            for(std::uint8_t& v : mt.collisions)
                v = collisions.at(i++).get<int>();

            unsigned j = 0;
            for(unsigned y = 0; y < 16; ++y)
            for(unsigned x = 0; x < 16; ++x)
            {
                if(j++ == mt.num)
                    break;
                mt.nw.push_back(mt_tiles[x*2 + y*64 + 0]);
                mt.ne.push_back(mt_tiles[x*2 + y*64 + 1]);
                mt.sw.push_back(mt_tiles[x*2 + y*64 + 32]);
                mt.se.push_back(mt_tiles[x*2 + y*64 + 33]);
            }

            mt.combined.resize(256);
            for(unsigned i = 0; i < 256; ++i)
                mt.combined[i] = (mt.attributes[i] & 0b11) | (mt.collisions[i] << 2);

            mt.combined_alt.resize(256);
            for(unsigned i = 0; i < 256; ++i)
                mt.combined_alt[i] = ((mt.attributes[i] & 0b11) << 6) | (mt.collisions[i]);

            mt_sets.push_back(std::move(mt));
        }
    }

    // Object Classes:
    {
        auto const& array = data.at("object_classes").get<json::array_t>();
        for(auto const& o : array)
        {
            std::string name = o.at("name").get<std::string>();
            std::deque<field_t> fields;

            auto const& fs = o.at("fields").get<json::array_t>();
            for(auto const& f : fs)
            {
                std::string name = f.at("name").get<std::string>();
                std::string type = f.at("type").get<std::string>();
                fields.emplace_back(std::move(name), std::move(type));
            }

            ocs.emplace(std::move(name), std::move(fields));
        }
    }

    // Levels:
    {
        auto const& array = data.at("levels").get<json::array_t>();
        for(auto const& l : array)
        {
            level_t level = {};

            level.name = l.at("name").get<std::string>();
            level.macro_name = l.at("macro").get<std::string>();
            level.chr_name = l.at("chr").get<std::string>();
            level.palette = l.at("palette").get<int>();
            level.metatiles_name = l.at("metatile_set").get<std::string>();

            level.w = l.at("width").get<int>();
            level.h = l.at("height").get<int>();
            level.tiles.resize(level.w * level.h);

            auto const& tiles = l.at("tiles").get<json::array_t>();
            unsigned i = 0;
            for(std::uint8_t& data : level.tiles)
                data = tiles.at(i++);

            level.objects.resize(ocs.size());
            level.objects_x.resize(ocs.size());
            level.objects_y.resize(ocs.size());

            auto const& objects = l.at("objects").get<json::array_t>();
            for(auto const& o : objects)
            {
                std::string oc = o.at("object_class").get<std::string>();
                std::int16_t const x = static_cast<std::int16_t>(o.at("x").get<int>());
                std::int16_t const y = static_cast<std::int16_t>(o.at("y").get<int>());

                auto it = ocs.find(oc);

                if(it != ocs.end())
                {
                    level.objects_x[it - ocs.begin()].push_back(x);
                    level.objects_y[it - ocs.begin()].push_back(y);
                    for(auto const& field : it->second)
                        level.objects[it - ocs.begin()].push_back(o.at("fields").at(field.name).get<std::string>());
                }
            }

            levels.push_back(std::move(level));
        }
    }
}

void mapfab_t::compute_mmt_32()
{
    for(auto& level : levels)
    {
        level.tiles32.clear();
        level.tiles32.reserve((level.w * level.h) / 4);

        for(unsigned y = 0; y < level.w; y += 2)
        for(unsigned x = 0; x < level.h; x += 2)
        {
            mt_t mt = {};
            bool const e = x+1 < level.w;
            bool const s = y+1 < level.h;
            mt.nw = level.tiles[(x) + (y)*level.w];
            mt.ne = e ? level.tiles[(x+1) + (y)*level.w] : 0;
            mt.sw = s ? level.tiles[(x) + (y+1)*level.w] : 0;
            mt.se = s && e ? level.tiles[(x+1) + (y+1)*level.w] : 0;

            mt_set_t* mt_set = nullptr;
            for(auto& m : mt_sets)
            {
                if(m.name == level.metatiles_name)
                {
                    mt_set = &m;
                    break;
                }
            }

            if(!mt_set)
                throw std::runtime_error(fmt("MapFab error: Undefined metatile set % used in level %.", 
                                             level.metatiles_name, level.name));

            auto result = mt_set->map32.emplace(mt, mt_set->map32.size());
            if(result.second)
            {
                mt_set->nw32.push_back(mt.nw);
                mt_set->ne32.push_back(mt.ne);
                mt_set->sw32.push_back(mt.sw);
                mt_set->se32.push_back(mt.se);

                std::uint8_t a = 0;
                a |= mt_set->attributes[mt.nw] << 0;
                a |= mt_set->attributes[mt.ne] << 2;
                a |= mt_set->attributes[mt.sw] << 4;
                a |= mt_set->attributes[mt.se] << 6;
                mt_set->attributes32.push_back(a);
            }

            if(mt_set->map32.size() > 256)
                throw std::runtime_error(fmt("MapFab error: Too many 32x32 metatiles in set %.", level.metatiles_name));

            level.tiles32.push_back(result.first->second);
        }
    }
}

void convert_mapfab(mapfab_convert_type_t ct, std::uint8_t const* const begin, std::size_t size, 
                    lpstring_t at, fs::path mapfab_path, mapfab_macros_t const& macros,
                    ident_map_t<global_ht>* base_private_globals,
                    ident_map_t<group_ht>* base_private_groups)
{
    using namespace std::literals;

    mapfab_t mapfab;

    if(mapfab_path.extension() == ".json")
        mapfab.load_json(begin, size, mapfab_path);
    else
        mapfab.load_binary(begin, size, mapfab_path);

    if(ct == MAPFAB_MMT_32)
        mapfab.compute_mmt_32();

    // CHR:
    for(unsigned i = 0; i < mapfab.chrs.size(); ++i)
    {
        auto const& chr = mapfab.chrs[i];

        ident_map_t<global_ht> private_globals;
        if(base_private_globals)
            private_globals = *base_private_globals;
        ident_map_t<group_ht> private_groups;
        if(base_private_groups)
            private_groups = *base_private_groups;

        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);

        macro_invocation_t m = { macros.chr };
        m.args.push_back(chr.name); // Name
        m.args.push_back(chr.path.string()); // File
        invoke_macro(std::move(m), std::move(private_globals), std::move(private_groups));
    }

    // Palettes:
    for(unsigned i = 0; i < mapfab.palettes.size(); ++i)
    {
        auto const& palette = mapfab.palettes[i];

        ident_map_t<global_ht> private_globals;
        if(base_private_globals)
            private_globals = *base_private_globals;
        ident_map_t<group_ht> private_groups;
        if(base_private_groups)
            private_groups = *base_private_groups;

        global_t& g = private_globals.lookup(at, "_palette"sv);
        define_ct(g, at, palette.data.data(), 25);
        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);

        macro_invocation_t m = { macros.palette };
        m.args.push_back(std::to_string(i));
        invoke_macro(std::move(m), std::move(private_globals), std::move(private_groups));
    }

    // Metatiles:
    for(unsigned i = 0; i < mapfab.mt_sets.size(); ++i)
    {
        auto const& mt_set = mapfab.mt_sets[i];

        ident_map_t<global_ht> private_globals;
        if(base_private_globals)
            private_globals = *base_private_globals;
        ident_map_t<group_ht> private_groups;
        if(base_private_groups)
            private_groups = *base_private_groups;

        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);
        define_ct_int(private_globals.lookup(at, "_num"sv), at, TYPE_INT, mt_set.num);
        define_ct(private_globals.lookup(at, "_nw"sv), at, mt_set.nw.data(), mt_set.num);
        define_ct(private_globals.lookup(at, "_ne"sv), at, mt_set.ne.data(), mt_set.num);
        define_ct(private_globals.lookup(at, "_sw"sv), at, mt_set.sw.data(), mt_set.num);
        define_ct(private_globals.lookup(at, "_se"sv), at, mt_set.se.data(), mt_set.num);
        define_ct(private_globals.lookup(at, "_attributes"sv), at, mt_set.attributes.data(), mt_set.num);
        define_ct(private_globals.lookup(at, "_collisions"sv), at, mt_set.collisions.data(), mt_set.num);
        define_ct(private_globals.lookup(at, "_combined"sv), at, mt_set.combined.data(), mt_set.num);
        define_ct(private_globals.lookup(at, "_combined_alt"sv), at, mt_set.combined_alt.data(), mt_set.num);

        if(ct == MAPFAB_MMT_32)
        {
            define_ct_int(private_globals.lookup(at, "_mmt_num"sv), at, TYPE_INT, mt_set.nw32.size());
            define_ct(private_globals.lookup(at, "_mmt_nw"sv), at, mt_set.nw32.data(), mt_set.nw32.size());
            define_ct(private_globals.lookup(at, "_mmt_ne"sv), at, mt_set.ne32.data(), mt_set.ne32.size());
            define_ct(private_globals.lookup(at, "_mmt_sw"sv), at, mt_set.sw32.data(), mt_set.sw32.size());
            define_ct(private_globals.lookup(at, "_mmt_se"sv), at, mt_set.se32.data(), mt_set.se32.size());
            define_ct(private_globals.lookup(at, "_mmt_attributes"sv), at, mt_set.attributes32.data(), mt_set.attributes32.size());
        }

        macro_invocation_t m = { macros.metatiles };
        m.args.push_back(mt_set.name);
        m.args.push_back(mt_set.chr_name);
        m.args.push_back(std::to_string(mt_set.palette));
        invoke_macro(std::move(m), std::move(private_globals), std::move(private_groups));
    }

    // Levels:
    for(unsigned i = 0; i < mapfab.levels.size(); ++i)
    {
        auto const& level = mapfab.levels[i];

        auto const calc_yx = [](unsigned w, unsigned h, std::vector<std::uint8_t> const& in)
        {
            std::vector<std::uint8_t> out(in.size());
            for(unsigned x = 0; x < w; ++x)
            for(unsigned y = 0; y < h; ++y)
                out[x*h + y] = in[x + y*w];
            return out;
        };

        ident_map_t<global_ht> private_globals;
        if(base_private_globals)
            private_globals = *base_private_globals;
        ident_map_t<group_ht> private_groups;
        if(base_private_groups)
            private_groups = *base_private_groups;

        define_ct_int(private_globals.lookup(at, "_index"sv), at, TYPE_INT, i);
        define_ct_int(private_globals.lookup(at, "_width"sv), at, TYPE_INT, level.w);
        define_ct_int(private_globals.lookup(at, "_height"sv), at, TYPE_INT, level.h);

        std::vector<std::uint8_t> tiles_xy, tiles_yx;

        if(ct == MAPFAB_MMT_32)
        {
            tiles_xy = level.tiles32;
            tiles_yx = calc_yx((level.w+1) / 2, (level.h+1) / 2, tiles_xy);
        }
        else
        {
            tiles_xy = level.tiles;
            tiles_yx = calc_yx(level.w, level.h, tiles_xy);

            switch(ct)
            {
            default: 
                break;
            case MAPFAB_MMT_32:
            case MAPFAB_RLZ:
                tiles_xy = compress_rlz(&*tiles_xy.begin(), &*tiles_xy.end(), false);
                tiles_yx = compress_rlz(&*tiles_yx.begin(), &*tiles_yx.end(), false);
                break;
            case MAPFAB_PBZ:
                tiles_xy = compress_pbz(&*tiles_xy.begin(), &*tiles_xy.end());
                tiles_yx = compress_pbz(&*tiles_yx.begin(), &*tiles_yx.end());
                break;
            }
        }

        define_ct(private_globals.lookup(at, "_row_major"sv), at, tiles_xy.data(), tiles_xy.size());
        define_ct(private_globals.lookup(at, "_column_major"sv), at, tiles_yx.data(), tiles_yx.size());

        std::string append;
        for(unsigned i = 0; i < level.objects.size(); ++i)
        {
            auto const& oc = *(mapfab.ocs.begin() + i);

            define_ct_int(private_globals.lookup(at, fmt("_%_num", oc.first)), at, TYPE_INT, level.objects_x[i].size());

            // TODO: Do this without strings.
            append += fmt("\nct Int{} _%_x = Int{}(", oc.first);
            for(unsigned j = 0; j < level.objects_x[i].size(); ++j)
            {
                if(j != 0)
                    append += ", ";
                append += std::to_string(level.objects_x[i][j]);
            }
            append += ")\n";

            append += fmt("\nct Int{} _%_y = Int{}(", oc.first);
            for(unsigned j = 0; j < level.objects_y[i].size(); ++j)
            {
                if(j != 0)
                    append += ", ";
                append += std::to_string(level.objects_y[i][j]);
            }
            append += ")\n";

            for(unsigned j = 0; j < oc.second.size(); ++j)
            {
                auto const& field = oc.second[j];

                append += fmt("\nct %{} _%_% = %{}(", 
                    field.type, oc.first, field.name, field.type);

                for(unsigned k = j; k < level.objects[i].size(); k += oc.second.size())
                {
                    if(k != j)
                        append += ", ";
                    append += fmt("%(%)", field.type, level.objects[i][k]);
                }
                append += ")\n";
            }
        }

        macro_invocation_t m = { macros.level };
        m.args.push_back(level.name);
        m.args.push_back(level.chr_name);
        m.args.push_back(std::to_string(level.palette));
        m.args.push_back(level.metatiles_name);
        m.args.push_back(level.macro_name);
        invoke_macro(std::move(m), std::move(private_globals), std::move(private_groups), std::move(append));
    }
}

