#include "xfab.hpp"

#include <array>
#include <map>

#include "json.hpp"

#include "macro.hpp"
#include "define.hpp"
#include "globals.hpp"
#include "debug_print.hpp"
#include "convert_compress.hpp"

using json = nlohmann::json;

struct xfab_t
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
        std::vector<std::uint16_t> tiles;
        std::uint8_t collision;

        auto operator<=>(mt_t const&) const = default;
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
        unsigned w;
        unsigned h;

        std::vector<std::uint16_t> tiles;
        std::vector<std::uint8_t> collisions;

        std::vector<std::vector<std::uint16_t>> mt_set;
        std::vector<std::uint8_t> mt_set_collisions;
        std::map<mt_t, unsigned> mt_map;
        std::vector<std::uint8_t> mts;
        std::vector<bc::deque<std::string>> objects;
        std::vector<std::vector<std::int16_t>> objects_x;
        std::vector<std::vector<std::int16_t>> objects_y;
        std::vector<std::vector<std::string>> objects_name;
    };

    unsigned metatile_size;
    unsigned collision_scale() const { return std::max<unsigned>(metatile_size, 1); }
    std::vector<chr_t> chrs;
    std::vector<palette_t> palettes;
    fc::vector_map<std::string, bc::deque<field_t>> ocs;
    std::vector<level_t> levels;

    void load_binary(std::uint8_t const* const begin, std::size_t size, fs::path xfab_path);
    void load_json(std::uint8_t const* const begin, std::size_t size, fs::path xfab_path);
    void compute_mt();
};

static constexpr std::uint8_t SAVE_VERSION = 1; 

void xfab_t::load_binary(std::uint8_t const* const begin, std::size_t size, fs::path xfab_path)
{
    fs::path base_path = xfab_path;
    base_path.remove_filename();

    log_t* log = nullptr;

    dprint(log, "8X8FAB_PATH", xfab_path);

    std::uint8_t const* ptr = begin;
    std::uint8_t const* const end = begin + size;;

    auto const get8 = [&](bool adjust = false) -> unsigned
    {
        if(ptr >= end)
            throw std::runtime_error("Invalid xfab file; unexpected EOF.");
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
        throw std::runtime_error("Invalid xfab file; no magic number.");

    if(memcmp(ptr, "8x8Fab", 7) != 0)
        throw std::runtime_error("Incorrect magic number.");
    unsigned const save_version = ptr[7];
    if(save_version > SAVE_VERSION)
        throw std::runtime_error("File is from a newer version of xfab.");
    ptr += 8;

    // Collision file:
    metatile_size = get8();
    std::string collisions_path = get_str();

    dprint(log, "8X8FAB_COLLISIONS_PATH", collisions_path);

    // CHR:
    unsigned const num_chr = get8(true);
    chrs.reserve(num_chr);
    for(unsigned i = 0; i < num_chr; ++i)
    {
        chr_t chr = {};
        chr.name = get_str();
        chr.path = convert_path(get_str());

        dprint(log, "8X8FAB_CHR", chr.name, chr.path);
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

    // Object Classes:
    unsigned const num_oc = get8(true);
    dprint(log, "8X8FAB_NUM_OC", num_oc);
    for(unsigned i = 0; i < num_oc; ++i)
    {
        std::string name = get_str();
        get8(); // R
        get8(); // G
        get8(); // B
        bc::deque<field_t> fields;

        unsigned const num_fields = get8();
        for(unsigned i = 0; i < num_fields; ++i)
            fields.emplace_back(get_str(), get_str());

        ocs.emplace(std::move(name), std::move(fields));
    }

    // Levels:
    unsigned const num_levels = get16();
    levels.reserve(num_levels);
    dprint(log, "8X8FAB_NUM_LEVELS", num_levels);
    for(unsigned i = 0; i < num_levels; ++i)
    {
        level_t level = {};

        level.name = get_str();
        level.macro_name = get_str();
        level.chr_name = get_str();
        level.palette = get8();
        dprint(log, "8X8FAB_LEVEL_MACRO", i, level.name, level.chr_name);
        level.w = get16();
        level.h = get16();

        level.tiles.resize(level.w * level.h);
        for(std::uint16_t& data : level.tiles)
            data = get16();

        level.collisions.resize((level.w / collision_scale()) * (level.h / collision_scale()));
        for(std::uint8_t& data : level.collisions)
            data = get8(false);

        level.objects.resize(ocs.size());
        level.objects_name.resize(ocs.size());
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
                level.objects_name[it - ocs.begin()].push_back(std::move(name));
                level.objects_x[it - ocs.begin()].push_back(x);
                level.objects_y[it - ocs.begin()].push_back(y);
                for(unsigned j = 0; j < it->second.size(); ++j)
                    level.objects[it - ocs.begin()].push_back(get_str());
            }
        }

        levels.push_back(std::move(level));
    }
}

void xfab_t::load_json(std::uint8_t const* const begin, std::size_t size, fs::path xfab_path)
{
    throw 0; // TODO
    /*
    fs::path base_path = xfab_path;
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

    dprint(log, "8X8FAB_PATH", xfab_path);

    json data = json::parse(begin, begin+size);

    if(data.at("version") > SAVE_VERSION)
        throw std::runtime_error("File is from a newer version of xfab.");

    // CHR:
    chrs.clear();
    for(auto const& v : data.at("chr").get<json::array_t>())
    {
        chr_t chr = {};
        chr.name = v.at("name").get<std::string>();
        chr.path = convert_path(v.at("path").get<std::string>());
        dprint(log, "8X8FAB_CHR", chr.name, chr.path);
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
            bc::deque<field_t> fields;

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
            level.objects_name.resize(ocs.size());
            level.objects_x.resize(ocs.size());
            level.objects_y.resize(ocs.size());

            auto const& objects = l.at("objects").get<json::array_t>();
            for(auto const& o : objects)
            {
                std::string oc = o.at("object_class").get<std::string>();
                std::int16_t const x = static_cast<std::int16_t>(o.at("x").get<int>());
                std::int16_t const y = static_cast<std::int16_t>(o.at("y").get<int>());
                std::string name = o.at("name").get<std::string>();

                auto it = ocs.find(oc);

                if(it != ocs.end())
                {
                    level.objects_name[it - ocs.begin()].push_back(std::move(name));
                    level.objects_x[it - ocs.begin()].push_back(x);
                    level.objects_y[it - ocs.begin()].push_back(y);
                    for(auto const& field : it->second)
                        level.objects[it - ocs.begin()].push_back(o.at("fields").at(field.name).get<std::string>());
                }
            }

            levels.push_back(std::move(level));
        }
    }
    */
}

void xfab_t::compute_mt()
{
    unsigned const s = metatile_size;

    if(s == 0)
        return;

    for(auto& level : levels)
    {
        level.mt_map.clear();
        level.mt_set.clear();
        level.mt_set_collisions.clear();
        level.mts.clear();

        level.mt_set.resize(s*s);

        for(unsigned y = 0; y < level.h; y += s)
        for(unsigned x = 0; x < level.w; x += s)
        {
            mt_t mt = {};

            for(unsigned yy = 0; yy < s; yy += 1)
            for(unsigned xx = 0; xx < s; xx += 1)
            {
                if(x+xx < level.w && y+yy < level.h)
                    mt.tiles.push_back(level.tiles[x+xx + (y+yy)*level.w]);
                else
                    mt.tiles.push_back(0);
            }

            mt.collision = level.collisions[(x / s) + (y / s) * ((level.w + s - 1) / s)];

            auto result = level.mt_map.emplace(mt, level.mt_map.size());
            if(result.second)
            {
                for(unsigned i = 0; i < s*s; i += 1)
                    level.mt_set[i].push_back(mt.tiles[i]);
                level.mt_set_collisions.emplace_back(mt.collision);
            }

            level.mts.push_back(result.first->second);
        }
    }

    for(auto& level : levels)
    {
        if(level.mt_map.size() > 256)
            throw std::runtime_error(fmt("xfab error: Too many metatiles (%) in level %", level.mt_map.size(), level.name));
    }
}

void convert_xfab(xfab_convert_type_t ct, std::uint8_t const* const begin, std::size_t size, 
                  lpstring_t at, fs::path xfab_path, xfab_macros_t const& macros,
                  ident_map_t<global_ht>* base_private_globals,
                  ident_map_t<group_ht>* base_private_groups)
{
    using namespace std::literals;

    xfab_t xfab;

    if(xfab_path.extension() == ".json")
        xfab.load_json(begin, size, xfab_path);
    else
        xfab.load_binary(begin, size, xfab_path);

    xfab.compute_mt();

    // CHR:
    for(unsigned i = 0; i < xfab.chrs.size(); ++i)
    {
        auto const& chr = xfab.chrs[i];

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
    for(unsigned i = 0; i < xfab.palettes.size(); ++i)
    {
        auto const& palette = xfab.palettes[i];

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

    // Levels:
    std::vector<unsigned> object_indices;
    for(unsigned i = 0; i < xfab.levels.size(); ++i)
    {
        auto const& level = xfab.levels[i];

        auto const calc_yx = [](unsigned w, unsigned h, auto const& in)
        {
            using v = typename std::remove_reference_t<decltype(in)>::value_type;
            std::vector<v> out(in.size());
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
        define_ct_int(private_globals.lookup(at, "_width"sv), at, TYPE_INT, level.w / xfab.collision_scale());
        define_ct_int(private_globals.lookup(at, "_height"sv), at, TYPE_INT, level.h / xfab.collision_scale());

        if(xfab.metatile_size == 0)
        {
            std::vector<std::uint8_t> tiles_xy, tiles_yx, attrs_xy, attrs_yx, colls_xy, colls_yx;

            for(std::uint16_t i : level.tiles)
            {
                tiles_xy.push_back(i);
                attrs_xy.push_back(i >> 8);
            }
            colls_xy = level.collisions;

            tiles_yx = calc_yx(level.w, level.h, tiles_xy);
            attrs_yx = calc_yx(level.w, level.h, attrs_xy);
            colls_yx = calc_yx(level.w, level.h, colls_xy);

            switch(ct)
            {
            default: 
                break;
            case XFAB_RLZ:
                tiles_xy = compress_rlz(&*tiles_xy.begin(), &*tiles_xy.end(), false);
                tiles_yx = compress_rlz(&*tiles_yx.begin(), &*tiles_yx.end(), false);
                attrs_xy = compress_rlz(&*attrs_xy.begin(), &*attrs_xy.end(), false);
                attrs_yx = compress_rlz(&*attrs_yx.begin(), &*attrs_yx.end(), false);
                colls_xy = compress_rlz(&*colls_xy.begin(), &*colls_xy.end(), false);
                colls_yx = compress_rlz(&*colls_yx.begin(), &*colls_yx.end(), false);
                break;
            case XFAB_PBZ:
                tiles_xy = compress_pbz(&*tiles_xy.begin(), &*tiles_xy.end());
                tiles_yx = compress_pbz(&*tiles_yx.begin(), &*tiles_yx.end());
                attrs_xy = compress_pbz(&*attrs_xy.begin(), &*attrs_xy.end());
                attrs_yx = compress_pbz(&*attrs_yx.begin(), &*attrs_yx.end());
                colls_xy = compress_pbz(&*attrs_xy.begin(), &*colls_xy.end());
                colls_yx = compress_pbz(&*attrs_yx.begin(), &*colls_yx.end());
                break;
            }

            define_ct(private_globals.lookup(at, "_tiles_row_major"sv), at, tiles_xy.data(), tiles_xy.size());
            define_ct(private_globals.lookup(at, "_tiles_column_major"sv), at, tiles_yx.data(), tiles_yx.size());
            define_ct(private_globals.lookup(at, "_attributes_row_major"sv), at, attrs_xy.data(), attrs_xy.size());
            define_ct(private_globals.lookup(at, "_attributes_column_major"sv), at, attrs_yx.data(), attrs_yx.size());
            define_ct(private_globals.lookup(at, "_collisions_row_major"sv), at, colls_xy.data(), colls_xy.size());
            define_ct(private_globals.lookup(at, "_collisions_column_major"sv), at, colls_yx.data(), colls_yx.size());
        }
        else
        {
            std::vector<std::uint8_t> tiles_xy, tiles_yx;

            tiles_xy = level.mts;
            tiles_yx = calc_yx(level.w / xfab.collision_scale(), level.h / xfab.collision_scale(), tiles_xy);

            switch(ct)
            {
            default: 
                break;
            case XFAB_RLZ:
                tiles_xy = compress_rlz(&*tiles_xy.begin(), &*tiles_xy.end(), false);
                tiles_yx = compress_rlz(&*tiles_yx.begin(), &*tiles_yx.end(), false);
                break;
            case XFAB_PBZ:
                tiles_xy = compress_pbz(&*tiles_xy.begin(), &*tiles_xy.end());
                tiles_yx = compress_pbz(&*tiles_yx.begin(), &*tiles_yx.end());
                break;
            }

            define_ct(private_globals.lookup(at, "_row_major"sv), at, tiles_xy.data(), tiles_xy.size());
            define_ct(private_globals.lookup(at, "_column_major"sv), at, tiles_yx.data(), tiles_yx.size());

            for(unsigned i = 0; i < xfab.collision_scale()*xfab.collision_scale(); i += 1)
                define_ct(private_globals.lookup(at, fmt("_mt_%", i)), at, level.mt_set[i].data(), level.mt_set[i].size());
            define_ct(private_globals.lookup(at, "_mt_collisions"sv), at, level.mt_set_collisions.data(), level.mt_set_collisions.size());
        }


        std::string append;
        for(unsigned i = 0; i < level.objects.size(); ++i)
        {
            auto const& oc = *(xfab.ocs.begin() + i);

            object_indices.resize(level.objects_name[i].size());
            std::iota(object_indices.begin(), object_indices.end(), 0);
            std::stable_sort(object_indices.begin(), object_indices.end(), [&](unsigned a, unsigned b)
            {
                if(level.objects_name[i][a].empty())
                    return false;
                if(level.objects_name[i][b].empty())
                    return true;
                return level.objects_name[i][a] < level.objects_name[i][b];
            });

            define_ct_int(private_globals.lookup(at, fmt("_%_num", oc.first)), at, TYPE_INT, level.objects_x[i].size());

            // TODO: Do this without strings.
            auto const build = [&](std::string const& initial, auto fn)
            {
                append += initial;
                bool first = true;
                for(unsigned j : object_indices)
                {
                    if(!first)
                        append += ", ";
                    first = false;
                    append += fn(j);
                }
                append += ")\n";
            };

            build(fmt("\nct Int{} _%_x = Int{}(", oc.first), [&](unsigned j)
            {
                return std::to_string(level.objects_x[i][j]);
            });

            build(fmt("\nct Int{} _%_y = Int{}(", oc.first), [&](unsigned j)
            {
                return std::to_string(level.objects_y[i][j]);
            });

            for(unsigned j = 0; j < oc.second.size(); ++j)
            {
                auto const& field = oc.second[j];

                append += fmt("\nct %{} _%_% = %{}(", 
                    field.type, oc.first, field.name, field.type);

                bool first = true;
                for(unsigned k : object_indices)
                {
                    if(!first)
                        append += ", ";
                    first = false;
                    unsigned const q = j + k * oc.second.size();
                    append += fmt("%(%)", field.type, level.objects[i][q]);
                }
                append += ")\n";
            }

            // Named objects
            for(unsigned j : object_indices)
            {
                auto const& name = level.objects_name[i][j];
                if(name.empty())
                    continue;
                define_ct_int(private_globals.lookup(at, fmt("_%_name_%", oc.first, name)), at, TYPE_INT, j);
            }
        }

        macro_invocation_t m = { macros.level };
        m.args.push_back(level.name);
        m.args.push_back(level.chr_name);
        m.args.push_back(std::to_string(level.palette));
        m.args.push_back(level.macro_name);
        invoke_macro(std::move(m), std::move(private_globals), std::move(private_groups), std::move(append));
    }
}

