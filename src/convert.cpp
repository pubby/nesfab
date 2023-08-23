#include "convert.hpp"

#include <filesystem>

#include "compiler_error.hpp"
#include "format.hpp"

#include "convert_compress.hpp"
#include "convert_png.hpp"
#include "ext_lex_tables.hpp"
#include "mods.hpp"
#include "globals.hpp"
#include "text.hpp"

namespace fs = ::std::filesystem;

string_literal_t convert_arg_t::filename() const
{
    if(auto* p = std::get_if<string_literal_t>(&value))
        return *p;
    else
        compiler_error(pstring, "Expecting filename.");
}

ext_lex::token_type_t lex_extension(char const* str)
{
    if(*str == '.')
        ++str;
    ext_lex::token_type_t lexed = ext_lex::TOK_START;
    for(char const* ptr = str; lexed > ext_lex::TOK_LAST_STATE; ++ptr)
    {
        unsigned char const c = *ptr;
        lexed = ext_lex::lexer_transition_table[lexed + ext_lex::lexer_ec_table[c]];
        if(!c)
            break;
    }
    return lexed;
}

static std::vector<std::uint8_t> convert_spr16(std::vector<std::uint8_t> const& in)
{
    std::vector<std::uint8_t> ret(in.size());

    if(in.size() % (16 * 32) != 0)
        throw convert_error_t("CHR dimensions are not a multiple of 8x16.");

    unsigned const height = in.size() / (16 * 32);
    unsigned i = 0;

    for(unsigned ty = 0; ty < height; ++ty)
    for(unsigned tx = 0; tx < 16; ++tx)
    {
        for(unsigned j = 0; j < 16; ++j)
            ret[i++] = in[tx*16 + ty*(16*32) + j];
        for(unsigned j = 0; j < 16; ++j)
            ret[i++] = in[tx*16 + ty*(16*32) + j + (16*16)];
    }

    return ret;
}

static std::vector<std::uint8_t> convert_pal3(std::vector<std::uint8_t> const& in)
{
    std::vector<std::uint8_t> ret;
    ret.reserve(in.size() * 3 / 4);

    for(unsigned i = 0; i < in.size(); ++i)
        if(i % 4 != 0)
            ret.push_back(in[i]);

    return ret;
}

static std::vector<std::uint8_t> convert_pal25(std::vector<std::uint8_t> const& in)
{
    std::vector<std::uint8_t> ret;
    ret.reserve(in.size());

    for(unsigned i = 0; i < in.size(); ++i)
    {
        unsigned j = i % 25;
        if(j % 4 != 0)
            ret.push_back(in[i]);
        if(j == 24)
            ret.push_back(in[i-24]);
    }

    return ret;
}

conversion_t convert_file(char const* source, pstring_t script, fs::path preferred_dir, 
                          string_literal_t const& filename, mods_t const* mods,
                          convert_arg_t* args, std::size_t argn)
{
    using namespace std::literals;

    try
    {
        fs::path path;
        if(!resource_path(preferred_dir, fs::path(filename.string), path))
            compiler_error(filename.pstring, fmt("Missing file: %", filename.string));

        std::string_view const view = script.view(source);
        conversion_t ret;

        auto const read_as_vec = [&]{ return read_binary_file(path.string(), filename.pstring); };
        auto const get_extension = [&]{ return lex_extension(path.extension().string().c_str()); };

        constexpr auto valid_mods = MOD_spr_8x16 | MOD_palette_3 | MOD_palette_25;

        auto const read_file = [&](bool format)
        {
            if(mods)
                mods->validate(script, valid_mods);

            bool const spr16 = mod_test(mods, MOD_spr_8x16);
            bool const pal3 = mod_test(mods, MOD_palette_3);
            bool const pal25 = mod_test(mods, MOD_palette_25);

            if(pal3 && pal25)
                compiler_error(filename.pstring, "+palette_3 is incompatible with +palette_25.");

            std::vector<std::uint8_t> vec = read_as_vec();

            if(format)
            {
                switch(get_extension())
                {
                case ext_lex::TOK_png:
                    vec = png_to_chr(vec.data(), vec.size(), spr16);
                    break;

                case ext_lex::TOK_txt:
                    vec.resize(normalize_line_endings(reinterpret_cast<char*>(vec.data()), vec.size()));
                    //fall-through
                case ext_lex::TOK_chr:
                case ext_lex::TOK_bin:
                case ext_lex::TOK_nam:
                case ext_lex::TOK_pal:
                    if(spr16)
                        vec = convert_spr16(vec);
                    break;

                default:
                    compiler_error(filename.pstring, fmt("% cannot process file format: %", view, filename.string));
                }
            }
            else if(spr16)
                vec = convert_spr16(vec);

            if(pal3)
                vec = convert_pal3(vec);

            if(pal25)
                vec = convert_pal25(vec);

            return vec;
        };

        auto const check_argn = [&](unsigned expected)
        {
            if(argn != expected)
                compiler_error(filename.pstring, fmt("Wrong number of arguments. Expecting %.", expected + 2));
        };

        if(view == "raw"sv)
        {
            check_argn(0);
            ret.data = read_file(false);
        }
        else if(view == "fmt"sv)
        {
            check_argn(0);
            ret.data = read_file(true);
        }
        else if(view == "pbz"sv)
        {
            check_argn(0);
            std::vector<std::uint8_t> vec = read_file(true);
            ret = convert_pbz(vec.data(), vec.data() + vec.size());
        }
        else if(view == "rlz"sv)
        {
            bool terminate = true;
            if(argn != 0)
            {
                check_argn(1);
                if(bool* b = std::get_if<bool>(&args[0].value))
                    terminate = *b;
                else
                    compiler_error(args[0].pstring, "Expecting true or false.");
            }
            std::vector<std::uint8_t> vec = read_file(true);
            ret = convert_rlz(vec.data(), vec.data() + vec.size(), terminate);
        }
        else
            compiler_error(script, fmt("Unknown file type: %", view));

        std::size_t size = 0;
        if(auto const* vec = std::get_if<std::vector<std::uint8_t>>(&ret.data))
            size = vec->size();
        else if(auto const* vec = std::get_if<std::vector<locator_t>>(&ret.data))
            size = vec->size();
        else if(auto const* proc = std::get_if<asm_proc_t>(&ret.data))
            size = proc->size();
        if(size > MAX_PAA_SIZE)
            compiler_error(filename.pstring, fmt("Data is of size % is too large to handle. Maximum size: %.", size, MAX_PAA_SIZE));

        return ret;
    }
    catch(convert_error_t const& error)
    {
        compiler_error(filename.pstring, error.what());
    }
    catch(...)
    {
        throw;
    }
}
