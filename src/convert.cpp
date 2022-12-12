#include "convert.hpp"

#include <filesystem>

#include "compiler_error.hpp"
#include "format.hpp"

#include "convert_compress.hpp"
#include "convert_png.hpp"
#include "ext_lex_tables.hpp"
#include "mods.hpp"

namespace fs = ::std::filesystem;

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

conversion_t convert_file(char const* source, pstring_t script, fs::path preferred_dir, 
                          string_literal_t const& filename, mods_t const* mods)
{
    using namespace std::literals;

    try
    {
        fs::path path;
        if(!resource_path(preferred_dir, fs::path(filename.string), path))
            compiler_error(filename.pstring, fmt("Missing file: %", filename.string));

        std::string_view const view = script.view(source);
        conversion_t ret;

        auto const read_as_vec = [&]
        {
            std::vector<std::uint8_t> vec;

            if(!read_binary_file(path.c_str(), [&](std::size_t size)
            {
                vec.resize(size);
                return vec.data();
            }))
            {
                compiler_error(filename.pstring, fmt("Unable to read: %", filename.string));
            }

            return vec;
        };

        auto const get_extension = [&]{ return lex_extension(path.extension().c_str()); };

        auto const read_file = [&]
        {
            bool const spr16 = mod_test(mods, MOD_spr16);

            std::vector<std::uint8_t> vec = read_as_vec();

            switch(get_extension())
            {
            case ext_lex::TOK_png:
                if(mods)
                    mods->validate(script, MOD_spr16);
                vec = png_to_chr(vec.data(), vec.size(), spr16);
                break;

            case ext_lex::TOK_chr:
            case ext_lex::TOK_bin:
            case ext_lex::TOK_nam:
                if(mods)
                    mods->validate(script, MOD_spr16);
                if(spr16)
                    vec = convert_spr16(vec);
                break;

            default:
                compiler_error(filename.pstring, fmt("% cannot process file format: %", view, filename.string));
            }

            return vec;
        };

        if(view == "bin"sv)
        {
            if(mods)
                mods->validate(script);
            ret.data = read_as_vec();
        }
        else if(view == "chr"sv)
        {
            ret.data = read_file();
        }
        else if(view == "pbz"sv)
        {
            std::vector<std::uint8_t> vec = read_file();
            ret = convert_pbz(vec.data(), vec.data() + vec.size());
        }
        else if(view == "rlz"sv)
        {
            std::vector<std::uint8_t> vec = read_file();
            ret = convert_rlz(vec.data(), vec.data() + vec.size());
        }
        else
            compiler_error(script, fmt("Unknown file type: %", view));

        std::size_t size = 0;
        if(auto const* vec = std::get_if<std::vector<std::uint8_t>>(&ret.data))
            size = vec->size();
        else if(auto const* vec = std::get_if<std::vector<locator_t>>(&ret.data))
            size = vec->size();
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
