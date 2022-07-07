#include "convert_file.hpp"

#include "compiler_error.hpp"
#include "format.hpp"

std::vector<locator_t> convert_file(char const* source, pstring_t script, string_literal_t const& filename)
{
    using namespace std::literals;

    std::string_view const view = script.view(source);

    std::vector<locator_t> ret;

    if(view == "bin"sv)
    {
        FILE* fp = std::fopen(filename.string.c_str(), "rb");

        if(!fp)
            goto cant_read;

        // Get the file size
        std::fseek(fp, 0, SEEK_END);
        std::size_t const size = ftell(fp);
        std::fseek(fp, 0, SEEK_SET);

        ret.reserve(size);
        while(true)
        {
            int c = std::fgetc(fp);
            if(c == EOF)
                break;
            ret.push_back(locator_t::const_byte(c));
        }
    }
    else
        compiler_error(script, fmt("Unknown file type %", view));

    return ret;

cant_read:
    compiler_error(filename.pstring, fmt("Unable to read %", filename.string));
}
