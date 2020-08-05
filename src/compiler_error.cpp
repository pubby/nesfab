#include "compiler_error.hpp"

#include "format.hpp"

namespace
{

    struct line_col_t
    {
        unsigned line;
        unsigned col;
    };

    line_col_t get_line_col(pstring_t pstring)
    {
        char const* source = _files[pstring.file_i].source();
        line_col_t ret = { 1, 1 };

        for(std::size_t i = 0; i < pstring.offset; ++i)
        {
            if(source[i] == '\n')
            {
                if(source[i+1] == '\r')
                    ++i;
                goto newline;
            }
            else if(source[i] == '\r')
            {
                if(source[i+1] == '\n')
                    ++i;
            newline:
                ++ret.line;
                ret.col = 1;
            }
            else
                ++ret.col;
        }

        return ret;
    }

    char const* get_line_begin(pstring_t pstring)
    {
        char const* source = _files[pstring.file_i].source();
        for(std::size_t i = pstring.offset;;--i)
        {
            if(source[i] == '\n' || source[i] == '\r')
                return source + std::min<std::size_t>(pstring.offset, i+1);
            if(i == 0)
                return source;
        }
    }

    char const* get_line_end(pstring_t pstring)
    {
        char const* source = _files[pstring.file_i].source();
        for(std::size_t i = pstring.offset + pstring.size;;++i)
            if(source[i] == '\n' || source[i] == '\r' || source[i] == '\0')
                return source + i;
    }

} // end anon namespace

std::string fmt_source_pos(pstring_t pstring)
{
    line_col_t line_col = get_line_col(pstring);
    return fmt("%:%:%", _files[pstring.file_i].filename(), 
               line_col.line, line_col.col);
}

std::string fmt_error(pstring_t pstring, std::string const& what)
{
    std::string str(fmt("%: error: %\n", 
                        fmt_source_pos(pstring), what));

    char const* line_begin = get_line_begin(pstring);
    char const* line_end = get_line_end(pstring);

    str.insert(str.end(), line_begin, line_end);
    str.push_back('\n');

    // If the source line contains a newline, don't bother drawing a
    // caret underneath it.
    for(char const* ptr = line_begin; ptr != line_end; ++ptr)
        if(*ptr == '\r' || *ptr == '\n')
            return str;

    unsigned const caret_position = 
        (_files[pstring.file_i].source() + pstring.offset) - line_begin;

    str.resize(str.size() + caret_position, ' ');

    unsigned i = 0;
    do
        str.push_back('^');
    while (++i < pstring.size);

    str.push_back('\n');
    return str;
}

void compiler_error(pstring_t pstring, std::string const& what)
{
    throw compiler_error_t(fmt_error(pstring, what));
}
