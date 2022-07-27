#include "compiler_error.hpp"

#include "format.hpp"

namespace
{
    struct line_col_t
    {
        unsigned line;
        unsigned col;
    };

    line_col_t get_line_col(char const* src, pstring_t pstring)
    {
        line_col_t ret = { 1, 1 };

        for(std::size_t i = 0; i < pstring.offset; ++i)
        {
            if(src[i] == '\n')
            {
                if(src[i+1] == '\r')
                    ++i;
                goto newline;
            }
            else if(src[i] == '\r')
            {
                if(src[i+1] == '\n')
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

    char const* get_line_begin(char const* src, pstring_t pstring)
    {
        while(pstring.offset && (src[pstring.offset] == '\n' || src[pstring.offset] == '\r'))
            --pstring.offset;

        for(std::size_t i = pstring.offset;;--i)
        {
            if(src[i] == '\n' || src[i] == '\r')
                return src + std::min<std::size_t>(pstring.offset, i+1);
            if(i == 0)
                return src;
        }
    }

    char const* get_line_end(char const* src, pstring_t pstring)
    {
        auto const is_nl = [](char c) { return c == '\n' || c == '\r' || c == '\0'; };

        while(pstring.offset && is_nl(src[pstring.offset]))
            --pstring.offset;

        for(std::size_t i = pstring.offset + pstring.size;; ++i)
            if(is_nl(src[i]))
                return src + i;
    }
} // end anon namespace

// Console colors:
#define RED   "\x1B[31m"
#define GRN   "\x1B[32m"
#define YEL   "\x1B[33m"
#define BLU   "\x1B[34m"
#define MAG   "\x1B[35m"
#define CYN   "\x1B[36m"
#define WHT   "\x1B[37m"
#define RESET "\x1B[0m"
#define BOLD  "\x1B[1m"
#define UNDERLINE "\x1B[4m"

std::string fmt_source_pos(file_contents_t const& file, pstring_t pstring)
{
    line_col_t line_col = get_line_col(file.source(), pstring);
    return fmt(BOLD "%:%:%" RESET, file.name(), line_col.line, line_col.col);
}

std::string fmt_error(file_contents_t const& file, pstring_t pstring, 
                      std::string const& what)
{
    return fmt_error(file, pstring, what, RED BOLD, "error");
}

std::string fmt_error(file_contents_t const& file, pstring_t pstring, 
                      std::string const& what, char const* color, char const* prefix)
{

    std::string str(fmt("%: %%:" RESET " %\n", 
                        fmt_source_pos(file, pstring), color, prefix, what));

    char const* line_begin = get_line_begin(file.source(), pstring);
    char const* line_end = get_line_end(file.source(), pstring);

    std::string pre = fmt(" % | ", get_line_col(file.source(), pstring).line);

    str += pre;
    str.insert(str.end(), line_begin, line_end);
    str.push_back('\n');

#ifndef NDEBUG
    for(char c : str)
        assert(c);
#endif

    // If the source line contains a newline, don't bother drawing a
    // caret underneath it.
    for(char const* ptr = line_begin; ptr != line_end; ++ptr)
        if(*ptr == '\r' || *ptr == '\n')
            return str;

    unsigned const caret_position = 
        pre.size() + (file.source() + pstring.offset) - line_begin;

    str.resize(str.size() + caret_position, ' ');

    str += color;

    // Remove trailing whitespace
    while(pstring.size && std::isspace(file.source()[pstring.offset + pstring.size - 1]))
        --pstring.size;

    unsigned i = 0;
    do
        str.push_back('^');
    while(++i < pstring.size);

    str += RESET;

    str.push_back('\n');
    return str;
}

std::string fmt_note(file_contents_t const& file, pstring_t pstring, std::string const& what)
{
    return fmt_error(file, pstring, what, CYN BOLD, "note");
}

std::string fmt_note(std::string const& what)
{
    return fmt(BOLD CYN "note: " RESET "%\n", what);
}

void compiler_error(file_contents_t const& file, pstring_t pstring, 
                    std::string const& what)
{
    throw compiler_error_t(fmt_error(file, pstring, what));
}

void compiler_error(pstring_t pstring, std::string const& what)
{
    file_contents_t file(pstring.file_i);
    compiler_error(file, pstring, what);
}

void compiler_warning(file_contents_t const& file, pstring_t pstring, std::string const& what)
{
    std::string msg = fmt_error(file, pstring, what, YEL BOLD, "warning");
    std::fputs(msg.data(), stderr);
}

void compiler_warning(pstring_t pstring, std::string const& what)
{
    file_contents_t file(pstring.file_i);
    compiler_warning(file, pstring, what);
}

void compiler_warning(std::string const& what)
{
    std::fprintf(stderr, YEL BOLD "warning: " RESET "%s\n", what.data());
    std::fflush(stderr);
}
