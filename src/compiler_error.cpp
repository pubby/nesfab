#include "compiler_error.hpp"

#include "format.hpp"
#include "options.hpp"
#include "assert.hpp"

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

std::string fmt_source_pos(file_contents_t const& file, pstring_t pstring)
{
    return fmt_source_pos(file.name(), file.source(), pstring);
}

std::string fmt_source_pos(std::string const& filename, char const* source, pstring_t pstring)
{
    line_col_t line_col = get_line_col(source, pstring);
    return fmt(CONSOLE_BOLD "%:%:%" CONSOLE_RESET, filename, line_col.line, line_col.col);
}

std::string fmt_error(
    pstring_t pstring, std::string const& what, file_contents_t const* file,
    char const* color, char const* prefix)
{
    file_contents_t pstring_file;
    if(!file)
    {
        pstring_file.reset(pstring.file_i);
        file = &pstring_file;
    }

    assert(file->index() == pstring.file_i);

    std::string str(fmt("%: %%:" CONSOLE_RESET " %\n", fmt_source_pos(*file, pstring), color, prefix, what));

    char const* line_begin = get_line_begin(file->source(), pstring);
    char const* line_end = get_line_end(file->source(), pstring);
    if(line_end <= line_begin)
        line_end = line_begin;

    std::string pre = fmt(" % | ", get_line_col(file->source(), pstring).line);

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
        pre.size() + (file->source() + pstring.offset) - line_begin;

    str.resize(str.size() + caret_position, ' ');

    str += color;

    // Remove trailing whitespace
    if(!std::isspace(file->source()[pstring.offset]))
        while(pstring.size && std::isspace(file->source()[pstring.offset + pstring.size - 1]))
            --pstring.size;

    unsigned i = 0;
    do
        str.push_back('^');
    while(++i < pstring.size);

    str += CONSOLE_RESET;

    str.push_back('\n');
    return str;
}

std::string fmt_note(pstring_t pstring, std::string const& what,
                     file_contents_t const* file)
{
    return fmt_error(pstring, what, file, CONSOLE_CYN CONSOLE_BOLD, "note");
}

std::string fmt_note(std::string const& what)
{
    return fmt(CONSOLE_BOLD CONSOLE_CYN "note: " CONSOLE_RESET "%\n", what);
}

std::string fmt_warning(pstring_t pstring, std::string const& what,
                     file_contents_t const* file)
{
    return fmt_error(pstring, what, file, CONSOLE_YEL CONSOLE_BOLD, "warning");
}

std::string fmt_error(std::string const& what)
{
    return fmt(CONSOLE_BOLD CONSOLE_YEL "error: " CONSOLE_RESET "%\n", what);
}

void compiler_error(pstring_t pstring, std::string const& what, 
                    file_contents_t const* file)
{
    throw compiler_error_t(fmt_error(pstring, what, file));
}

void compiler_warning(pstring_t pstring, std::string const& what, 
                      file_contents_t const* file)
{
    std::string msg = fmt_warning(pstring, what, file);

    if(compiler_options().werror)
    {
        msg += fmt_note("This is an error because --error-on-warning is enabled.");
        throw compiler_error_t(std::move(msg));
    }
    else
    {
        std::fputs(msg.c_str(), stderr);
        std::fflush(stderr);
    }
}

void compiler_warning(std::string const& what, bool formatted)
{
    std::string msg = formatted ? what : fmt(CONSOLE_YEL CONSOLE_BOLD "warning: " CONSOLE_RESET "%\n", what);

    if(compiler_options().werror)
    {
        msg += fmt_note("This is an error because --error-on-warning is enabled.");
        throw compiler_error_t(std::move(msg));
    }
    else
    {
        std::fputs(msg.c_str(), stderr);
        std::fflush(stderr);
    }
}
