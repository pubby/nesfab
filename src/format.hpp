#ifndef FORMAT_HPP
#define FORMAT_HPP

// Functions for formatting strings.

#include <cstdio>
#include <sstream>
#include <string>
#include <string_view>

template<char F>
void fmt_impl(std::ostringstream& ss, char const* str)
{
    while(*str)
        ss.rdbuf()->sputc(*str++);
}

template<char F, typename T, typename... Ts>
void fmt_impl(std::ostringstream& ss, char const* str, T const& t, Ts const&... ts)
{
    while(*str)
    {
        char const c = *str++;
        if(c == F)
        {
            ss << t;
            fmt_impl<F>(ss, str, ts...);
            return;
        }
        else
            ss.rdbuf()->sputc(c);
    }
}

// A really basic wrapper around ostringstream.
// Example use: fmt("value % = %", str, num)
template<char F = '%', typename... Ts>
std::string fmt(char const* str, Ts const&... ts)
{
    std::ostringstream ss;
    fmt_impl<F>(ss, str, ts...);
    return ss.str();
}

template<char F = '%', typename... Ts>
int ffmt(FILE* fp, char const* str, Ts const&... ts)
{
    return std::fputs(fmt<F>(str, ts...).c_str(), fp);
}


template<char F>
void ezcat_impl(std::ostringstream& ss) {}

template<char F, typename T, typename... Ts>
void ezcat_impl(std::ostringstream& ss, T const& t, Ts const&... ts)
{
    ss << t;
    fmt_impl<F>(ss, str, ts...);
}

// Just combines the string representations together.
template<typename... Ts>
std::string ezcat(Ts const&... ts)
{
    std::ostringstream ss;
    ezcat_impl<F>(ss, ts...);
    return ss.str();
}

#endif
