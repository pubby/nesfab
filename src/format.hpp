#ifndef FORMAT_HPP
#define FORMAT_HPP

// Functions for formatting strings.

#include <cstdio>
#include <sstream>
#include <string>
#include <string_view>

template<typename T>
std::string to_hex_string(T const& t)
{
    std::stringstream ss;
    ss << std::hex << t;
    return ss.str();
}

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


template<typename P>
void ezcat_impl(std::ostringstream& ss, P const& postfix) {}

template<typename P, typename T, typename... Ts>
void ezcat_impl(std::ostringstream& ss, P const& postfix, T const& t, Ts const&... ts)
{
    ss << t << postfix;
    ezcat_impl(ss, postfix, ts...);
}

// Just combines the string representations together.
template<typename P, typename... Ts>
std::string ezcat(P const& postfix, Ts const&... ts)
{
    std::ostringstream ss;
    ezcat_impl(ss, postfix, ts...);
    return ss.str();
}

#endif
