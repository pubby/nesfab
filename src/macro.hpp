#ifndef MACRO_HPP
#define MACRO_HPP

#include <string>
#include <vector>
#include <exception>

#include "pstring.hpp"

struct macro_error_t : public std::exception
{
    explicit macro_error_t(std::string const& msg, pstring_t pstring) 
    : msg(msg) 
    , pstring(pstring)
    {}

    virtual const char* what() const noexcept { return msg.c_str(); }
    std::string msg;
    pstring_t pstring;
};

std::string invoke_macro(unsigned file_i, std::vector<std::string> const& args);

#endif
