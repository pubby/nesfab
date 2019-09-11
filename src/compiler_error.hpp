#ifndef COMPILER_ERROR_HPP
#define COMPILER_ERROR_HPP

#include <exception>
#include <string>

#include "format.hpp"
#include "pstring.hpp"

class compiler_error_t : public std::runtime_error
{
public:
    explicit compiler_error_t(char const* what) 
    : std::runtime_error(what) {}

    explicit compiler_error_t(std::string const& what) 
    : std::runtime_error(what) {}
};

std::string format_source_pos(pstring_t pstring);

std::string format_error_message(pstring_t pstring, std::string const& what);

[[gnu::noreturn]] 
void compiler_error(pstring_t pstring, std::string const& what);

#endif
