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

std::string fmt_source_pos(file_contents_t const& file, pstring_t pstring);

std::string fmt_error(file_contents_t const& file, pstring_t pstring, 
                      std::string const& what);
std::string fmt_error(file_contents_t const& file, pstring_t pstring,
                      std::string const& what, char const* color, char const* prefix);

[[gnu::noreturn]] 
void compiler_error(file_contents_t const& file, pstring_t pstring, 
                    std::string const& what);

void compiler_warning(file_contents_t const& file, pstring_t pstring, std::string const& what);
void compiler_warning(pstring_t pstring, std::string const& what);
void compiler_warning(std::string const& what);

#endif
