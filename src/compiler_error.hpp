#ifndef COMPILER_ERROR_HPP
#define COMPILER_ERROR_HPP

#include <exception>
#include <string>

#include "format.hpp"
#include "pstring.hpp"
#include "console.hpp"

class compiler_error_t : public std::runtime_error
{
public:
    explicit compiler_error_t(char const* what, bool warning = false) 
    : std::runtime_error(what)
    , warning(warning)
    {}

    explicit compiler_error_t(std::string const& what, bool warning = false) 
    : std::runtime_error(what)
    , warning(warning)
    {}

    bool warning = false;
};

std::string fmt_source_pos(file_contents_t const& file, pstring_t pstring);
std::string fmt_source_pos(std::string const& filename, char const* source, pstring_t pstring);

std::string fmt_error(std::string const& what);
std::string fmt_error(pstring_t pstring, std::string const& what, 
                      file_contents_t const* file = nullptr, 
                      char const* color = CONSOLE_RED CONSOLE_BOLD, char const* prefix = "error");

std::string fmt_note(std::string const& what);
std::string fmt_note(pstring_t pstring, std::string const& what,
                     file_contents_t const* file = nullptr);

std::string fmt_warning(pstring_t pstring, std::string const& what,
                     file_contents_t const* file = nullptr);

[[gnu::noreturn]] 
void compiler_error(pstring_t pstring, std::string const& what, 
                    file_contents_t const* file = nullptr);

void compiler_warning(pstring_t pstring, std::string const& what, 
                      file_contents_t const* file = nullptr);

void compiler_warning(std::string const& what, bool formatted = false);

#endif
