#ifndef FILE_CONVERT_HPP
#define FILE_CONVERT_HPP

#include <string>
#include <vector>

#include "pstring.hpp"
#include "locator.hpp"
#include "parser_decl.hpp"

std::vector<locator_t> convert_file(char const* source, pstring_t script, string_literal_t const& filename);

#endif
