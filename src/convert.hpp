#ifndef CONVERT_HPP
#define CONVERT_HPP

#include <exception>
#include <string>
#include <vector>
#include <variant>

#include "pstring.hpp"
#include "locator.hpp"
#include "parser_decl.hpp"
#include "ir_edge.hpp"
#include "asm_proc.hpp"

struct mods_t;

struct convert_arg_t
{
    using variant_t = std::variant<bool, std::uint64_t, string_literal_t, pstring_t>;

    variant_t value;
    pstring_t pstring;

    string_literal_t filename() const;
};

class convert_error_t : public std::runtime_error
{
public:
    explicit convert_error_t(char const* what) : std::runtime_error(what) {}
    explicit convert_error_t(std::string const& what) : std::runtime_error(what) {}
};

struct conversion_named_values_t
{
    char const* name;
    ssa_value_t value;
};

struct conversion_t
{
    std::variant<std::vector<std::uint8_t>, std::vector<locator_t>, asm_proc_t> data;
    bc::small_vector<conversion_named_values_t, 2> named_values;
};

conversion_t convert_file(char const* source, pstring_t script, fs::path preferred_dir, 
                          string_literal_t const& filename, mods_t const* mods,
                          convert_arg_t* args, std::size_t argn);

template<typename T>
struct convert_u8_impl_t 
{ static T call(std::uint8_t u) { return T(u); } };

template<>
struct convert_u8_impl_t<locator_t>
{ static locator_t call(std::uint8_t u) { return locator_t::const_byte(u); } };

// Allows writing conversion functions in a slightly generic way.
template<typename T>
T convert_u8(std::uint8_t u) { return convert_u8_impl_t<T>::call(u); }

#endif
