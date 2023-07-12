#ifndef DEFINE_HPP
#define DEFINE_HPP

#include <string_view>

#include "decl.hpp"
#include "pstring.hpp"
#include "asm_proc.hpp"
#include "mods.hpp"

const_ht define_const(pstring_t at, std::string_view name, asm_proc_t&& proc, 
                      defined_group_data_t const& d, bool omni, mod_flags_t flags);

const_ht define_ct(pstring_t at, std::string_view name, std::uint8_t value);
const_ht define_ct(global_t& global, pstring_t at, std::uint8_t value);
const_ht define_ct_int(global_t& global, pstring_t at, type_t const& type, unsigned value);

const_ht define_ct(global_t& global, pstring_t at, std::uint8_t const* data, std::size_t length);
const_ht define_ct(global_t& global, pstring_t at, std::int16_t const* data, std::size_t length);

#endif
