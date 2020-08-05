#ifndef PARSER_TYPES_HPP
#define PARSER_TYPES_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "array_pool.hpp"
#include "fixed.hpp"
#include "lex_tables.hpp"
#include "pstring.hpp"
#include "types.hpp"

namespace bc = boost::container;

struct token_t
{
    using int_type = fixed_int_t;
    static_assert(sizeof(int_type) >= sizeof(std::uintptr_t));

    token_type_t type;
    pstring_t pstring;
    int_type value;

    void set_ptr(void* ptr) { value = reinterpret_cast<int_type>(ptr); }

    template<typename T>
    T* ptr() const { return reinterpret_cast<T*>(value); }

    // Used for debugging and logging.
    std::string to_string() const;
};

struct var_decl_t
{
    type_t type = TYPE_VOID;
    pstring_t name;
};

using expr_temp_t = bc::small_vector<token_t, 16>;

#endif
