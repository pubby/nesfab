#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <cstdint>
#include <string>

#include <boost/container/small_vector.hpp>

#include "lex_tables.hpp"
#include "pstring.hpp"

namespace bc = boost::container;

struct token_t
{
    using int_type = std::uint64_t; // Should match fixed_uint_t
    static_assert(sizeof(int_type) >= sizeof(std::uintptr_t));

    token_type_t type;
    pstring_t pstring;
    int_type value;

    void set_ptr(void const* ptr) { value = reinterpret_cast<int_type>(ptr); }

    template<typename T>
    T* ptr() const { return reinterpret_cast<T*>(value); }

    template<typename T>
    static token_t make_ptr(token_type_t type, pstring_t pstring, T const* ptr)
    {
        token_t token{ type, pstring };
        token.set_ptr(ptr);
        return token;
    }

    // Used for debugging and logging.
    std::string to_string(char const* source) const;
};

#endif
