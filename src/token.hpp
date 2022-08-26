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

    lex::token_type_t type = {};
    //bool is_rval = false; TODO
    pstring_t pstring = {};
    int_type value = {};

    std::int64_t signed_() const { return static_cast<std::int64_t>(value); }

    void set_ptr(void const* ptr) { value = reinterpret_cast<int_type>(ptr); }

    template<typename T>
    T* ptr() const { return reinterpret_cast<T*>(value); }

    template<typename T>
    static token_t make_ptr(lex::token_type_t type, pstring_t pstring, T const* ptr)
    {
        token_t token{ .type = type, .pstring = pstring };
        token.set_ptr(ptr);
        return token;
    }

    // Used for debugging and logging.
    std::string to_string(char const* source) const;
};

constexpr bool is_operator(lex::token_type_t type)
    { return type > lex::TOK_lparen && type < lex::TOK_rparen; }

constexpr bool operator_right_assoc(lex::token_type_t type)
    { return lex::token_right_assoc_table[type] & 0x80; }

constexpr int operator_precedence(lex::token_type_t type)
    { return lex::token_precedence_table[type] & 0x7F; }

constexpr bool is_type_prefix(lex::token_type_t type)
    { return (type >= lex::TOK_Void && type <= lex::TOK_Bool) || type == lex::TOK_type_ident || type == lex::TOK_lbracket; }

constexpr bool is_ident(lex::token_type_t type)
    { return type == lex::TOK_ident || type == lex::TOK_type_ident; }


// TODO: move this
enum value_time_t : char
{
    CT,
    LT,
    RT,
};

#endif
