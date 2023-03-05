#ifndef AST_HPP
#define AST_HPP

#include <cstdint>

#include "token.hpp"

struct mods_t;
class global_t;

struct ast_node_t
{
    using int_type = std::uint64_t; // Should match fixed_uint_t

    token_t token = {};
    union
    {
        ast_node_t* children = nullptr;
        mods_t* mods;
        global_t const* charmap;
        int_type uint;
    };

    unsigned num_children() const;
    void weaken_idents();
};

#endif
