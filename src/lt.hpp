#ifndef LT_HPP
#define LT_HPP

// LT = linktime

#include <vector>

#include "type.hpp"
#include "handle.hpp"
#include "parser_decl.hpp"
#include "ast.hpp"
#include "locator.hpp"
#include "rom_decl.hpp"
#include "rval.hpp"

struct lt_value_t
{
    static constexpr compiler_phase_t impl_deque_phase = PHASE_COMPILE;

    type_t type;
    ast_node_t ast;

    // If this was handled in 'rom_prune.hpp':
    bool prune_processed = false;

    struct result_t
    {
        rval_t rval;
        std::vector<locator_t> bytes;
    };

    // After linking, the resolved value:
    std::array<result_t, NUM_ROMV> results;
    bool resolved(romv_t romv) const { return results[romv].bytes.size(); }

    void resolve(romv_t romv);
    
    template<typename Fn>
    void for_each_locator(Fn const fn) const
    {
        for_each_locator(ast, fn);
    }

private:
    template<typename Fn>
    static void for_each_locator(ast_node_t const& ast, Fn const fn)
    {
        if(ast.token.type == lex::TOK_shift_atom)
        {
            fn(locator_t::from_uint(ast.uint));
            return;
        }

        unsigned const num_children = ast.num_children();
        for(unsigned i = 0; i < num_children; ++i)
        {
            ast_node_t const& child = ast.children[i];

            if(child.token.type == lex::TOK_rpair)
            {
                for(auto const& v : child.token.ptr<rpair_t>()->value)
                    if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
                        if(ssa->is_locator())
                            fn(ssa->locator());
            }
            else
                for_each_locator(child, fn);
        }
    }
};

lt_ht alloc_lt_value(type_t type, ast_node_t const& expr);

#endif
