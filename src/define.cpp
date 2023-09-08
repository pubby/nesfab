#include "define.hpp"

#include "eternal_new.hpp"
#include "globals.hpp"
#include "group.hpp"

const_ht define_const(lpstring_t at, std::string_view name, asm_proc_t&& proc, 
                      defined_group_data_t const& d, bool omni, mod_flags_t flags)
{
    using namespace lex;

    std::unique_ptr<mods_t> mods;
    if(flags)
        mods = std::make_unique<mods_t>(flags);

    ast_node_t* sub_proc = eternal_emplace<ast_node_t>(ast_node_t{
        .token = token_t::make_ptr(TOK_byte_block_sub_proc, at, 
                                   eternal_emplace<asm_proc_t>(std::move(proc))),
        .children = nullptr,
    });

    ast_node_t* expr = eternal_emplace<ast_node_t>(ast_node_t{
        .token = { .type = TOK_byte_block_proc, .pstring = at, .value = 1 },
        .children = sub_proc,
    });

    auto paa_def = std::make_unique<paa_def_t>();

    global_t& global = global_t::lookup_sourceless(at, name);
    const_ht gconst = global.define_const(
        at, {}, { at, type_t::paa(0, d.group ? d.group->handle() : group_ht{}) }, d, omni,
        expr, std::move(paa_def), std::move(mods));

    assert(gconst);
    return gconst;
}

const_ht define_ct(lpstring_t at, std::string_view name, std::uint8_t value)
{
    global_t& global = global_t::lookup_sourceless(at, name);
    return define_ct(global, at, value);
}

const_ht define_ct(global_t& global, lpstring_t at, std::uint8_t value)
{
    return define_ct_int(global, at, TYPE_U, value);
}

const_ht define_ct_int(global_t& global, lpstring_t at, type_t const& type, unsigned value)
{
    using namespace lex;

    ast_node_t* expr = eternal_emplace<ast_node_t>(ast_node_t{
        .token = { .type = TOK_int, .pstring = at, .value = fixed_uint_t(value) << fixed_t::shift },
    });

    const_ht gconst = global.define_const(
        at, {}, { at, type }, {}, false, expr, {}, {});

    assert(gconst);
    return gconst;
}

template<typename T>
const_ht define_ct_array(global_t& global, lpstring_t at, T const* data, std::size_t length, type_t const& elem_type)
{
    using namespace lex;

    type_t const type = type_t::tea(elem_type, length);

    std::vector<ast_node_t> children;
    children.reserve(length + 1);

    children.push_back({ 
        .token = token_t::make_ptr(TOK_cast_type, at, type_t::new_type(type)) 
    });

    for(std::size_t i = 0; i < length; ++i)
    {
        children.push_back({
            .token = { .type = TOK_int, .pstring = at, .value = fixed_uint_t(data[i]) << fixed_t::shift },
        });
    }

    ast_node_t* expr = eternal_emplace<ast_node_t>(ast_node_t{
        .token = { .type = TOK_cast, .pstring = at, .value = children.size() },
    });

    expr->children = eternal_new<ast_node_t>(&*children.begin(), &*children.end());

    const_ht gconst = global.define_const(
        at, {}, { at, type }, {}, false, expr, {}, {});

    assert(gconst);
    return gconst;
}

const_ht define_ct(global_t& global, lpstring_t at, std::uint8_t const* data, std::size_t length)
{
    return define_ct_array(global, at, data, length, TYPE_U);
}

const_ht define_ct(global_t& global, lpstring_t at, std::int16_t const* data, std::size_t length)
{
    return define_ct_array(global, at, data, length, TYPE_S20);
}
