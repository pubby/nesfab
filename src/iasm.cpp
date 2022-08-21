/* TODO: remove
#include "iasm.hpp"

#include "compiler_error.hpp"

void iasm_def_t::clear()
{
    m_idents.clear();
    m_labels.clear();
    m_values.clear();
    m_code.clear();
    m_code_expr.clear();
}

unsigned iasm_def_t::push_ident(std::string const& str, pstring_t name)
{
    auto const result = m_idents.insert({ str, name });

    if(!result.second)
    {
        throw compiler_error_t(
            fmt_error(name, fmt("Identifier % already in use.", str))
            + fmt_error(result.first->second, "Previous definition here:"));
    }

    return result.first - m_idents.begin();
}

unsigned iasm_def_t::push_label(pstring_t name, char const* source)
{
    std::string str(name.view(source));
    unsigned const ret = push_ident(str, name);
    m_labels.insert({ std::move(str), m_code.size() });
    return ret;
}

unsigned iasm_def_t::push_value(pstring_t name, char const* source, token_t const* expr)
{
    std::string str(name.view(source));
    unsigned const ret = push_ident(str, name);
    m_values.insert({ std::move(str), expr });
    return ret;
}

void iasm_def_t::push_op(op_t op, token_t const* expr)
{
    m_code.push_back({ op });
    m_code_expr.push_back(expr);
}
*/

