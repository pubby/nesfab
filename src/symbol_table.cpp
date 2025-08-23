#include "symbol_table.hpp"

#include "compiler_error.hpp"
#include "fnv1a.hpp"

auto symbol_table_t::new_def(handle_type handle, std::string_view name) -> handle_type const*
{
    assert(scope_stack.size() > 0);

    hash_type hash = fnv1a<hash_type>::hash(name);
    if(hash_counts[hash & table_mask] != 0)
        for(auto i = scope_stack.back(); i != assoc_list.size(); ++i)
            if(assoc_list[i].hash == hash && assoc_list[i].name == name)
                return &assoc_list[i].handle;
    assoc_list.push_back({ name, hash, handle });
    ++hash_counts[hash & table_mask];
    if(hash_counts[hash & table_mask] == 0)
        throw std::runtime_error("Symbol table overflow.");

    return nullptr;
}

auto symbol_table_t::find(std::string_view name) const -> handle_type const*
{
    if(scope_stack.size() <= 0)
        return nullptr;

    // Early exit if the table doesn't hold the hash.
    hash_type hash = fnv1a<hash_type>::hash(name);
    if(hash_counts[hash & table_mask] == 0)
        return nullptr;

    // Linear search backwards through 'assoc_list' to find a match.
    for(auto it = assoc_list.rbegin(); it != assoc_list.rend(); ++it)
        if(it->hash == hash && it->name == name)
            return &it->handle;

    return nullptr;
}

void symbol_table_t::pop_scope()
{
    assert(scope_stack.size() > 0);

    std::size_t const old_size = assoc_list.size();
    std::size_t const new_size = scope_stack.back();
    scope_stack.pop_back();
    for(std::size_t i = new_size; i != old_size; ++i)
    {
        assert(hash_counts[assoc_list[i].hash & table_mask] > 0);
        --hash_counts[assoc_list[i].hash & table_mask];
    }
    assoc_list.resize(new_size);
}

void symbol_table_t::clear()
{
    assoc_list.clear();
    scope_stack.clear();
    hash_counts = {};
}

int anonymous_table_t::get_absolute(pstring_t at, unsigned scope, int index) const
{
    unsigned s = scope;
    std::printf("%i %i\n", s, index);

    while(index < 0)
    {
        if(s == 0)
            compiler_error(at, "Anonymous label does not exist.");

        index += scopes[s].position;
        s = scopes[s].parent;
    }

    while(index >= scopes[s].defs.size())
    {
        if(s == 0)
            compiler_error(at, "Anonymous label does not exist.");

        index -= scopes[s].defs.size();
        index += scopes[s].position;
        s = scopes[s].parent;
    }

    assert(index >= 0);
    std::printf("new: %i %i\n", s, index);
    return scopes[s].defs[index];
}
