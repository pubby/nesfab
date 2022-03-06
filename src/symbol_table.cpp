#include "symbol_table.hpp"

#include "compiler_error.hpp"
#include "fnv1a.hpp"

unsigned const* symbol_table_t::new_def(unsigned handle, std::string_view name)
{
    assert(scope_stack.size() > 0);

    hash_type hash = fnv1a<hash_type>::hash(name);
    if(hash_counts[hash & table_mask] != 0)
    {
        for(auto i = scope_stack.back(); i != assoc_list.size(); ++i)
            if(assoc_list[i].hash == hash && assoc_list[i].name == name)
                return &assoc_list[i].handle;
    }
    assoc_list.push_back({ name, hash, handle });
    ++hash_counts[hash & table_mask];
    if(hash_counts[hash & table_mask] == 0)
        throw std::runtime_error("Symbol table overflow.");

    return nullptr;
}

unsigned const* symbol_table_t::find(std::string_view name) const
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
