#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include <array>
#include <cassert>
#include <cstdint>
#include <string_view>
#include <vector>

#include <boost/container/small_vector.hpp>

namespace bc = boost::container;

struct pstring_t;

// Implements local-variable symbol tables as an association list.
class symbol_table_t
{
public:
    using handle_type = int;
    using hash_type = std::uint32_t;

    // Adds a local var to the table with handle 'handle'.
    // If the var already exists, return a pointer to its handle.
    // Otherwise, return nullptr.
    handle_type const* new_def(handle_type handle, std::string_view name);

    // Looks up a local var using 'name'.
    // Returns a pointer to the handle if found.
    handle_type const* find(std::string_view name) const;

    void push_scope() { scope_stack.push_back(assoc_list.size()); }
    void pop_scope();

    void clear();
    bool empty() const { return scope_stack.empty(); }
    std::size_t size() const { return scope_stack.size(); }

private:
    symbol_table_t const* const_this() const { return this; }

    struct storage_t
    {
        std::string_view name;
        hash_type hash;
        handle_type handle;
    };

    bc::small_vector<storage_t, 32> assoc_list;

    // Holds indexes into assoc_list for each stack frame.
    bc::small_vector<unsigned, 16> scope_stack;
    
    // Assoc lists are slow to check if elements are NOT in them.
    // To speed things up, count which hashes are contained in the assoc list
    // using a fixed-size hash table. 
    // A count of zero for an element's hash implies it's not in the container.
    static constexpr std::size_t table_size = 1024; // Must be power of 2.
    static constexpr hash_type table_mask = table_size - 1;
    std::array<unsigned char, table_size> hash_counts = {};
};

// Used for anonymous labels.
class anonymous_table_t
{
public:
    int new_def(int v) 
    { 
        auto& defs = scopes[scope].defs;
        defs.push_back(v); 
        return defs.size(); 
    }

    // Returns the value in 'scope' at position 'index':
    int get_absolute(pstring_t at, unsigned scope, int index) const;

    void push_scope() 
    { 
        scopes.push_back({ .parent = scope, .position = scopes.empty() ? 0 : current_position() });
        scope = scopes.size() - 1;
    }

    void pop_scope()
    {
        scope = scopes[scope].parent;
    }

    int current_scope() const { return scope; }
    int current_position() const { return scopes[scope].defs.size(); }

    void clear() { scopes.clear(); scope = 0; }
    bool empty() const { return scopes.empty(); }
    std::size_t size() const { return scopes.size(); }

private:
    struct scope_t
    {
        unsigned parent;
        int position; 
        std::vector<int> defs;
    };

    unsigned scope = 0;
    std::vector<scope_t> scopes;
};

#endif
