#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include <array>
#include <cassert>
#include <cstdint>
#include <string_view>
#include <vector>

#include <boost/container/small_vector.hpp>

namespace bc = boost::container;

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

#endif
