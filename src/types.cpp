#include "types.hpp"

#include <algorithm>

#include "hash.hpp"

using namespace std::literals;

rh::robin_auto_table<type_t::map_elem_t> type_t::tail_map;
std::vector<type_t> type_t::tails;

unsigned type_t::get_tail_i(type_t const* begin, type_t const* end)
{
    if(end - begin == 0)
        throw std::runtime_error("type has too few arguments.");

    // Hash the type string.
    std::size_t hash = (end - begin);

    for(type_t const* it = begin; it < end; ++it)
    {
        hash = combine_hashes(hash, it->name);
        hash = combine_hashes(hash, it->tail_i);
    }

    rh::apair<map_elem_t*, bool> result = tail_map.emplace(
        hash,
        [begin, end](map_elem_t elem) -> bool
        {
            return (elem.size == end - begin
                    && std::equal(begin, end, &tails[elem.tail_i]));
        },
        [begin, end]() -> map_elem_t
        { 
            // Substring search to see if the type string is already in
            // the tails vector.
            auto it = std::search(tails.begin(), tails.end(), begin, end);
            if(it == tails.end())
            {
                // Create new.
                unsigned const index = tails.size();
                tails.insert(tails.end(), begin, end);
                return { end - begin, index };
            }
            else
                return { end - begin, it - tails.begin() };

        });

    return result.first->tail_i;
}

void type_t::clear_all()
{
    tail_map.clear();
    tails.clear();
}

type_t type_t::array(type_t elem_type, unsigned size)
{
    return { TYPE_ARRAY, size, get_tail_i(&elem_type, &elem_type + 1) };
}

type_t type_t::ptr(type_t pointed_to_type)
{
    return { TYPE_PTR, 1, get_tail_i(&pointed_to_type, &pointed_to_type + 1) };
}

type_t type_t::fn(type_t* begin, type_t* end)
{
    return { TYPE_FN, end - begin, get_tail_i(begin, end) };
}

std::string type_string(type_t type) 
{ 
    std::string str;

    switch(type.name)
    {
    default: 
        if(is_fixed(type.name))
        {
            std::string str("fixed"s);
            str.push_back(whole_bytes(type.name) + '0');
            str.push_back(frac_bytes(type.name) + '0');
            return str;
        }
        throw std::runtime_error("bad type");
    case TYPE_VOID:  return "void"s;
    case TYPE_BOOL:  return "bool"s;
    case TYPE_BYTE:  return "byte"s;
    case TYPE_SHORT: return "short"s;
    case TYPE_INT:   return "int"s;
    case TYPE_TABLE: // TODO
    case TYPE_ARRAY: // TODO
        throw std::runtime_error("TODO - bad type"s);
    case TYPE_PTR:
        return "%" + type_string(type[0]);
    case TYPE_FN:
        assert(type.size > 0);
        std::string str("fn("s);
        for(int i = 0; i < type.size; ++i)
        {
            if(i == type.size - 1)
                str += ") "s;
            else if(i != 0)
                str += ", "s;
            str += type_string(type[i]);
        }
        return str;
    }
}

std::ostream& operator<<(std::ostream& ostr, type_t const& type)
{
    ostr << type_string(type);
    return ostr;
}

cast_result_t can_cast(type_t const& from, type_t const& to)
{
    // Same types; no cast needed!
    if(from == to)
        return CAST_NOP;

    // Pointers can be casted to bool.
    if(from.name == TYPE_PTR && to == type_t{ TYPE_BOOL })
        return CAST_OP;

    // Otherwise you can't cast pointers.
    if(from.name == TYPE_PTR || to.name == TYPE_PTR)
        return CAST_FAIL;

    // Arithmetic types can be converted amongst each other.
    if(is_arithmetic(from.name) && is_arithmetic(to.name))
        return CAST_OP;

    return CAST_FAIL;
}

