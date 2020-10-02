#include "types.hpp"

#include <algorithm>

#include "robin/hash.hpp"

using namespace std::literals;

array_pool_t<type_t> type_t::tails;

type_t const* type_t::get_tail(type_t const& type)
{
    return get_tail(&type, &type + 1);
}

type_t const* type_t::get_tail(type_t const* begin, type_t const* end)
{
    if(end - begin == 0)
        throw std::runtime_error("type has too few arguments.");

    // Hash the type string.
    std::size_t size = end - begin;
    std::size_t hash = size;

    for(type_t const* it = begin; it < end; ++it)
    {
        std::hash<type_t const*> tail_hasher;
        hash = rh::hash_combine(hash, it->name());
        hash = rh::hash_combine(hash, tail_hasher(it->tail()));
    }

    std::lock_guard<std::mutex> const lock(tail_mutex);

    rh::apair<map_elem_t*, bool> result = tail_map.emplace(
        hash,
        [begin, end, size](map_elem_t elem) -> bool
        {
            return (elem.size == size && std::equal(begin, end, elem.tail));
        },
        [begin, end, size]() -> map_elem_t
        { 
            return { size, tails.insert(begin, end) };
        });

    return result.first->tail;
}

void type_t::clear_all()
{
    std::lock_guard<std::mutex> const lock(tail_mutex);
    tail_map.clear();
    tails.clear();
}

type_t type_t::array(type_t elem_type, unsigned size)
{
    return type_t(TYPE_ARRAY, size, get_tail(elem_type));
}

type_t type_t::ptr(type_t pointed_to_type)
{
    return type_t(TYPE_PTR, 1, get_tail(pointed_to_type));
}

type_t type_t::fn(type_t* begin, type_t* end)
{
    return type_t(TYPE_FN, end - begin, get_tail(begin, end));
}

std::size_t type_t::size_of() const
{
    if(is_arithmetic(name()))
        return whole_bytes(name()) + frac_bytes(name());

    switch(name())
    {
    default: assert(false); return 0;
    case TYPE_PTR:   return 2;
    case TYPE_ARRAY: return size() * m_tail[0].size_of();
    }
}

std::string to_string(type_t type) 
{ 
    std::string str;

    switch(type.name())
    {
    default: 
        if(frac_bytes(type.name()) > 0)
        {
            std::string str("fixed"s);
            str.push_back(whole_bytes(type.name()) + '0');
            str.push_back(frac_bytes(type.name()) + '0');
            return str;
        }
        throw std::runtime_error("bad type");
    case TYPE_VOID:  return "void"s;
    case TYPE_BOOL:  return "bool"s;
    case TYPE_CARRY: return "carry"s;
    case TYPE_BYTE:  return "byte"s;
    case TYPE_SHORT: return "short"s;
    case TYPE_INT:   return "int"s;
    case TYPE_ARRAY: // TODO
    case TYPE_BUFFER: // TODO
        throw std::runtime_error("TODO - unimplemented type"s);
    case TYPE_PTR:
        return "%" + to_string(type[0]);
    case TYPE_FN:
        assert(type.size() > 0);
        std::string str("fn("s);
        for(unsigned i = 0; i < type.size(); ++i)
        {
            if(i == type.size() - 1)
                str += ") "s;
            else if(i != 0)
                str += ", "s;
            str += to_string(type[i]);
        }
        return str;
    }
}

std::ostream& operator<<(std::ostream& ostr, type_t const& type)
{
    ostr << to_string(type);
    return ostr;
}

cast_result_t can_cast(type_t const& from, type_t const& to)
{
    // Same types; no cast needed!
    if(from == to)
        return CAST_NOP;

    // Othewise arithmetic types can be converted to bool using "!= 0".
    if(is_arithmetic(from) && to == TYPE_BOOL)
        return CAST_BOOLIFY;

    // Otherwise you can't cast different pointers.
    if(from == TYPE_PTR || to == TYPE_PTR)
        return CAST_FAIL;

    // Otherwise arithmetic types can be converted amongst each other.
    if(is_arithmetic(from) && is_arithmetic(to))
        return CAST_OP;

    return CAST_FAIL;
}

/////////////
// STRUCTS //
/////////////

type_t arg_struct(type_t fn_type)
{
    assert(fn_type.name() == TYPE_FN);
    assert(fn_type.size() >= 1);
    fn_type.m_name = TYPE_STRUCT;
    fn_type.m_size -= 1;
    return fn_type;
}

std::size_t struct_size(type_t type)
{
    if(type.name() != TYPE_STRUCT)
        return 1;
    std::size_t size = 0;
    for(type_t const& sub : type)
        size += struct_size(sub);
    return size;
}

static type_t const* _struct_index(type_t const& type, unsigned& i)
{
    if(type.name() != TYPE_STRUCT)
    {
        if(i == 0)
            return &type;
        --i;
        return nullptr;
    }

    for(type_t const& sub : type)
        if(type_t const* found = _struct_index(sub, i))
            return found;

    return nullptr;
}

type_t struct_index(type_t type, unsigned i)
{
    type_t const* result = _struct_index(type, i);
    assert(result);
    return *result;
}

static void _struct_fill(type_t type, type_t*& vec)
{
    if(type.name() != TYPE_STRUCT)
        *(vec++) = type;
    else for(type_t const& sub : type)
        _struct_fill(sub, vec);
}

void struct_fill(type_t type, type_t* vec)
{
    struct_fill(type, vec);
}
