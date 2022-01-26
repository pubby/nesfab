#include "types.hpp"

#include <algorithm>

#include "robin/hash.hpp"

#include "fixed.hpp"
#include "format.hpp"
#include "globals.hpp"

using namespace std::literals;

type_tails_manager_t type_t::type_tails;

vbank_ht type_t::vbank() const { assert(has_vbank(name())); return { m_impl.vbank }; }

type_t type_t::buffer(unsigned size)
{ 
    return type_t(TYPE_BUFFER, size, {}); 
}

type_t type_t::ram_ptr(group_bitset_t group_bitset)
{ 
    return type_t(TYPE_RAM_PTR, 0, { .group_bitset = group_bitset }); 
}

type_t type_t::rom_ptr(vbank_ht vbank)
{ 
    return type_t(TYPE_ROM_PTR, 0, { .vbank = vbank.value });
}

type_t type_t::array(type_t elem_type, unsigned size)
{ 
    return type_t(TYPE_ARRAY, size, { .tail = type_tails.get(elem_type) });
}

type_t type_t::fn(type_t* begin, type_t* end)
{ 
    return type_t(TYPE_FN, end - begin, { .tail = type_tails.get(begin, end) }); 
}

std::size_t type_t::size_of() const
{
    if(is_arithmetic(name()))
        return whole_bytes(name()) + frac_bytes(name());

    switch(name())
    {
    // TODO: Some rom ptrs are only 2 bytes?
    default: assert(false); return 0;
    case TYPE_RAM_PTR:   return 2;
    case TYPE_ROM_PTR:   return 3;
    case TYPE_FN:        return 3;
    case TYPE_ARRAY: return size() * types()[0].size_of();
    }
}

std::size_t type_t::hash() const
{
    std::size_t hash = name();
    hash = rh::hash_combine(hash, size());

    if(has_tail(name()))
        for(unsigned i = 0; i < size(); ++i)
            hash = rh::hash_combine(hash, types()[i].hash());
    else if(has_vbank(name()))
        hash = rh::hash_combine(hash, vbank().value);
    else if(has_group_bitset(name()))
        hash = rh::hash_combine(hash, group_bitset());

    return hash;
}

std::string to_string(type_t type) 
{ 
    std::string str;

    switch(type.name())
    {
    default: 
        if(frac_bytes(type.name()) > 0)
        {
            str += "fixed"sv;
            str.push_back(whole_bytes(type.name()) + '0');
            str.push_back(frac_bytes(type.name()) + '0');
            break;
        }
        throw std::runtime_error("bad type");
    case TYPE_VOID:  str += "void"sv;  break;
    case TYPE_BOOL:  str += "bool"sv;  break;
    case TYPE_CARRY: str += "carry"sv; break;
    case TYPE_BYTE:  str += "byte"sv;  break;
    case TYPE_SHORT: str += "short"sv; break;
    case TYPE_INT:   str += "int"sv;   break;
    case TYPE_ARRAY:
        str = fmt("%[%]", to_string(type.elem_type()), type.size());
        break;
    case TYPE_BUFFER:
        str += fmt("buffer[%]", type.size());
        break;
    case TYPE_RAM_PTR:
        str += fmt("ram{%}", type.group_bitset());
        break;
    case TYPE_ROM_PTR:
        str += fmt("rom{%}", type.vbank().value);
        break;
    case TYPE_FN:
        assert(type.size() > 0);
        str += "fn("sv;
        for(unsigned i = 0; i < type.size(); ++i)
        {
            if(i == type.size() - 1)
                str += ") "sv;
            else if(i != 0)
                str += ", "sv;
            str += to_string(type[i]);
        }
        if(type.vbank())
            str += fmt("{%}", type.vbank().value);
        else
            str += "{}"sv;
        break;
    }

    return str;
}

std::ostream& operator<<(std::ostream& ostr, type_t const& type)
{
    ostr << to_string(type);
    return ostr;
}

cast_result_t can_cast(type_t const& from, type_t const& to)
{
    // Buffers should be converted to ptrs, prior.
    assert(from.name() != TYPE_BUFFER && to.name() != TYPE_BUFFER);

    // Same types; no cast needed!
    if(from == to)
        return CAST_NOP;

    /* TODO: remove
    // Buffers can convert to pointers.
    // TODO: buffers should convert earlier, not as cast
    if(from.name() == TYPE_BUFFER)
    {
        if(from_ram && to.name() == TYPE_RAM_PTR)
        {
            if((1ull << ramb.value)  & to.ramb_bitset())
                return CAST_ADDROF;
            return CAST_FAIL;
        }
        else if(!from_ram && to.name() == TYPE_RAM_PTR)
        {
            if(bank == to.bank())
                return CAST_ADDROF;
            return CAST_FAIL;
        }
        return CAST_FAIL;
    }
    */

    // RAM pointers can generalize
    // i.e. ram{foo} can convert to ram{foo, bar}
    if(from.name() == TYPE_RAM_PTR && to.name() == TYPE_RAM_PTR)
    {
        if((from.group_bitset() & to.group_bitset()) == from.group_bitset())
            return CAST_NOP;
        return CAST_FAIL;
    }

    // Othewise arithmetic types can be converted to bool using "!= 0".
    if(is_arithmetic(from) && to == TYPE_BOOL)
        return CAST_BOOLIFY;

    // Otherwise you can't cast different pointers.
    if(is_ptr(from) || is_ptr(to))
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

type_name_t smallest_representable(fixed_t fixed)
{
    type_name_t name = TYPE_INT;

    if(!fixed)
        return TYPE_BYTE;

    int const min = builtin::ctz(fixed.value) / 8;
    int const max = builtin::rclz(fixed.value) / 8;

    int const whole = std::max(max - 3, 1);
    int const frac  = std::max(3 - min, 0);

    return TYPE_arithmetic(whole, frac);
}

//////////////////////////
// type_tails_manager_t //
//////////////////////////

type_t const* type_tails_manager_t::get(type_t const* begin, type_t const* end)
{
    assert(end - begin != 0);

    // Hash the range.

    std::size_t size = end - begin;
    std::size_t hash = size;

    for(type_t const* it = begin; it < end; ++it)
    {
        std::hash<type_t> hasher;
        hash = rh::hash_combine(hash, hasher(*it));
    }

    // Now insert into the map:

    std::lock_guard<std::mutex> const lock(mutex);

    rh::apair<map_elem_t*, bool> result = map.emplace(
        hash,
        [begin, end, size](map_elem_t elem) -> bool
        {
            return (elem.size == size && std::equal(begin, end, elem.tail));
        },
        [this, begin, end, size]() -> map_elem_t
        { 
            return { size, tails.insert(begin, end) };
        });

    return result.first->tail;
}

void type_tails_manager_t::clear()
{
    std::lock_guard<std::mutex> const lock(mutex);
    map.clear();
    tails.clear();
}
