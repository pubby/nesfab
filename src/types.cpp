#include "types.hpp"

#include <algorithm>

#include "robin/hash.hpp"

#include "alloca.hpp"
#include "fixed.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"

using namespace std::literals;

tails_manager_t<type_t> type_t::type_tails;
tails_manager_t<group_ht> type_t::group_tails;

bool type_t::operator==(type_t o) const
{
    if(m_name != o.m_name || m_size != o.m_size)
        return false;

    if(has_type_tail(name()))
        return std::equal(types(), types() + size(), o.types());
    else if(has_group_tail(name()))
        return std::equal(groups(), groups() + size(), o.groups());

    return true;
}

group_ht type_t::group(unsigned i) const { return groups()[i]; }

type_t type_t::buffer(unsigned size)
{ 
    return type_t(TYPE_BUFFER, size); 
}

type_t type_t::array(type_t elem_type, unsigned size)
{ 
    return type_t(TYPE_ARRAY, size, type_tails.get(elem_type));
}

type_t type_t::ptr(group_ht const* begin, group_ht const* end, bool banked)
{
    std::size_t const n = end - begin;
    group_ht* groups = ALLOCA_T(group_ht, n);
    std::copy(begin, end, groups);
    std::sort(groups, groups + n);
    group_ht* groups_end = std::unique(groups, groups + n);
    return type_t(banked ? TYPE_BANKED_PTR : TYPE_PTR, 
                  groups_end - groups, 
                  group_tails.get(groups, groups_end));
}

type_t type_t::fn(type_t* begin, type_t* end)
{ 
    return type_t(TYPE_FN, end - begin, type_tails.get(begin, end)); 
}

std::size_t type_t::size_of() const
{
    if(is_arithmetic(name()))
        return whole_bytes(name()) + frac_bytes(name());

    switch(name())
    {
    default: assert(false); return 0;
    case TYPE_PTR:          return 2;
    case TYPE_BANKED_PTR:   return 3;
    case TYPE_ARRAY: return size() * types()[0].size_of();
    }
}

std::size_t type_t::hash() const
{
    std::size_t hash = name();
    hash = rh::hash_combine(hash, size());

    if(has_type_tail(name()))
        for(unsigned i = 0; i < size(); ++i)
            hash = rh::hash_combine(hash, type(i).hash());
    else if(has_group_tail(name()))
        for(unsigned i = 0; i < size(); ++i)
            hash = rh::hash_combine(hash, group(i).value);

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
    case TYPE_BANKED_PTR:
        str += "P";
        // fall-through
    case TYPE_PTR:
        str += "PP";
        for(unsigned i = 0; i < type.size(); ++i)
            str += type.group(i)->name;
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
            str += to_string(type.type(i));
        }
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
    /* TODO
    if(from.name() == TYPE_RAM_PTR && to.name() == TYPE_RAM_PTR)
    {
        if((from.group_bitset() & to.group_bitset()) == from.group_bitset())
            return CAST_NOP;
        return CAST_FAIL;
    }
    */

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

type_name_t smallest_representable(fixed_t fixed)
{
    if(!fixed)
        return TYPE_BYTE;

    int const min = builtin::ctz(fixed.value) / 8;
    int const max = builtin::rclz(fixed.value) / 8;

    int const whole = std::max(max - 3, 1);
    int const frac  = std::max(3 - min, 0);

    return TYPE_arithmetic(whole, frac);
}

unsigned num_fields(type_t type)
{
    switch(type.name())
    {
    case TYPE_ARRAY: return type.elem_type().size_of();
    case TYPE_PTR: return 1;
    case TYPE_BANKED_PTR: return 2;
    default: return type.size_of();
    }
}

/////////////
// STRUCTS //
/////////////

/* TODO
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
*/

