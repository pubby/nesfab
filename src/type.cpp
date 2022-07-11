#include "type.hpp"

#include <algorithm>
#include <string>

#include "robin/hash.hpp"
#include "robin/collection.hpp"

#include "alloca.hpp"
#include "array_pool.hpp"
#include "compiler_error.hpp"
#include "fixed.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "pstring.hpp"
#include "eval.hpp"
#include "eternal_new.hpp"

using namespace std::literals;

namespace  // Anonymous
{
    // Implementation detail!
    // This takes a range of types and returns a pointer to allocated memory
    // that contains the same data.
    // The point being, it's faster to pass a pointer around than the actual range.
    template<typename T>
    class tails_manager_t
    {
        struct map_elem_t
        {
            std::uint16_t size;
            T const* tail;
        };

        rh::robin_auto_table<map_elem_t> map;
        array_pool_t<T> tails;

    public:
        T const* get(T const* begin, T const* end)
        {
            if(end - begin == 0)
                return nullptr;

            // Hash the range.

            std::size_t size = end - begin;
            std::size_t hash = size;

            for(T const* it = begin; it < end; ++it)
            {
                std::hash<T> hasher;
                hash = rh::hash_combine(hash, hasher(*it));
            }

            // Now insert into the map:

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

            assert(std::equal(begin, end, result.first->tail));

            return result.first->tail;
        }

        T const* get(T const& t) { return get(&t, &t+1); }
    };

    thread_local tails_manager_t<type_t> type_tails;
    thread_local tails_manager_t<group_ht> group_tails;
} // end anonymous namespace

type_t const* type_t::new_type(type_t const& type) { return type_tails.get(type); }

bool type_t::operator==(type_t o) const
{
    if(m_name != o.m_name || m_size != o.m_size)
        return false;

    if(has_type_tail(name()))
        return std::equal(types(), types() + type_tail_size(), o.types());
    else if(has_group_tail(name()))
        return std::equal(groups(), groups() + group_tail_size(), o.groups());

    return true;
}

group_ht type_t::group(unsigned i) const { return groups()[i]; }

type_t type_t::paa(unsigned size, group_ht group)
{ 
    return type_t(TYPE_PAA, size, group_tails.get(group)); 
}

type_t type_t::paa(std::int64_t size, group_ht group, pstring_t pstring)
{ 
    type_t ret = type_t::paa(0, group);
    ret.set_array_length(size, pstring);
    return ret;
}

type_t type_t::paa_thunk(pstring_t pstring, token_t const* tokens, group_ht group)
{
    return type_t(TYPE_PAA_THUNK, 0, eternal_emplace<paa_thunk_t>(pstring, tokens, group));
}

type_t type_t::tea(type_t elem_type, unsigned size)
{ 
    assert(is_thunk(elem_type.name()) || !has_tea(elem_type));
    return type_t(TYPE_TEA, size, type_tails.get(elem_type));
}

type_t type_t::tea(type_t elem_type, std::int64_t size, pstring_t pstring)
{ 
    type_t ret = type_t::tea(elem_type, 0);
    ret.set_array_length(size, pstring);
    return ret;
}

type_t type_t::tea_thunk(pstring_t pstring, type_t elem_type, token_t const* tokens)
{
    return type_t(TYPE_TEA_THUNK, 0, eternal_emplace<tea_thunk_t>(pstring, elem_type, tokens));
}

type_t type_t::ptr(group_ht group, bool banked) { return ptr(&group, &group + 1, banked); }

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

type_t type_t::struct_thunk(global_t const& global)
{
    return type_t(TYPE_STRUCT_THUNK, 0, &global);
}

type_t type_t::struct_(struct_t const& s)
{
    return type_t(TYPE_STRUCT, 0, &s);
}

std::size_t type_t::size_of() const
{
    if(is_arithmetic(name()))
        return whole_bytes(name()) + frac_bytes(name());

    switch(name())
    {
    default:                return 0; // Error!
    case TYPE_PTR:          return 2;
    case TYPE_BANKED_PTR:   return 3;
    case TYPE_TEA:          return size() * types()[0].size_of();
    case TYPE_PAA:          return size();
    case TYPE_STRUCT:
        std::size_t size = 0;
        for(unsigned i = 0; i < struct_().fields().size(); ++i)
            size += struct_().field(i).type().size_of();
        return size;
    }
}

std::size_t type_t::array_length() const
{
    if(name() == TYPE_TEA || name() == TYPE_PAA)
        return size();
    return 0;
}

void type_t::set_array_length(std::size_t size)
{
    assert(name() == TYPE_TEA || name() == TYPE_PAA);
    m_size = size;
}

void type_t::set_array_length(std::int64_t size, pstring_t pstring)
{
    assert(name() == TYPE_TEA || name() == TYPE_PAA);
    if(size <= 0 
       || (name() == TYPE_TEA && size > 256)
       || (name() == TYPE_PAA && size > 65535))
    {
        compiler_error(pstring, fmt("Invalid array length of %.", size));
    }
    set_array_length(size);
}

std::size_t type_t::hash() const
{
    std::size_t hash = name();
    hash = rh::hash_combine(hash, size());

    if(has_type_tail(name()))
        for(unsigned i = 0; i < type_tail_size(); ++i)
            hash = rh::hash_combine(hash, type(i).hash());
    else if(has_group_tail(name()))
        for(unsigned i = 0; i < group_tail_size(); ++i)
            hash = rh::hash_combine(hash, group(i).value);

    return hash;
}

std::string to_string(type_t type) 
{ 
    std::string str;

    switch(type.name())
    {
    default: 
        str = to_string(type.name()); break;
    case TYPE_STRUCT:
        str = type.struct_().global.name;
        break;
    case TYPE_TEA:
        str = fmt("%[%]", to_string(type.elem_type()), type.size() ? std::to_string(type.size()) : "");
        break;
    case TYPE_PAA:
        str = fmt("[%]", type.size() ? std::to_string(type.size()) : "");
        break;
    case TYPE_BANKED_PTR:
        str = "P";
        // fall-through
    case TYPE_PTR:
        str = "PP";
        for(unsigned i = 0; i < type.size(); ++i)
            str += type.group(i)->name;
        break;
    case TYPE_FN:
        assert(type.size() > 0);
        str = "fn("sv;
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

bool can_size_unsized_array(type_t const& sized, type_t const& unsized)
{
    return (unsized.is_unsized_array()
            && sized.name() == unsized.name()
            && sized.elem_type() == unsized.elem_type());
}

cast_result_t can_cast(type_t const& from, type_t const& to, bool implicit)
{
    assert(!is_thunk(from.name()) && !is_thunk(to.name()));

    // Buffers should be converted to ptrs, prior.
    assert(from.name() != TYPE_PAA && to.name() != TYPE_PAA);

    // Same types; no cast needed!
    if(from == to)
        return CAST_NOP;

    if(!implicit && is_ptr(from.name()) && is_arithmetic(to.name()) && !is_ct(to.name()))
        return CAST_INTIFY_PTR;

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

    // Otherwise you can't cast different pointers.
    if(is_ptr(from.name()) || is_ptr(to.name()))
        return CAST_FAIL;

    // Othewise arithmetic types can be converted to bool using "!= 0".
    if(is_arithmetic(from.name()) && to == TYPE_BOOL)
        return CAST_BOOLIFY;

    // Otherwise Reals have special casting rules:
    if(from.name() == TYPE_REAL)
    {
        if(implicit && frac_bytes(to.name()) == 0)
            return CAST_FAIL; // Can't implicitly convert to non-fixed point.
        else if(is_arithmetic(to.name()))
            return CAST_ROUND_REAL; // Reals implement rounding.
    }

    // Otherwise Ints have special casting rules:
    if(from.name() == TYPE_INT && is_arithmetic(to.name()))
        return CAST_CONVERT_INT;

    // Otherwise arithmetic types can be converted amongst each other,
    if(is_arithmetic(from.name()) && is_arithmetic(to.name()))
    {
        if(is_arithmetic_subset(from.name(), to.name()))
            return CAST_PROMOTE;
        else
            return implicit ? CAST_FAIL : CAST_TRUNCATE;
    }

    return CAST_FAIL;
}

/* TODO
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
*/

bool is_ct(type_t type)
{
    switch(type.name())
    {
    case TYPE_REAL:
    case TYPE_INT:
        return true;
    case TYPE_TEA:
        return is_ct(type.elem_type());
    case TYPE_STRUCT:
        for(auto const& pair : type.struct_().fields())
            if(is_ct(pair.second.type()))
                return true;
        return false;
    case TYPE_FN:
        for(unsigned i = 0; i < type.size(); ++i)
            if(is_ct(type.type(i)))
                return true;
        return false;
    default:
        return false;
    }
}

/* TODO: remove?
unsigned calc_num_members(type_t type)
{
    assert(!is_thunk(type.name()));

    if(type.name() == TYPE_STRUCT)
    {
        unsigned count = 0; 
        for(auto const& pair : type.struct_().record().fields())
            count += num_members(pair.second.type());
        return count;
    }
    else if(type.name() == TYPE_TEA)
        return calc_num_members(type.elem_type());
    return 1;
}
*/

unsigned num_members(type_t type)
{
    assert(type.name() != TYPE_STRUCT_THUNK);
    if(type.name() == TYPE_STRUCT)
        return type.struct_().num_members();
    else if(is_tea(type.name()))
        return num_members(type.elem_type());
    return 1;
}

unsigned num_atoms(type_t type)
{
    assert(!is_thunk(type.name()));

    switch(type.name())
    {
    case TYPE_STRUCT: assert(false); // TODO
    case TYPE_TEA: return num_atoms(type.elem_type());
    case TYPE_PAA: return 1;
    case TYPE_PTR: return 1;
    case TYPE_BANKED_PTR: return 2;
    default: 
        assert(is_scalar(type.name()));
        return type.size_of();
    }
}

unsigned num_offsets(type_t type, unsigned atom)
{
    assert(!is_thunk(type.name()));

    switch(type.name())
    {
    case TYPE_TEA: 
    case TYPE_PAA: 
        assert(atom == 0);
        return type.array_length();
    case TYPE_BANKED_PTR:
        if(atom == 1)
            return 1;
        // fall through
    case TYPE_PTR:
        if(atom == 0)
            return 2;
        assert(false); // Invalid atom.
        // fall through
    default:
        return 1;
    }
}

unsigned member_index(type_t const& type, unsigned i)
{
    assert(type.name() != TYPE_STRUCT_THUNK);

    switch(type.name())
    {
    case TYPE_STRUCT: 
        return type.struct_().member(i);
    case TYPE_TEA: 
    case TYPE_TEA_THUNK: 
        return member_index(type.elem_type(), i);
    default: 
        return 0;
    }
}

type_t member_type(type_t const& type, unsigned i)
{
    assert(i < num_members(type));
    if(type.name() == TYPE_STRUCT)
        return type.struct_().member_type(i);
    else if(type.name() == TYPE_TEA)
    {
        type_t mt = member_type(type.elem_type(), i);
        assert(!is_aggregate(mt.name()));
        return type_t::tea(mt, type.size());
    }
    return type;
}

type_t strip_array(type_t const& type)
{
    if(type.name() == TYPE_TEA)
        return type.elem_type();
    return type;
}

bool has_tea(type_t const& type)
{
    assert(type.name() != TYPE_STRUCT_THUNK);

    switch(type.name())
    {
    case TYPE_STRUCT: return type.struct_().has_tea_member();
    case TYPE_TEA_THUNK:
    case TYPE_TEA: return true;
    default: return false;
    }
}

type_t dethunkify(src_type_t src_type, bool full, eval_t* env)
{
    type_t& t = src_type.type;

    assert(compiler_phase() > PHASE_PARSE);
    switch(t.name())
    {
    case TYPE_STRUCT_THUNK:
        if(t.global().gclass() != GLOBAL_STRUCT)
            throw std::runtime_error(fmt("%: Expected struct type.", t.global().name));
        return type_t::struct_(t.global().impl<struct_t>());

    case TYPE_TEA_THUNK:
        {
            tea_thunk_t const& thunk = t.tea_thunk();
            type_t const elem_type = dethunkify({ thunk.elem_type, src_type.pstring }, full, env);

            if(full)
            {
                spair_t const result = interpret_expr(thunk.pstring, thunk.expr, TYPE_INT, env);
                assert(result.value.size());
                if(std::holds_alternative<expr_vec_t>(result.value[0]))
                    compiler_error(thunk.pstring, "Unable to determine array size at compile-time.");
                auto size = std::get<ssa_value_t>(result.value[0]).signed_whole();

                if(has_tea(elem_type))
                    compiler_error(thunk.pstring, "Arrays cannot be multidimensional.");

                return type_t::tea(elem_type, size, src_type.pstring);
            }
            else
                return type_t::tea_thunk(thunk.pstring, elem_type, thunk.expr);
        }

    case TYPE_PAA_THUNK:
        {
            paa_thunk_t const& thunk = t.paa_thunk();

            if(full)
            {
                spair_t const result = interpret_expr(thunk.pstring, thunk.expr, TYPE_INT, env);
                assert(result.value.size());
                if(std::holds_alternative<expr_vec_t>(result.value[0]))
                    compiler_error(thunk.pstring, "Unable to determine array size at compile-time.");
                auto size = std::get<ssa_value_t>(result.value[0]).signed_whole();

                return type_t::paa(size, thunk.group, src_type.pstring);
            }
            else
                return t;
        }

    case TYPE_TEA:
        {
            type_t const elem = dethunkify({ t.elem_type(), src_type.pstring }, full, env);
            if(has_tea(elem))
                compiler_error(src_type.pstring, "Arrays cannot be multi-dimensional.");
            return type_t::tea(elem, t.size());
        }

    case TYPE_FN:
        {
            type_t* args = ALLOCA_T(type_t, t.size());
            for(unsigned i = 0; i < t.size(); ++i)
                args[i] = dethunkify({ t.type(i), src_type.pstring }, full, env);
            return type_t::fn(args, args + t.size());
        }

    default:
        return t;
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

