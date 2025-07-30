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
#include "thread.hpp"
#include "rom_decl.hpp"

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

    public:
        T const* get(T const* begin, T const* end)
        {
            if(end - begin == 0)
                return nullptr;

            // Hash the range.

            std::size_t size = end - begin;
            std::size_t hash = size;

            assert(size < 65536);

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
                    return { size, eternal_new<T>(begin, end) };
                });

            assert(std::equal(begin, end, result.first->tail));

            return result.first->tail;
        }

        T const* get(T const& t) { return get(&t, &t+1); }
    };

    TLS tails_manager_t<type_t> type_tails;
    TLS tails_manager_t<group_ht> group_tails;
} // end anonymous namespace

type_t const* type_t::new_type(type_t const& type) 
{ 
    type_t const* result = type_tails.get(type);
    assert(type.name() == result->name());
    assert(type == *result);
    return result;
}

bool type_t::operator==(type_t o) const
{
    if(m_name != o.m_name || m_unsized != o.m_unsized || m_size != o.m_size)
        return false;

    if(has_type_tail(name()))
        return std::equal(types(), types() + type_tail_size(), o.types());
    else if(has_group_tail(name()))
        return std::equal(groups(), groups() + group_tail_size(), o.groups());
    else if(has_tail(name()))
        return m_tail == o.m_tail;

    return true;
}

group_ht type_t::group(unsigned i) const { return groups() ? groups()[i] : group_ht{}; }

type_t type_t::paa( group_ht group)
{ 
    type_t type(TYPE_PAA, 0, group_tails.get(group)); 
    type.m_unsized = true;
    return type;
}

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

type_t type_t::paa_thunk(pstring_t pstring, ast_node_t const& ast, group_ht group)
{
    return type_t(TYPE_PAA_THUNK, 0, eternal_emplace<paa_thunk_t>(pstring, ast, group));
}

type_t type_t::tea(type_t elem_type)
{ 
    assert(is_thunk(elem_type.name()) || !has_tea(elem_type));
    type_t type(TYPE_TEA, 0, type_tails.get(elem_type));
    type.m_unsized = true;
    return type;
}

type_t type_t::tea(type_t elem_type, unsigned size)
{ 
    passert(is_thunk(elem_type.name()) || !has_tea(elem_type), elem_type);
    return type_t(TYPE_TEA, size, type_tails.get(elem_type));
}

type_t type_t::tea(type_t elem_type, std::int64_t size, pstring_t pstring)
{ 
    if(!is_thunk(elem_type) && has_tea(elem_type))
        compiler_error(pstring, "Arrays cannot be multi-dimensional");
    type_t ret = type_t::tea(elem_type, 0);
    ret.set_array_length(size, pstring);
    return ret;
}

type_t type_t::tea_thunk(pstring_t pstring, type_t elem_type, ast_node_t const& ast)
{
    return type_t(TYPE_TEA_THUNK, 0, eternal_emplace<tea_thunk_t>(pstring, ast, elem_type));
}

type_t type_t::ptr(group_ht group, type_name_t tn)
{ 
    if(group)
        return ptr(&group, &group + 1, tn); 
    else
        return ptr(nullptr, nullptr, tn); 
}

type_t type_t::ptr(group_ht const* begin, group_ht const* end, type_name_t tn)
{
    assert(is_ptr(tn));
    type_t t = group_set(begin, end);
    t.unsafe_set_name(tn);
    return t;
}

type_t type_t::group_set(group_ht const* begin, group_ht const* end)
{
    std::size_t const n = end - begin;
    group_ht* groups = ALLOCA_T(group_ht, n);
    std::copy(begin, end, groups);
    std::sort(groups, groups + n);
    group_ht* groups_end = std::unique(groups, groups + n);
    return type_t(TYPE_GROUP_SET, groups_end - groups, group_tails.get(groups, groups_end));
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

type_t type_t::addr(bool banked)
{
    return type_t(banked ? TYPE_BANKED_APTR : TYPE_APTR, 0, nullptr);
}

type_t type_t::vec(type_t elem_type)
{
    return type_t(TYPE_VEC, 0, type_tails.get(elem_type));
}

type_t type_t::fn_ptr(fn_set_t const& fn_set)
{
    return type_t(TYPE_FN_PTR, 0, &fn_set);
}

void type_t::set_banked(bool banked)
{
    assert(is_ptr(name()));
    unsafe_set_name(::with_banked(name(), banked));
}

type_t type_t::with_banked(bool banked) const
{
    type_t copy = *this;
    copy.set_banked(banked);
    return copy;
}

std::size_t type_t::size_of() const
{
    if(is_arithmetic(name()))
    {
        assert(!is_ptr(name()));
        return whole_bytes(name()) + frac_bytes(name());
    }

    switch(name())
    {
    default: 
        if(is_ptr(name()))
        {
            if(is_banked_ptr(name()))
                return 3;
            else
                return 2;
        }
        // fall-through
    case TYPE_VOID:
    case TYPE_VEC:
        return 0; // Error!
    case TYPE_TEA: 
        return size() * types()[0].size_of();
    case TYPE_PAA: 
        return size();
    case TYPE_FN_PTR:
        return fn_set().size_of();
    case TYPE_STRUCT:
        std::size_t size = 0;
        for(unsigned i = 0; i < struct_().fields().size(); ++i)
            size += struct_().field(i).type().size_of();
        return size;
    }
}

std::size_t type_t::size_of_bits() const
{
    if(name() == TYPE_BOOL)
        return 1;
    return size_of() * 8;
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
    m_unsized = false;
}

void type_t::set_array_length(std::int64_t size, pstring_t pstring)
{
    assert(name() == TYPE_TEA || name() == TYPE_PAA);
    if(size < 0 
       || (name() == TYPE_TEA && size > 65536)
       || (name() == TYPE_PAA && size > 65536))
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
            hash = rh::hash_combine(hash, group(i).id);

    return hash;
}

std::string to_string(type_t type) 
{ 
    std::string str;

    switch(type.name())
    {
    default: 
        str = to_string(type.name()); 
        break;
    case TYPE_STRUCT_THUNK:
        str = fmt("STRUCT THUNK % %", type.global().name, &type.global());
        break;
    case TYPE_STRUCT:
        str = type.struct_().global.name;
        break;
    case TYPE_TEA:
        str = fmt("%[%]", to_string(type.elem_type()), type.unsized() ? "" : std::to_string(type.size()));
        break;
    case TYPE_PAA:
        str = fmt("[%]%", type.unsized() ? "" : std::to_string(type.size()),
                  type.group() ? type.group()->name : "");
        break;
    case TYPE_VEC:
        str = fmt("%{}", to_string(type.elem_type()));
        break;
    case TYPE_BANKED_APTR: str = "AAA"; goto ptr_groups;
    case TYPE_APTR:        str = "AA";  goto ptr_groups;
    case TYPE_BANKED_MPTR: str = "MMM"; goto ptr_groups;
    case TYPE_MPTR:        str = "MM";  goto ptr_groups;
    case TYPE_BANKED_CPTR: str = "CCC"; goto ptr_groups;
    case TYPE_CPTR:        str = "CC";  goto ptr_groups;
    case TYPE_BANKED_PPTR: str = "PPP"; goto ptr_groups;
    case TYPE_PPTR:        str = "PP";  goto ptr_groups;
    ptr_groups:
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
    case TYPE_FN_PTR:
        str = "Fn."sv;
        str += type.fn_set().global.name;
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

static bool can_cast_groups(type_t const& from, type_t const& to)
{
    assert(is_group_ptr(from.name()));
    assert(is_group_ptr(to.name()));
    assert(from.size() == from.group_tail_size());
    assert(to.size() == to.group_tail_size());

    unsigned from_i = 0;
    unsigned to_i = 0;

    while(from_i < from.size())
    {
        while(true)
        {
            if(to_i == to.size())
                return false;
            if(from.group(from_i) == to.group(to_i))
                break;
            ++to_i;
        }
        ++from_i;
    }
    return true;
}

cast_result_t can_cast(type_t const& from, type_t const& to, bool implicit)
{
    passert(!is_thunk(from.name()) && !is_thunk(to.name()), from, " : ", to);

    // Same types; no cast needed!
    if(from == to)
        return CAST_NOP;

    // PAAs should be converted to ptrs, prior.
    if(from.name() == TYPE_PAA || to.name() == TYPE_PAA)
        return CAST_FAIL;

    if(from.name() == TYPE_TEA && to.name() == TYPE_VEC && from.elem_type() == to.elem_type())
        return CAST_VECIFY_TEA;

    if(from.name() == TYPE_VEC && to.name() == TYPE_TEA && from.elem_type() == to.elem_type())
        return CAST_TEAIFY_VEC;

    // Ptrs can convert to ints.
    if((is_ptr(from.name()) || is_aptr(from.name())) && is_arithmetic(to.name()))
    {
        assert(!is_ptr(to.name()));
        if(to.name() == TYPE_BOOL)
            return CAST_BOOLIFY;
        if(!implicit)
            return CAST_INTIFY_PTR;
    }

    // Any ptr can convert to an aptr.
    if(is_ptr(from.name(), true) && is_aptr(to.name()) 
       && (is_banked_ptr_or_fn_ptr(from) || !is_banked_ptr(to.name())))
    {
        return CAST_NOP_RETYPE;
    }

    // Likewise, aptr can convert to any pointer.
    if(!implicit && is_aptr(from.name()) && is_ptr(to.name(), true) 
       && (is_banked_ptr(from.name()) || !is_banked_ptr_or_fn_ptr(to)))
    {
        return CAST_NOP_RETYPE;
    }

    // Ptrs can strip their bank:
    if(!implicit && is_banked_ptr(from.name()) && is_ptr(to.name()) && !is_banked_ptr(to.name()))
    {
        cast_result_t result = can_cast(from.with_banked(false), to, implicit);
        if(result == CAST_NOP || result == CAST_NOP_RETYPE)
            return CAST_NOP_RETYPE;
    }

    // Ptrs can add a dummy bank:
    if(is_ptr(from.name()) && !is_banked_ptr(from.name()) 
       && (to.name() == TYPE_BANKED_PPTR || to == from.with_banked(true)))
    {
        cast_result_t result = can_cast(from.with_banked(true), to, implicit);
        if(result == CAST_NOP || result == CAST_NOP_RETYPE)
            return CAST_DUMMY_BANK;
    }

    // Ints can convert to ptrs
    if(!implicit && is_arithmetic(from.name()) && is_ptr(to.name()))
        return CAST_PTRIFY_INT;

    // Any int can convert to AA.
    if(is_arithmetic(from.name()) 
       && to.name() == TYPE_APTR
       && (from.name() == TYPE_INT || whole_bytes(from.name()) <= 2)
       && frac_bytes(from.name()) == 0)
    {
        return CAST_PTRIFY_INT;
    }

    // Pointers can generalize
    // i.e. ram{foo} can convert to ram{foo, bar}
    // Likewise, mptrs convert to cptrs
    if(is_group_ptr(from.name()) && is_group_ptr(to.name()) 
       && is_banked_ptr(from.name()) == is_banked_ptr(to.name())
       && (::with_banked(from.name()) == ::with_banked(to.name()) || is_pptr(to.name()))
       && can_cast_groups(from, to))
    {
        return CAST_NOP_RETYPE;
    }

    // Otherwise you can't cast different pointers.
    if(is_ptr(from.name()) || is_ptr(to.name()))
        return CAST_FAIL;

    // Otherwise arithmetic types can be converted to bool using "!= 0".
    if(is_arithmetic(from.name()) && to == TYPE_BOOL)
        return CAST_BOOLIFY;

    // Fn pointers too:
    if(from.name() == TYPE_FN_PTR && to == TYPE_BOOL)
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

    // Otherwise arithmetic types can be converted amongst each other:
    if(is_arithmetic(from.name()) && is_arithmetic(to.name()))
    {
        if(is_arithmetic_subset(from.name(), to.name()))
            return CAST_PROMOTE;
        else
            return implicit ? CAST_FAIL : CAST_TRUNCATE;
    }

    if(!implicit && is_tea(from.name()) && is_tea(to.name()))
    {
        if(from.elem_type() == to.elem_type())
            return CAST_RESIZE_TEA;
    }

    return CAST_FAIL;
}

bool is_ct(type_t type, bool include_vec)
{
    switch(type.name())
    {
    default:
        return is_ct(type.name(), include_vec);
    case TYPE_TEA:
        return is_ct(type.elem_type(), include_vec);
    case TYPE_STRUCT:
        for(auto const& pair : type.struct_().fields())
            if(is_ct(pair.second.type(), include_vec))
                return true;
        return false;
    case TYPE_FN:
        for(unsigned i = 0; i < type.size(); ++i)
            if(is_ct(type.type(i), include_vec))
                return true;
        return false;
    }
}

bool is_thunk(type_t type)
{
    switch(type.name())
    {
    default:
        return is_thunk(type.name());
    case TYPE_TEA:
        return is_thunk(type.elem_type());
    case TYPE_STRUCT:
        for(auto const& pair : type.struct_().fields())
            if(is_thunk(pair.second.type()))
                return true;
        return false;
    case TYPE_FN:
        for(unsigned i = 0; i < type.size(); ++i)
            if(is_thunk(type.type(i)))
                return true;
        return false;
    }
}

bool is_banked_ptr_or_fn_ptr(type_t type)
{
    if(type.name() == TYPE_FN_PTR)
        return type.fn_set().banked_ptrs();
    return is_banked_ptr(type.name());
}

unsigned num_members(type_t type, bool early)
{
    passert(type.name() != TYPE_STRUCT_THUNK, type, type.global().name);
    unsigned ret = 0;
    if(type.name() == TYPE_STRUCT)
        ret = type.struct_().num_members();
    else if(is_tea(type.name()))
        ret = num_members(type.elem_type());
    else if(is_banked_ptr(type.name()))
        ret = 2; // The bank is a member
    else if(type.name() == TYPE_FN_PTR)
        return type.fn_set().num_members();
    else if(type.name() == TYPE_VOID)
        return 0;
    else
        ret = 1;

    return ret;
}

unsigned num_atoms(type_t type, unsigned member)
{
    assert(!is_thunk(type.name()));

    switch(type.name())
    {
    case TYPE_STRUCT: 
        assert(false); // TODO
        return 0;
    case TYPE_TEA:
        return num_atoms(type.elem_type(), member);
    case TYPE_PAA: 
        assert(member == 0);
        return 1;
    case TYPE_FN_PTR:
        {
            fn_set_t const& fn_set = type.fn_set();
            if(fn_set.banked_ptrs() && (member & 1))
                return 1; // The banks
            return 2;
        }
    default: 
        if(is_ptr(type.name()))
        {
            if(is_banked_ptr(type.name()))
                return member == 0 ? 2 : 1;
            return 2;
        }
        passert(is_scalar(type.name()), type);
        passert(member == 0, member, type);
        return type.size_of();
    }
}

unsigned num_offsets(type_t type)
{
    assert(!is_thunk(type.name()));

    switch(type.name())
    {
    case TYPE_TEA: 
    case TYPE_PAA: 
        return type.array_length();
    default:
        return 1;
    }
}

unsigned member_offset(type_t type, unsigned member)
{
    assert(type.name() != TYPE_STRUCT_THUNK);

    switch(type.name())
    {
    case TYPE_STRUCT: 
        return type.struct_().member_offset(member);
    case TYPE_TEA: 
    case TYPE_TEA_THUNK: 
    case TYPE_VEC: 
        return member_offset(type.elem_type(), member) * type.array_length();
    case TYPE_FN_PTR:
        {
            fn_set_t const& fn_set = type.fn_set();
            if(fn_set.banked_ptrs())
                return (member >> 1) * 3 + (member & 1) * 2;
            return member * 2;
        }
    default: 
        if(is_banked_ptr(type.name()))
            return member == 1 ? 2 : 0;
        assert(member == 0);
        return 0;
    }
}

unsigned member_index(type_t const& type, unsigned member)
{
    assert(type.name() != TYPE_STRUCT_THUNK);

    switch(type.name())
    {
    case TYPE_STRUCT: 
        return type.struct_().member(member);
    case TYPE_TEA: 
    case TYPE_TEA_THUNK: 
    case TYPE_VEC: 
        return member_index(type.elem_type(), member);
    default: 
        if(is_banked_ptr(type.name()))
        {
            assert(member < 2);
            return member;
        }
        passert(member == 0, type, member);
        return 0;
    }
}

type_t member_type(type_t const& type, unsigned member)
{
    passert(::num_members(type) == 0 || member < ::num_members(type), member, type, ::num_members(type));

    if(type.name() == TYPE_STRUCT)
        return type.struct_().member_type(member);
    else if(type.name() == TYPE_TEA)
    {
        type_t mt = member_type(type.elem_type(), member);
        passert(!is_banked_ptr(mt.name()), type, member, mt);
        passert(!has_tea(mt), mt, type);
        return type_t::tea(mt, type.size());
    }
    else if(is_banked_ptr(type.name()))
    {
        if(member == 0)
            return type.with_banked(false);
        else
        {
            assert(member == 1);
            return TYPE_U;
        }
    }
    else if(type.name() == TYPE_FN_PTR)
    {
        assert(member < 2);
        if(member == 1)
            return TYPE_U; // The bank
        return type_t::addr(false); // The ptrs
    }
    return type;
}

type_t strip_array(type_t const& type)
{
    if(type.name() == TYPE_TEA || type.name() == TYPE_VEC)
        return type.elem_type();
    return type;
}

type_t unstrip_array(type_t const& type, type_t const& replace)
{
    if(type.name() == TYPE_TEA)
        return type_t::tea(replace, type.array_length());
    if(type.name() == TYPE_VEC)
        return type_t::vec(replace);
    return replace;
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

bool ptr_to_vars(type_t const& type)
{
    return !is_cptr(type.name());
}

type_t dethunkify(src_type_t src_type, bool full, eval_t* env, local_const_t const* local_consts)
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
            type_t const elem_type = dethunkify({ src_type.pstring, thunk.elem_type }, full, env, local_consts);
            if(has_tea(elem_type))
                compiler_error(src_type.pstring, "Arrays cannot be multi-dimensional.");

            if(full)
            {
                rpair_t const result = interpret_expr(thunk.pstring, thunk.expr, TYPE_INT, env, local_consts);
                assert(result.value.size());
                if(calc_time(result.type, result.value) >= LT)
                    compiler_error(thunk.pstring, "Unable to determine array size at compile-time.");
                auto size = std::get<ssa_value_t>(result.value[0]).signed_whole();

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
                rpair_t const result = interpret_expr(thunk.pstring, thunk.expr, TYPE_INT, env, local_consts);
                assert(result.value.size());
                if(calc_time(result.type, result.value) >= LT)
                    compiler_error(thunk.pstring, "Unable to determine array size at compile-time.");
                auto size = std::get<ssa_value_t>(result.value[0]).signed_whole();

                return type_t::paa(size, thunk.group, src_type.pstring);
            }
            else
                return t;
        }

    case TYPE_TEA:
        {
            type_t const elem = dethunkify({ src_type.pstring, t.elem_type() }, full, env, local_consts);
            if(has_tea(elem))
                compiler_error(src_type.pstring, "Arrays cannot be multi-dimensional.");
            return t.unsized() ? type_t::tea(elem) : type_t::tea(elem, t.size());
        }

    case TYPE_VEC:
        {
            type_t const elem = dethunkify({ src_type.pstring, t.elem_type() }, full, env, local_consts);
            return type_t::vec(elem);
        }

    case TYPE_FN:
        {
            type_t* args = ALLOCA_T(type_t, t.size());
            for(unsigned i = 0; i < t.size(); ++i)
                args[i] = dethunkify({ src_type.pstring, t.type(i) }, full, env, local_consts);
            return type_t::fn(args, args + t.size());
        }

    default:
        return t;
    }
}

