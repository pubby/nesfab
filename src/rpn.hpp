#ifndef RPN_HPP
#define RPN_HPP

// Stack-based RPN utilities, used to translate expression ASTs.

#include <cassert>
#include <cstdint>

#include <boost/container/small_vector.hpp>

#include "fixed.hpp"
#include "type.hpp"
#include "type_mask.hpp"
#include "pstring.hpp"
#include "ir_edge.hpp"
#include "compiler_error.hpp"
#include "rval.hpp"

namespace bc = boost::container;

enum value_category_t : char
{
    RVAL, 
    LVAL_PTR, 
    FIRST_LVAL = LVAL_PTR,
    LVAL_ARRAY, 
    LVAL,
    LAST_LVAL = LVAL,
};

constexpr bool is_lval(value_category_t vc) { return vc >= FIRST_LVAL && vc <= LAST_LVAL; }


// Expressions are stored in RPN form.
// This struct is what the RPN stack holds.
struct rpn_value_t
{
    value_category_t category = RVAL;
    value_time_t time = CT; // When the value is computed

    // LVAL stuff.
    // (Only used for LVALs.)
    std::int8_t atom = -1; // negative means no atom.
    std::uint16_t member = 0;
    std::uint16_t var_i = std::uint16_t(~0u);
    ssa_value_t index = {};

    // RVAL stuff.
    // (Used for LVALs and RVALs.)
    // Each value will always keep an up-to-date version of its RVAL.
    rval_t rval;

    type_t type = TYPE_VOID;
    pstring_t pstring = {};

    //type_t derefed_from = TYPE_VOID;

public:
    fixed_t fixed() const;

    fixed_uint_t u() const { return fixed().value; }
    fixed_sint_t s() const { return static_cast<fixed_sint_t>(fixed().value); }
    fixed_uint_t whole() const { return u() >> fixed_t::shift; }
    fixed_sint_t swhole() const { return s() >> fixed_t::shift; }

    /*
    ssa_value_t& ssa(unsigned member = 0)
    {
        assert(member < rval().size());
        return std::get<ssa_value_t>(rval()[member]);
    }
    */

    ssa_value_t const& ssa(unsigned member = 0) const
        { return std::get<ssa_value_t>(rval[member]); }
    ssa_value_t& ssa(unsigned member = 0)
        { return std::get<ssa_value_t>(rval[member]); }

    ct_array_t ct_array(unsigned member = 0) const
        { return std::get<ct_array_t>(rval[member]); }

    bool is_ct() const { return time == CT && ::is_ct(rval); }
    bool is_lt() const { return time == LT || ::is_lt(rval); }
};

class rpn_stack_t
{
private:
    bc::small_vector<rpn_value_t, 32> stack;
public:
    void clear() { stack.clear(); }
    std::size_t size() const { return stack.size(); }

    // For when the stack has exactly 1 element, returns that element
    rpn_value_t& only1() { assert(stack.size() == 1); return stack[0]; }

    rpn_value_t& peek(int i) { assert(i < (int)stack.size()); return stack.rbegin()[i]; }

    void pop() { assert(!stack.empty()); stack.pop_back(); }
    void pop(unsigned i) { assert(i <= stack.size()); stack.resize(stack.size() - i); }

    void push(rpn_value_t const& value) { stack.push_back(value); }
    void push(rpn_value_t&& value) { stack.push_back(std::move(value)); }

    void tuck(rpn_value_t value, unsigned place) 
    { 
        assert(place <= stack.size());
        stack.insert(stack.end() - place, std::move(value));
    }

    rpn_value_t* past_top() { return &*stack.end(); }
};

#endif
