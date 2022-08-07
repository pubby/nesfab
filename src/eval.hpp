#ifndef INTERPRET_HPP
#define INTERPRET_HPP

#include <string>
#include <exception>
#include <vector>

#include <boost/container/small_vector.hpp>

#include "flat/small_map.hpp"

#include "bitset.hpp"
#include "pstring.hpp"
#include "globals.hpp"

namespace bc = ::boost::container;

class ir_t;
class fn_t;
class eval_t;
class type_t;
class locator_t;
struct spair_t;
struct pstring_t;
struct token_t;

// Thrown when the interpreter takes too much time to complete
struct out_of_time_t : public std::exception
{
    explicit out_of_time_t(std::string const& msg)
    : msg(msg) {}

    virtual const char* what() const noexcept { return msg.c_str(); }
    std::string msg;
};

struct var_lookup_error_t : public std::exception
{
    virtual const char* what() const noexcept { return "Failed var lookup."; }
};

struct fn_not_rt_t : public std::exception
{
    explicit fn_not_rt_t(pstring_t pstring) : pstring(pstring) {}

    virtual const char* what() const noexcept { return "Function is not defined for run-time."; }
    pstring_t pstring;
};

spair_t interpret_expr(pstring_t pstring, token_t const* expr, 
                       type_t expected_type, eval_t* env = nullptr);

std::vector<locator_t> interpret_paa(pstring_t pstring, token_t const* expr);

precheck_tracked_t build_tracked(fn_t const& fn);

void build_ir(ir_t& ir, fn_t const& fn);

#endif
