#ifndef INTERPRET_HPP
#define INTERPRET_HPP

#include <string>
#include <exception>
#include <vector>

class eval_t;
class type_t;
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

spair_t interpret_expr(pstring_t pstring, token_t const* expr, 
                       type_t expected_type, eval_t* env = nullptr);

#endif
