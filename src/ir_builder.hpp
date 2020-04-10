#ifndef IR_BUILDER_HPP
#define IR_BUILDER_HPP

#include <exception>

class global_manager_t;
class global_t;
class ir_t;

struct local_lookup_error_t : public std::exception
{
    virtual const char* what() const noexcept
        { return "Failed local lookup."; }
};

ir_t build_ir(global_manager_t& global_manager, global_t& global);

#endif
