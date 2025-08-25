#include "asm.hpp"

#include <ostream>

#include <boost/preprocessor/stringize.hpp>
#include <boost/preprocessor/cat.hpp>

std::string to_string(addr_mode_t addr_mode)
{
    using namespace std::string_literals;
    switch(addr_mode)
    {
#define ADDR_MODE(name) case MODE_##name: \
        return BOOST_PP_CAT(BOOST_PP_STRINGIZE(name),s);
#include "addr_mode.inc"
#undef ADDR_MODE
    default: return ""s;
    }
}

std::string to_string(op_name_t name)
{
    using namespace std::string_literals;
    switch(name)
    {
#define OP_NAME(name) case name: \
        return BOOST_PP_CAT(BOOST_PP_STRINGIZE(name),s);
#include "op_name.inc"
#undef OP_NAME
    default: return ""s;
    }
}

std::string to_string(op_t op)
{
    using namespace std::string_literals;
    switch(op)
    {
#define OP(name, flags) case name: \
        return BOOST_PP_CAT(BOOST_PP_STRINGIZE(name),s);
#include "op.inc"
#undef OP
    default: 
        if(op >= BEGIN_REG_READ_OP && op < END_REG_READ_OP)
            return "REG_READ"s;
        if(op >= BEGIN_REG_WRITE_OP && op < END_REG_WRITE_OP)
            return "REG_WRITE"s;
        return ""s;
    }
}

std::ostream& operator<<(std::ostream& os, addr_mode_t addr_mode)
{
    os << to_string(addr_mode);
    return os;
}

std::ostream& operator<<(std::ostream& os, op_name_t name)
{
    os << to_string(name);
    return os;
}

std::ostream& operator<<(std::ostream& os, op_t op)
{
    os << to_string(op);
    return os;
}
