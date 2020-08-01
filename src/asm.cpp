#include "asm.hpp"

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
#define OP(name) case name: \
        return BOOST_PP_CAT(BOOST_PP_STRINGIZE(name),s);
#include "op.inc"
#undef OP
    default: return ""s;
    }
}
