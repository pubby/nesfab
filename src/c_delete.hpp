#ifndef C_DELETE_HPP
#define C_DELETE_HPP

#include <cstdlib>

struct c_delete 
{ 
    void operator()(void* ptr) noexcept { std::free(ptr); } 
};

#endif

