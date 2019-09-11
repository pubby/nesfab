#ifndef PUBBY_ALLOCA_HPP
#define PUBBY_ALLOCA_HPP

// alloca can be platform specific.
// This header exists for portability reasons.

#include <cstddef>
#include <type_traits>

#ifdef _MSC_VER
// Note that MSVC's alloca can throw something called a structured exception.
// This is NOT a standard C++ exception. It's something weird and fucky.
// It's probably easiest to just ignore this fucky exception and let
// everything crash and burn on error.
# include <malloc.h>
# define alloca _alloca // have to use define because alloca is special.
#else
# include <alloca.h>
#endif

// Have to wrap the assert in a function call to use it in a comma expression.
template<typename T> 
void allocaT_trait_check()
{
    static_assert(std::is_trivially_destructible<T>::value);
}

// It's the sequel to alloca.
// Expects "number of elements" rather than "total size of elements".
#define ALLOCA_T(t, n) \
    (allocaT_trait_check<t>(), static_cast<t*>(alloca(n * sizeof(t))))

#endif
