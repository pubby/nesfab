#ifndef PUBBY_ALLOCA_HPP
#define PUBBY_ALLOCA_HPP

// alloca can be platform specific.
// This header exists for portability reasons.

#include <cstddef>
#include <type_traits>
#include <algorithm>

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__MINGW64__)
// Note that MSVC's alloca can throw something called a structured exception.
// This is NOT a standard C++ exception. It's something weird and fucky.
// It's probably easiest to just ignore this fucky exception and let
// everything crash and burn on error.
#  include <malloc.h>
#  if defined(_MSC_VER)
#    define alloca _alloca // have to use define because alloca is special.
#  endif
#else
#  include <alloca.h>
#endif

// Have to wrap the assert in a function call to use it in a comma expression.
template<typename T> 
void allocaT_trait_check()
{
    static_assert(std::is_trivially_destructible<T>::value);
}

template<typename T>
T* callocaT_impl(T* ptr, std::size_t n, T const& fill) { std::fill(ptr, ptr+n, fill); return ptr; }

// It's the sequel to alloca.
// Expects "number of elements" rather than "total size of elements".
#define ALLOCA_T(t, n) \
    (allocaT_trait_check<t>(), static_cast<t*>(alloca((n) * sizeof(t))))
#define CALLOCA_T(t, n) \
    (allocaT_trait_check<t>(), callocaT_impl(static_cast<t*>(alloca((n) * sizeof(t))), (n), (t())))

#endif
