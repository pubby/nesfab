#ifndef BUILTIN_HPP
#define BUILTIN_HPP

#include <cassert>

#include "sizeof_bits.hpp"

namespace builtin
{

using int128 = __int128;
using uint128 = unsigned __int128;

[[gnu::unused]] static int128 mul128(int128 lhs, int128 rhs) 
    { return lhs * rhs; }

[[gnu::unused]] static uint128 umul128(uint128 lhs, uint128 rhs)
    { return lhs * rhs; }


[[gnu::always_inline]] inline auto clz(unsigned i) 
    { assert(i); return __builtin_clz(i); }
[[gnu::always_inline]] inline unsigned long clz(unsigned long i) 
    { assert(i); return __builtin_clzl(i); }
[[gnu::always_inline]] inline unsigned long long clz(unsigned long long i) 
    { assert(i); return __builtin_clzll(i); }

[[gnu::always_inline]] inline auto rclz(unsigned i) 
    { assert(i); return ((unsigned)sizeof_bits<unsigned> - (unsigned)__builtin_clz(i)); }
[[gnu::always_inline]] inline unsigned long rclz(unsigned long i) 
    { assert(i); return ((unsigned long)sizeof_bits<unsigned long>
              - (unsigned long)__builtin_clzl(i)); }
[[gnu::always_inline]] inline unsigned long long rclz(unsigned long long i) 
    { assert(i); return ((unsigned long long)sizeof_bits<unsigned long long>
                         - (unsigned long long)__builtin_clzll(i)); }

[[gnu::always_inline]] inline auto ctz(unsigned i) 
    { assert(i); return __builtin_ctz(i); }
[[gnu::always_inline]] inline unsigned long ctz(unsigned long i) 
    { assert(i); return __builtin_ctzl(i); }
[[gnu::always_inline]] inline unsigned long long ctz(unsigned long long i) 
    { assert(i); return __builtin_ctzll(i); }

[[gnu::always_inline]] inline auto rctz(unsigned i) 
    { assert(i); return ((unsigned)sizeof_bits<unsigned> - (unsigned)__builtin_ctz(i)); }
[[gnu::always_inline]] inline unsigned long rctz(unsigned long i) 
    { assert(i); return ((unsigned long)sizeof_bits<unsigned long>
                         - (unsigned long)__builtin_ctzl(i)); }
[[gnu::always_inline]] inline unsigned long long rctz(unsigned long long i) 
    { assert(i); return ((unsigned long long)sizeof_bits<unsigned long long>
                         - (unsigned long long)__builtin_ctzll(i)); }

[[gnu::always_inline]] inline auto popcount(unsigned i) 
    { return __builtin_popcount(i); }
[[gnu::always_inline]] inline auto popcount(unsigned long i) 
    { return __builtin_popcountl(i); }
[[gnu::always_inline]] inline auto popcount(unsigned long long i) 
    { return __builtin_popcountll(i); }

template<typename T> [[gnu::always_inline]]
inline bool add_overflow(T l, T r, T& o)
    { return __builtin_add_overflow(l, r, &o); }

} // namespace builtin

#endif
