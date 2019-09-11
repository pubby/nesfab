#ifndef ROBIN_HOOD_APAIR_HPP
#define ROBIN_HOOD_APAIR_HPP

// A POD alternative to std::pair.
// (the 'a' in 'apair' stands for aggregate)

#include <utility>

namespace rh
{

template<typename A, typename B>
struct apair
{
    using first_type = A;
    using second_type = B;
    A first;
    B second;
};

template<typename A, typename B>
constexpr apair<A, B> make_apair(A&& a, B&& b)
{
    return { std::forward<A>(a), std::forward<B>(b) };
}

} // namespace

#endif
