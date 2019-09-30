#ifndef SIZEOF_BITS_HPP
#define SIZEOF_BITS_HPP

#include <cstdint>

template<typename T>
constexpr std::size_t sizeof_bits = sizeof(T) * CHAR_BIT;

#endif
