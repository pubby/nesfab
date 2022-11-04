#ifndef UNROLL_DIVISOR_HPP
#define UNROLL_DIVISOR_HPP

// Given a loop iteration count 'n',
// and a desired unroll amount of 'd',
// estimates an unroll amount that divides 'n', which is <= 'd'.
unsigned estimate_unroll_divisor(unsigned n, unsigned d);

#endif
