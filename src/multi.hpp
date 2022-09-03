#ifndef MULTI_HPP
#define MULTI_HPP

#include "type_name.hpp"
#include "ir_decl.hpp"

struct multi_lt_info_t
{
    explicit multi_lt_info_t(ssa_ht h, bool flip = false);

    type_name_t lt;
    type_name_t rt;

    int lwhole;
    int rwhole;
    int minwhole;
    int maxwhole;
    int sbcwhole; // This is used to implement the multi-byte subtraction.

    int lfrac;
    int rfrac;
    int maxfrac;

    int lsize;
    int rsize;

    // Offsets into the node's input array.
    int lstart;
    int rstart;

    int loffset() const { return lstart + lfrac; }
    int roffset() const { return rstart + rfrac; }

    bool lsigned;
    bool rsigned;

    bool validl(int i) const;
    bool validr(int i) const;

    bool signedl(int i) const;
    bool signedr(int i) const;
};

#endif

