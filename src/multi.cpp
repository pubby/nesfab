#include "multi.hpp"

#include "ir.hpp"

multi_lt_info_t::multi_lt_info_t(ssa_ht h, bool flip)
{
    lt = type_name_t(h->input(0).whole());
    rt = type_name_t(h->input(1).whole());

    lwhole = whole_bytes(lt);
    rwhole = whole_bytes(rt);
    minwhole = std::min(lwhole, rwhole);
    maxwhole = std::max(lwhole, rwhole);
    sbcwhole = minwhole;

    lfrac = frac_bytes(lt);
    rfrac = frac_bytes(rt);
    maxfrac = std::max(lfrac, rfrac);

    lsize = lwhole + lfrac;
    rsize = rwhole + rfrac;

    // Offsets into the node's input array.
    lstart = 2 + (flip ? rsize : 0);
    rstart = 2 + (flip ? 0 : lsize);
    assert(lstart != rstart);

    lsigned = is_signed(lt);
    rsigned = is_signed(rt);
};

bool multi_lt_info_t::validl(int i) const
{ 
    return i >= lstart && i < lstart + lsize;
};

bool multi_lt_info_t::validr(int i) const
{ 
    return i >= rstart && i < rstart + rsize;
};

bool multi_lt_info_t::signedl(int i) const
{
    assert(validl(i));
    return lsigned && i == lstart + lsize - 1;
}

bool multi_lt_info_t::signedr(int i) const
{
    assert(validr(i));
    return rsigned && i == rstart + rsize - 1;
}
