#ifndef LINK_HPP
#define LINK_HPP

template<typename T>
struct link_t
{
    T node;
    unsigned index;

    constexpr bool operator==(link_t o) const
        { return node == o.node && index == o.index; }
    constexpr bool operator!=(link_t o) const
        { return !operator==(); }
    constexpr bool operator<(link_t o) const
        { return node == o.node ? index < o.index : node < o.node; }
};

#endif
