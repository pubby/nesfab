#ifndef FLAGS_HPP
#define FLAGS_HPP

// Flags that are useful among different passes.

enum mark_t : std::uint16_t
{
    MARK_NONE      = 0,
    MARK_TEMPORARY = 1,
    MARK_PERMANENT = 2,
    MARK_DELAYED   = 3,
};

constexpr std::uint16_t MARK_OFFSET         = 0;
constexpr std::uint16_t MARK_MASK           = 0b11;

constexpr std::uint16_t FLAG_PRUNED         = 1ull << 2;
constexpr std::uint16_t FLAG_DAISY          = 1ull << 3;

constexpr std::uint16_t FLAG_IN_WORKLIST    = 1ull << 4;
constexpr std::uint16_t FLAG_PROCESSED      = 1ull << 5;
constexpr std::uint16_t FLAG_ARRAY          = 1ull << 6;
constexpr std::uint16_t FLAG_BANK_PRELOADED = 1ull << 7;

class flag_owner_t
{
public:
    void set_flags(std::uint16_t f) { m_flags |= f; }
    void clear_flags(std::uint16_t f) { m_flags &= ~f; }
    bool test_flags(std::uint16_t f) const { return (m_flags & f) == f; }

    void set_mark(mark_t mark) { m_flags &= ~MARK_MASK; m_flags |= mark; }
    void clear_mark() { m_flags &= ~MARK_MASK; }
    mark_t get_mark() const { return (mark_t)(m_flags & MARK_MASK); }
protected:
    std::uint16_t m_flags = 0;
};

#endif
