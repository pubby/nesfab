#ifndef CG_ISEL_CPU_HPP
#define CG_ISEL_CPU_HPP

#include <array>
#include <cstdint>

#include "asm.hpp"
#include "locator.hpp"
#include "ir.hpp"
#include "carry.hpp"

namespace isel
{

using options_flags_t = std::uint8_t;
constexpr options_flags_t OPT_NO_DIRECT   = 0b10; // Works as a 2-bit counter

// Options, to be passed to various construction functions:
struct options_t
{
    regs_t can_set = REGF_ISEL;
    regs_t set_mask = REGF_ISEL;
    options_flags_t flags = 0;

    // Restricted registers CANNOT be modified.
    options_t restrict_to(regs_t regs) { return { can_set & regs, set_mask, flags }; }

    // Masked registers CAN be modified, but their defs are replaced with nulls.
    options_t valid_for(regs_t regs) { return { can_set, set_mask & regs, flags }; }

    options_t add_flags(options_flags_t f) { return { can_set, set_mask, flags | f }; }
    options_t remove_flags(options_flags_t f) { return { can_set, set_mask, flags & ~f }; }
};

// A compile-time version of the above.
// (TODO: in C++20 we should be able to use just options_t as a template value param,
//  but as of writing GCC is buggy and ICEs, and clang doesn't have the feature.)
template<regs_t CanSet = REGF_ISEL, regs_t SetMask = REGF_ISEL, options_flags_t Flags = 0>
struct options
{
    static constexpr regs_t can_set = CanSet;
    static constexpr regs_t set_mask = SetMask;
    static constexpr options_flags_t flags = Flags;

    template<regs_t Regs>
    using restrict_to = options<CanSet & Regs, SetMask, Flags>;

    template<regs_t Regs>
    using unrestrict = options<CanSet | Regs, SetMask, Flags>;

    template<regs_t Regs>
    using valid_for = options<CanSet, SetMask & Regs, Flags>;

    template<options_flags_t NewFlags>
    using add_flags = options<CanSet, SetMask, Flags | NewFlags>;

    // Used to implement OPT_NO_DIRECT; it adds 1 to flags.
    using inc_no_direct = options<CanSet, SetMask, Flags + 1>;

    template<options_flags_t NewFlags>
    using remove_flags = options<CanSet, SetMask, Flags & ~NewFlags>;

    static constexpr options_t to_struct = options_t{ CanSet, SetMask, Flags };
};

// An approximation of the CPU's state at a given position.
struct cpu_t
{
    std::array<locator_t, NUM_ISEL_REGS> defs = {};

    // Sometimes the register will known to hold a specific constant value.
    // These vars track that:
    std::array<std::uint8_t, NUM_KNOWN_REGS> known = {};
    regs_t known_mask = 0; // If bit is set, known_values holds constant.

    // The highest bit of 'known_mask' is repurposed.
    // It tracks if the 'Z' register holds the opposite value it says.
    static constexpr regs_t REG_INVERTED_Z = 7;
    static constexpr regs_t REGF_INVERTED_Z = 1 << REG_INVERTED_Z;

    // The second bit of 'known_mask' is repurposed.
    // It tracks if the 'N' register holds a boolean value.
    static constexpr regs_t REG_BOOL_IN_N = 6;
    static constexpr regs_t REGF_BOOL_IN_N = 1 << REG_BOOL_IN_N;

    static_assert((REGF_ISEL & REGF_INVERTED_Z) == 0);
    static_assert((REGF_ISEL & REGF_BOOL_IN_N) == 0);

    // When implementing minor branches (such as in multi-byte comparisons),
    // some registers will be conditionally set.
    // These flags track which registers are conditional.
    regs_t conditional_regs = 0;

    // The highest bit of 'conditional_regs' is repurposed,
    // and tracks if conditional execution is happening.
    static constexpr regs_t CONDITIONAL_EXEC = 1 << 7;
    static_assert((REGF_ISEL & CONDITIONAL_EXEC) == 0);

    // This bitset keeps track of which variables must be stored.
    // To shrink the size down to 64 bits, a rolling window is used
    // based around the live ranges occurring within a single CFG node.
    std::uint64_t req_store = 0;

    bool inverted_z() const { return known_mask & REGF_INVERTED_Z; }
    bool bool_in_n() const { return known_mask & REGF_BOOL_IN_N; }

    // Debug function used to ensure 'known' holds zeroes for unknown values.
    bool known_array_valid() const
    {
        for(unsigned i = 0; i < known.size(); ++i)
            if(!(known_mask & (1 << i)) && known[i] != 0)
                return false;
        return true;
    }

    // Determines if two cpus are reasonably equivalent.
    // Keep in sync with 'hash'.
    // DO NOT COMPARE 'conditional_regs'!
    bool operator==(cpu_t const& o) const 
    { 
        assert(known_array_valid() && o.known_array_valid());
        return (req_store == o.req_store 
                && defs == o.defs 
                && known_mask == o.known_mask
                && known == o.known);
    }

    // Keep in sync with 'operator=='.
    // DO NOT HASH 'conditional_regs'!
    std::size_t hash() const
    {
        std::size_t h = req_store;
        for(locator_t const& v : defs)
            h = rh::hash_combine(h, v.to_uint());
        for(unsigned i = 0; i < known.size(); ++i)
            h = rh::hash_combine(h, known[i]);
        h = rh::hash_combine(h, known_mask);
        return h;
    }
    
    // If we know the value of a register:
    bool is_known(regs_t reg) const 
        { assert(reg < known.size()); return known_mask & (1 << reg); }
    bool is_known(regs_t reg, std::uint8_t value) const 
        { assert(reg < known.size()); return is_known(reg) && known[reg] == value; }
    bool are_known(regs_t regs) const 
        { return (regs & known_mask) == regs; }

    void set_known(regs_t reg, std::uint8_t value)
    {
        if(reg == REG_C || reg == REG_Z || reg == REG_N)
            assert(!!value == value);
        assert(reg < known.size());
        known[reg] = value;
        known_mask |= 1 << reg;
        assert(known_array_valid());
    }

    void clear_known(regs_t reg)
    {
        assert(reg < known.size());
        known_mask &= ~(1 << reg);
        known[reg] = 0; // Must do this to ensure operator== works.
        assert(known_array_valid());
    }

    template<regs_t Special = 0> [[gnu::always_inline]]
    bool def_eq(regs_t reg, locator_t v) const
    {
        static_assert(!(Special & REGF_ISEL));

        assert(reg < defs.size());
        assert(v);
        assert(v.lclass() != 0xFF);
        assert(defs[reg].lclass() != 0xFF);
        assert(is_orig_def(defs[reg]));
        assert(is_orig_def(v));
        if(reg == REG_Z && inverted_z() == !(Special & REGF_INVERTED_Z))
            return false;
        if(reg == REG_N && bool_in_n() == !(Special & REGF_BOOL_IN_N))
            return false;
        return defs[reg] == v;
    }

    template<regs_t Special = 0> [[gnu::always_inline]]
    bool value_eq(regs_t reg, locator_t v) const
    {
        assert(reg < defs.size());
        if(def_eq<Special>(reg, v))
           return true;
        if(v.is_const_num())
            return is_known(reg, v.data());
        return false;
    }

    template<regs_t Reg> [[gnu::noinline]]
    bool set_def(options_t opt, locator_t value, bool keep_value = false)
    {
        if(!((1 << Reg) & ((opt.can_set & REGF_ISEL) | REGF_INVERTED_Z | REGF_BOOL_IN_N)))
            return false;
        if(Reg < NUM_ISEL_REGS)
            conditional_regs |= 1 << Reg;
        set_def_impl<Reg>(opt, value, keep_value);
        return true;
    }

    template<regs_t R> [[gnu::always_inline]]
    void set_def_impl(options_t opt, locator_t value, bool keep_value = false)
    {
        constexpr regs_t Reg = (R == REG_BOOL_IN_N) ? REG_N : ((R == REG_INVERTED_Z) ? REG_Z : R);

        assert(value.lclass() != 0xFF);

        if(!(opt.set_mask & (1 << Reg)))
        {
            defs[Reg] = locator_t{};
            clear_known(Reg);
            assert(known_array_valid());
            return;
        }

        if(value.is_const_num())
        {
            assert(R != REG_BOOL_IN_N);
            assert(R != REG_INVERTED_Z);
            assert(R == Reg);

            defs[Reg] = locator_t{};

            if(Reg == REG_Z)
                set_known(Reg, !value.data());
            else if(Reg == REG_C)
                set_known(Reg, !!value.data());
            else if(Reg == REG_N)
                set_known(Reg, !!(value.data() & 0x80));
            else if(Reg < known.size())
                set_known(Reg, value.data());
        }
        else
        {
            defs[Reg] = value;

            if(R == REG_BOOL_IN_N)
                known_mask |= REGF_BOOL_IN_N;
            else if(R == REG_INVERTED_Z)
                known_mask |= REGF_INVERTED_Z;
            else if(R == REG_Z)
                known_mask &= ~REGF_INVERTED_Z;
            else if(R == REG_N)
                known_mask &= ~REGF_BOOL_IN_N;

            if(!keep_value)
                clear_known(Reg);
        }

        assert(known_array_valid());
    }

    template<regs_t Regs> [[gnu::noinline]]
    void set_defs_impl(options_t opt, locator_t value, bool keep_value = false)
    {
        assert(value.lclass() != 0xFF);

        if(Regs & REGF_A)
            set_def_impl<REG_A>(opt, value, keep_value);
        if(Regs & REGF_X)
            set_def_impl<REG_X>(opt, value, keep_value);
        if(Regs & REGF_Y)
            set_def_impl<REG_Y>(opt, value, keep_value);
        if(Regs & REGF_C)
            set_def_impl<REG_C>(opt, value, keep_value);
        if(Regs & REGF_Z)
            set_def_impl<REG_Z>(opt, value, keep_value);
        if(Regs & REGF_N)
            set_def_impl<REG_N>(opt, value, keep_value);
        if(Regs & REGF_INVERTED_Z)
            set_def_impl<REG_INVERTED_Z>(opt, value, keep_value);
        if(Regs & REGF_BOOL_IN_N)
            set_def_impl<REG_BOOL_IN_N>(opt, value, keep_value);
        assert(known_array_valid());
    }

    template<regs_t Regs> [[gnu::noinline]]
    bool set_defs(options_t opt, locator_t value, bool keep_value = false)
    {
        static_assert(!(Regs & REGF_INVERTED_Z) | !(Regs & REGF_Z));
        static_assert(!(Regs & REGF_BOOL_IN_N) | !(Regs & REGF_N));

        assert(value.lclass() != 0xFF);

        if((Regs & ((opt.can_set & REGF_ISEL) | REGF_INVERTED_Z | REGF_BOOL_IN_N)) != Regs)
            return false;
        conditional_regs |= (Regs & REGF_ISEL);
        set_defs_impl<Regs>(opt, value, keep_value);
        return true;
    }

    template<op_t Op>
    bool set_output_defs_impl(options_t opt, locator_t value)
    {
        constexpr regs_t Regs = op_output_regs(Op) & REGF_ISEL;
        //assert(!value.is_const_num());
        return set_defs<Regs>(opt, value);
    }

    // Dumbly sets registers based on 'op_output_regs'.
    // Use 'set_defs_for' for the smarter version!
    template<op_t Op>
    bool set_output_defs(options_t opt, locator_t value)
    {
        constexpr regs_t Regs = op_output_regs(Op) & REGF_ISEL;
        if((Regs & opt.can_set) != Regs)
            return false;
        if((op_flags(Op) & ASMF_BRANCH) && !(conditional_regs & CONDITIONAL_EXEC))
            conditional_regs = CONDITIONAL_EXEC;
        conditional_regs |= Regs & REGF_ISEL;
        return set_defs<Regs>(opt, value);
    }

    // Sets registers based on an assembly op.
    // If the registers and inputs are known constants,
    // the set values may be constants too.
    template<op_t Op>
    std::enable_if_t<Op < NUM_NORMAL_OPS, bool> set_defs_for(options_t opt, locator_t def, locator_t arg);

    template<op_t Op>
    std::enable_if_t<Op >= NUM_NORMAL_OPS, bool> set_defs_for(options_t opt, locator_t def, locator_t arg)
    {
        return set_output_defs<Op>(opt, def);
    }
};

// Like cpu_t, but tracks far, far less state.
// This is used to pass CPU state across CFG boundaries.
struct cross_cpu_t
{
    cross_cpu_t() = default;
    explicit cross_cpu_t(cpu_t const& cpu, carry_t carry0, carry_t carry1, bool strip_phi = false);

    auto operator<=>(cross_cpu_t const&) const = default;
    bool has(locator_t loc) const { return std::find(defs.begin(), defs.end(), loc) != defs.end(); }
    cpu_t to_cpu() const;

    std::array<locator_t, NUM_CROSS_REGS> defs = {};
};

inline int heuristic_penalty(locator_t const* defs)
{
    int count = 0;

    for(unsigned i = 0; i < NUM_CROSS_REGS; ++i)
    {
        if(!defs[i])
        {
            switch(i)
            {
            case REG_A: count += 2; break;
            case REG_X: count += 1; break;
            case REG_Y: count += 1; break;
            default: break;
            }
            continue;
        }

        for(unsigned j = 0; j < NUM_CROSS_REGS; ++j)
        {
            if(j != i && defs[i] == defs[j])
            {
                count += 3;
                break;
            }
        }
    }

    return count;
}

struct cross_transition_t
{
    cross_cpu_t in_state;
    cross_cpu_t out_state;
    auto operator<=>(cross_transition_t const&) const = default;
    int heuristic_penalty(bool out) const 
        { return isel::heuristic_penalty(in_state.defs.data()) + isel::heuristic_penalty(out_state.defs.data()); }
};

} // end namespace isel


template<>
struct std::hash<isel::cpu_t>
{
    std::size_t operator()(isel::cpu_t const& cpu) const noexcept { return cpu.hash(); }
};

template<>
struct std::hash<isel::cross_cpu_t>
{
    std::size_t operator()(isel::cross_cpu_t const& cross) const noexcept
    {
        std::size_t h = 0xDEADBEEF;
        for(locator_t const& v : cross.defs)
            h = rh::hash_combine(h, v.to_uint());
        return h;
    }
};

template<>
struct std::hash<isel::cross_transition_t>
{
    std::size_t operator()(isel::cross_transition_t const& ct) const noexcept
    {
        std::hash<isel::cross_cpu_t> hasher;
        return rh::hash_combine(hasher(ct.in_state), hasher(ct.out_state));
    }
};

#endif
