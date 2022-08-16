#ifndef CG_ISEL_CPU_HPP
#define CG_ISEL_CPU_HPP

#include <array>
#include <cstdint>

#include "asm.hpp"
#include "locator.hpp"
#include "ir.hpp"

namespace isel
{

using options_flags_t = std::uint8_t;
constexpr options_flags_t OPT_NO_DIRECT   = 0b10; // Works as a 2-bit counter
constexpr options_flags_t OPT_CONDITIONAL = 1 << 2;

// Options, to be passed to various construction functions:
struct options_t
{
    regs_t can_set = REGF_CPU;
    regs_t set_mask = REGF_CPU;
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
template<regs_t CanSet = REGF_CPU, regs_t SetMask = REGF_CPU, options_flags_t Flags = 0>
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
    std::array<locator_t, NUM_CPU_REGS> defs = {};

    // Sometimes the register will known to hold a specific constant value.
    // These vars track that:
    std::array<std::uint8_t, NUM_CPU_REGS> known = {};
    regs_t known_mask = 0; // If bit is set, known_values holds constant.

    // When implementing minor branches (such as in multi-byte comparisons),
    // some registers will be conditionally set.
    // These flags track which registers are conditional:
    regs_t conditional_regs = 0;

    // This bitset keeps track of which variables must be stored.
    // To shrink the size down to 64 bits, a rolling window is used
    // based around the live ranges occuring within a single CFG node.
    std::uint64_t req_store = 0;

    // Debug function used to ensure 'known' holds zeroes for unknown values.
    bool known_array_valid() const
    {
        for(unsigned i = 0; i < known.size(); ++i)
            if(!(known_mask & (1 << i)) && known[i] != 0)
                return false;
        return true;
    }

    // Determines if two cpus are reasonably equivalent.
    bool operator==(cpu_t const& o) const 
    { 
        assert(known_array_valid() && o.known_array_valid());
        return accurate_eq(o);
    }

    bool accurate_eq(cpu_t const& o) const 
    { 
        assert(known_array_valid() && o.known_array_valid());
        return (req_store == o.req_store && defs == o.defs && known_mask == o.known_mask && known == o.known); 
    }

    bool approximate_eq(cpu_t const& o) const
    {
        return ((known_mask & REGF_AC) == (o.known_mask & REGF_AC)
                && defs[REG_A] == o.defs[REG_A]);
    }

    std::size_t accurate_hash() const
    {
        std::size_t h = req_store;
        for(locator_t const& v : defs)
            h = rh::hash_combine(h, v.to_uint());
        for(unsigned i = 0; i < known.size(); ++i)
            h ^= known[i] << i * 8;
        for(std::uint8_t k : known)
            h = rh::hash_combine(h, k);
        h = rh::hash_combine(h, known_mask);
        return h;
    }

    std::size_t approximate_hash() const
    {
        std::size_t h = known_mask & REGF_AC;
        h = rh::hash_combine(h, defs[REG_A].to_uint());
        return h;
    }
    
    // If we know the value of a register:
    bool is_known(regs_t reg) const { return known_mask & (1 << reg); }
    bool is_known(regs_t reg, std::uint8_t value) const { return is_known(reg) && known[reg] == value; }
    bool are_known(regs_t regs) const { return (regs & known_mask) == regs; }

    void set_known(regs_t reg, std::uint8_t value)
    {
        if(reg == REG_C || reg == REG_Z || reg == REG_N)
            assert(!!value == value);
        known[reg] = value;
        known_mask |= 1 << reg;
    }

    void clear_known(regs_t reg)
    {
        known_mask &= ~(1 << reg);
        known[reg] = 0; // Must do this to ensure operator== works.
    }

    bool def_eq(regs_t reg, locator_t v) const
    {
        assert(v);
        assert(is_orig_def(defs[reg]));
        assert(is_orig_def(v));
        return defs[reg] == v;
    }

    bool value_eq(regs_t reg, locator_t v) const
    {
        if(def_eq(reg, v))
           return true;
        if(v.is_const_num())
            return is_known(reg, v.data());
        return false;
    }

    template<regs_t Reg> [[gnu::noinline]]
    bool set_def(options_t opt, locator_t value, bool keep_value = false)
    {
        if(!(opt.can_set & (1 << Reg)))
            return false;
        if(opt.flags & OPT_CONDITIONAL)
            conditional_regs |= 1 << Reg;
        set_def_impl<Reg>(opt, value, keep_value);
        return true;
    }

    template<regs_t Reg> [[gnu::always_inline]]
    void set_def_impl(options_t opt, locator_t value, bool keep_value = false)
    {
        if(!(opt.set_mask & (1 << Reg)))
        {
            defs[Reg] = locator_t{};
            clear_known(Reg);
            return;
        }

        if(value.is_const_num())
        {
            defs[Reg] = locator_t{};

            if(Reg == REG_Z)
                set_known(Reg, !value.data());
            else if(Reg == REG_C)
                set_known(Reg, !!value.data());
            else if(Reg == REG_N)
                set_known(Reg, !!(value.data() & 0x80));
            else
                set_known(Reg, value.data());
        }
        else
        {
            defs[Reg] = value;
            if(!keep_value)
                clear_known(Reg);
        }
    }

    template<regs_t Regs> [[gnu::noinline]]
    void set_defs_impl(options_t opt, locator_t value, bool keep_value = false)
    {
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
        if(Regs & REGF_B)
            set_def_impl<REG_B>(opt, value, keep_value);
    }

    template<regs_t Regs> [[gnu::noinline]]
    bool set_defs(options_t opt, locator_t value, bool keep_value = false)
    {
        if((Regs & opt.can_set) != Regs)
            return false;
        if(opt.flags & OPT_CONDITIONAL)
            conditional_regs |= Regs;
        set_defs_impl<Regs>(opt, value, keep_value);
        return true;
    }

    template<op_t Op>
    bool set_output_defs_impl(options_t opt, locator_t value)
    {
        constexpr regs_t Regs = op_output_regs(Op) & REGF_CPU;
        //assert(!value.is_const_num());
        return set_defs<Regs>(opt, value);
    }

    // Dumbly sets registers based on 'op_output_regs'.
    // Use 'set_defs_for' for the smarter version!
    template<op_t Op>
    bool set_output_defs(options_t opt, locator_t value)
    {
        constexpr regs_t Regs = op_output_regs(Op) & REGF_CPU;
        if((Regs & opt.can_set) != Regs)
            return false;
        if(opt.flags & OPT_CONDITIONAL)
            conditional_regs |= Regs;
        return set_defs<Regs>(opt, value);
    }

    // Sets registers based on an assembly op.
    // If the registers and inputs are known constants,
    // the set values may be constants too.
    template<op_t Op>
    bool set_defs_for(options_t opt, locator_t def, locator_t arg);

    /* TODO: remove?
    locator_t normalize(locator_t loc)
    {
        if(l.lclass() == LOC_SSA)
        {
            ssa_ht const h = l.ssa();

            if(h->op() == SSA_phi_copy)
            {
                assert(cset_locator(h).lclass() == LOC_PHI);
                return cset_locator(h);
            }
        }
    }

    void strip_transient() 
    {
        conditional_regs = 0;
        req_store = 0;
        for(locator_t& loc : defs)
            loc = normalize(loc);
    }
    */

};

struct approximate_hash_t
{
    std::size_t operator()(isel::cpu_t const& cpu) const noexcept
        { return cpu.approximate_hash(); }
};

struct approximate_eq_t
{
    std::size_t operator()(isel::cpu_t const& l, isel::cpu_t const& r) const noexcept
        { return l.approximate_eq(r); }
};

} // end namespace isel


template<>
struct std::hash<isel::cpu_t>
{
    std::size_t operator()(isel::cpu_t const& cpu) const noexcept
    {
        return cpu.accurate_hash();
    }
};

#endif
