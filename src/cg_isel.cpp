#include "cg_isel.hpp"

#include <cstdint>
#include <iostream> // TODO
#include <functional>
#include <type_traits>
#include <vector>

#include "robin/hash.hpp"
#include "robin/map.hpp"

#include "array_pool.hpp"
#include "format.hpp"
#include "globals.hpp"

#define CONT_TEMPLATE 
#define CONT cont_fn_t cont

namespace isel
{
    using options_flags_t = std::uint8_t;
    constexpr options_flags_t OPT_CONDITIONAL = 1 << 0;
    constexpr options_flags_t OPT_NO_DIRECT   = 1 << 1;

    // Options, to be passed to various construction functions:
    struct options_t
    {
        regs_t can_set = REGF_CPU;
        regs_t set_mask = REGF_CPU;
        options_flags_t flags = 0;

        options_t restrict_to(regs_t regs) { return { can_set & regs, set_mask, flags }; }
        options_t mask(regs_t regs) { return { can_set, set_mask & regs, flags }; }
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
        using mask = options<CanSet, SetMask & Regs, Flags>;

        template<options_flags_t NewFlags>
        using add_flags = options<CanSet, SetMask, Flags | NewFlags>;

        template<options_flags_t NewFlags>
        using remove_flags = options<CanSet, SetMask, Flags & ~NewFlags>;

        static constexpr options_t to_struct = options_t{ CanSet, SetMask, Flags };
    };

    // An approximation of the CPU's state at a given position.
    struct cpu_t
    {
        std::array<locator_t, NUM_CPU_REGS> regs;

        // When implementing minor branches (such as in multi-byte comparisons),
        // some registers will be conditionally set.
        // These flags track which registers are conditional:
        regs_t conditional_regs;

        // This bitset keeps track of which variables must be stored.
        // To shrink the size down to 64 bits, a rolling window is used
        // based around the live ranges occuring within a single CFG node.
        std::uint64_t req_store;

        bool operator==(cpu_t const& o) const
        { 
            return (req_store == o.req_store 
                    && conditional_regs == o.conditional_regs
                    && regs == o.regs);
        }

        bool reg_eq(regs_t reg, locator_t v) const
        {
            assert(is_orig_def(regs[reg]));
            assert(is_orig_def(v));
            return regs[reg] == v;
        }

        template<regs_t Reg> [[gnu::noinline]]
        bool set_reg(options_t opt, locator_t value)
        {
            if(!(opt.can_set & (1 << Reg)))
                return false;
            if(opt.flags & OPT_CONDITIONAL)
                conditional_regs |= 1 << Reg;
            set_reg_impl<Reg>(opt, value);
            return true;
        }

        template<regs_t Reg> [[gnu::always_inline]]
        bool set_reg(options_t opt, std::uint8_t b)
        {
            return set_reg<Reg>(opt, locator_t::const_byte(b));
        }


        template<regs_t Reg> [[gnu::always_inline]]
        void set_reg_impl(options_t opt, locator_t value)
        {
            if((Reg == REG_C || Reg == REG_Z) && value.is_const_num())
                value.set_data(!!value.data());

            if(value.is_const_num() || (opt.set_mask & (1 << Reg)))
                regs[Reg] = value;
            else
                regs[Reg] = locator_t{};
        }

        template<regs_t Reg> [[gnu::always_inline]]
        void set_reg_impl(options_t opt, std::uint8_t b)
        {
            return set_reg_impl<Reg>(opt, locator_t::const_byte(b));
        }

        template<regs_t Regs> [[gnu::noinline]]
        void set_regs_impl(options_t opt, locator_t value)
        {
            if(Regs & REGF_A)
                set_reg_impl<REG_A>(opt, value);
            if(Regs & REGF_X)
                set_reg_impl<REG_X>(opt, value);
            if(Regs & REGF_Y)
                set_reg_impl<REG_Y>(opt, value);
            if(Regs & REGF_C)
                set_reg_impl<REG_C>(opt, value);
            if(Regs & REGF_Z)
                set_reg_impl<REG_Z>(opt, value);
        }

        template<regs_t Regs> [[gnu::noinline]]
        bool set_regs(options_t opt, locator_t value)
        {
            if((Regs & opt.can_set) != Regs)
                return false;
            if(opt.flags & OPT_CONDITIONAL)
                conditional_regs |= Regs;
            set_regs_impl<Regs>(opt, value);
            return true;
        }

        template<op_t Op>
        bool set_output_regs_impl(options_t opt, locator_t value)
        {
            constexpr regs_t Regs = op_output_regs(Op) & REGF_CPU;
            assert(!value.is_const_num());
            return set_regs<Regs>(opt, value);
        }

        // Dumbly sets registers based on 'op_output_regs'.
        // Use 'set_regs_for' for the smarter version!
        template<op_t Op>
        bool set_output_regs(options_t opt, locator_t value)
        {
            constexpr regs_t Regs = op_output_regs(Op) & REGF_CPU;
            if((Regs & opt.can_set) != Regs)
                return false;
            if(opt.flags & OPT_CONDITIONAL)
                conditional_regs |= Regs;
            assert(!value.is_const_num());
            return set_regs<Regs>(opt, value);
        }

        // Sets registers based on an assembly op.
        // If the registers and inputs are known constants,
        // the set values may be constants too.
        template<op_t Op>
        bool set_regs_for(options_t opt, locator_t def, locator_t arg);

        // Queries if multiple registers are known numeric constants.
        template<regs_t Regs>
        bool all_num() const
        {
            bool result = true;

            if(Regs & REGF_A)
                result &= regs[REG_A].is_const_num();
            if(Regs & REGF_X)
                result &= regs[REG_X].is_const_num();
            if(Regs & REGF_Y)
                result &= regs[REG_Y].is_const_num();
            if(Regs & REGF_C)
                result &= regs[REG_C].is_const_num();
            if(Regs & REGF_Z)
                result &= regs[REG_Z].is_const_num();

            return result;
        }

    };
}

namespace std
{
    template<>
    struct hash<isel::cpu_t>
    {
        std::size_t operator()(isel::cpu_t const& cpu) const noexcept
        {
            std::size_t h = rh::hash_finalize(
                cpu.req_store ^ cpu.conditional_regs);
            for(locator_t const& v : cpu.regs)
                h = rh::hash_combine(h, v.to_uint());
            return h;
        }
    };
}

namespace isel
{
    template<op_t Op>
    struct set_regs_for_impl
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            cpu.set_output_regs<Op>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ADC_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A | REGF_C>())
            {
                static_assert(op_output_regs(ADC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_C));
                unsigned const result = cpu.regs[REG_A].data() + !!cpu.regs[REG_C].data() + arg.data();
                cpu.set_reg_impl<REG_A>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, (result & 0xFF) == 0);
                cpu.set_reg_impl<REG_C>(opt, !!(result & 0x100));
            }
            else
                cpu.set_output_regs_impl<ADC_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<AND_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(AND_IMMEDIATE) == (REGF_A | REGF_Z));
                std::uint8_t const result = cpu.regs[REG_A].data() & arg.data();
                cpu.set_reg_impl<REG_A>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
            }
            else if(arg.eq_const(0))
            {
                cpu.set_reg_impl<REG_A>(opt, 0u);
                cpu.set_reg_impl<REG_Z>(opt, 1u);
            }
            else
                cpu.set_output_regs_impl<AND_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ASL_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ASL_IMPLIED) == (REGF_A | REGF_Z | REGF_C));
                std::uint8_t const a = cpu.regs[REG_A].data() << 1;
                bool const c = cpu.regs[REG_A].data() & 128;
                cpu.set_reg_impl<REG_A>(opt, a);
                cpu.set_reg_impl<REG_Z>(opt, a == 0);
                cpu.set_reg_impl<REG_C>(opt, c);
            }
            else
                cpu.set_output_regs_impl<ASL_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<CLC_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            cpu.set_reg_impl<REG_C>(opt, 0u);
        }
    };

    template<>
    struct set_regs_for_impl<CMP_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(CMP_IMMEDIATE) == (REGF_Z | REGF_C));
                bool const z = arg.data() == cpu.regs[REG_A].data();
                bool const c = arg.data() <= cpu.regs[REG_A].data();
                cpu.set_reg_impl<REG_Z>(opt, z);
                cpu.set_reg_impl<REG_C>(opt, c);
            }
            else if(arg.eq_const(0))
            {
                cpu.set_reg_impl<REG_Z>(opt, def);
                cpu.set_reg_impl<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs_impl<CMP_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<CPX_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(CPX_IMMEDIATE) == (REGF_Z | REGF_C));
                bool const z = arg.data() == cpu.regs[REG_X].data();
                bool const c = arg.data() <= cpu.regs[REG_X].data();
                cpu.set_reg_impl<REG_Z>(opt, z);
                cpu.set_reg_impl<REG_C>(opt, c);
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg_impl<REG_Z>(opt, def);
                cpu.set_reg_impl<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs_impl<CPX_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<CPY_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(CPY_IMMEDIATE) == (REGF_Z | REGF_C));
                bool const z = arg.data() == cpu.regs[REG_Y].data();
                bool const c = arg.data() <= cpu.regs[REG_Y].data();
                cpu.set_reg_impl<REG_Z>(opt, z);
                cpu.set_reg_impl<REG_C>(opt, c);
            }
            else if(arg.eq_const(0))
            {
                cpu.set_reg_impl<REG_Z>(opt, def);
                cpu.set_reg_impl<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs_impl<CPY_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<DEX_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(DEX_IMPLIED) == (REGF_Z | REGF_X));
                std::uint8_t const result = cpu.regs[REG_X].data() - 1;
                cpu.set_reg_impl<REG_X>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs_impl<DEX_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<DEY_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(DEY_IMPLIED) == (REGF_Z | REGF_Y));
                std::uint8_t const result = cpu.regs[REG_Y].data() - 1;
                cpu.set_reg_impl<REG_Y>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs_impl<DEY_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<EOR_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(EOR_IMMEDIATE) == (REGF_Z | REGF_A));
                std::uint8_t const result = cpu.regs[REG_Y].data() ^ arg.data();
                cpu.set_reg_impl<REG_A>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs_impl<EOR_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<INX_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(INX_IMPLIED) == (REGF_Z | REGF_X));
                std::uint8_t const result = cpu.regs[REG_X].data() + 1;
                cpu.set_reg_impl<REG_X>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs_impl<INX_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<INY_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(INY_IMPLIED) == (REGF_Z | REGF_Y));
                std::uint8_t const result = cpu.regs[REG_Y].data() + 1;
                cpu.set_reg_impl<REG_Y>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs_impl<INY_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<LDA_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num())
            {
                static_assert(op_output_regs(LDA_IMMEDIATE) == (REGF_Z | REGF_A));
                cpu.set_reg_impl<REG_A>(opt, arg);
                cpu.set_reg_impl<REG_Z>(opt, arg.data() == 0);
            }
            else
                cpu.set_output_regs_impl<LDA_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<LDX_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num())
            {
                static_assert(op_output_regs(LDX_IMMEDIATE) == (REGF_Z | REGF_X));
                cpu.set_reg_impl<REG_X>(opt, arg);
                cpu.set_reg_impl<REG_Z>(opt, arg.data() == 0);
            }
            else
                cpu.set_output_regs_impl<LDX_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<LDY_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num())
            {
                static_assert(op_output_regs(LDY_IMMEDIATE) == (REGF_Z | REGF_Y));
                cpu.set_reg_impl<REG_Y>(opt, arg);
                cpu.set_reg_impl<REG_Z>(opt, arg.data() == 0);
            }
            else
                cpu.set_output_regs_impl<LDY_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<LSR_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(LSR_IMPLIED) == (REGF_A | REGF_Z | REGF_C));
                std::uint8_t const a = cpu.regs[REG_A].data() >> 1;
                std::uint8_t const c  = cpu.regs[REG_A].data() & 1;
                cpu.set_reg_impl<REG_A>(opt, a);
                cpu.set_reg_impl<REG_Z>(opt, a == 0);
                cpu.set_reg_impl<REG_C>(opt, c);
            }
            else
                cpu.set_output_regs_impl<LSR_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ORA_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ORA_IMMEDIATE) == (REGF_A | REGF_Z));
                std::uint8_t const result = cpu.regs[REG_Y].data() | arg.data();
                cpu.set_reg_impl<REG_A>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
            }
            else if(arg.eq_const(0xFF))
            {
                cpu.set_reg_impl<REG_A>(opt, 0xFFu);
                cpu.set_reg_impl<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs_impl<ORA_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ROL_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(ROL_IMPLIED) == (REGF_A | REGF_Z | REGF_C));

            if(cpu.all_num<REGF_A>())
            {
                bool const c = cpu.regs[REG_A].data() & 128;
                cpu.set_reg_impl<REG_C>(opt, c);

                if(cpu.all_num<REGF_C>())
                {
                    std::uint8_t const a = (cpu.regs[REG_A].data() << 1) | !!cpu.regs[REG_C].data();
                    cpu.set_reg_impl<REG_A>(opt, locator_t::const_byte(a));
                    cpu.set_reg_impl<REG_Z>(opt, locator_t::const_byte(a == 0));
                }
                else
                {
                    cpu.set_reg_impl<REG_A>(opt, def);
                    cpu.set_reg_impl<REG_Z>(opt, def);
                }
            }
            else if(cpu.all_num<REGF_C>() && cpu.regs[REG_C].data())
            {
                cpu.set_reg_impl<REG_A>(opt, def);
                cpu.set_reg_impl<REG_C>(opt, def);
                cpu.set_reg_impl<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs_impl<ROL_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ROR_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(ROR_IMPLIED) == (REGF_A | REGF_Z | REGF_C));

            if(cpu.all_num<REGF_A>())
            {
                bool c = (cpu.regs[REG_A].data() & 1);
                cpu.set_reg_impl<REG_C>(opt, c);

                if(cpu.all_num<REGF_C>())
                {
                    std::uint8_t const a = (cpu.regs[REG_A].data() >> 1) | (!!cpu.regs[REG_C].data() << 7);
                    cpu.set_reg_impl<REG_A>(opt, a);
                    cpu.set_reg_impl<REG_Z>(opt, a == 0);
                }
                else
                {
                    cpu.set_reg_impl<REG_A>(opt, def);
                    cpu.set_reg_impl<REG_Z>(opt, def);
                }
            }
            else if(cpu.all_num<REGF_C>() && !!cpu.regs[REG_C].data())
            {
                cpu.set_reg_impl<REG_A>(opt, def);
                cpu.set_reg_impl<REG_C>(opt, def);
                cpu.set_reg_impl<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs_impl<ROR_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<SBC_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A | REGF_C>())
            {
                static_assert(op_output_regs(SBC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_C));
                unsigned const result = cpu.regs[REG_A].data() + !!cpu.regs[REG_C].data() + ~arg.data();
                cpu.set_reg_impl<REG_A>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, (result & 0xFF) == 0);
                cpu.set_reg_impl<REG_C>(opt, !!(result & 0x100));
            }
            else
                cpu.set_output_regs_impl<SBC_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<TAX_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TAX_IMPLIED) == (REGF_X | REGF_Z));
            if(cpu.all_num<REGF_A>())
            {
                cpu.set_reg_impl<REG_X>(opt, cpu.regs[REG_A]);
                cpu.set_reg_impl<REG_Z>(opt, cpu.regs[REG_A].data() == 0);
            }
            else
                cpu.set_output_regs_impl<TAX_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<TAY_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TAY_IMPLIED) == (REGF_Y | REGF_Z));
            if(cpu.all_num<REGF_A>())
            {
                cpu.set_reg_impl<REG_Y>(opt, cpu.regs[REG_A]);
                cpu.set_reg_impl<REG_Z>(opt, cpu.regs[REG_A].data() == 0);
            }
            else
                cpu.set_output_regs_impl<TAY_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<TXA_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TXA_IMPLIED) == (REGF_A | REGF_Z));
            if(cpu.all_num<REGF_X>())
            {
                cpu.set_reg_impl<REG_A>(opt, cpu.regs[REG_X]);
                cpu.set_reg_impl<REG_Z>(opt, cpu.regs[REG_X].data() == 0);
            }
            else
                cpu.set_output_regs_impl<TXA_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<TYA_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TYA_IMPLIED) == (REGF_A | REGF_Z));
            if(cpu.all_num<REGF_Y>())
            {
                cpu.set_reg_impl<REG_A>(opt, cpu.regs[REG_Y]);
                cpu.set_reg_impl<REG_Z>(opt, cpu.regs[REG_Y].data() == 0);
            }
            else
                cpu.set_output_regs_impl<TYA_IMPLIED>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<AXS_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_X | REGF_A>())
            {
                static_assert(op_output_regs(AXS_IMMEDIATE) == (REGF_Z | REGF_X | REGF_C));
                unsigned const result = (cpu.regs[REG_A].data() & cpu.regs[REG_X].data()) + ~arg.data() + 1;
                cpu.set_reg_impl<REG_X>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, (result & 0xFF) == 0);
                cpu.set_reg_impl<REG_C>(opt, !!(result & 0x100));
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg_impl<REG_X>(opt, def);
                cpu.set_reg_impl<REG_Z>(opt, def);
                cpu.set_reg_impl<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs_impl<AXS_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ANC_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ANC_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
                std::uint8_t const result = cpu.regs[REG_A].data() & arg.data();
                cpu.set_reg_impl<REG_A>(opt, result);
                cpu.set_reg_impl<REG_Z>(opt, result == 0);
                cpu.set_reg_impl<REG_C>(opt, !!(result & 128));
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg_impl<REG_A>(opt, 0u);
                cpu.set_reg_impl<REG_Z>(opt, 1u);
                cpu.set_reg_impl<REG_C>(opt, 0u);
            }
            else
                cpu.set_output_regs_impl<ANC_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ALR_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ALR_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
                std::uint8_t const and_ = cpu.regs[REG_A].data() & arg.data();
                std::uint8_t const a = and_ >> 1;
                std::uint8_t const c = and_ & 1;
                cpu.set_reg_impl<REG_A>(opt, a);
                cpu.set_reg_impl<REG_Z>(opt, a == 0);
                cpu.set_reg_impl<REG_C>(opt, c);
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg_impl<REG_A>(opt, 0u);
                cpu.set_reg_impl<REG_Z>(opt, 1u);
                cpu.set_reg_impl<REG_C>(opt, 0u);
            }
            else if(arg.is_const_num() && arg.data() == 1)
            {
                cpu.set_reg_impl<REG_A>(opt, 0u);
                cpu.set_reg_impl<REG_Z>(opt, 1u);
                cpu.set_reg_impl<REG_C>(opt, def);
            }
            else
                cpu.set_output_regs_impl<ALR_IMMEDIATE>(opt, def);
        }
    };

    template<>
    struct set_regs_for_impl<ARR_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(ARR_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                std::uint8_t const and_ = cpu.regs[REG_A].data() & arg.data();
                std::uint8_t const c = !!(and_ & 128);
                cpu.set_reg_impl<REG_C>(opt, c);

                if(cpu.all_num<REGF_C>())
                {
                    std::uint8_t const a = (and_ >> 1) | (!!cpu.regs[REG_C].data() << 7);
                    cpu.set_reg_impl<REG_A>(opt, a);
                    cpu.set_reg_impl<REG_Z>(opt, a == 0);
                }
                else
                {
                    cpu.set_reg_impl<REG_A>(opt, def);
                    cpu.set_reg_impl<REG_Z>(opt, def);
                }
            }
            else if(cpu.all_num<REGF_C>() && !!cpu.regs[REG_C].data())
            {
                cpu.set_reg_impl<REG_A>(opt, def);
                cpu.set_reg_impl<REG_C>(opt, def);
                cpu.set_reg_impl<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs_impl<ARR_IMMEDIATE>(opt, def);
        }
    };

    template<op_t Op>
    bool cpu_t::set_regs_for(options_t opt, locator_t def, locator_t arg)
    {
        constexpr regs_t Regs = op_output_regs(Op) & REGF_CPU;
        if((Regs & opt.can_set) != Regs)
            return false;
        if(opt.flags & OPT_CONDITIONAL)
            conditional_regs |= Regs;
        set_regs_for_impl<Op>::call(opt, *this, def, arg);
        return true;
    }

    struct state_t
    {
        rh::batman_map<cpu_t, sel_t const*> map;
        rh::batman_map<cpu_t, sel_t const*> next_map;

        array_pool_t<sel_t> sel_pool;

        unsigned best_cost = ~0;
        unsigned next_best_cost = ~0;
        sel_t const* best_sel = nullptr;

        cfg_ht cfg_node;

        unsigned next_label = 0;

        ssa_op_t ssa_op;

        fn_ht fn;

        locator_t minor_label()
        {
            return locator_t::minor_label(fn, next_label++);
        }
    };

    // Main global state of the instruction selection algorithm.
    static thread_local state_t state;

    ssa_value_t asm_arg(ssa_value_t v)
    {
        if(v.holds_ref())
        {
            if(locator_t loc = cset_locator(v.handle()))
                return loc;
            if(ssa_flags(v->op()) & SSAF_CG_NEVER_STORE)
                return {};
        }
        return v;
    }

    template<typename Tag>
    struct param
    {
        static inline thread_local ssa_value_t _node = {};
        static inline thread_local locator_t _value = {};
        static inline thread_local locator_t _trans = {};

        static void set(ssa_value_t v)
        {
            _node = v;
            _value = locator_t::from_ssa_value(orig_def(v));
            _trans = locator_t::from_ssa_value(asm_arg(v));
        }

        [[gnu::always_inline]]
        static ssa_value_t node() { return _node; }

        [[gnu::always_inline]]
        static locator_t value() { return _value; }

        [[gnu::always_inline]]
        static locator_t trans() { return _trans; }
    };

    template<typename Param>
    struct array_index
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(2); }

        [[gnu::always_inline]]
        static locator_t value() { return locator_t::from_ssa_value(orig_def(node())); }

        [[gnu::always_inline]]
        static locator_t trans() { return locator_t::from_ssa_value(asm_arg(node())); }
    };

    template<typename Param>
    struct array_mem
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(0); }

        [[gnu::always_inline]]
        static locator_t value() { return locator_t::from_ssa_value(orig_def(node())); }

        [[gnu::always_inline]]
        static locator_t trans() { return locator_t::from_ssa_value(asm_arg(node())); }
    };

    struct null_
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return {}; }

        [[gnu::always_inline]]
        static locator_t value() { return {}; }

        [[gnu::always_inline]]
        static locator_t trans() { return {}; }

        // Pass-thru
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            Cont::run(cpu, prev);
        }
    };

    template<std::uint8_t I>
    struct const_
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return {}; }

        [[gnu::always_inline]]
        static locator_t value() { return locator_t::const_byte(I); }

        [[gnu::always_inline]]
        static locator_t trans() { return locator_t::const_byte(I); }
    };

    unsigned get_cost(sel_t const* sel)
    {
        return sel ? sel->cost : 0;
    }

    template<op_t Op>
    constexpr unsigned op_penalty()
    {
        switch(op_name(Op))
        {
        default: 
            return 0;
        // Very slightly penalize ROL/ROR, to prefer LSR/ASL:
        case ROL:
        case ROR:
        // Very slighty penalize LAX, to prefer LDA or LDX:
        case LAX: 
        // Same with SAX and AND:
        case SAX: 
            return 1;
        }
    }

    template<op_t Op>
    constexpr unsigned cost_fn = (op_cycles(Op) * 256) + op_size(Op) + op_penalty<Op>();

    // THIS DETERMINES HOW BIG THE MAP GETS, AND HOW WIDE THE SEARCH IS:
    constexpr unsigned COST_CUTOFF = cost_fn<LDA_ABSOLUTE> * 3;

    template<op_t Op>
    sel_t& alloc_sel(options_t opt, sel_t const* prev, locator_t arg = {}, unsigned extra_cost = 0)
    {
        unsigned total_cost = cost_fn<Op>;
        if(opt.flags & OPT_CONDITIONAL)
            total_cost = (total_cost * 3) / 4; // Conditional ops are cheaper.
        total_cost += get_cost(prev) + extra_cost;

        return state.sel_pool.emplace(prev, total_cost, asm_inst_t{ Op, state.ssa_op, arg });
    }

    template<op_t Op, op_t NextOp, op_t... Ops, typename... Args>
    sel_t& alloc_sel(options_t opt, sel_t const* prev, locator_t arg, Args... args)
    {
        return alloc_sel<NextOp, Ops...>(opt, alloc_sel<Op>(opt, prev, arg), args...);
    }

    // Represents a list of functions.
    struct cons_t
    {
        std::type_identity_t<void(cpu_t const&, sel_t const*, cons_t const*)>* fn;
        cons_t const* next;

        [[gnu::always_inline]]
        void call(cpu_t const& cpu, sel_t const* sel) const { fn(cpu, sel, next); }
    };

    using cont_t = std::type_identity_t<void(cpu_t const&, sel_t const*, cons_t const*)>*;

    template<cont_t Head, cont_t... Conts>
    struct chain_t
    {
        [[gnu::flatten]]
        explicit chain_t(cons_t const* tail)
        : chain(tail)
        , cons{Head, &chain.cons}
        {}

        chain_t<Conts...> chain;
        cons_t cons;
    };

    template<cont_t Head>
    struct chain_t<Head>
    {   
        [[gnu::always_inline]]
        explicit chain_t(cons_t const* tail)
        : cons{Head, tail}
        {}
        
        cons_t cons;
    };

    // Combines multiple functions into a single one.
    template<cont_t... Conts> [[gnu::flatten]]
    void chain(cpu_t const& cpu, sel_t const* sel, cons_t const* cont)
    {
        chain_t<Conts...> c(cont);
        c.cons.call(cpu, sel);
    }

    // Finishes the selection.
    void finish(cpu_t const& cpu, sel_t const* sel, cons_t const*)
    {
        auto result = state.next_map.insert({ cpu, sel });

        unsigned const sel_cost = get_cost(sel);

        if(!result.second && sel_cost < get_cost(result.first->second))
            result.first->second = sel;

        if(sel_cost < state.next_best_cost)
        {
            state.next_best_cost = sel_cost;
            state.best_sel = sel;
        }
    }

    // Runs the function and adds the results to the state map.
    template<typename Fn>
    void select_step(Fn fn)
    {
        state.next_map.clear();
        state.next_best_cost = ~0 - COST_CUTOFF;

        cons_t const cont = { finish };

        for(auto const& pair : state.map)
            if(get_cost(pair.second) < state.best_cost + COST_CUTOFF)
                fn(pair.first, pair.second, &cont);

        if(state.next_map.empty())
            throw std::runtime_error("Instruction selection failed to make progress.");

        state.map.swap(state.next_map);
        state.best_cost = state.next_best_cost;
    }

    template<typename Opt, regs_t Regs, typename Param>
    void set_regs(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_regs<Regs>(Opt::to_struct, Param::value()))
            cont->call(cpu_copy, prev);
    };

    template<typename Opt, op_t Op, typename Def, typename Arg>
    void set_regs_for(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_regs_for<Op>(Opt::to_struct, Def::value(), Arg::trans()))
            cont->call(cpu_copy, prev);
    };

    constexpr bool can_set_regs_for(options_t opt, op_t op)
    {
        return (opt.can_set & op_output_regs(op) & REGF_CPU) == (op_output_regs(op) & REGF_CPU);
    }

    void clear_conditional(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.conditional_regs & REGF_A)
            cpu_copy.regs[REG_A] = {};
        if(cpu_copy.conditional_regs & REGF_X)
            cpu_copy.regs[REG_X] = {};
        if(cpu_copy.conditional_regs & REGF_Y)
            cpu_copy.regs[REG_Y] = {};
        if(cpu_copy.conditional_regs & REGF_C)
            cpu_copy.regs[REG_C] = {};
        if(cpu_copy.conditional_regs & REGF_Z)
            cpu_copy.regs[REG_Z] = {};
        cpu_copy.conditional_regs = 0;
        cont->call(cpu_copy, prev);
    };

    template<typename Label>
    void label(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cont->call(cpu, &alloc_sel<ASM_LABEL>({}, prev, Label::trans()));
    };

    // TODO
    // Generates an op, picking the addressing mode based on its paramters.
    template<typename Opt, op_name_t OpName, typename Def, typename Arg>
    void pick_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont);

    // Modifies 'cpu' and returns a penalty, handling 'req_store'.

    constexpr bool ssa_addr_mode(addr_mode_t mode)
    {
        switch(mode)
        {
        case MODE_IMPLIED:
        case MODE_IMMEDIATE:
        case MODE_RELATIVE:
        case MODE_LONG:
            return false;
        default:
            return true;
        }
    }

    [[gnu::noinline]]
    unsigned handle_req_store_penalty(cpu_t& cpu, locator_t loc)
    {
        if(loc.lclass() != LOC_SSA)
            return 0;

        ssa_ht h = loc.ssa_node();
        auto& d = cg_data(h);

        if(h->cfg_node() != state.cfg_node && d.isel.store_mask)
            return 0; 

        // Nodes that belong to this CFG node are tracked more
        // precisely using the 'req_store' part of 'cpu_t'.

        cpu.req_store |= d.isel.store_mask;

        unsigned const stores = builtin::popcount(~cpu.req_store & d.isel.store_mask);
        return cost_fn<STA_ABSOLUTE> * stores;
    }

    // Spits out the op specified.
    template<op_t Op>
    void exact_op(options_t opt, locator_t def, locator_t arg,
                  cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
#ifndef NDEBUG 
        switch(op_addr_mode(Op))
        {
        case MODE_IMPLIED:
            assert(!arg);
            break;
        case MODE_RELATIVE:
        case MODE_LONG:
            assert(is_label(arg.lclass()));
            break;
        case MODE_IMMEDIATE:
            assert(arg.is_const_num());
            break;
        case MODE_ZERO_PAGE:
        case MODE_ZERO_PAGE_X:
        case MODE_ZERO_PAGE_Y:
        case MODE_ABSOLUTE:
        case MODE_ABSOLUTE_X:
        case MODE_ABSOLUTE_Y:
        case MODE_INDIRECT:
        case MODE_INDIRECT_X:
        case MODE_INDIRECT_Y:
            assert(arg);
            break;
        default:
            break;
        }
#endif
        cpu_t cpu_copy = cpu;

        unsigned penalty = 0;
        if(ssa_addr_mode(op_addr_mode(Op)))
            penalty = handle_req_store_penalty(cpu_copy, arg);
        if(cpu_copy.set_regs_for<Op>(opt, def, arg))
            cont->call(cpu_copy, &alloc_sel<Op>(opt, prev, arg, penalty));
    }

    template<typename Opt, op_t Op, typename Def = null_, typename Arg = null_>
    void exact_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        exact_op<Op>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
    }

    // Generates an op using the 0..255 table.
    template<typename Opt, op_t Op, typename Def = null_>
    void iota_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        static_assert(op_addr_mode(Op) == MODE_ABSOLUTE_X || op_addr_mode(Op) == MODE_ABSOLUTE_Y);
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_output_regs<Op>(Opt::to_struct, Def::value()))
            cont->call(cpu_copy, &alloc_sel<Op>(Opt::to_struct, prev, locator_t::iota()));
    };

    template<typename Opt, typename Def>
    void load_Z(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(cpu.reg_eq(REG_Z, Def::value()))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_Z))
            return;
        else if(cpu.reg_eq(REG_A, Def::value()))
        {
            exact_op<EOR_IMMEDIATE>(
                Opt::template mask<REGF_A | REGF_X>::template unrestrict<REGF_A>::to_struct, 
                Def::value(), locator_t::const_byte(0),
                cpu, prev, cont);

            exact_op<TAX_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);

            exact_op<TYA_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        }
        else if(cpu.reg_eq(REG_X, Def::value()))
        {
            chain
            < exact_op<typename Opt::mask<REGF_Z | REGF_X>::unrestrict<REGF_X>, INX_IMPLIED>
            , exact_op<typename Opt::mask<REGF_Z | REGF_X>::unrestrict<REGF_X>, DEX_IMPLIED, Def>
            >(cpu, prev, cont);

            exact_op<CPX_IMMEDIATE>(
                Opt::template mask<REGF_X>::to_struct, 
                Def::value(), locator_t::const_byte(0),
                cpu, prev, cont);

            exact_op<TXA_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        }
        else if(cpu.reg_eq(REG_Y, Def::value()))
        {
            chain
            < exact_op<typename Opt::template mask<REGF_Z | REGF_X>::template unrestrict<REGF_Y>, INY_IMPLIED>
            , exact_op<typename Opt::template mask<REGF_Z | REGF_X>::template unrestrict<REGF_Y>, DEY_IMPLIED, Def>
            >(cpu, prev, cont);

            exact_op<CPY_IMMEDIATE>(
                Opt::template mask<REGF_Y>::to_struct, 
                Def::value(), locator_t::const_byte(0),
                cpu, prev, cont);

            exact_op<TYA_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        }
        else
        {
            pick_op<Opt, LDA, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LDX, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LDY, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Def>(cpu, prev, cont);
        }
    }
    
    [[gnu::noinline]]
    void load_A_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(value.is_const_num())
        {
            cpu_t cpu_copy;

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<ANC_IMMEDIATE>(opt, {}, value)
            && cpu_copy.regs[REG_A] == value)
                cont->call(cpu_copy, &alloc_sel<ANC_IMMEDIATE>(opt, prev, value));

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<LSR_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_A] == value)
                cont->call(cpu_copy, &alloc_sel<LSR_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<ASL_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_A] == value)
                cont->call(cpu_copy, &alloc_sel<ASL_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<ROL_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_A] == value)
                cont->call(cpu_copy, &alloc_sel<ROL_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<ROR_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_A] == value)
                cont->call(cpu_copy, &alloc_sel<ROR_IMPLIED>(opt, prev));

            if(cpu.regs[REG_A].is_const_num())
            {
                unsigned mask = value.data() << 1;
                if((mask & cpu.regs[REG_A].data() & 0xFF) == mask)
                {
                    assert(mask < 0x100);
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        locator_t loc = locator_t::const_byte(mask | i);
                        cpu_copy = cpu;
                        if(cpu_copy.set_regs_for<ALR_IMMEDIATE>(opt, {}, value)
                        && cpu_copy.regs[REG_A] == value)
                            cont->call(cpu_copy, &alloc_sel<ALR_IMMEDIATE>(opt, prev, value));

                        if((cpu.regs[REG_A].data() & 1) == 0)
                            break;
                    }
                }
            }
        }
    }

    template<typename Opt, typename Def>
    void load_A(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(cpu.reg_eq(REG_A, Def::value()))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_A))
            return;
        else if(cpu.reg_eq(REG_X, Def::value()))
            exact_op<TXA_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else if(cpu.reg_eq(REG_Y, Def::value()))
            exact_op<TYA_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else
        {
            pick_op<Opt, LDA, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Def>(cpu, prev, cont);

            load_A_impl(Opt::template mask<REGF_A>::to_struct, Def::value(), cpu, prev, cont);
        }
    }

    [[gnu::noinline]]
    void load_X_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(value.is_const_num())
        {
            cpu_t cpu_copy;

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<INX_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_X] == value)
                cont->call(cpu_copy, &alloc_sel<INX_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<DEX_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_X] == value)
                cont->call(cpu_copy, &alloc_sel<DEX_IMPLIED>(opt, prev));
        }
    }

    template<typename Opt, typename Def>
    void load_X(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(cpu.reg_eq(REG_X, Def::value()))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_X))
            return;
        else if(cpu.reg_eq(REG_A, Def::value()))
            exact_op<TAX_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else
        {
            if(cpu.reg_eq(REG_Y, Def::value()))
            {
                chain
                < exact_op<Opt, TYA_IMPLIED>
                , exact_op<Opt, TAY_IMPLIED, Def>
                >(cpu, prev, cont);
            }

            pick_op<Opt, LDX, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Def>(cpu, prev, cont);

            load_X_impl(Opt::template mask<REGF_X>::to_struct, Def::value(), cpu, prev, cont);
        }
    }

    [[gnu::noinline]]
    void load_Y_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(value.is_const_num())
        {
            cpu_t cpu_copy;

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<INY_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_X] == value)
                cont->call(cpu_copy, &alloc_sel<INY_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_regs_for<DEY_IMPLIED>(opt, {}, {})
            && cpu_copy.regs[REG_Y] == value)
                cont->call(cpu_copy, &alloc_sel<DEY_IMPLIED>(opt, prev));
        }
    }

    template<typename Opt, typename Def>
    void load_Y(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(cpu.reg_eq(REG_Y, Def::value()))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_Y))
            return;
        else if(cpu.reg_eq(REG_A, Def::value()))
            exact_op<TAY_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else
        {
            if(cpu.reg_eq(REG_X, Def::value()))
            {
                chain
                < exact_op<Opt, TXA_IMPLIED>
                , exact_op<Opt, TAX_IMPLIED, Def>
                >(cpu, prev, cont);
            }

            pick_op<Opt, LDY, Def, Def>(cpu, prev, cont);

            load_Y_impl(Opt::template mask<REGF_Y>::to_struct, Def::value(), cpu, prev, cont);
        }
    }

    template<typename Opt, typename Def>
    void load_C(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(cpu.reg_eq(REG_C, Def::value()))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_C))
            return;
        else if(Def::value().is_const_num())
        {
            if(Def::value().data())
                exact_op<SEC_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
            else
                exact_op<CLC_IMPLIED>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        }
        /* TODO: this may not be valid.
        else if(cpu.reg_eq(REG_Z, Def::value()))
        {
            if(cpu.reg_eq(REG_C, locator_t::const_byte(0)))
            {
                chain
                < exact_op<Opt, BEQ_IMPLIED, none_, const_<1>>
                , exact_op<typename Opt::add_flag<OPT_CONDITIONAL>, SEC_IMPLIED>
                , set_regs<Opt, REGF_C, Def>
                >(cpu, prev, cont);
            }
            else if(cpu.reg_eq(REG_C, locator_t::const_byte(1)))
            {
                chain
                < exact_op<Opt, BNE_IMPLIED, none_, const_<1>>
                , exact_op<typename Opt::add_flag<OPT_CONDITIONAL>, CLC_IMPLIED>
                , set_regs<Opt, REGF_C, Def>
                >(cpu, prev, cont);
            }
            else
            {
                chain
                < exact_op<Opt, CLC_IMPLIED>
                , exact_op<Opt, BNE_IMPLIED, none_, const_<1>>
                , exact_op<typename Opt::add_flag<OPT_CONDITIONAL>, SEC_IMPLIED>
                , set_regs<Opt, REGF_C, Def>
                >(cpu, prev, cont);
            }
        }
        */
        else
        {
            chain
            < load_A<Opt, Def>
            , exact_op<typename Opt::mask<REGF_C>, LSR_IMPLIED, Def>
            >(cpu, prev, cont);

            chain
            < load_A<Opt, Def>
            , exact_op<typename Opt::mask<REGF_C>, ALR_IMMEDIATE, Def, const_<1>>
            >(cpu, prev, cont);
        }
    }

    template<typename Opt, typename A, typename X>
    void load_AX(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        chain
        < load_A<Opt, A>
        , load_X<typename Opt::restrict_to<~REGF_A>, X>
        >(cpu, prev, cont);

        chain
        < load_X<Opt, X>
        , load_A<typename Opt::restrict_to<~REGF_X>, A>
        >(cpu, prev, cont);
    };

    template<typename Opt, typename A, typename Y>
    void load_AY(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        chain
        < load_A<Opt, A>
        , load_Y<typename Opt::restrict_to<~REGF_A>, Y>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, Y>
        , load_A<typename Opt::restrict_to<~REGF_Y>, A>
        >(cpu, prev, cont);
    };

    template<typename Opt, typename A, typename C>
    void load_AC(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        chain
        < load_C<Opt, C>
        , load_A<typename Opt::restrict_to<~REGF_C>, A>
        >(cpu, prev, cont);
    }

    template<typename Opt, typename Def, typename Arg, op_t AbsoluteX, op_t AbsoluteY, bool Enable = (AbsoluteX || AbsoluteY) && (Opt::flags & OPT_NO_DIRECT)>
    struct pick_op_xy
    {
        static void call(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
        {
            using OptN = typename Opt::add_flags<OPT_NO_DIRECT>;

            if(AbsoluteX)
            {
                chain
                < load_X<OptN, array_index<Arg>>
                , exact_op<Opt, AbsoluteX, Def, array_mem<Arg>>
                >(cpu, prev, cont);
            }

            if(AbsoluteY)
            {
                chain
                < load_Y<OptN, array_index<Arg>>
                , exact_op<Opt, AbsoluteY, Def, array_mem<Arg>>
                >(cpu, prev, cont);
            }
        }
    };

    template<typename Opt, typename Def, typename Arg, op_t AbsoluteX, op_t AbsoluteY>
    struct pick_op_xy<Opt, Def, Arg, AbsoluteX, AbsoluteY, false>
    {
        static void call(cpu_t const& cpu, sel_t const* prev, cons_t const* cont) {}
    };


    // pick_op impl
    template<typename Opt, op_name_t OpName, typename Def, typename Arg>
    void pick_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        constexpr op_t implied    = get_op(OpName, MODE_IMPLIED);
        constexpr op_t relative   = get_op(OpName, MODE_RELATIVE);
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);
        constexpr op_t absolute_X = get_op(OpName, MODE_ABSOLUTE_X);
        constexpr op_t absolute_Y = get_op(OpName, MODE_ABSOLUTE_Y);

        if(implied && !Arg::trans())
            exact_op<implied>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else if(relative)
            exact_op<relative>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if(immediate && Arg::trans().is_const_num())
            exact_op<immediate>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if((absolute_X || absolute_Y) && Arg::node().holds_ref() && Arg::node()->op() == SSA_cg_read_array_direct)
            pick_op_xy<Opt, Def, Arg, absolute_X, absolute_Y>::call(cpu, prev, cont);
        else if(absolute && !Arg::trans().is_const_num())
            exact_op<absolute>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
    }

    // Adds a store operation.
    // 'Maybe' means the store may not be required in the final code;
    // such instructions can be pruned later.
    template<typename Opt, op_name_t StoreOp, typename Param, bool Maybe = true>
    void store(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        // Store the node, locally:
        if(Maybe && Param::trans().lclass() == LOC_SSA)
        {
            switch(StoreOp)
            {
            case STA: cont->call(cpu, &alloc_sel<MAYBE_STA>(Opt::to_struct, prev, Param::trans())); break;
            case STX: cont->call(cpu, &alloc_sel<MAYBE_STX>(Opt::to_struct, prev, Param::trans())); break;
            case STY: cont->call(cpu, &alloc_sel<MAYBE_STY>(Opt::to_struct, prev, Param::trans())); break;
            case SAX: cont->call(cpu, &alloc_sel<MAYBE_SAX>(Opt::to_struct, prev, Param::trans())); break;
            default: assert(false); break;
            }
        }
        else
        {
            cpu_t new_cpu = cpu;

            if(Param::trans().lclass() == LOC_SSA)
            {
                ssa_ht h = Param::trans().ssa_node();
                auto& d = cg_data(h);

                if(h->cfg_node() == state.cfg_node)
                    new_cpu.req_store |= d.isel.store_mask;
            }

            cont->call(new_cpu, &alloc_sel<get_op(StoreOp, MODE_ABSOLUTE)>(Opt::to_struct, prev, Param::trans()));
        }
    }

    template<typename Opt, typename Def, typename Load, typename Store>
    void load_then_store(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        chain
        < load_A<Opt, Load>
        , set_regs<Opt, REGF_A, Def>
        , store<Opt, STA, Store>
        >(cpu, prev, cont);

        chain
        < load_X<Opt, Load>
        , set_regs<Opt, REGF_X, Def>
        , store<Opt, STX, Store>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, Load>
        , set_regs<Opt, REGF_Y, Def>
        , store<Opt, STY, Store>
        >(cpu, prev, cont);
    };

    static void no_effect(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cont->call(cpu, prev);
    }

    template<typename Opt, typename Condition, cont_t Then, cont_t Else = nullptr>
    static void if_(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cons_t c = { nullptr, cont };
        
        if(Condition::value())
            c.fn = Then ? Then : no_effect;
        else
            c.fn = Else ? Then : no_effect;

        c.call(cpu, prev);
    }

    template<typename Tag>
    struct condition
    {
        static inline thread_local bool b;
        static void set(bool s) { b = s; }
        static bool value() { return b; }
    };

    template<typename Tag, unsigned I>
    struct i_tag {};

    using p_def = param<struct def_tag>;
    template<unsigned I>
    using p_arg = param<i_tag<struct arg_tag, I>>;
    using p_lhs = p_arg<0>;
    using p_rhs = p_arg<1>;
    using p_carry = param<struct carry_tag>;
    using p_temp = param<struct temp_tag>;
    template<unsigned I>
    using p_label = param<i_tag<struct label_tag, I>>;
    using p_condition = condition<struct condition_tag>;

    template<typename Opt, op_name_t BranchOp, typename FailLabel, typename SuccessLabel>
    void eq_branch(ssa_ht h)
    {
        using OptC = typename Opt::add_flags<OPT_CONDITIONAL>;
        constexpr op_t InverseOp = get_op(invert_branch(BranchOp), MODE_RELATIVE);

        for(unsigned i = 0; i < h->input_size(); i += 2)
        {
            select_step([&, i](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
            {
                for(unsigned j = 0; j < 2; ++j)
                {
                    p_lhs::set(h->input(i + j));
                    p_rhs::set(h->input(i + 1-j));
                    
                    if(p_rhs::value().eq_const_byte(0))
                    {
                        chain
                        < load_Z<OptC, p_lhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);
                    }
                    else
                    {
                        chain
                        < load_A<OptC, p_lhs>
                        , pick_op<OptC, CMP, null_, p_rhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_X<OptC, p_lhs>
                        , pick_op<OptC, CPX, null_, p_rhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_Y<OptC, p_lhs>
                        , pick_op<OptC, CPY, null_, p_rhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);
                    }
                }
            });
        }

        constexpr op_t Op = get_op(BranchOp, MODE_RELATIVE);
        select_step(
            chain
            < exact_op<OptC, Op, null_, SuccessLabel>
            , clear_conditional
            >);
    }

    /* TODO
    template<op_name_t BranchOp>
    void eq_store(ssa_ht h)
    {
        using fail = p_label<0>;
        using success = p_label<1>;
        using complete = p_label<2>;

        using O = options<>;

        fail::set(state.minor_label());
        success::set(state.minor_label());
        complete::set(state.minor_label());

        select_step([&](cpu_t cpu, sel_t const* const prev)
        {
            exact_op<O, LDA_IMMEDIATE, null_, const_<0>>
            ::template run<finish>(cpu, prev);

            exact_op<O, LDX_IMMEDIATE, null_, const_<0>>
            ::template run<finish>(cpu, prev);

            exact_op<O, LDY_IMMEDIATE, null_, const_<0>>
            ::template run<finish>(cpu, prev);

            finish::run(cpu, prev);
        });

        using OC = O::add_flags<OPT_CONDITIONAL>;
        eq_branch<options<>, BranchOp, fail, success>(h);

        p_def::set(h);
        
        select_step([&](cpu_t cpu, sel_t const* const prev)
        {
            if(cpu.regs[REG_X].eq_const_byte(0) && (O::can_set & REGF_X))
            {
                chain
                < label<OC, success>
                , exact_op<OC, INX_IMPLIED>
                , clear_conditional
                , label<O, fail>
                , set_regs<O, REGF_X, p_def>
                , store<O, STX, p_def>
                >::template run<finish>(cpu, prev);
            }
            else if(cpu.regs[REG_Y].eq_const_byte(0) && (O::can_set & REGF_Y))
            {
                chain
                < label<OC, success>
                , exact_op<OC, INY_IMPLIED>
                , clear_conditional
                , label<O, fail>
                , set_regs<O, REGF_Y, p_def>
                , store<O, STY, p_def>
                >::template run<finish>(cpu, prev);
            }
            else if(cpu.regs[REG_A].eq_const_byte(0) && (O::can_set & REGF_A))
            {
                chain
                < label<OC, success>
                , exact_op<OC, LDA_IMMEDIATE, null_, const_<1>>
                , clear_conditional
                , label<O, fail>
                , set_regs<O, REGF_Y, p_def>
                , store<O, STY, p_def>
                >::template run<finish>(cpu, prev);
            }
            else
            {
                chain
                < label<OC, fail>
                , exact_op<OC, LDA_ABSOLUTE, null_, const_<0>>
                , exact_op<OC, JMP_ABSOLUTE, null_, complete>
                , clear_conditional
                , label<OC, success>
                , exact_op<OC, LDA_ABSOLUTE, null_, const_<1>>
                , label<OC, complete>
                , clear_conditional
                , set_regs<O, REGF_A, p_def>
                , store<options<>, STA, p_def>
                >::template run<finish>(cpu, prev);

                chain
                < label<OC, fail>
                , exact_op<OC, LDX_ABSOLUTE, null_, const_<0>>
                , exact_op<OC, JMP_ABSOLUTE, null_, complete>
                , clear_conditional
                , label<OC, success>
                , exact_op<OC, LDX_ABSOLUTE, null_, const_<1>>
                , label<OC, complete>
                , clear_conditional
                , set_regs<O, REGF_X, p_def>
                , store<options<>, STX, p_def>
                >::template run<finish>(cpu, prev);

                chain
                < label<OC, fail>
                , exact_op<OC, LDY_ABSOLUTE, null_, const_<0>>
                , exact_op<OC, JMP_ABSOLUTE, null_, complete>
                , clear_conditional
                , label<OC, success>
                , exact_op<OC, LDY_ABSOLUTE, null_, const_<1>>
                , label<OC, complete>
                , clear_conditional
                , set_regs<O, REGF_Y, p_def>
                , store<options<>, STY, p_def>
                >::template run<finish>(cpu, prev);
            }
        });
    }
    */

    template<typename Opt, typename FailLabel, typename SuccessLabel>
    void lt_branch(ssa_ht h)
    {
        //if(OrEqual)
            //std::swap(fail_label, success_label);

        using OptC = typename Opt::add_flags<OPT_CONDITIONAL>;

        int const input_size = h->input_size();
        assert(input_size >= 2);
        assert(input_size % 2 == 0);

        using last_iter = condition<struct lt_last_iter_tag>;
        using next_label = param<struct lt_next_label_tag>;

        for(int i = input_size - 2; i >= 0; i -= 2)
        {
            last_iter::set(i == 0);
            next_label::set(state.minor_label());

            select_step([&, i](cpu_t cpu, sel_t const* const prev, cons_t const* cont)
            {
                p_lhs::set(h->input(i + 0));
                p_rhs::set(h->input(i + 1));
                
                if(p_lhs::value().eq_const_byte(0))
                {
                    chain
                    < load_Z<OptC, p_rhs>
                    , exact_op<OptC, BNE_RELATIVE, null_, SuccessLabel>
                    , if_<OptC, last_iter, exact_op<OptC, BEQ_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);
                }
                else if(p_rhs::value().eq_const_byte(0))
                {
                    if(i == 0)
                        exact_op<JMP_ABSOLUTE>(OptC::to_struct, {}, FailLabel::trans(), cpu, prev, cont);
                    else
                        chain
                        < load_Z<OptC, p_lhs>
                        , exact_op<OptC, BNE_RELATIVE, null_, FailLabel>
                        >(cpu, prev, cont);
                }
                else
                {
                    constexpr cont_t normal = &chain
                        < (cont_t)&if_<OptC, last_iter, nullptr, (cont_t)&exact_op<OptC, BEQ_RELATIVE, null_, next_label> >
                        , (cont_t)&exact_op<OptC, BCS_RELATIVE, null_, FailLabel>
                        , (cont_t)&exact_op<OptC, BCC_RELATIVE, null_, SuccessLabel>
                        , (cont_t)&if_<OptC, last_iter, nullptr, label<next_label>>
                        >;

                    constexpr cont_t inverted = &chain
                        < if_<OptC, last_iter, exact_op<OptC, BEQ_RELATIVE, null_, FailLabel>,
                                               exact_op<OptC, BEQ_RELATIVE, null_, next_label>>
                        , exact_op<OptC, BCC_RELATIVE, null_, FailLabel>
                        , exact_op<OptC, BCS_RELATIVE, null_, SuccessLabel>
                        , if_<OptC, last_iter, nullptr, label<next_label>>
                        >;

                    chain
                    < load_A<OptC, p_lhs>
                    , pick_op<OptC, CMP, null_, p_rhs>
                    , normal
                    >(cpu, prev, cont);

                    chain
                    < load_A<OptC, p_rhs>
                    , pick_op<OptC, CMP, null_, p_lhs>
                    , inverted
                    >(cpu, prev, cont);

                    chain
                    < load_X<OptC, p_lhs>
                    , pick_op<OptC, CPX, null_, p_rhs>
                    , normal
                    >(cpu, prev, cont);

                    chain
                    < load_X<OptC, p_rhs>
                    , pick_op<OptC, CPX, null_, p_lhs>
                    , inverted
                    >(cpu, prev, cont);

                    chain
                    < load_Y<OptC, p_lhs>
                    , pick_op<OptC, CPY, null_, p_rhs>
                    , normal
                    >(cpu, prev, cont);

                    chain
                    < load_Y<OptC, p_rhs>
                    , pick_op<OptC, CPY, null_, p_lhs>
                    , inverted
                    >(cpu, prev, cont);
                }
            });
        }

        // TODO
        select_step(
            chain
            //< exact_op<OC, BCC_RELATIVE, null_, SuccessLabel>
            < clear_conditional
            >);
    }

    template<typename Opt>
    void write_globals(ssa_ht h)
    {
        for_each_written_global(h, [h](ssa_value_t def, locator_t loc)
        {
            if(def.is_handle() && cset_locator(def.handle()) == loc)
                return;

            p_def::set(def);
            p_arg<0>::set(loc);

            select_step(load_then_store<Opt, p_def, p_def, p_arg<0>>);
        });
    }

    void isel_node_simple(ssa_ht h, cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        auto const commutative = [](ssa_ht h, auto fn)
        {
            p_lhs::set(h->input(0));
            p_rhs::set(h->input(1));
            fn();

            p_lhs::set(h->input(1));
            p_rhs::set(h->input(0));
            fn();
        };

        cfg_ht const cfg_node = h->cfg_node();
        p_def::set(h);

        using Opt = options<>;
        using OptC = Opt::add_flags<OPT_CONDITIONAL>;

        switch(h->op())
        {
        case SSA_carry:
            if(h->output_size() > 1 || (h->output_size() == 1 && h->output(0)->cfg_node() != h->cfg_node()))
            {
                p_label<0>::set(state.minor_label());

                chain
                < exact_op<Opt, AND_IMMEDIATE, null_, const_<0>>
                , exact_op<Opt::mask<REGF_A>, ROL_IMPLIED, p_def>
                , store<Opt, STA, p_def, false>
                >(cpu, prev, cont);

                chain
                < load_X<Opt, const_<0>>
                , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<OptC, INX_IMPLIED>
                , label<p_label<0>>
                , clear_conditional
                , set_regs<Opt, REGF_X | REGF_C, p_def>
                , store<Opt, STX, p_def, false>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, const_<0>>
                , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<OptC, INY_IMPLIED>
                , label<p_label<0>>
                , clear_conditional
                , set_regs<Opt, REGF_Y | REGF_C, p_def>
                , store<Opt, STY, p_def, false>
                >(cpu, prev, cont);
            }
            else
            {
                cpu_t new_cpu = cpu;
                if(new_cpu.set_reg<REG_C>(Opt::to_struct, p_def::value()))
                    cont->call(new_cpu, &alloc_sel<MAYBE_STORE_C>(Opt::to_struct, prev, p_def::trans()));
            }
            break;

        case SSA_add:
            p_lhs::set(h->input(0));
            p_rhs::set(h->input(1));
            p_carry::set(h->input(2));

            // TODO: This should be a math identity, right?
            if(p_lhs::value() == p_rhs::value())
            {
                chain
                < load_AC<Opt, p_lhs, p_carry>
                , exact_op<Opt::mask<REGF_A | REGF_Z>, ROL_IMPLIED, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_carry::value().eq_const_byte(0))
                    chain
                    < load_A<Opt, p_lhs>
                    , exact_op<Opt::mask<REGF_A | REGF_Z>, ASL_IMPLIED, p_def>
                    , store<Opt, STA, p_def>
                    >(cpu, prev, cont);
            }

            commutative(h, [&]()
            {
                chain
                < load_AC<Opt, p_lhs, p_carry>
                , pick_op<Opt::mask<REGF_A | REGF_Z>, ADC, p_def, p_rhs>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AC<Opt, p_lhs, p_carry>
                , load_X<Opt::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<Opt::mask<REGF_A | REGF_Z>, ADC_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AC<Opt, p_lhs, p_carry>
                , load_Y<Opt::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<Opt::mask<REGF_A | REGF_Z>, ADC_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_rhs::value().is_const_num())
                {
                    p_label<0>::set(state.minor_label());

                    if(p_rhs::value().data() == 0 && carry_output_i(*h) == -1)
                    {
                        chain
                        < load_C<Opt, p_carry>
                        , load_X<Opt, p_lhs>
                        , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, INX_IMPLIED>
                        , label<p_label<0>>
                        , clear_conditional
                        , set_regs<Opt, REGF_X, p_def>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_C<Opt, p_carry>
                        , load_Y<Opt, p_lhs>
                        , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, INY_IMPLIED>
                        , label<p_label<0>>
                        , clear_conditional
                        , set_regs<Opt, REGF_Y, p_def>
                        , store<Opt, STY, p_def>
                        >(cpu, prev, cont);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                            , pick_op<OptC, INC, p_def, p_def>
                            , label<p_label<0>>
                            , clear_conditional
                            >(cpu, prev, cont);
                        }
                    }

                    if(p_rhs::value().data() == 0xFF && carry_output_i(*h) == -1)
                    {
                        p_label<0>::set(state.minor_label());

                        chain
                        < load_C<Opt, p_carry>
                        , load_X<Opt, p_lhs>
                        , exact_op<Opt, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, DEX_IMPLIED>
                        , label<p_label<0>>
                        , clear_conditional
                        , set_regs<Opt, REGF_X, p_def>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_C<Opt, p_carry>
                        , load_Y<Opt, p_lhs>
                        , exact_op<Opt, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, DEY_IMPLIED>
                        , label<p_label<0>>
                        , clear_conditional
                        , set_regs<Opt, REGF_Y, p_def>
                        , store<Opt, STY, p_def>
                        >(cpu, prev, cont);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<Opt, BCS_RELATIVE, null_, p_label<0>>
                            , pick_op<OptC, DEC, p_def, p_def>
                            , label<p_label<0>>
                            , clear_conditional
                            >(cpu, prev, cont);
                        }
                    }

                    if(p_carry::value().is_const_num())
                    {
                        p_temp::set((0x100 - p_rhs::value().data() - !!p_carry::value().data()) & 0xFF);

                        chain
                        < load_AX<Opt, p_lhs, p_lhs>
                        , exact_op<Opt::mask<REGF_X | REGF_Z>, AXS_IMMEDIATE, p_def, p_temp>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        if((p_rhs::value().data() + !!p_carry::value().data()) == 1 && carry_output_i(*h) == -1)
                        {
                            chain
                            < load_X<Opt, p_lhs>
                            , exact_op<Opt, INX_IMPLIED, p_def>
                            , store<Opt, STX, p_def>
                            >(cpu, prev, cont);

                            chain
                            < load_Y<Opt, p_lhs>
                            , exact_op<Opt, INY_IMPLIED, p_def>
                            , store<Opt, STY, p_def>
                            >(cpu, prev, cont);

                            if(p_def::trans() == p_lhs::trans())
                                pick_op<Opt, INC, p_def, p_def>(cpu, prev, cont);
                        }

                        if(((p_rhs::value().data() + !!p_carry::value().data()) & 0xFF) == 0xFF && carry_output_i(*h) == -1)
                        {
                            chain
                            < load_X<Opt, p_lhs>
                            , exact_op<Opt, DEX_IMPLIED, p_def>
                            , store<Opt, STX, p_def>
                            >(cpu, prev, cont);

                            chain
                            < load_Y<Opt, p_lhs>
                            , exact_op<Opt, DEY_IMPLIED, p_def>
                            , store<Opt, STY, p_def>
                            >(cpu, prev, cont);

                            if(p_def::trans() == p_lhs::trans())
                                pick_op<Opt, DEC, p_def, p_def>(cpu, prev, cont);
                        }
                    }
                }
            });
            break;

        case SSA_and:
            commutative(h, [&]()
            {
                chain
                < load_A<Opt, p_lhs>
                , pick_op<Opt, AND, p_def, p_rhs>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , exact_op<Opt::mask<REGF_X | REGF_Z>, AXS_IMMEDIATE, p_def, const_<0>>
                , store<Opt, STX, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , iota_op<Opt, AND_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, p_lhs, p_rhs>
                , iota_op<Opt, AND_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , store<Opt, SAX, p_def, false>
                >(cpu, prev, cont);

                // TODO: consider using subtraction instructions,
                // checking constraints when it's applicable.
            });
            break;

        case SSA_or:
            commutative(h, [&]()
            {
                chain
                < load_A<Opt, p_lhs>
                , pick_op<Opt, ORA, p_def, p_rhs>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , iota_op<Opt, ORA_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, p_lhs, p_rhs>
                , iota_op<Opt, ORA_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                // TODO: consider using add instructions, 
                // checking constraints when it's applicable.
            });
            break;

        case SSA_early_store:
            p_arg<0>::set(h->input(0));
            load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            break;

        case SSA_read_global:
            if(h->input(1).locator() != cset_locator(h))
            {
                p_arg<0>::set(h->input(1));
                load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            }
            else
                cont->call(cpu, prev);
            break;

        case SSA_phi_copy:
            if(!h->input(0).holds_ref() || cset_head(h) != cset_head(h->input(0).handle()))
            {
                p_arg<0>::set(h->input(0));
                load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            }
            else
                cont->call(cpu, prev);
            break;

        case SSA_phi:
            assert(h->input_size() > 0);
            if(cset_head(h) != cset_head(h->input(0).handle()))
            {
                p_arg<0>::set(h->input(0));
                load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            }
            else
                cont->call(cpu, prev);
            break;

        case SSA_write_array:
            if(h->input(0).holds_ref() && cset_head(h->input(0).handle()) != cset_head(h))
            {
                // TODO! Insert an array copy here.
                throw std::runtime_error("Unimplemented array copy.");
            }
            else
            {
                using p_index = p_arg<0>;
                using p_assignment = p_arg<1>;

                p_index::set(h->input(2));
                p_assignment::set(h->input(3));

                chain
                < load_AX<Opt, p_assignment, p_index>
                , exact_op<Opt, STA_ABSOLUTE_X, null_, p_def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, p_assignment, p_index>
                , exact_op<Opt, STA_ABSOLUTE_Y, null_, p_def>
                >(cpu, prev, cont);
            }
            break;

        case SSA_read_array:
            {
                using p_index = p_arg<0>;

                p_index::set(h->input(2));

                chain
                < load_X<Opt, p_index>
                , exact_op<Opt, LDA_ABSOLUTE_X, p_def, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_X<Opt, p_index>
                , exact_op<Opt, LDY_ABSOLUTE_X, p_def, p_def>
                , store<Opt, STY, p_def>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, p_index>
                , exact_op<Opt, LDA_ABSOLUTE_Y, p_def, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, p_index>
                , exact_op<Opt, LDX_ABSOLUTE_Y, p_def, p_def>
                , store<Opt, STX, p_def>
                >(cpu, prev, cont);
            }

            break;

        case SSA_fn_call:
            p_arg<0>::set(h->input(0));
            chain
            < exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<0>>
            , set_regs<Opt, REGF_CPU, null_>
            >(cpu, prev, cont);
            break;

        case SSA_goto_mode:
            p_arg<0>::set(h->input(0));
            chain
            < exact_op<Opt, JMP_ABSOLUTE, null_, p_arg<0>>
            , set_regs<Opt, REGF_CPU, null_>
            >(cpu, prev, cont);
            break;

        case SSA_return:
            exact_op<Opt, RTS_IMPLIED>(cpu, prev, cont);
            break;

        case SSA_jump:
            assert(cfg_node->output_size() == 1);
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            exact_op<Opt, JMP_ABSOLUTE, null_, p_label<0>>(cpu, prev, cont);
            break;

            /* TODO: remove?
        case SSA_if:
            (load_Z<>{ h->input(0) }
            >>= def_op<BNE>{ {}, locator_t::cfg_label(cfg_node->output(0)) }
            >>= def_op<BEQ>{ {}, locator_t::cfg_label(cfg_node->output(1)) }
            >>= finish)(cpu, prev);
            break;
            */

        case SSA_eq:
        case SSA_not_eq:
        case SSA_branch_eq:
        case SSA_branch_not_eq:
        case SSA_branch_lt:
        case SSA_entry:
        case SSA_aliased_store:
        case SSA_uninitialized:
        case SSA_cg_read_array_direct:
            cont->call(cpu, prev);
            break;
        default:
            throw std::runtime_error(fmt("Unhandled SSA op in code gen: %i", h->op()));
        }
    }

    void isel_node(ssa_ht h)
    {
        state.ssa_op = h->op();
        std::cout << "doing op " << state.ssa_op << std::endl;

        using Opt = options<>;

        cfg_ht const cfg_node = h->cfg_node();

        switch(h->op())
        {
        case SSA_eq:     
            throw 0; // TODO
            //eq_store<BEQ>(h); 
            break;

        case SSA_not_eq: 
            throw 0; // TODO
            //eq_store<BNE>(h); 
            break;

        // Branch ops jump directly:
        case SSA_branch_eq:
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(state.fn, cfg_node->output(1)));
            eq_branch<Opt, BEQ, p_label<0>, p_label<1>>(h);
            break;
        case SSA_branch_not_eq:
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(state.fn, cfg_node->output(1)));
            eq_branch<Opt, BNE, p_label<0>, p_label<1>>(h);
            break;

        case SSA_branch_lt:
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(state.fn, cfg_node->output(1)));
            lt_branch<Opt, p_label<0>, p_label<1>>(h);
            break;

            /*
        case SSA_branch_lte:
            lt_branch<true>(h, locator_t::cfg_label(cfg_node->output(0)), 
                               locator_t::cfg_label(cfg_node->output(1)));
            break;
            */

        case SSA_return:
        case SSA_fn_call:
        case SSA_goto_mode:
            write_globals<Opt>(h);
            break;

        default: 
            break;
        }

        select_step([h](cpu_t cpu, sel_t const* prev, cons_t const* cont)
        {
            isel_node_simple(h, cpu, prev, cont);
        });
    }

} // namespace isel

std::vector<asm_inst_t> select_instructions(fn_t const& fn, cfg_ht cfg_node)
{
    using namespace isel;

    state.fn = fn.handle();
    auto& cd = cg_data(cfg_node);

    //////////////////////////////
    // SETUP THE ROLLING WINDOW //
    //////////////////////////////

    // A bitset is used to track which variables have been stored.
    // To shrink the bitset size down to 64 bits, a rolling window is used
    // based around the live ranges occuring within a single CFG node.

    // This code finds that rolling window and allocates each node to a bit:
    std::uint64_t free = ~0ull; // Set of availible bits to use
    for(ssa_ht h : cd.schedule)
    {
        if(free)
        {
            std::uint64_t const allocated = 1ull << builtin::ctz(free);
            assert((free & allocated) == allocated);
            free ^= allocated;

            ssa_ht last_use = h;
            for(unsigned j = 0; j < h->output_size(); ++j)
            {
                ssa_ht const output = h->output(j);
                // Assume that all stores used across basic block
                // boundaries must be stored. 
                // (They may not be in the final generated code.)
                // This is highly pessimistic, but simplifies the code gen.
                if(output->cfg_node() != h->cfg_node())
                    goto skip;

                if(cg_data(output).schedule.index > cg_data(last_use).schedule.index)
                    last_use = output;
            }

            cg_data(last_use).isel.last_use |= allocated;
            cg_data(h).isel.store_mask = allocated;
        }
    skip:
        // Reclaim bits after the last use has been seen:
        assert((free & cg_data(h).isel.last_use) == 0);
        free |= cg_data(h).isel.last_use;
    }

    ///////////////////////
    // DO THE SELECTIONS //
    ///////////////////////

    state.sel_pool.clear();
    state.map.clear();
    assert(state.map.empty());
    state.best_cost = ~0 - COST_CUTOFF;
    state.best_sel = nullptr;

    // Starting state:
    state.map.insert({ cpu_t{}, nullptr });

    for(ssa_ht h : cd.schedule)
        isel_node(h);

    std::vector<asm_inst_t> code;
    for(sel_t const* sel = state.best_sel; sel; sel = sel->prev)
        code.push_back(sel->inst);
    code.push_back({ ASM_LABEL, SSA_null, locator_t::cfg_label(state.fn, cfg_node) });
    std::reverse(code.begin(), code.end());

    return code;
}
