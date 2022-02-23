#include "cg_isel.hpp"

#include <cstdint>
#include <iostream> // TODO
#include <functional>
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
    using cont_t = std::type_identity_t<void(cpu_t, sel_t const*)>*;

    using options_flags_t = std::uint8_t;
    constexpr options_flags_t OPT_CONDITIONAL = 1 << 0;
    constexpr options_flags_t OPT_NO_DIRECT   = 1 << 1;

    // Options, to be passed to various construction functions:
    struct options_t
    {
        regs_t can_set;
        regs_t set_mask;
        options_flags_t flags;

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
        void set_reg(options_t opt, locator_t value)
        {
            if(opt.flags & OPT_CONDITIONAL)
                conditional_regs |= 1 << Reg;
            set_reg_no_conditional(value, opt);
        }

        template<regs_t Reg> [[gnu::always_inline]]
        void set_reg_no_conditional(options_t opt, locator_t value)
        {
            regs[Reg] = (opt.set_mask & (1 << Reg)) ? value : locator_t{};
        }

        template<regs_t Reg> [[gnu::always_inline]]
        void set_reg(options_t opt, std::uint8_t b)
        {
            return set_reg<Options, Reg>(locator_t::const_byte(b), opt);
        }

        template<regs_t Regs> [[gnu::noinline]]
        void set_regs(options_t opt, locator_t value)
        {
            if(opt.flags & OPT_CONDITIONAL)
                conditional_regs |= Regs;
            if(Regs & REGF_A)
                set_reg_no_conditional<REG_A>(value, opt);
            if(Regs & REGF_X)
                set_reg_no_conditional<REG_X>(value, opt);
            if(Regs & REGF_Y)
                set_reg_no_conditional<REG_Y>(value, opt);
            if(Regs & REGF_C)
                set_reg_no_conditional<REG_C>(value, opt);
            if(Regs & REGF_Z)
                set_reg_no_conditional<REG_Z>(value, opt);
        }

        // Dumbly sets registers based on 'op_output_regs'.
        // Use 'set_regs_for' for the smarter version!
        template<op_t Op> [[gnu::noinline]]
        void set_output_regs(options_t opt, locator_t value)
        {
            assert(!value.is_const_num());
            set_regs<op_output_regs(Op) & REGF_CPU>(value, opt);
        }

        // Sets registers based on an assembly op.
        // If the registers and inputs are known constants,
        // the set values may be constants too.
        template<op_t Op>
        void set_regs_for(options_t opt, locator_t def, locator_t arg);

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

    struct set_regs_for_impl<ADC_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A | REGF_C>())
            {
                static_assert(op_output_regs(ADC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_C));
                unsigned const result = cpu.regs[REG_A].data() + !!cpu.regs[REG_C].data() + arg.data();
                cpu.set_reg<REG_A>(opt, result);
                cpu.set_reg<REG_Z>(opt, (result & 0xFF) == 0);
                cpu.set_reg<REG_C>(opt, !!(result & 0x100));
            }
            else
                cpu.set_output_regs<ADC_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<AND_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(AND_IMMEDIATE) == (REGF_A | REGF_Z));
                std::uint8_t const result = cpu.regs[REG_A].data() & arg.data();
                cpu.set_reg<REG_A>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg<REG_A>(opt, 0u);
                cpu.set_reg<REG_Z>(opt, 1u);
            }
            else
                cpu.set_output_regs<AND_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<ASL_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ASL_IMPLIED) == (REGF_A | REGF_Z | REGF_C));
                std::uint8_t const a = cpu.regs[REG_A].data() << 1;
                bool const c = cpu.regs[REG_A].data() & 128;
                cpu.set_reg<REG_A>(opt, a);
                cpu.set_reg<REG_Z>(opt, a == 0);
                cpu.set_reg<REG_C>(opt, c);
            }
            else
                cpu.set_output_regs<ASL_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<CLC_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            cpu.set_reg<REG_C>(opt, 0u);
        }
    };

    struct set_regs_for_impl<CMP_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(CMP_IMMEDIATE) == (REGF_Z | REGF_C));
                bool const z = arg.data() == cpu.regs[REG_A].data();
                bool const c = arg.data() <= cpu.regs[REG_A].data();
                cpu.set_reg<REG_Z>(opt, z);
                cpu.set_reg<REG_C>(opt, c);
            }
            else if(arg.eq_const(0))
            {
                cpu.set_reg<REG_Z>(opt, def);
                cpu.set_reg<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs<CMP_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<CPX_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(CPX_IMMEDIATE) == (REGF_Z | REGF_C));
                bool const z = arg.data() == cpu.regs[REG_X].data();
                bool const c = arg.data() <= cpu.regs[REG_X].data();
                cpu.set_reg<REG_Z>(opt, z);
                cpu.set_reg<REG_C>(opt, c);
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg<REG_Z>(opt, def);
                cpu.set_reg<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs<CPX_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<CPY_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(CPY_IMMEDIATE) == (REGF_Z | REGF_C));
                bool const z = arg.data() == cpu.regs[REG_Y].data();
                bool const c = arg.data() <= cpu.regs[REG_Y].data();
                cpu.set_reg<REG_Z>(opt, z);
                cpu.set_reg<REG_C>(opt, c);
            }
            else if(arg.eq_const(0))
            {
                cpu.set_reg<REG_Z>(opt, def);
                cpu.set_reg<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs<CPY_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<DEX_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(DEX_IMPLIED) == (REGF_Z | REGF_X));
                std::uint8_t const result = cpu.regs[REG_X].data() - 1;
                cpu.set_reg<REG_X>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs<DEX_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<DEY_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(DEY_IMPLIED) == (REGF_Z | REGF_Y));
                std::uint8_t const result = cpu.regs[REG_Y].data() - 1;
                cpu.set_reg<REG_Y>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs<DEY_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<EOR_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(EOR_IMMEDIATE) == (REGF_Z | REGF_A));
                std::uint8_t const result = cpu.regs[REG_Y].data() ^ arg.data();
                cpu.set_reg<REG_A>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs<EOR_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<INX_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_X>())
            {
                static_assert(op_output_regs(INX_IMPLIED) == (REGF_Z | REGF_X));
                std::uint8_t const result = cpu.regs[REG_X].data() + 1;
                cpu.set_reg<REG_X>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs<INX_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<INY_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_Y>())
            {
                static_assert(op_output_regs(INY_IMPLIED) == (REGF_Z | REGF_Y));
                std::uint8_t const result = cpu.regs[REG_Y].data() + 1;
                cpu.set_reg<REG_Y>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
            }
            else
                cpu.set_output_regs<INY_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<LDA_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num())
            {
                static_assert(op_output_regs(LDA_IMMEDIATE) == (REGF_Z | REGF_A));
                cpu.set_reg<REG_A>(opt, arg);
                cpu.set_reg<REG_Z>(opt, arg.data() == 0);
            }
            else
                cpu.set_output_regs<LDA_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<LDX_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num())
            {
                static_assert(op_output_regs(LDX_IMMEDIATE) == (REGF_Z | REGF_X));
                cpu.set_reg<REG_X>(opt, arg);
                cpu.set_reg<REG_Z>(opt, arg.data() == 0);
            }
            else
                cpu.set_output_regs<LDX_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<LDY_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num())
            {
                static_assert(op_output_regs(LDY_IMMEDIATE) == (REGF_Z | REGF_Y));
                cpu.set_reg<REG_Y>(opt, arg);
                cpu.set_reg<REG_Z>(opt, arg.data() == 0);
            }
            else
                cpu.set_output_regs<LDY_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<LSR_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(LSR_IMPLIED) == (REGF_A | REGF_Z | REGF_C));
                std::uint8_t const a = cpu.regs[REG_A].data() >> 1;
                std::uint8_t const c  = cpu.regs[REG_A].data() & 1;
                cpu.set_reg<REG_A>(opt, a);
                cpu.set_reg<REG_Z>(opt, a == 0);
                cpu.set_reg<REG_C>(opt, c);
            }
            else
                cpu.set_output_regs<LSR_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<ORA_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ORA_IMMEDIATE) == (REGF_A | REGF_Z));
                std::uint8_t const result = cpu.regs[REG_Y].data() | arg.data();
                cpu.set_reg<REG_A>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
            }
            else if(arg.eq_const(0xFF))
            {
                cpu.set_reg<REG_A>(opt, 0xFFu);
                cpu.set_reg<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs<ORA_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<ROL_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(ROL_IMPLIED) == (REGF_A | REGF_Z | REGF_C));

            if(cpu.all_num<REGF_A>())
            {
                bool const c = cpu.regs[REG_A].data() & 128;
                cpu.set_reg<REG_C>(opt, c);

                if(cpu.all_num<REGF_C>())
                {
                    std::uint8_t const a = (cpu.regs[REG_A].data() << 1) | !!cpu.regs[REG_C].data();
                    cpu.set_reg<REG_A>(opt, locator_t::const_byte(a));
                    cpu.set_reg<REG_Z>(opt, locator_t::const_byte(a == 0));
                }
                else
                {
                    cpu.set_reg<REG_A>(opt, def);
                    cpu.set_reg<REG_Z>(opt, def);
                }
            }
            else if(cpu.all_num<REGF_C>() && cpu.regs[REG_C].data())
            {
                cpu.set_reg<REG_A>(opt, def);
                cpu.set_reg<REG_C>(opt, def);
                cpu.set_reg<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs<ROL_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<ROR_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(ROR_IMPLIED) == (REGF_A | REGF_Z | REGF_C));

            if(cpu.all_num<REGF_A>())
            {
                unsigned bool c = (cpu.regs[REG_A].data() & 1);
                cpu.set_reg<REG_C>(opt, c);

                if(cpu.all_num<REGF_C>())
                {
                    std::uint8_t const a = (cpu.regs[REG_A].data() >> 1) | (!!cpu.regs[REG_C].data() << 7);
                    cpu.set_reg<REG_A>(opt, a);
                    cpu.set_reg<REG_Z>(opt, a == 0);
                }
                else
                {
                    cpu.set_reg<REG_A>(opt, def);
                    cpu.set_reg<REG_Z>(opt, def);
                }
            }
            else if(cpu.all_num<REGF_C>() && !!cpu.regs[REG_C].data())
            {
                cpu.set_reg<REG_A>(opt, def);
                cpu.set_reg<REG_C>(opt, def);
                cpu.set_reg<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs<ROR_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<SBC_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A | REGF_C>())
            {
                static_assert(op_output_regs(SBC_IMMEDIATE) == (REGF_A | REGF_Z | REGF_C));
                unsigned const result = cpu.regs[REG_A].data() + !!cpu.regs[REG_C].data() + ~arg.data();
                cpu.set_reg<REG_A>(opt, result);
                cpu.set_reg<REG_Z>(opt, (result & 0xFF) == 0);
                cpu.set_reg<REG_C>(opt, !!(result & 0x100));
            }
            else
                cpu.set_output_regs<SBC_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<TAX_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TAX_IMPLIED) == (REGF_X | REGF_Z));
            if(cpu.all_num<REGF_A>())
            {
                cpu.set_reg<REG_X>(opt, cpu.regs[REG_A]);
                cpu.set_reg<REG_Z>(opt, cpu.regs[REG_A].data() == 0);
            }
            else
                cpu.set_output_regs<TAX_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<TAY_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TAY_IMPLIED) == (REGF_Y | REGF_Z));
            if(cpu.all_num<REGF_A>())
            {
                cpu.set_reg<REG_Y>(opt, cpu.regs[REG_A]);
                cpu.set_reg<REG_Z>(opt, cpu.regs[REG_A].data() == 0);
            }
            else
                cpu.set_output_regs<TAY_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<TXA_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TXA_IMPLIED) == (REGF_A | REGF_Z));
            if(cpu.all_num<REGF_X>())
            {
                cpu.set_reg<REG_A>(opt, cpu.regs[REG_X]);
                cpu.set_reg<REG_Z>(opt, cpu.regs[REG_X].data() == 0);
            }
            else
                cpu.set_output_regs<TXA_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<TYA_IMPLIED>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(TYA_IMPLIED) == (REGF_A | REGF_Z));
            if(cpu.all_num<REGF_Y>())
            {
                cpu.set_reg<REG_A>(opt, cpu.regs[REG_Y]);
                cpu.set_reg<REG_Z>(opt, cpu.regs[REG_Y].data() == 0);
            }
            else
                cpu.set_output_regs<TYA_IMPLIED>(opt, def);
        }
    };

    struct set_regs_for_impl<AXS_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_X | REGF_A>())
            {
                static_assert(op_output_regs(AXS_IMMEDIATE) == (REGF_Z | REGF_X | REGF_C));
                unsigned const result = (cpu.regs[REG_A].data() & cpu.regs[REG_X].data()) + ~arg.data() + 1;
                cpu.set_reg<REG_X>(opt, result);
                cpu.set_reg<REG_Z>(opt, (result & 0xFF) == 0);
                cpu.set_reg<REG_C>(opt, !!(result & 0x100));
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg<REG_X>(opt, def);
                cpu.set_reg<REG_Z>(opt, def);
                cpu.set_reg<REG_C>(opt, 1u);
            }
            else
                cpu.set_output_regs<AXS_IMMEDIATE>(def);
        }
    };

    struct set_regs_for_impl<ANC_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                static_assert(op_output_regs(ANC_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
                std::uint8_t const result = cpu.regs[REG_A].data() & arg.data();
                cpu.set_reg<REG_A>(opt, result);
                cpu.set_reg<REG_Z>(opt, result == 0);
                cpu.set_reg<REG_C>(opt, !!(result & 128));
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg<REG_A>(opt, 0u);
                cpu.set_reg<REG_Z>(opt, 1u);
                cpu.set_reg<REG_C>(opt, 0u);
            }
            else
                cpu.set_output_regs<ANC_IMMEDIATE>(opt, def);
        }
    };

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
                cpu.set_reg<REG_A>(opt, a);
                cpu.set_reg<REG_Z>(opt, a == 0);
                cpu.set_reg<REG_C>(opt, c);
            }
            else if(arg.is_const_num() && arg.data() == 0)
            {
                cpu.set_reg<REG_A>(opt, 0u);
                cpu.set_reg<REG_Z>(opt, 1u);
                cpu.set_reg<REG_C>(opt, 0u);
            }
            else
                cpu.set_output_regs<ALR_IMMEDIATE>(opt, def);
        }
    };

    struct set_regs_for_impl<ARR_IMMEDIATE>
    {
        static void call(options_t opt, cpu_t& cpu, locator_t def, locator_t arg)
        {
            static_assert(op_output_regs(ARR_IMMEDIATE) == (REGF_Z | REGF_A | REGF_C));
            if(arg.is_const_num() && cpu.all_num<REGF_A>())
            {
                std::uint8_t const and_ = cpu.regs[REG_A].data() & arg.data();
                std::uint8_t const c = !!(and_ & 128);
                cpu.set_reg<Options, REG_C>(c);

                if(cpu.all_num<REGF_C>())
                {
                    std::uint8_t const a = (and_ >> 1) | (!!cpu.regs[REG_C].data() << 7);
                    cpu.set_reg<REG_A>(opt, a);
                    cpu.set_reg<REG_Z>(opt, a == 0);
                }
                else
                {
                    cpu.set_reg<REG_A>(opt, def);
                    cpu.set_reg<REG_Z>(opt, def);
                }
            }
            else if(cpu.all_num<REGF_C>() && !!cpu.regs[REG_C].data())
            {
                cpu.set_reg<REG_A>(opt, def);
                cpu.set_reg<REG_C>(opt, def);
                cpu.set_reg<REG_Z>(opt, 0u);
            }
            else
                cpu.set_output_regs<ARR_IMMEDIATE>(def);
        }
    };

    template<op_t Op>
    void cpu_t::set_regs_for(options_t opt, locator_t def, locator_t arg)
    {
        set_regs_for_impl<Op>::call(opt, *this, def, arg);
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

    template<op_t Op, bool Conditional = false>
    constexpr unsigned cost_fn = Conditional 
        ? (op_cycles(Op) * 192) + op_size(Op) + op_penalty<Op>() // Conditional ops are cheaper.
        : (op_cycles(Op) * 256) + op_size(Op) + op_penalty<Op>();

    // THIS DETERMINES HOW BIG THE MAP GETS, AND HOW WIDE THE SEARCH IS:
    constexpr unsigned COST_CUTOFF = cost_fn<LDA_ABSOLUTE> * 3;

    template<typename Options, op_t Op>
    sel_t& alloc_sel(sel_t const* prev, locator_t arg, unsigned extra_cost = 0)
    {
        unsigned const total_cost = get_cost(prev) + extra_cost + cost_fn<Op, Options::flags & OPT_CONDITIONAL>;
        return state.sel_pool.emplace(prev, total_cost, asm_inst_t{ Op, state.ssa_op, arg });
    }

    template<typename Options, typename Label>
    struct label
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev) 
        {
            Cont::run(cpu, &alloc_sel<Options, ASM_LABEL>(prev, Label::trans()));
        }
    };

    struct finish
    {
        static void run(cpu_t const& cpu, sel_t const* sel)
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
    };

#define ARG_LIST option_t opt, cpu_t const& cpu, sel_t const* sel, cons_t* cont

    // Represents a list of functions.
    struct cons_t
    {
        id<void(options_t, cpu_t const&, sel_t const*, cons_t*)>* fn;
        cons_t* next;

        [[gnu::always_inline]]
        void call(options_t opt, cpu_t const& cpu, sel_t const* sel) const { fn(opt, cpu, sel, next); }
    };

    using cont_t = std::type_identity_t<void(options_t, cpu_t const&, sel_t const*, cons_t*)>*;

    template<cont_t Head, cont_t... Conts>
    struct chain_t
    {
        [[gnu::flatten]]
        explicit chain_t(cons_t* tail)
        : chain(tail)
        , cons{Head, &c.p}
        {}

        chain_t<Conts...> chain;
        cons_t cons;
    };

    template<cont_t Head>
    struct chain_t<Head>
    {   
        [[gnu::always_inline]]
        explicit chain_t(cons_t* tail)
        : cons{Head, tail}
        {}
        
        cons_t cons;
    };

    template<cont_t... Conts> [[gnu::always_inline]]
    void chain(options_t opt, cpu_t const& cpu, sel_t const* sel, cons_t* cont)
    {
        chain_t<Conts...> c(cont);
        c.cons.call(opt, cpu, sel, cont);
    }

    // Combines multiple 'run' functions into a single one.
    template<typename... Fns>
    struct chain
    {};

    template<typename Fn, typename... Fns>
    struct chain<Fn, Fns...>
    {
        [[gnu::always_inline]]
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::template run<chain<Fns...>>(cpu, sel);
        }

        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::template run<chain<Fns..., Cont>>(cpu, sel);
        }
    };

    template<typename Fn>
    struct chain<Fn>
    {
        [[gnu::always_inline]]
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::run(cpu, sel);
        }

        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* sel)
        {
            Fn::template run<Cont>(cpu, sel);
        }
    };

    // Runs the function and adds the results to the state map.
    template<typename Fn>
    void select_step(Fn fn)
    {
        state.next_map.clear();
        state.next_best_cost = ~0 - COST_CUTOFF;

        for(auto const& pair : state.map)
            if(get_cost(pair.second) < state.best_cost + COST_CUTOFF)
                fn(pair.first, pair.second);

        if(state.next_map.empty())
            throw std::runtime_error("Instruction selection failed to make progress.");

        state.map.swap(state.next_map);
        state.best_cost = state.next_best_cost;
    }

    template<typename Options, regs_t Regs, typename Param,
             bool Enable = (Regs & Options::can_set & REGF_CPU) == (Regs & REGF_CPU)>
    struct set_regs
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert((Regs & Options::can_set) == Regs);
            cpu_t cpu_copy = cpu;
            cpu_copy.set_regs<Options, Regs>(Param::value());
            Cont::run(cpu_copy, prev);
        }
    };

    template<typename Options, regs_t Regs, typename Param>
    struct set_regs<Options, Regs, Param, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {}
    };

    template<typename Options, op_t Op, typename Param>
    using set_regs_for = set_regs<Options, op_output_regs(Op) & REGF_CPU, Param>;

    template<typename Options, op_t Op>
    constexpr bool can_set_regs_for = 
        ((Options::can_set & op_output_regs(Op) & REGF_CPU) == (op_output_regs(Op) & REGF_CPU));

    struct clear_conditional
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev) 
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
            Cont::run(cpu_copy, prev);
        }
    };

    // Generates an op, picking the addressing mode based on its paramters.
    template<typename Options, op_name_t OpName, typename Def = null_, typename Arg = null_>
    struct pick_op
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev);
    };

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
    template<typename Options, op_t Op, typename Def = null_, typename Arg = null_
            , bool Enable = can_set_regs_for<Options, Op>>
    struct exact_op
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            /*
#ifndef NDEBUG 
            switch(op_addr_mode(Op))
            {
            case MODE_IMPLIED:
                assert(!Arg::trans());
                break;
            case MODE_RELATIVE:
            case MODE_LONG:
                assert(is_label(Arg::trans().lclass()));
                break;
            case MODE_IMMEDIATE:
                assert(Arg::trans().is_const_num());
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
                assert(Arg::trans());
                break;
            default:
                break;
            }
#endif
*/
            unsigned penalty = 0;
            if(ssa_addr_mode(op_addr_mode(Op)))
                penalty = handle_req_store_penalty(cpu, Arg::trans());
            cpu.set_regs_for<Options, Op>(Def::value(), Arg::trans());
            Cont::run(cpu, &alloc_sel<Options, Op>(prev, Arg::trans(), penalty));
        }
    };

    template<typename Options, op_t Op, typename Def, typename Arg>
    struct exact_op<Options, Op, Def, Arg, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev) {}
    };

    // Generates an op using the 0..255 table.
    template<typename Options, op_t Op, typename Def = null_>
    struct iota_op
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(op_addr_mode(Op) == MODE_ABSOLUTE_X || op_addr_mode(Op) == MODE_ABSOLUTE_Y);
            if(can_set_regs_for<Options, Op>)
            {
                cpu_t cpu_copy = cpu;
                cpu_copy.set_output_regs<Options, Op>(Def::value());
                Cont::run(cpu_copy, &alloc_sel<Options, Op>(prev, locator_t::iota()));
            }
        }
    };

    template<typename Options, typename Def, 
             bool Enable = Options::can_set & REGF_Z>
    struct load_Z
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_Z);

            if(cpu.reg_eq(REG_Z, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_A, Def::value()))
            {
                exact_op<typename Options::mask<REGF_Z>, EOR_IMMEDIATE, Def, const_<0>>
                ::template run<Cont>(cpu, prev);

                exact_op<Options, TAX_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);

                exact_op<Options, TAY_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            }
            else if(cpu.reg_eq(REG_X, Def::value()))
            {
                chain
                < exact_op<Options, INX_IMPLIED>
                , exact_op<Options, DEX_IMPLIED, Def>
                >::template run<Cont>(cpu, prev);

                chain
                < exact_op<typename Options::mask<REGF_Z>, CPX_IMMEDIATE, Def, const_<0u>>
                , set_regs<Options, REGF_C, const_<1u>>
                >::template run<Cont>(cpu, prev);

                exact_op<Options, TXA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            }
            else if(cpu.reg_eq(REG_Y, Def::value()))
            {
                chain
                < exact_op<Options, INY_IMPLIED>
                , exact_op<Options, DEY_IMPLIED, Def>
                >::template run<Cont>(cpu, prev);

                chain
                < exact_op<typename Options::mask<REGF_Z>, CPY_IMMEDIATE, Def, const_<0u>>
                , set_regs<Options, REGF_C, const_<1u>>
                >::template run<Cont>(cpu, prev);

                exact_op<Options, TYA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            }
            else
            {
                pick_op<Options, LDA, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LDX, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LDY, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LAX, Def, Def>
                ::template run<Cont>(cpu, prev);
            }
        }
    };

    template<typename Options, typename Def>
    struct load_Z<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_Z, Def::value()))
                Cont::run(cpu, prev);
        }
    };
    
    void load_A_impl(cpu_t const& cpu, sel_t const* prev, locator_t value,
                     cont_t cont)
    {
        // TODO: this is wrong!
        // It ignores CanSet

        if(value.is_const_num())
        {
            cpu_t cpu_copy;

            cpu_copy = cpu;
            cpu_copy.set_regs_for<Options, ANC_IMMEDIATE>({}, Def::value());
            if(cpu_copy.regs[REG_A] == Def::value())
                Cont::run(cpu_copy, &alloc_sel<Options, ANC_IMMEDIATE>(prev, Def::value()));

            cpu_copy = cpu;
            cpu_copy.set_regs_for<Options, LSR_IMPLIED>({}, {});
            if(cpu_copy.regs[REG_A] == Def::value())
                Cont::run(cpu_copy, &alloc_sel<Options, LSR_IMPLIED>(prev, {}));

            if(cpu.regs[REG_A].is_const_num())
            {
                unsigned mask = Def::value().data() << 1;
                if((mask & cpu.regs[REG_A].data() & 0xFF) == mask)
                {
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        locator_t loc = locator_t::const_byte(mask | i);
                        cpu_copy = cpu;
                        cpu_copy.set_regs_for<Options, ALR_IMMEDIATE>({}, loc);
                        if(cpu_copy.regs[REG_A] == Def::value())
                            Cont::run(cpu_copy, &alloc_sel<Options, ALR_IMMEDIATE>(prev, loc));

                        // TODO

                        if((cpu.regs[REG_A].data() & 1) == 0)
                            break;
                    }
                }
            }

            cpu_copy = cpu;
            cpu_copy.set_regs_for<Options, ASL_IMPLIED>({}, {});
            if(cpu_copy.regs[REG_A] == Def::value())
                Cont::run(cpu_copy, &alloc_sel<Options, ASL_IMPLIED>(prev, {}));

            cpu_copy = cpu;
            cpu_copy.set_regs_for<Options, ROL_IMPLIED>({}, {});
            if(cpu_copy.regs[REG_A] == Def::value())
                Cont::run(cpu_copy, &alloc_sel<Options, ROR_IMPLIED>(prev, {}));

            cpu_copy = cpu;
            cpu_copy.set_regs_for<Options, ROR_IMPLIED>({}, {});
            if(cpu_copy.regs[REG_A] == Def::value())
                Cont::run(cpu_copy, &alloc_sel<Options, ROL_IMPLIED>(prev, {}));
        }
    }

    template<typename Options, typename Def,
             bool Enable = Options::can_set & REGF_A>
    struct load_A
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_A);

            if(cpu.reg_eq(REG_A, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_X, Def::value()))
                exact_op<Options, TXA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else if(cpu.reg_eq(REG_Y, Def::value()))
                exact_op<Options, TYA_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else
            {
                pick_op<Options, LDA, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LAX, Def, Def>
                ::template run<Cont>(cpu, prev);

            }
        }
    };

    template<typename Options, typename Def>
    struct load_A<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, Def::value()))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename Def, 
             bool Enable = Options::can_set & REGF_X>
    struct load_X
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_X);

            if(cpu.reg_eq(REG_X, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_A, Def::value()))
                exact_op<Options, TAX_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else
            {
                if(cpu.reg_eq(REG_Y, Def::value()))
                    chain
                    < exact_op<Options, TYA_IMPLIED>
                    , exact_op<Options, TAX_IMPLIED, Def>
                    >::template run<Cont>(cpu, prev);

                pick_op<Options, LDX, Def, Def>
                ::template run<Cont>(cpu, prev);

                pick_op<Options, LAX, Def, Def>
                ::template run<Cont>(cpu, prev);

                if(Def::value().is_const_num())
                {
                    cpu_t cpu_copy;

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, INX_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_X] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, INX_IMPLIED>(prev, {}));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, DEX_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_X] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, DEX_IMPLIED>(prev, {}));
                }
            }
        }
    };

    template<typename Options, typename Def>
    struct load_X<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_X, Def::value()))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename Def, 
             bool Enable = Options::can_set & REGF_Y>
    struct load_Y
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            static_assert(Options::can_set & REGF_Y);

            if(cpu.reg_eq(REG_Y, Def::value()))
                Cont::run(cpu, prev);
            else if(cpu.reg_eq(REG_A, Def::value()))
                exact_op<Options, TAY_IMPLIED, Def>
                ::template run<Cont>(cpu, prev);
            else
            {
                if(cpu.reg_eq(REG_X, Def::value()))
                    chain
                    < exact_op<Options, TXA_IMPLIED>
                    , exact_op<Options, TAY_IMPLIED, Def>
                    >::template run<Cont>(cpu, prev);

                pick_op<Options, LDY, Def, Def>
                ::template run<Cont>(cpu, prev);

                /* TODO: remove?
                if(Def::node().holds_ref() && Def::node()->op() == SSA_cg_read_array_direct)
                    chain
                    < pick_op<Options, LDA, Def, Def>
                    , exact_op<Options, TAY_IMMEDIATE, Def>
                    >::template run<Cont>(cpu, prev);
                    */

                if(Def::value().is_const_num())
                {
                    cpu_t cpu_copy;

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, INY_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_Y] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, INY_IMPLIED>(prev, {}));

                    cpu_copy = cpu;
                    cpu_copy.set_regs_for<Options, DEY_IMPLIED>({}, {});
                    if(cpu_copy.regs[REG_Y] == Def::value())
                        Cont::run(cpu_copy, &alloc_sel<Options, DEY_IMPLIED>(prev, {}));
                }
            }
        }
    };

    template<typename Options, typename Def>
    struct load_Y<Options, Def, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_Y, Def::value()))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename A, typename X,
             bool Enable = Options::can_set & REGF_AX>
    struct load_AX
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            chain
            < load_A<Options, A>
            , load_X<typename Options::restrict_to<~REGF_A>, X>
            >::template run<Cont>(cpu, prev);

            chain
            < load_X<Options, X>
            , load_A<typename Options::restrict_to<~REGF_X>, A>
            >::template run<Cont>(cpu, prev);
        }
    };

    template<typename Options, typename A, typename X>
    struct load_AX<Options, A, X, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, A::value() && cpu.reg_eq(REG_X, X::value())))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename A, typename Y,
             bool Enable = Options::can_set & REGF_AY>
    struct load_AY
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            chain
            < load_A<Options, A>
            , load_X<typename Options::restrict_to<~REGF_A>, Y>
            >::template run<Cont>(cpu, prev);

            chain
            < load_X<Options, Y>
            , load_A<typename Options::restrict_to<~REGF_Y>, A>
            >::template run<Cont>(cpu, prev);
        }
    };

    template<typename Options, typename A, typename Y>
    struct load_AY<Options, A, Y, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, A::value() && cpu.reg_eq(REG_Y, Y::value())))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, typename C,
             bool Enable = (Options::can_set & REGF_C)>
    struct load_C
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_C, C::value()))
                Cont::run(cpu, prev);
            else if(C::value().is_const_num())
            {
                if(C::value().data())
                    exact_op<Options, SEC_IMPLIED, C>
                    ::template run<Cont>(cpu, prev);
                else
                    exact_op<Options, CLC_IMPLIED, C>
                    ::template run<Cont>(cpu, prev);
            }
            else
            {
                chain
                < load_A<Options, C>
                , exact_op<Options, LSR_IMPLIED, C>
                >::template run<Cont>(cpu, prev);

                if((Options::can_set & (REGF_Z | REGF_A)) == REGF_Z)
                {
                    // We need a version that preserves 'A', so do things this way:

                    sel_t const* sel = prev;
                    sel = &alloc_sel<Options, PHA_IMPLIED>(sel, {});
                    sel = &alloc_sel<Options, LDA_ABSOLUTE>(sel, C::trans());
                    sel = &alloc_sel<Options, LSR_IMPLIED>(sel, {});
                    sel = &alloc_sel<Options, PLA_IMPLIED>(sel, {});

                    cpu_t new_cpu = cpu;
                    new_cpu.set_reg<Options, REG_C>(C::value());
                    if(new_cpu.regs[REG_A].is_const_num())
                        new_cpu.set_reg<Options, REG_Z>(locator_t::const_byte(new_cpu.regs[REG_A].data() == 1));
                    else
                        new_cpu.set_reg<Options, REG_Z>(new_cpu.regs[REG_A]);

                    Cont::run(new_cpu, sel);
                }
            }
        }
    };

    template<typename Options, typename A, typename C,
             bool Enable = Options::can_set & REGF_AC>
    struct load_AC
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_C, C::value()))
                load_A<typename Options::restrict_to<~REGF_C>, A>
                ::template run<Cont>(cpu, prev);
            else if(C::value().is_const_num())
            {
                chain
                < load_A<Options, A>
                , load_C<typename Options::restrict_to<~REGF_A>, C>
                >::template run<Cont>(cpu, prev);

                chain
                < load_A<Options, C>
                , exact_op<typename Options::mask<REGF_C>, LSR_IMPLIED, C>
                , load_A<typename Options::restrict_to<~REGF_C>, A>
                >::template run<Cont>(cpu, prev);
            }
        }
    };

    template<typename Options, typename A, typename C>
    struct load_AC<Options, A, C, false>
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(cpu.reg_eq(REG_A, A::value() && cpu.reg_eq(REG_C, C::value())))
                Cont::run(cpu, prev);
        }
    };

    template<typename Options, op_t AbsoluteX, op_t AbsoluteY, typename Def, typename Arg
            , bool Enable = (AbsoluteX || AbsoluteY) && !(Options::flags & OPT_NO_DIRECT)>
    struct absolute_xy_op
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            using O2 = typename Options::template add_flags<OPT_NO_DIRECT>;

            if(AbsoluteX != BAD_OP)
            {
                chain
                < load_X<O2, array_index<Arg>>
                , exact_op<O2, AbsoluteX, Def, array_mem<Arg>>
                >::template run<Cont>(cpu, prev);
            }

            if(AbsoluteY != BAD_OP)
            {
                chain
                < load_Y<O2, array_index<Arg>>
                , exact_op<O2, AbsoluteY, Def, array_mem<Arg>>
                >::template run<Cont>(cpu, prev);
            }
        }
    };

    template<typename Options, op_t AbsoluteX, op_t AbsoluteY, typename Def, typename Arg>
    struct absolute_xy_op<Options, AbsoluteX, AbsoluteY, Def, Arg, false>
    {
        template<typename Cont> [[gnu::always_inline]]
        static void run(cpu_t const& cpu, sel_t const* prev) {}
    };

    // pick_op impl
    template<typename Options, op_name_t OpName, typename Def, typename Arg>
    template<typename Cont>
    void pick_op<Options, OpName, Def, Arg>
    ::run(cpu_t const& cpu, sel_t const* prev)
    {
        constexpr op_t implied    = get_op(OpName, MODE_IMPLIED);
        constexpr op_t relative   = get_op(OpName, MODE_RELATIVE);
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);
        constexpr op_t absolute_X = get_op(OpName, MODE_ABSOLUTE_X);
        constexpr op_t absolute_Y = get_op(OpName, MODE_ABSOLUTE_Y);

        if(implied && !Arg::trans())
            exact_op<Options, implied, Def, null_>::template run<Cont>(cpu, prev);
        else if(relative)
            exact_op<Options, implied, Def, Arg>::template run<Cont>(cpu, prev);
        else if(immediate && Arg::trans().is_const_num())
            exact_op<Options, immediate, Def, Arg>::template run<Cont>(cpu, prev);
        else if((absolute_X || absolute_Y) && Arg::node().holds_ref() && Arg::node()->op() == SSA_cg_read_array_direct)
            absolute_xy_op<Options, absolute_X, absolute_Y, Def, Arg>::template run<Cont>(cpu, prev);
        else if(absolute && !Arg::trans().is_const_num())
            exact_op<Options, absolute, Def, Arg>::template run<Cont>(cpu, prev);
    }

    // Adds a store operation.
    // 'Maybe' means the store may not be required in the final code;
    // such instructions can be pruned later.
    template<typename Options, op_name_t StoreOp, typename Param, bool Maybe = true>
    struct store
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            // Store the node, locally:
            if(Maybe && Param::trans().lclass() == LOC_SSA)
            {
                switch(StoreOp)
                {
                case STA: Cont::run(cpu, &alloc_sel<Options, MAYBE_STA>(prev, Param::trans())); break;
                case STX: Cont::run(cpu, &alloc_sel<Options, MAYBE_STX>(prev, Param::trans())); break;
                case STY: Cont::run(cpu, &alloc_sel<Options, MAYBE_STY>(prev, Param::trans())); break;
                case SAX: Cont::run(cpu, &alloc_sel<Options, MAYBE_SAX>(prev, Param::trans())); break;
                default: assert(false); break;
                }
            }
            else
                 Cont::run(cpu, &alloc_sel<Options, get_op(StoreOp, MODE_ABSOLUTE)>(prev, Param::trans()));
        }
    };

    template<typename Options, typename Def, typename Load, typename Store>
    struct load_then_store
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            chain
            < load_A<Options, Load>
            , set_regs<Options, REGF_A, Def>
            , store<Options, STA, Store>
            >::template run<Cont>(cpu, prev);

            chain
            < load_X<Options, Load>
            , set_regs<Options, REGF_X, Def>
            , store<Options, STX, Store>
            >::template run<Cont>(cpu, prev);

            chain
            < load_Y<Options, Load>
            , set_regs<Options, REGF_Y, Def>
            , store<Options, STY, Store>
            >::template run<Cont>(cpu, prev);
        }
    };

    template<typename Options, typename Condition, typename Then, typename Else = null_>
    struct if_
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            if(Condition::value())
                Then::template run<Cont>(cpu, prev);
            else
                Else::template run<Cont>(cpu, prev);
        }
    };

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

    /* TODO: remove?
    template<typename Param, unsigned I>
    struct param_arg
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(I); }

        [[gnu::always_inline]]
        static ssa_value_t value() { return orig_def(node()); }

        [[gnu::always_inline]]
        static ssa_value_t trans() { return asm_arg(node()); }
    };

    template<typename Options, op_name_t BranchOp, typename FailLabel, typename SuccessLabel, unsigned I>
    struct eq_branch
    {
        template<typename Cont>
        static void run(cpu_t const& cpu, sel_t const* prev)
        {
            constexpr op_name_t InverseOp = get_op(invert_branch(BranchOp), MODE_RELATIVE);

            using lhs = param_arg<Node, I>;
            using rhs = param_arg<Node, I+1>;

            if(

            chain
            < load_A<OC, lhs>
            , if_<OC, p_condition, pick_op<OC, CMP, {}, p_rhs>>
            , exact_op<OC, InverseOp, null_, FailLabel>
            >::run<eq_branch<Options, BranchOp, FailLabel, SuccessLabel, I-2>>(cpu, prev);

            chain
            < load_X<OC, p_lhs>
            , if_<OC, p_condition, pick_op<OC, CPX, {}, p_rhs>>
            , exact_op<OC, InverseOp, null_, FailLabel>
            >::run<eq_branch<Options, BranchOp, FailLabel, SuccessLabel, I-2>>(cpu, prev);

            chain
            < load_Y<OC, p_lhs>
            , if_<OC, p_condition, pick_op<OC, CPY, {}, p_rhs>>
            , exact_op<OC, InverseOp, null_, FailLabel>
            >::run<eq_branch<Options, BranchOp, FailLabel, SuccessLabel, I-2>>(cpu, prev);
        }
    }

    template<typename Options, op_name_t BranchOp, typename FailLabel, typename SuccessLabel>
    struct eq_branch<Options, BranchOp, FailLabel, SuccessLabel, 0>
    {
        constexpr op_name_t Op = get_op(BranchOp, MODE_RELATIVE);

        chain
        < exact_op<OC, Op, null_, SuccessLabel>
        , clear_conditional
        >::run<finish>;
    }
    */

    template<typename Options, op_name_t BranchOp, typename FailLabel, typename SuccessLabel>
    void eq_branch(ssa_ht h)
    {
        using OC = typename Options::add_flags<OPT_CONDITIONAL>;
        constexpr op_t InverseOp = get_op(invert_branch(BranchOp), MODE_RELATIVE);

        for(unsigned i = 0; i < h->input_size(); i += 2)
        {
            select_step([&, i](cpu_t cpu, sel_t const* const prev)
            {
                for(unsigned j = 0; j < 2; ++j)
                {
                    p_lhs::set(h->input(i + j));
                    p_rhs::set(h->input(i + 1-j));
                    
                    if(p_rhs::value().eq_const_byte(0))
                    {
                        chain
                        < load_Z<OC, p_lhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);
                    }
                    else
                    {
                        chain
                        < load_A<OC, p_lhs>
                        , pick_op<OC, CMP, null_, p_rhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);

                        chain
                        < load_X<OC, p_lhs>
                        , pick_op<OC, CPX, null_, p_rhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);

                        chain
                        < load_Y<OC, p_lhs>
                        , pick_op<OC, CPY, null_, p_rhs>
                        , exact_op<OC, InverseOp, null_, FailLabel>
                        >::template run<finish>(cpu, prev);
                    }
                }
            });
        }

        constexpr op_t Op = get_op(BranchOp, MODE_RELATIVE);
        select_step(
            chain
            < exact_op<OC, Op, null_, SuccessLabel>
            , clear_conditional
            >::template run<finish>);
    }

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

    template<typename Options, typename FailLabel, typename SuccessLabel>
    void lt_branch(ssa_ht h)
    {
        //if(OrEqual)
            //std::swap(fail_label, success_label);

        using OC = typename Options::add_flags<OPT_CONDITIONAL>;

        int const input_size = h->input_size();
        assert(input_size >= 2);
        assert(input_size % 2 == 0);

        using last_iter = condition<struct lt_last_iter_tag>;
        using next_label = param<struct lt_next_label_tag>;

        for(int i = input_size - 2; i >= 0; i -= 2)
        {
            last_iter::set(i == 0);
            next_label::set(state.minor_label());

            select_step([&, i](cpu_t cpu, sel_t const* const prev)
            {
                p_lhs::set(h->input(i + 0));
                p_rhs::set(h->input(i + 1));
                
                if(p_lhs::value().eq_const_byte(0))
                {
                    chain
                    < load_Z<OC, p_rhs>
                    , exact_op<OC, BNE_RELATIVE, null_, SuccessLabel>
                    , if_<OC, last_iter, exact_op<OC, BEQ_RELATIVE, null_, FailLabel>>
                    >::template run<finish>(cpu, prev);
                }
                else if(p_rhs::value().eq_const_byte(0))
                {
                    if(i == 0)
                        exact_op<OC, JMP_ABSOLUTE, null_, FailLabel>
                        ::template run<finish>(cpu, prev);
                    else
                        chain
                        < load_Z<OC, p_lhs>
                        , exact_op<OC, BNE_RELATIVE, null_, FailLabel>
                        >::template run<finish>(cpu, prev);
                }
                else
                {
                    using normal = chain
                        < if_<OC, last_iter, null_, exact_op<OC, BEQ_RELATIVE, null_, next_label>>
                        , exact_op<OC, BCS_RELATIVE, null_, FailLabel>
                        , exact_op<OC, BCC_RELATIVE, null_, SuccessLabel>
                        , if_<OC, last_iter, null_, label<OC, next_label>>
                        >;

                    using inverted = chain
                        < if_<OC, last_iter, exact_op<OC, BEQ_RELATIVE, null_, FailLabel>,
                                             exact_op<OC, BEQ_RELATIVE, null_, next_label>>
                        , exact_op<OC, BCC_RELATIVE, null_, FailLabel>
                        , exact_op<OC, BCS_RELATIVE, null_, SuccessLabel>
                        , if_<OC, last_iter, null_, label<OC, next_label>>
                        >;
                    chain
                    < load_A<OC, p_lhs>
                    , pick_op<OC, CMP, null_, p_rhs>
                    , normal
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_A<OC, p_rhs>
                    , pick_op<OC, CMP, null_, p_lhs>
                    , inverted
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_X<OC, p_lhs>
                    , pick_op<OC, CPX, null_, p_rhs>
                    , normal
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_X<OC, p_rhs>
                    , pick_op<OC, CPX, null_, p_lhs>
                    , inverted
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_Y<OC, p_lhs>
                    , pick_op<OC, CPY, null_, p_rhs>
                    , normal
                    >::template run<finish>(cpu, prev);

                    chain
                    < load_Y<OC, p_rhs>
                    , pick_op<OC, CPY, null_, p_lhs>
                    , inverted
                    >::template run<finish>(cpu, prev);
                }
            });
        }

        select_step(
            chain
            //< exact_op<OC, BCC_RELATIVE, null_, SuccessLabel>
            < clear_conditional
            >::template run<finish>);
    }

    template<typename Options>
    void write_globals(ssa_ht h)
    {
        for_each_written_global(h, [h](ssa_value_t def, locator_t loc)
        {
            if(def.is_handle())
            {
                //assert(def->op() == SSA_early_store || def->op() == SSA_aliased_store);

                //if(def->test_flags(FLAG_COALESCED))
                    //return;

                if(cset_locator(def.handle()) == loc)
                    return;

                //def = def->input(0);
            }

            p_def::set(def);
            p_arg<0>::set(loc);

            select_step([loc, def](cpu_t cpu, sel_t const* prev)
            {
                load_then_store<Options, p_def, p_def, p_arg<0>>
                ::template run<finish>(cpu, prev);
            });
        });
    }

    void isel_node_simple(ssa_ht h, cpu_t cpu, sel_t const* prev)
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

        using O = options<>;
        using OC = O::add_flags<OPT_CONDITIONAL>;

        switch(h->op())
        {
        case SSA_carry:
            if(h->output_size() > 1 || (h->output_size() == 1 && h->output(0)->cfg_node() != h->cfg_node()))
            {
                p_label<0>::set(state.minor_label());

                chain
                < exact_op<O, AND_IMMEDIATE, null_, const_<0>>
                , exact_op<O, ROL_IMPLIED>
                , store<O, STA, p_def, false>
                >::run<finish>(cpu, prev);

                chain
                < load_X<O, const_<0>>
                , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<typename O::add_flags<OPT_CONDITIONAL>, INX_IMPLIED>
                , label<O, p_label<0>>
                , clear_conditional
                , store<O, STX, p_def, false>
                >::run<finish>(cpu, prev);

                chain
                < load_Y<O, const_<0>>
                , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<typename O::add_flags<OPT_CONDITIONAL>, INY_IMPLIED>
                , label<O, p_label<0>>
                , clear_conditional
                , store<O, STY, p_def, false>
                >::run<finish>(cpu, prev);
            }
            else
            {
                cpu_t new_cpu = cpu;
                new_cpu.set_reg<O, REG_C>(p_def::value());
                finish::run(new_cpu, &alloc_sel<O, MAYBE_STORE_C>(prev, p_def::trans()));
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
                < load_AC<O, p_lhs, p_carry>
                , exact_op<O, ROL_IMPLIED, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                if(p_carry::value().eq_const_byte(0))
                    chain
                    < load_A<O, p_lhs>
                    , exact_op<O, ASL_IMPLIED, p_def>
                    , store<O, STA, p_def>
                    >::run<finish>(cpu, prev);
            }

            commutative(h, [&]()
            {
                chain
                < load_AC<O, p_lhs, p_carry>
                , pick_op<O, ADC, p_def, p_rhs>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AC<O, p_lhs, p_carry>
                , load_X<O::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<O, ADC_ABSOLUTE_X, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AC<O, p_lhs, p_carry>
                , load_Y<O::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<O, ADC_ABSOLUTE_Y, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                if(p_rhs::value().is_const_num())
                {
                    p_label<0>::set(state.minor_label());

                    if(p_rhs::value().data() == 0 && carry_output_i(*h) == -1)
                    {
                        chain
                        < load_C<O, p_carry>
                        , load_X<O, p_lhs>
                        , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, INX_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_X, p_def>
                        , store<O, STX, p_def>
                        >::run<finish>(cpu, prev);

                        chain
                        < load_C<O, p_carry>
                        , load_Y<O, p_lhs>
                        , exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, INY_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_Y, p_def>
                        , store<O, STY, p_def>
                        >::run<finish>(cpu, prev);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<O, BCC_RELATIVE, null_, p_label<0>>
                            , pick_op<OC, INC, p_def, p_def>
                            , label<O, p_label<0>>
                            , clear_conditional
                            >::run<finish>(cpu, prev);
                        }
                    }

                    if(p_rhs::value().data() == 0xFF && carry_output_i(*h) == -1)
                    {
                        p_label<0>::set(state.minor_label());

                        chain
                        < load_C<O, p_carry>
                        , load_X<O, p_lhs>
                        , exact_op<O, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, DEX_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_X, p_def>
                        , store<O, STX, p_def>
                        >::run<finish>(cpu, prev);

                        chain
                        < load_C<O, p_carry>
                        , load_Y<O, p_lhs>
                        , exact_op<O, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OC, DEY_IMPLIED>
                        , label<O, p_label<0>>
                        , clear_conditional
                        , set_regs<O, REGF_Y, p_def>
                        , store<O, STY, p_def>
                        >::run<finish>(cpu, prev);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<O, BCS_RELATIVE, null_, p_label<0>>
                            , pick_op<OC, DEC, p_def, p_def>
                            , label<O, p_label<0>>
                            , clear_conditional
                            >::run<finish>(cpu, prev);
                        }
                    }

                    if(p_carry::value().is_const_num())
                    {
                        p_temp::set((0x100 - p_rhs::value().data() - !!p_carry::value().data()) & 0xFF);

                        chain
                        < load_AX<O, p_lhs, p_lhs>
                        , exact_op<O, AXS_IMMEDIATE, p_def, p_temp>
                        , store<O, STX, p_def>
                        >::run<finish>(cpu, prev);

                        if((p_rhs::value().data() + !!p_carry::value().data()) == 1 && carry_output_i(*h) == -1)
                        {
                            chain
                            < load_X<O, p_lhs>
                            , exact_op<O, INX_IMPLIED, p_def>
                            , store<O, STX, p_def>
                            >::run<finish>(cpu, prev);

                            chain
                            < load_Y<O, p_lhs>
                            , exact_op<O, INY_IMPLIED, p_def>
                            , store<O, STY, p_def>
                            >::run<finish>(cpu, prev);

                            if(p_def::trans() == p_lhs::trans())
                                pick_op<O, INC, p_def, p_def>
                                ::run<finish>(cpu, prev);
                        }

                        if(((p_rhs::value().data() + !!p_carry::value().data()) & 0xFF) == 0xFF && carry_output_i(*h) == -1)
                        {
                            chain
                            < load_X<O, p_lhs>
                            , exact_op<O, DEX_IMPLIED, p_def>
                            , store<O, STX, p_def>
                            >::run<finish>(cpu, prev);

                            chain
                            < load_Y<O, p_lhs>
                            , exact_op<O, DEY_IMPLIED, p_def>
                            , store<O, STY, p_def>
                            >::run<finish>(cpu, prev);

                            if(p_def::trans() == p_lhs::trans())
                                pick_op<O, DEC, p_def, p_def>
                                ::run<finish>(cpu, prev);
                        }
                    }
                }
            });
            break;

        case SSA_and:
            commutative(h, [&]()
            {
                chain
                < load_A<O, p_lhs>
                , pick_op<O, AND, p_def, p_rhs>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , exact_op<O, AXS_IMMEDIATE, p_def, const_<0>>
                , store<O, STX, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , iota_op<O, AND_ABSOLUTE_X, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AY<O, p_lhs, p_rhs>
                , iota_op<O, AND_ABSOLUTE_Y, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , store<O, SAX, p_def, false>
                >::run<finish>(cpu, prev);

                // TODO: consider using subtraction instructions,
                // checking constraints when it's applicable.
            });
            break;

        case SSA_or:
            commutative(h, [&]()
            {
                chain
                < load_A<O, p_lhs>
                , pick_op<O, ORA, p_def, p_rhs>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AX<O, p_lhs, p_rhs>
                , iota_op<O, ORA_ABSOLUTE_X, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AY<O, p_lhs, p_rhs>
                , iota_op<O, ORA_ABSOLUTE_Y, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                // TODO: consider using add instructions, 
                // checking constraints when it's applicable.
            });
            break;

        case SSA_early_store:
            p_arg<0>::set(h->input(0));
            load_then_store<O, p_def, p_arg<0>, p_def>
            ::run<finish>(cpu, prev);
            break;

        case SSA_read_global:
            if(h->input(1).locator() != cset_locator(h))
            {
                p_arg<0>::set(h->input(1));
                load_then_store<O, p_def, p_arg<0>, p_def>
                ::run<finish>(cpu, prev);
            }
            else
                finish::run(cpu, prev);
            break;

        case SSA_phi_copy:
            if(!h->input(0).holds_ref() || cset_head(h) != cset_head(h->input(0).handle()))
            {
                p_arg<0>::set(h->input(0));
                load_then_store<O, p_def, p_arg<0>, p_def>
                ::run<finish>(cpu, prev);
            }
            else
                finish::run(cpu, prev);
            break;

        case SSA_phi:
            assert(h->input_size() > 0);
            if(cset_head(h) != cset_head(h->input(0).handle()))
            {
                p_arg<0>::set(h->input(0));
                load_then_store<O, p_def, p_arg<0>, p_def>
                ::run<finish>(cpu, prev);
            }
            else
                finish::run(cpu, prev);
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
                < load_AX<O, p_assignment, p_index>
                , exact_op<O, STA_ABSOLUTE_X, null_, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_AY<O, p_assignment, p_index>
                , exact_op<O, STA_ABSOLUTE_Y, null_, p_def>
                >::run<finish>(cpu, prev);
            }
            break;

        case SSA_read_array:
            {
                using p_index = p_arg<0>;

                p_index::set(h->input(2));

                chain
                < load_X<O, p_index>
                , exact_op<O, LDA_ABSOLUTE_X, p_def, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_X<O, p_index>
                , exact_op<O, LDY_ABSOLUTE_X, p_def, p_def>
                , store<O, STY, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_Y<O, p_index>
                , exact_op<O, LDA_ABSOLUTE_Y, p_def, p_def>
                , store<O, STA, p_def>
                >::run<finish>(cpu, prev);

                chain
                < load_Y<O, p_index>
                , exact_op<O, LDX_ABSOLUTE_Y, p_def, p_def>
                , store<O, STX, p_def>
                >::run<finish>(cpu, prev);
            }

            break;

        case SSA_fn_call:
            p_arg<0>::set(h->input(0));
            chain
            < exact_op<O, JSR_ABSOLUTE, null_, p_arg<0>>
            , set_regs<O, REGF_CPU, null_>
            >::run<finish>(cpu, prev);
            break;

        case SSA_goto_mode:
            p_arg<0>::set(h->input(0));
            chain
            < exact_op<O, JMP_ABSOLUTE, null_, p_arg<0>>
            , set_regs<O, REGF_CPU, null_>
            >::run<finish>(cpu, prev);
            break;

        case SSA_return:
            exact_op<O, RTS_IMPLIED>
            ::run<finish>(cpu, prev);
            break;

        case SSA_jump:
            assert(cfg_node->output_size() == 1);
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            exact_op<O, JMP_ABSOLUTE, null_, p_label<0>>
            ::run<finish>(cpu, prev);
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
            finish::run(cpu, prev);
            break;
        default:
            throw std::runtime_error(fmt("Unhandled SSA op in code gen: %i", h->op()));
        }
    }

    void isel_node(ssa_ht h)
    {
        state.ssa_op = h->op();
        std::cout << "doing op " << state.ssa_op << std::endl;

        using O = options<>;

        cfg_ht const cfg_node = h->cfg_node();

        switch(h->op())
        {
        case SSA_eq:     
            eq_store<BEQ>(h); 
            break;

        case SSA_not_eq: 
            eq_store<BNE>(h); 
            break;

        // Branch ops jump directly:
        case SSA_branch_eq:
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(state.fn, cfg_node->output(1)));
            eq_branch<O, BEQ, p_label<0>, p_label<1>>(h);
            break;
        case SSA_branch_not_eq:
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(state.fn, cfg_node->output(1)));
            eq_branch<O, BNE, p_label<0>, p_label<1>>(h);
            break;

        case SSA_branch_lt:
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(state.fn, cfg_node->output(1)));
            lt_branch<O, p_label<0>, p_label<1>>(h);
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
            write_globals<O>(h);
            break;

        default: 
            break;
        }

        select_step([h](cpu_t cpu, sel_t const* prev)
        {
            isel_node_simple(h, cpu, prev);
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
