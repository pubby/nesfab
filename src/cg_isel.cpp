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
#include "cg_isel_cpu.hpp"

#define CONT cont_fn_t cont

namespace isel
{
    // Backbone state of the instruction selection algorithm.
    struct state_t
    {
        //rh::batman_map<cpu_t, sel_t const*> map;
        //rh::batman_map<cpu_t, sel_t const*> next_map;

        rh::batman_map<cpu_t, sel_t const*> map;
        rh::batman_map<cpu_t, sel_t const*> next_map;

        array_pool_t<sel_t> sel_pool;

        unsigned best_cost = ~0;
        unsigned next_best_cost = ~0;
        sel_t const* best_sel = nullptr;

        cfg_ht cfg_node;

        unsigned next_label = 0;
        unsigned next_var = 0;

        ssa_op_t ssa_op;

        fn_ht fn;

        locator_t minor_label() { return locator_t::minor_label(fn, next_label++); }
        locator_t minor_var() { return locator_t::minor_var(fn, next_var++); }
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
    template<cont_t... Conts> [[gnu::noinline]]
    void chain(cpu_t const& cpu, sel_t const* sel, cons_t const* cont)
    {
        chain_t<Conts...> c(cont);
        c.cons.call(cpu, sel);
    }

    template<typename Label> [[gnu::noinline]]
    void label(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cont->call(cpu, &alloc_sel<ASM_LABEL>({}, prev, Label::trans()));
    };

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

    // Used for debugging
#ifndef NDEBUG
    void debug_kill(cpu_t const& cpu, sel_t const* sel, cons_t const*)
    {
        assert(0);
    }
#endif

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
            throw isel_no_progress_error_t{};

        state.map.swap(state.next_map);
        state.best_cost = state.next_best_cost;
    }

    template<typename Opt, regs_t Regs, typename Param> [[gnu::noinline]]
    void set_defs(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_defs<Regs>(Opt::to_struct, Param::value()))
            cont->call(cpu_copy, prev);
    };

    template<typename Opt, op_t Op, typename Def, typename Arg = null_> [[gnu::noinline]]
    void set_defs_for(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_defs_for<Op>(Opt::to_struct, Def::value(), Arg::trans()))
            cont->call(cpu_copy, prev);
    };

    constexpr bool can_set_defs_for(options_t opt, op_t op)
    {
        return (opt.can_set & op_output_regs(op) & REGF_CPU) == (op_output_regs(op) & REGF_CPU);
    }

    [[gnu::noinline]]
    void clear_conditional(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.conditional_regs & REGF_A)
            cpu_copy.defs[REG_A] = {};
        if(cpu_copy.conditional_regs & REGF_X)
            cpu_copy.defs[REG_X] = {};
        if(cpu_copy.conditional_regs & REGF_Y)
            cpu_copy.defs[REG_Y] = {};
        if(cpu_copy.conditional_regs & REGF_C)
            cpu_copy.defs[REG_C] = {};
        if(cpu_copy.conditional_regs & REGF_Z)
            cpu_copy.defs[REG_Z] = {};
        if(cpu_copy.conditional_regs & REGF_N)
            cpu_copy.defs[REG_N] = {};
        cpu_copy.known_mask &= ~cpu_copy.conditional_regs;
        cpu_copy.conditional_regs = 0;
        cont->call(cpu_copy, prev);
    };

    // TODO
    // Generates an op, picking the addressing mode based on its paramters.
    template<typename Opt, op_name_t OpName, typename Def, typename Arg> [[gnu::noinline]]
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
    // NOTE: You generally shouldn't use this for any op that uses
    // a memory argument, as that argument can't be a SSA_cg_read_array_direct.
    // Thus, prefer pick_op.
    template<op_t Op> [[gnu::noinline]]
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
        if(cpu_copy.set_defs_for<Op>(opt, def, arg))
            cont->call(cpu_copy, &alloc_sel<Op>(opt, prev, arg, penalty));
    }

    template<typename Opt, op_t Op, typename Def = null_, typename Arg = null_> [[gnu::noinline]]
    void exact_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        exact_op<Op>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
    }

    // Generates an op using the 0..255 table.
    template<typename Opt, op_t Op, typename Def = null_> [[gnu::noinline]]
    void iota_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        static_assert(op_addr_mode(Op) == MODE_ABSOLUTE_X || op_addr_mode(Op) == MODE_ABSOLUTE_Y);
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_output_defs<Op>(Opt::to_struct, Def::value()))
            cont->call(cpu_copy, &alloc_sel<Op>(Opt::to_struct, prev, locator_t::iota()));
    };

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_NZ_for(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if((cpu.def_eq(REG_Z, v) || (v.is_const_num() && cpu.is_known(REG_Z) && cpu.known[REG_Z] == !v.data()))
        && (cpu.def_eq(REG_N, v) || (v.is_const_num() && cpu.is_known(REG_N) && cpu.known[REG_N] == !!(v.data() & 0x80))))
            cont->call(cpu, prev);
        else if((Opt::can_set & REGF_NZ) != REGF_NZ)
            return;
        else if(cpu.def_eq(REG_A, v))
        {
            exact_op<EOR_IMMEDIATE>(
                Opt::template unrestrict<REGF_A>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            exact_op<TAX_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);

            exact_op<TAY_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else if(cpu.def_eq(REG_X, v))
        {
            chain
            < exact_op<typename Opt::unrestrict<REGF_X>, INX_IMPLIED>
            , exact_op<typename Opt::unrestrict<REGF_X>, DEX_IMPLIED, Def>
            >(cpu, prev, cont);

            exact_op<CPX_IMMEDIATE>(
                Opt::template valid_for<REGF_NZ>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            exact_op<TXA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else if(cpu.def_eq(REG_Y, v))
        {
            chain
            < exact_op<typename Opt::template unrestrict<REGF_Y>, INY_IMPLIED>
            , exact_op<typename Opt::template unrestrict<REGF_Y>, DEY_IMPLIED, Def>
            >(cpu, prev, cont);

            exact_op<CPY_IMMEDIATE>(
                Opt::template valid_for<REGF_NZ>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            exact_op<TYA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else
        {
            std::cout << v.data() << std::endl;
            std::cout << !v.data() << std::endl;
            std::cout << !!(v.data() & 0x80) << std::endl;
            std::cout << cpu.defs[REG_A] << std::endl;
            std::cout << 'a' << (int)cpu.is_known(REG_A) << std::endl;
            std::cout << 'n' << (int)cpu.is_known(REG_N) << std::endl;
            std::cout << 'z' << (int)cpu.is_known(REG_Z) << std::endl;
            std::cout << (int)cpu.known[REG_A] << std::endl;
            std::cout << (int)cpu.known[REG_N] << std::endl;
            std::cout << (int)cpu.known[REG_Z] << std::endl;
            std::cout << v << std::endl;
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
            std::uint8_t const byte = value.data();

            cpu_t cpu_copy;

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<ANC_IMMEDIATE>(opt, {}, value) && cpu_copy.is_known(REG_A, byte))
                cont->call(cpu_copy, &alloc_sel<ANC_IMMEDIATE>(opt, prev, value));

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<LSR_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
                cont->call(cpu_copy, &alloc_sel<LSR_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<ASL_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
                cont->call(cpu_copy, &alloc_sel<ASL_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<ROL_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
                cont->call(cpu_copy, &alloc_sel<ROL_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<ROR_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_A, byte))
                cont->call(cpu_copy, &alloc_sel<ROR_IMPLIED>(opt, prev));

            if(cpu.is_known(REG_A))
            {
                unsigned mask = value.data() << 1;
                if((mask & cpu.known[REG_A] & 0xFF) == mask)
                {
                    assert(mask < 0x100);
                    // ALR can set the carry, or clear the carry.
                    // We'll try to find both:
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        locator_t const arg = locator_t::const_byte(mask | i);
                        cpu_copy = cpu;
                        if(cpu_copy.set_defs_for<ALR_IMMEDIATE>(opt, {}, arg) && cpu_copy.is_known(REG_A, byte))
                            cont->call(cpu_copy, &alloc_sel<ALR_IMMEDIATE>(opt, prev, value));

                        // No point in doing the second iteration if it can't set the carry:
                        if(!(cpu.known[REG_A] & 1))
                            break;
                    }
                }
            }
        }
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_A(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.value_eq(REG_A, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_A))
            return;
        else if(cpu.value_eq(REG_X, v))
            exact_op<TXA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        else if(cpu.value_eq(REG_Y, Def::value()))
            exact_op<TYA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        else
        {
            pick_op<Opt, LDA, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Def>(cpu, prev, cont);

            load_A_impl(Opt::template valid_for<REGF_A | REGF_NZ>::to_struct, v, cpu, prev, cont);
        }
    }

    [[gnu::noinline]]
    void load_X_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(value.is_const_num())
        {
            std::uint8_t const byte = value.data();

            cpu_t cpu_copy;

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<INX_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_X, byte))
                cont->call(cpu_copy, &alloc_sel<INX_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<DEX_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_X, byte))
                cont->call(cpu_copy, &alloc_sel<DEX_IMPLIED>(opt, prev));
        }
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_X(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.value_eq(REG_X, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_X))
            return;
        else if(cpu.value_eq(REG_A, v))
            exact_op<TAX_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        else
        {
            if(cpu.value_eq(REG_Y, v))
            {
                chain
                < exact_op<Opt, TYA_IMPLIED>
                , exact_op<Opt, TAX_IMPLIED, Def>
                >(cpu, prev, cont);
            }

            pick_op<Opt, LDX, Def, Def>(cpu, prev, cont);

            pick_op<Opt, LAX, Def, Def>(cpu, prev, cont);

            load_X_impl(Opt::template valid_for<REGF_X | REGF_NZ>::to_struct, v, cpu, prev, cont);
        }
    }

    [[gnu::noinline]]
    void load_Y_impl(options_t opt, locator_t value, cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(value.is_const_num())
        {
            std::uint8_t const byte = value.data();

            cpu_t cpu_copy;

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<INY_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_X, byte))
                cont->call(cpu_copy, &alloc_sel<INY_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<DEY_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_X, byte))
                cont->call(cpu_copy, &alloc_sel<DEY_IMPLIED>(opt, prev));
        }
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_Y(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.value_eq(REG_Y, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_Y))
            return;
        else if(cpu.value_eq(REG_A, v))
            exact_op<TAY_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        else
        {
            if(cpu.value_eq(REG_X, v))
            {
                chain
                < exact_op<Opt, TXA_IMPLIED>
                , exact_op<Opt, TAY_IMPLIED, Def>
                >(cpu, prev, cont);
            }

            pick_op<Opt, LDY, Def, Def>(cpu, prev, cont);

            load_Y_impl(Opt::template valid_for<REGF_Y | REGF_NZ>::to_struct, v, cpu, prev, cont);
        }
    }

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_C(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        if(cpu.value_eq(REG_C, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_C))
            return;
        else if(v.is_const_num())
        {
            if(v.data())
                exact_op<SEC_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
            else
                exact_op<CLC_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        /* TODO: this may not be valid.
        else if(cpu.reg_eq(REG_NZ, Def::value()))
        {
            if(cpu.reg_eq(REG_C, locator_t::const_byte(0)))
            {
                chain
                < exact_op<Opt, BEQ_IMPLIED, none_, const_<1>>
                , exact_op<typename Opt::add_flag<OPT_CONDITIONAL>, SEC_IMPLIED>
                , set_defs<Opt, REGF_C, Def>
                >(cpu, prev, cont);
            }
            else if(cpu.reg_eq(REG_C, locator_t::const_byte(1)))
            {
                chain
                < exact_op<Opt, BNE_IMPLIED, none_, const_<1>>
                , exact_op<typename Opt::add_flag<OPT_CONDITIONAL>, CLC_IMPLIED>
                , set_defs<Opt, REGF_C, Def>
                >(cpu, prev, cont);
            }
            else
            {
                chain
                < exact_op<Opt, CLC_IMPLIED>
                , exact_op<Opt, BNE_IMPLIED, none_, const_<1>>
                , exact_op<typename Opt::add_flag<OPT_CONDITIONAL>, SEC_IMPLIED>
                , set_defs<Opt, REGF_C, Def>
                >(cpu, prev, cont);
            }
        }
        */
        else
        {
            chain
            < load_A<Opt, Def>
            , exact_op<typename Opt::valid_for<REGF_C>, LSR_IMPLIED, Def>
            >(cpu, prev, cont);

            chain
            < load_A<Opt, Def>
            , exact_op<typename Opt::valid_for<REGF_C>, ALR_IMMEDIATE, Def, const_<1>>
            >(cpu, prev, cont);
        }
    }

    template<typename Opt, typename A, typename X> [[gnu::noinline]]
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

    template<typename Opt, typename A, typename Y> [[gnu::noinline]]
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

    template<typename Opt, typename A, typename C> [[gnu::noinline]]
    void load_AC(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        chain
        < load_C<Opt, C>
        , load_A<typename Opt::restrict_to<~REGF_C>, A>
        >(cpu, prev, cont);
    }

    template<typename Opt, typename Def, typename Arg, op_t AbsoluteX, op_t AbsoluteY
            , bool Enable = (AbsoluteX || AbsoluteY) && (Opt::flags & OPT_NO_DIRECT) < OPT_NO_DIRECT>
    struct pick_op_xy
    {
        [[gnu::noinline]]
        static void call(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
        {
            using OptN = typename Opt::inc_no_direct;

            if(AbsoluteX != BAD_OP)
            {
                chain
                < load_X<OptN, array_index<Arg>>
                , exact_op<Opt, AbsoluteX, Def, array_mem<Arg>>
                >(cpu, prev, cont);
            }

            if(AbsoluteY != BAD_OP)
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
        [[gnu::noinline]]
        static void call(cpu_t const& cpu, sel_t const* prev, cons_t const* cont) {}
    };


    // pick_op impl
    template<typename Opt, op_name_t OpName, typename Def, typename Arg> [[gnu::noinline]]
    void pick_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        constexpr op_t implied    = get_op(OpName, MODE_IMPLIED);
        constexpr op_t relative   = get_op(OpName, MODE_RELATIVE);
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);
        constexpr op_t absolute_X = get_op(OpName, MODE_ABSOLUTE_X);
        constexpr op_t absolute_Y = get_op(OpName, MODE_ABSOLUTE_Y);

        bool const read_direct = Arg::node().holds_ref() && Arg::node()->op() == SSA_cg_read_array_direct;

        if(implied && !Arg::trans())
            exact_op<implied>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else if(relative)
            exact_op<relative>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if(immediate && Arg::trans().is_const_num())
            exact_op<immediate>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if((absolute_X || absolute_Y) && read_direct)
            pick_op_xy<Opt, Def, Arg, absolute_X, absolute_Y>::call(cpu, prev, cont);
        else if(absolute && !Arg::trans().is_const_num() && !read_direct)
            exact_op<absolute>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
    }

    template<typename Opt, typename Label, bool Sec> [[gnu::noinline]]
    void carry_label_clear_conditional(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        using C = typename Opt::add_flags<OPT_CONDITIONAL>;
        using NC = typename Opt::remove_flags<OPT_CONDITIONAL>;

        if(Sec)
        {
            chain
            < load_C<C, const_<1>>
            , label<Label>
            , clear_conditional
            , set_defs_for<NC, SEC_IMPLIED, null_>
            >(cpu, prev, cont);
        }
        else
        {
            chain
            < load_C<C, const_<0>>
            , label<Label>
            , clear_conditional
            , set_defs_for<NC, CLC_IMPLIED, null_>
            >(cpu, prev, cont);
        }

        /*
        chain
        < label<Label>
        , clear_conditional
        >(cpu, prev, cont);
        */
    };


    // Adds a store operation.
    // 'Maybe' means the store may not be required in the final code;
    // such instructions can be pruned later.
    template<typename Opt, op_name_t StoreOp, typename Param, bool Maybe = true> [[gnu::noinline]]
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

    template<typename Opt, typename Def, typename Load, typename Store> [[gnu::noinline]]
    void load_then_store(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        chain
        < load_A<Opt, Load>
        , set_defs<Opt, REGF_A, Def>
        , store<Opt, STA, Store>
        >(cpu, prev, cont);
        assert(cpu_copy == cpu);

        chain
        < load_X<Opt, Load>
        , set_defs<Opt, REGF_X, Def>
        , store<Opt, STX, Store>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, Load>
        , set_defs<Opt, REGF_Y, Def>
        , store<Opt, STY, Store>
        >(cpu, prev, cont);
    };

    static void no_effect(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cont->call(cpu, prev);
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Waddress"
    template<typename Opt, typename Condition, cont_t Then, cont_t Else = nullptr> [[gnu::noinline]]
    static void if_(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cons_t c = { nullptr, cont };
        
        if(Condition::value())
            c.fn = Then ? Then : no_effect;
        else
            c.fn = Else ? Else : no_effect;

        c.call(cpu, prev);
    }
#pragma GCC diagnostic pop

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

    template<typename Opt, typename Def, typename Value> [[gnu::noinline]]
    void sign_extend(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        using OptC = typename Opt::add_flags<OPT_CONDITIONAL>;
        using label = param<i_tag<struct sign_extend_label_tag, 0>>;
        label::set(state.minor_label());

        chain
        < load_A<Opt, const_<0>>
        , load_NZ_for<typename Opt::restrict_to<~REGF_A>, Value>
        , exact_op<Opt, BMI_RELATIVE, null_, label>
        , exact_op<OptC, LDA_IMMEDIATE, null_, const_<0xFF>>
        , clear_conditional
        , set_defs<Opt, REGF_A, Def>
        , store<Opt, STA, Def>
        >(cpu, prev, cont);

        chain
        < load_X<Opt, const_<0>>
        , load_NZ_for<typename Opt::restrict_to<~REGF_X>, Value>
        , exact_op<Opt, BMI_RELATIVE, null_, label>
        , exact_op<OptC, DEX_IMPLIED, null_, null_>
        , clear_conditional
        , set_defs<Opt, REGF_X, Def>
        , store<Opt, STX, Def>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, const_<0>>
        , load_NZ_for<typename Opt::restrict_to<~REGF_Y>, Value>
        , exact_op<Opt, BMI_RELATIVE, null_, label>
        , exact_op<OptC, DEY_IMPLIED, null_, null_>
        , clear_conditional
        , set_defs<Opt, REGF_Y, Def>
        , store<Opt, STY, Def>
        >(cpu, prev, cont);
    }

    template<typename Opt, op_name_t BranchOp, typename FailLabel, typename SuccessLabel>
    void eq_branch(ssa_ht h)
    {
        using OptC = typename Opt::add_flags<OPT_CONDITIONAL>;
        constexpr op_t Op = get_op(BranchOp, MODE_RELATIVE);
        constexpr op_t InverseOp = get_op(invert_branch(BranchOp), MODE_RELATIVE);
        using SignLabel = std::conditional_t<BranchOp == BEQ, FailLabel, SuccessLabel>;
        using sign_check = condition<struct eq_sign_check_tag>;
        using last_iter = condition<struct eq_last_iter_tag>;

        for(unsigned i = 0; i < h->input_size(); i += 2)
        {
            // The last comparison cares about sign.
            sign_check::set(i + 2 == h->input_size());

            select_step([&, i](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
            {
                last_iter::set(i + 2 >= h->input_size());

                for(unsigned j = 0; j < 2; ++j)
                {
                    p_lhs::set(h->input(i + j));
                    p_rhs::set(h->input(i + 1-j));

                    assert(p_lhs::value());
                    assert(p_rhs::value());

                    //assert(p_lhs::trans());
                    //assert(p_rhs::trans());
                    
                    if(p_lhs::value().eq_const_byte(0))
                    {
                        if(p_rhs::value().eq_const_byte(0))
                        {
                            cont->call(cpu, prev);
                            break;
                        }
                    }
                    else if(p_rhs::value().eq_const_byte(0))
                    {
                        chain
                        < load_NZ_for<OptC, p_lhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);
                    }
                    else
                    {
                        chain
                        < load_A<OptC, p_lhs>
                        , if_<OptC, sign_check, chain<load_NZ_for<OptC, p_lhs>,
                                                      exact_op<OptC, BMI_RELATIVE, null_, SignLabel>>>
                        , pick_op<OptC, CMP, null_, p_rhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_X<OptC, p_lhs>
                        , if_<OptC, sign_check, chain<load_NZ_for<OptC, p_lhs>,
                                                      exact_op<OptC, BMI_RELATIVE, null_, SignLabel>>>
                        , pick_op<OptC, CPX, null_, p_rhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);

                        chain
                        < load_Y<OptC, p_lhs>
                        , if_<OptC, sign_check, chain<load_NZ_for<OptC, p_lhs>,
                                                      exact_op<OptC, BMI_RELATIVE, null_, SignLabel>>>
                        , pick_op<OptC, CPY, null_, p_rhs>
                        , exact_op<OptC, InverseOp, null_, FailLabel>
                        >(cpu, prev, cont);
                    }
                }
            });
        }

        select_step(
            chain
            < exact_op<OptC, Op, null_, SuccessLabel>
            , clear_conditional
            >);
    }

    template<op_name_t BranchOp>
    void eq_store(ssa_ht h)
    {
        using fail = p_label<0>;
        using success = p_label<1>;
        using O = options<>;

        // For now, this implementation only loads the result in register X.
        select_step(load_X<O, const_<0>>);

        fail::set(state.minor_label());
        success::set(state.minor_label());

        p_def::set(h);

        eq_branch<O::restrict_to<~REGF_X>, BranchOp, fail, success>(h);

        using OC = O::add_flags<OPT_CONDITIONAL>;
        
        select_step([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
        {
            // Explicitly instantiate the labels.
            // (For some reason, GCC can't link without these lines. Could be a compiler bug.)
            (void)&label<success>;
            (void)&label<fail>;

            chain
            < label<success>
            , exact_op<OC, INX_IMPLIED>
            , clear_conditional
            , label<fail>
            , set_defs<O, REGF_X, p_def>
            , store<O, STX, p_def>
            >(cpu, prev, cont);
        });
    }

    template<typename Opt, typename FailLabel, typename SuccessLabel, bool Flip = false>
    void lt_branch(ssa_ht h)
    {
        using OptC = typename Opt::add_flags<OPT_CONDITIONAL>;
        using last_comp = condition<struct lt_last_comp_tag>;

        type_name_t const lt = type_name_t(h->input(0 ^ Flip).whole());
        type_name_t const rt = type_name_t(h->input(1 ^ Flip).whole());

        int const lwhole = whole_bytes(lt);
        int const rwhole = whole_bytes(rt);
        int const minwhole = std::min(lwhole, rwhole);
        int const maxwhole = std::max(lwhole, rwhole);
        int sbcwhole = minwhole; // This is used to implement the multi-byte subtraction.

        int const lfrac = frac_bytes(lt);
        int const rfrac = frac_bytes(rt);
        int const minfrac = std::min(lfrac, rfrac);
        int const maxfrac = std::max(lfrac, rfrac);

        int const lsize = lwhole + lfrac;
        int const rsize = rwhole + rfrac;

        // Offsets into the node's input array.
        int const loffset = 2 + lfrac + (Flip ? rsize : 0);
        int const roffset = 2 + rfrac + (Flip ? 0 : lsize);
        assert(loffset != roffset);

        bool const lsigned = is_signed(lt);
        bool const rsigned = is_signed(rt);

        auto const validl = [&](int i) { return i >= loffset - lfrac && i < loffset + lwhole; };
        auto const validr = [&](int i) { return i >= roffset - rfrac && i < roffset + rwhole; };

        if(lwhole != rwhole)
        {
            // One number has more whole bytes than the other.
            // We'll compare these bytes to zero.

            // Signed < Signed comparisons should always have the same whole bytes. 
            assert(!(lsigned && rsigned));

            if(lwhole < rwhole)
            {
                for(int i = maxwhole - 1; i >= minwhole; --i)
                {
                    assert(validr(roffset + i));
                    p_rhs::set(h->input(roffset + i));
                    last_comp::set(i == minwhole && sbcwhole <= -maxfrac);

                    if(rsigned && i == maxwhole - 1) // If sign byte
                    {
                        assert(!lsigned);

                        select_step(
                            chain
                            < load_NZ_for<OptC, p_rhs>
                            , exact_op<OptC, BMI_RELATIVE, null_, FailLabel>
                            , exact_op<OptC, BNE_RELATIVE, null_, SuccessLabel>
                            , if_<OptC, last_comp, exact_op<OptC, BEQ_RELATIVE, null_, FailLabel>>
                            >);
                    }
                    else
                    {
                        select_step(
                            chain
                            < load_NZ_for<OptC, p_rhs>
                            , exact_op<OptC, BNE_RELATIVE, null_, SuccessLabel>
                            , if_<OptC, last_comp, exact_op<OptC, BEQ_RELATIVE, null_, FailLabel>>
                            >);
                    }
                }
            }
            else
            {
                for(int i = maxwhole - 1; i >= minwhole; --i)
                {
                    assert(validl(loffset + i));
                    p_lhs::set(h->input(loffset + i));
                    last_comp::set(i == minwhole && sbcwhole <= -maxfrac);

                    if(lsigned && i == maxwhole - 1) // If sign byte
                    {
                        assert(!rsigned);

                        select_step(
                            chain
                            < load_NZ_for<OptC, p_lhs>
                            , exact_op<OptC, BMI_RELATIVE, null_, SuccessLabel>
                            , exact_op<OptC, BNE_RELATIVE, null_, FailLabel>
                            , if_<OptC, last_comp, exact_op<OptC, BEQ_RELATIVE, null_, SuccessLabel>, nullptr>
                            >);
                    }
                    else
                    {
                        select_step(
                            chain
                            < load_NZ_for<OptC, p_lhs>
                            , exact_op<OptC, BNE_RELATIVE, null_, FailLabel>
                            , if_<OptC, last_comp, exact_op<OptC, BEQ_RELATIVE, null_, SuccessLabel>, nullptr>
                            >);
                    }
                }
            }
        }
        else if(lsigned != rsigned)
        {
            // The types have the same number whole bytes, but the signs differ.

            // The multi-byte subtraction won't use the highest bytes.
            sbcwhole -= 1;

            assert(validl(loffset + lwhole - 1));
            assert(validr(roffset + rwhole - 1));

            p_lhs::set(h->input(loffset + lwhole - 1));
            p_rhs::set(h->input(roffset + rwhole - 1));
            last_comp::set(sbcwhole <= -maxfrac);

            if(lsigned)
            {
                select_step([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
                {
                    chain
                    < load_A<OptC, p_lhs>
                    , exact_op<OptC, BMI_RELATIVE, null_, SuccessLabel>
                    , pick_op<OptC, CMP, null_, p_rhs>
                    , exact_op<OptC, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<OptC, last_comp, exact_op<OptC, BCS_RELATIVE, null_, FailLabel>,
                                           exact_op<OptC, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_X<OptC, p_lhs>
                    , exact_op<OptC, BMI_RELATIVE, null_, SuccessLabel>
                    , pick_op<OptC, CPX, null_, p_rhs>
                    , exact_op<OptC, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<OptC, last_comp, exact_op<OptC, BCS_RELATIVE, null_, FailLabel>,
                                           exact_op<OptC, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_Y<OptC, p_lhs>
                    , exact_op<OptC, BMI_RELATIVE, null_, SuccessLabel>
                    , pick_op<OptC, CPY, null_, p_rhs>
                    , exact_op<OptC, BCC_RELATIVE, null_, SuccessLabel>
                    , if_<OptC, last_comp, exact_op<OptC, BCS_RELATIVE, null_, FailLabel>,
                                           exact_op<OptC, BNE_RELATIVE, null_, FailLabel>>
                    >(cpu, prev, cont);
                });
            }
            else // if rsigned
            {
                assert(rsigned);

                select_step([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
                {
                    chain
                    < load_A<OptC, p_rhs>
                    , exact_op<OptC, BMI_RELATIVE, null_, FailLabel>
                    , pick_op<OptC, CMP, null_, p_lhs>
                    , exact_op<OptC, BCC_RELATIVE, null_, FailLabel>
                    , if_<OptC, last_comp, exact_op<OptC, BCS_RELATIVE, null_, SuccessLabel>,
                                          exact_op<OptC, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_X<OptC, p_rhs>
                    , exact_op<OptC, BMI_RELATIVE, null_, FailLabel>
                    , pick_op<OptC, CPX, null_, p_lhs>
                    , exact_op<OptC, BCC_RELATIVE, null_, FailLabel>
                    , if_<OptC, last_comp, exact_op<OptC, BCS_RELATIVE, null_, SuccessLabel>,
                                          exact_op<OptC, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);

                    chain
                    < load_Y<OptC, p_rhs>
                    , exact_op<OptC, BMI_RELATIVE, null_, FailLabel>
                    , pick_op<OptC, CPY, null_, p_lhs>
                    , exact_op<OptC, BCC_RELATIVE, null_, FailLabel>
                    , if_<OptC, last_comp, exact_op<OptC, BCS_RELATIVE, null_, SuccessLabel>,
                                          exact_op<OptC, BNE_RELATIVE, null_, SuccessLabel>>
                    >(cpu, prev, cont);
                });
            }
        }

        // Now do a multi-byte subtraction.

        int iteration = 0; // Tracks comparisons inserted.
        for(int i = -maxfrac; i < sbcwhole; ++i)
        {
            if(i < -lfrac)
            {
                if(validr(i) && h->input(roffset + i).eq_whole(0))
                    continue; // No point to comparing
                else
                    p_lhs::set(ssa_value_t(0, TYPE_U));
            }
            else
                p_lhs::set(h->input(loffset + i));

            if(i < -rfrac)
            {
                if(validl(i) && h->input(loffset + i).eq_whole(0))
                    continue; // No point to comparing.
                else if(i + 1 == maxwhole)
                    p_rhs::set(ssa_value_t(0, TYPE_U));
                else
                    continue; // No point to comparing.
            }
            else
                p_rhs::set(h->input(roffset + i));

            if(iteration == 0) // If this is the first iteration
            {
                if(i + 1 == maxwhole) // If this is the only iteration
                {
                    select_step([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
                    {
                        chain
                        < load_AC<OptC, p_lhs, const_<1>>
                        , pick_op<OptC, SBC, null_, p_rhs>
                        >(cpu, prev, cont);

                        chain
                        < load_C<OptC, const_<1>>
                        , load_AX<typename Opt::restrict_to<~REGF_C>, p_lhs, p_rhs>
                        , iota_op<typename Opt::restrict_to<~REGF_C>, SBC_ABSOLUTE_X, null_>
                        >(cpu, prev, cont);

                        chain
                        < load_C<OptC, const_<1>>
                        , load_AY<typename Opt::restrict_to<~REGF_C>, p_lhs, p_rhs>
                        , iota_op<typename Opt::restrict_to<~REGF_C>, SBC_ABSOLUTE_Y, null_>
                        >(cpu, prev, cont);
                    });
                }
                else
                {
                    // We can use CMP for the first iteration:
                    select_step([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
                    {
                        chain
                        < load_A<OptC, p_lhs>
                        , pick_op<OptC, CMP, null_, p_rhs>
                        >(cpu, prev, cont);

                        chain
                        < load_X<OptC, p_lhs>
                        , pick_op<OptC, CPX, null_, p_rhs>
                        >(cpu, prev, cont);

                        chain
                        < load_Y<OptC, p_lhs>
                        , pick_op<OptC, CPY, null_, p_rhs>
                        >(cpu, prev, cont);
                    });
                }
            }
            else // This isn't the first iteration
            {
                select_step([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
                {
                    chain
                    < load_A<OptC, p_lhs>
                    , pick_op<OptC, SBC, null_, p_rhs>
                    >(cpu, prev, cont);

                    chain
                    < load_AX<OptC, p_lhs, p_rhs>
                    , iota_op<OptC, SBC_ABSOLUTE_X, null_>
                    >(cpu, prev, cont);

                    chain
                    < load_AY<OptC, p_lhs, p_rhs>
                    , iota_op<OptC, SBC_ABSOLUTE_Y, null_>
                    >(cpu, prev, cont);
                });
            }

            ++iteration;

        } // End for

        if(-maxfrac < sbcwhole)
        {
            assert(iteration > 0);

            using p_overflow_label = param<struct lt_overflow_label_tag>;
            p_overflow_label::set(state.minor_label());

            if(lsigned && rsigned)
            {
                select_step(
                    chain
                    < exact_op<OptC, BVC_RELATIVE, null_, p_overflow_label>
                    , exact_op<OptC, EOR_ABSOLUTE, null_, const_<0x80>>
                    , label<p_overflow_label>
                    , clear_conditional
                    , exact_op<OptC, BMI_RELATIVE, null_, SuccessLabel>
                    , exact_op<OptC, BPL_RELATIVE, null_, FailLabel>
                    >);
            }
            else
            {
                select_step(
                    chain
                    < exact_op<OptC, BCC_RELATIVE, null_, SuccessLabel>
                    , exact_op<OptC, BCS_RELATIVE, null_, FailLabel>
                    >);
            }
        }
        else
            assert(iteration == 0);

        select_step(clear_conditional);
    }

    template<bool LTE = false>
    void lt_store(ssa_ht h)
    {
        using fail = p_label<0>;
        using success = p_label<1>;
        using O = options<>;

        // For now, this implementation only loads the result in register X.
        select_step(load_X<O, const_<0>>);

        fail::set(state.minor_label());
        success::set(state.minor_label());

        p_def::set(h);

        lt_branch<O::restrict_to<~REGF_X>, fail, success, LTE>(h);

        using OC = O::add_flags<OPT_CONDITIONAL>;
        
        select_step([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
        {
            // Explicitly instantiate the labels.
            // (For some reason, GCC can't link without these lines. Could be a compiler bug.)
            (void)&label<success>;
            (void)&label<fail>;

            chain
            < label<std::conditional_t<LTE, fail, success>>
            , exact_op<OC, INX_IMPLIED>
            , clear_conditional
            , label<std::conditional_t<LTE, success, fail>>
            , set_defs<O, REGF_X, p_def>
            , store<O, STX, p_def>
            >(cpu, prev, cont);
        });
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
                , exact_op<Opt::valid_for<REGF_A>, ROL_IMPLIED, p_def>
                , store<Opt, STA, p_def, false>
                >(cpu, prev, cont);

                chain
                < load_X<Opt, const_<0>>
                , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<OptC, INX_IMPLIED>
                , carry_label_clear_conditional<Opt, p_label<0>, false>
                , set_defs<Opt, REGF_X | REGF_C, p_def>
                , store<Opt, STX, p_def, false>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, const_<0>>
                , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                , exact_op<OptC, INY_IMPLIED>
                , carry_label_clear_conditional<Opt, p_label<0>, false>
                , set_defs<Opt, REGF_Y | REGF_C, p_def>
                , store<Opt, STY, p_def, false>
                >(cpu, prev, cont);
            }
            else
            {
                cpu_t new_cpu = cpu;
                if(new_cpu.set_def<REG_C>(Opt::to_struct, p_def::value(), true))
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
                , exact_op<Opt::valid_for<REGF_A | REGF_NZ>, ROL_IMPLIED, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_carry::value().eq_const_byte(0))
                    chain
                    < load_A<Opt, p_lhs>
                    , exact_op<Opt::valid_for<REGF_A | REGF_NZ>, ASL_IMPLIED, p_def>
                    , store<Opt, STA, p_def>
                    >(cpu, prev, cont);
            }

            commutative(h, [&]()
            {
                chain
                < load_AC<Opt, p_lhs, p_carry>
                , pick_op<Opt::valid_for<REGF_A | REGF_NZ>, ADC, p_def, p_rhs>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AC<Opt, p_lhs, p_carry>
                , load_X<Opt::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<Opt::valid_for<REGF_A | REGF_NZ>, ADC_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AC<Opt, p_lhs, p_carry>
                , load_Y<Opt::restrict_to<~REGF_AC>, p_rhs>
                , iota_op<Opt::valid_for<REGF_A | REGF_NZ>, ADC_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_rhs::value().is_const_num())
                {
                    p_label<0>::set(state.minor_label());

                    if(p_rhs::value().data() == 0 && !carry_used(*h))
                    {
                        chain
                        < load_C<Opt, p_carry>
                        , load_X<Opt, p_lhs>
                        , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, INX_IMPLIED>
                        , carry_label_clear_conditional<Opt, p_label<0>, false>
                        , set_defs<Opt, REGF_X, p_def>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_C<Opt, p_carry>
                        , load_Y<Opt, p_lhs>
                        , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, INY_IMPLIED>
                        , carry_label_clear_conditional<Opt, p_label<0>, false>
                        , set_defs<Opt, REGF_Y, p_def>
                        , store<Opt, STY, p_def>
                        >(cpu, prev, cont);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                            , pick_op<OptC, INC, p_def, p_def>
                            , carry_label_clear_conditional<Opt, p_label<0>, false>
                            >(cpu, prev, cont);
                        }
                    }

                    if(p_rhs::value().data() == 0xFF && !carry_used(*h))
                    {
                        p_label<0>::set(state.minor_label());

                        chain
                        < load_C<Opt, p_carry>
                        , load_X<Opt, p_lhs>
                        , exact_op<Opt, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, DEX_IMPLIED>
                        , label<p_label<0>>
                        , clear_conditional
                        , set_defs<Opt, REGF_X, p_def>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_C<Opt, p_carry>
                        , load_Y<Opt, p_lhs>
                        , exact_op<Opt, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, DEY_IMPLIED>
                        , label<p_label<0>>
                        , clear_conditional
                        , set_defs<Opt, REGF_Y, p_def>
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
                        p_temp::set(ssa_value_t((0x100 - p_rhs::value().data() - !!p_carry::value().data()) & 0xFF, TYPE_U));

                        chain
                        < load_AX<Opt, p_lhs, p_lhs>
                        , exact_op<Opt::valid_for<REGF_X | REGF_NZ>, AXS_IMMEDIATE, p_def, p_temp>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        if((p_rhs::value().data() + !!p_carry::value().data()) == 1 && !carry_used(*h))
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

                        if(((p_rhs::value().data() + !!p_carry::value().data()) & 0xFF) == 0xFF && !carry_used(*h))
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
                , exact_op<Opt::valid_for<REGF_X | REGF_NZ>, AXS_IMMEDIATE, p_def, const_<0>>
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

        case SSA_xor:
            commutative(h, [&]()
            {
                chain
                < load_A<Opt, p_lhs>
                , pick_op<Opt, EOR, p_def, p_rhs>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AX<Opt, p_lhs, p_rhs>
                , iota_op<Opt, EOR_ABSOLUTE_X, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, p_lhs, p_rhs>
                , iota_op<Opt, EOR_ABSOLUTE_Y, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                // TODO: consider using add instructions, 
                // checking constraints when it's applicable.
            });
            break;

        case SSA_sign_extend:
            {
                p_arg<0>::set(h->input(0));
                sign_extend<Opt, p_def, p_arg<0>>(cpu, prev, cont);
            }
            break;

        case SSA_early_store:
            p_arg<0>::set(h->input(0));
            load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            break;

        case SSA_read_global:
            if(h->input(1).locator().mem_head() != cset_locator(h))
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

        case SSA_aliased_store:
            // Aliased stores normally produce no code, however, 
            // we must implement an input 'cg_read_array_direct' as a store:
            if(h->input(0).holds_ref() && h->input(0)->op() == SSA_cg_read_array_direct)
            {
                h = h->input(0).handle();
                goto do_read_array_direct;
            }
            else
                cont->call(cpu, prev);
            break;


        case SSA_read_array:
            do_read_array_direct:
            {
                using p_array = p_arg<0>;
                using p_index = p_arg<1>;

                p_array::set(h->input(0));
                p_index::set(h->input(2));

                chain
                < load_X<Opt, p_index>
                , exact_op<Opt, LDA_ABSOLUTE_X, p_def, p_array>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < load_X<Opt, p_index>
                , exact_op<Opt, LDY_ABSOLUTE_X, p_def, p_array>
                , store<Opt, STY, p_def>
                >(cpu, prev, cont);

                chain
                < load_Y<Opt, p_index>
                , exact_op<Opt, LDA_ABSOLUTE_Y, p_def, p_array>
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
            , set_defs<Opt, REGF_CPU, null_>
            >(cpu, prev, cont);
            break;

        case SSA_goto_mode:
            p_arg<0>::set(h->input(0));
            chain
            < exact_op<Opt, JMP_ABSOLUTE, null_, p_arg<0>>
            , set_defs<Opt, REGF_CPU, null_>
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
            (load_NZ<>{ h->input(0) }
            >>= def_op<BNE>{ {}, locator_t::cfg_label(cfg_node->output(0)) }
            >>= def_op<BEQ>{ {}, locator_t::cfg_label(cfg_node->output(1)) }
            >>= finish)(cpu, prev);
            break;
            */

        case SSA_multi_eq:
        case SSA_multi_not_eq:
        case SSA_multi_lt:
        case SSA_multi_lte:
        case SSA_branch_eq:
        case SSA_branch_not_eq:
        case SSA_branch_lt:
        case SSA_branch_lte:
        case SSA_entry:
        case SSA_uninitialized:
        case SSA_cg_read_array_direct:
            cont->call(cpu, prev);
            break;
        default:
            throw std::runtime_error(fmt("Unhandled SSA op in code gen: %", h->op()));
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
        case SSA_multi_eq:
            eq_store<BEQ>(h); 
            break;

        case SSA_multi_not_eq: 
            eq_store<BNE>(h); 
            break;

        case SSA_multi_lt:
            lt_store<false>(h);
            break;

        case SSA_multi_lte:
            lt_store<true>(h);
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
            lt_branch<Opt, p_label<0>, p_label<1>, false>(h);
            break;

        case SSA_branch_lte:
            p_label<0>::set(locator_t::cfg_label(state.fn, cfg_node->output(1)));
            p_label<1>::set(locator_t::cfg_label(state.fn, cfg_node->output(0)));
            lt_branch<Opt, p_label<0>, p_label<1>, true>(h);
            break;

        case SSA_return:
        case SSA_fn_call:
        case SSA_goto_mode:
            write_globals<Opt>(h);
            break;

        case SSA_init_array:
            // TODO
            assert(0);
            for(unsigned i = 0; i < h->input_size(); ++i)
            {
                /*
                p_arg<0>::set(h->input(0));
                p_arg<1>::set(e);
                select_step(load_then_store<Opt, p_arg<0>, p_arg<1>>);

                {
                    chain
                    <
    template<typename Opt, typename Def, typename Load, typename Store> [[gnu::noinline]]
    void load_then_store(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)

                });
                */
            }
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

do_selections:
    for(ssa_ht h : cd.schedule)
        cg_data(h).isel = {}; // Reset isel data.
    state.sel_pool.clear();
    state.map.clear();
    assert(state.map.empty());
    state.best_cost = ~0 - COST_CUTOFF;
    state.best_sel = nullptr;

    // Starting state:
    state.map.insert({ cpu_t{}, nullptr });

    for(ssa_ht h : cd.schedule)
    {
        try
        {
            isel_node(h);
        }
        catch(isel_no_progress_error_t const&)
        {
            // Try and repair.
            bool repaired = false;
            for_each_node_input(h, [&](ssa_ht input)
            {
                if(input->cfg_node() != cfg_node)
                    return;

                if(input->op() == SSA_cg_read_array_direct)
                {
                    input->unsafe_set_op(SSA_read_array);
                    repaired = true;
                }
            });

            if(repaired)
                goto do_selections;
            else
                throw;
        }
        catch(...)
        {
            throw;
        }
    }

    std::vector<asm_inst_t> code;
    for(sel_t const* sel = state.best_sel; sel; sel = sel->prev)
        code.push_back(sel->inst);
    code.push_back({ ASM_LABEL, SSA_null, locator_t::cfg_label(state.fn, cfg_node) });
    std::reverse(code.begin(), code.end());

    return code;
}
