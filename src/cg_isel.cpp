#include "cg_isel.hpp"

#include <cstdint>
#include <iostream> // TODO
#include <functional>
#include <vector>

#include "robin/combine.hpp"
#include "robin/map.hpp"

#include "array_pool.hpp"

using regs_array_t = std::array<ssa_value_t, NUM_REGS>;

namespace std
{
    template<>
    struct hash<regs_array_t>
    {
        std::size_t operator()(regs_array_t const& regs) const noexcept
        {
            std::size_t h = 0;
            for(ssa_value_t const& v : regs)
                h = rh::hash_combine(h, v.target());
            return h;
        }
    };
}

namespace isel
{
    static thread_local rh::batman_map<regs_array_t, sel_t const*> map;
    static thread_local rh::batman_map<regs_array_t, sel_t const*> next_map;

    static thread_local unsigned best_cost = ~0;
    static thread_local unsigned next_best_cost = ~0;
    static thread_local sel_t const* best_sel = nullptr;

    static thread_local array_pool_t<sel_t> sel_pool;

    unsigned get_cost(sel_t const* sel)
    {
        return sel ? sel->cost : 0;
    }

    template<op_t Op>
    constexpr unsigned cost_fn = (op_cycles(Op) << 8) + op_size(Op);

    template<op_t Op, typename... Ts>
    sel_t& alloc_sel(sel_t const* prev, ssa_value_t arg, 
                     unsigned extra_cost = 0)
    {
        return sel_pool.emplace(
            prev, Op, get_cost(prev) + cost_fn<Op> + extra_cost, arg);
    }


    template<typename A, typename B> [[gnu::flatten]]
    auto operator>>=(A a, B b)
    { 
        return [=](regs_array_t regs, sel_t const* prev)
        { 
            a(regs, prev, b); 
        };
    }

    template<regs_t Regs, regs_t CanSet = REGF_ALL>
    struct set_regs
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
        {
            if((Regs & CanSet) == Regs)
            {
                if(Regs & REGF_A)
                    regs[REG_A] = v;
                if(Regs & REGF_X)
                    regs[REG_X] = v;
                if(Regs & REGF_Y)
                    regs[REG_Y] = v;
                if(Regs & REGF_C)
                    regs[REG_C] = v;
                cont(regs, prev);
            }
            else
                assert(false);
        }
    };

    template<op_t Op, regs_t CanSet>
    using set_regs_for = set_regs<op_output_regs(Op), CanSet>;

    template<op_t Op, regs_t CanSet>
    constexpr bool can_set_regs_for = 
        (CanSet & op_output_regs(Op)) == op_output_regs(Op);

    template<op_name_t OpName, regs_t CanSet = REGF_ALL>
    struct def_op
    {
        ssa_value_t def;
        ssa_value_t arg;

        template<typename Cont> 
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const;
    };

    template<regs_t CanSet = REGF_ALL>
    struct load_A
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
        {
            if((CanSet & REGF_A) == 0)
            {
                assert(false);
                return;
            }

            assert(v);
            assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            if(regs[REG_A] == v)
                cont(regs, prev);
            else if(regs[REG_X] == v)
                (def_op<TXA, CanSet>{v} >>= cont)(regs, prev);
            else if(regs[REG_Y] == v)
                (def_op<TYA, CanSet>{v} >>= cont)(regs, prev);
            else
            {
                (def_op<LDA, CanSet>{v, v} >>= cont)(regs, prev);
                if(CanSet & REGF_X)
                    (def_op<LAX, CanSet>{v, v} >>= cont)(regs, prev);
            }
        }
    };

    template<regs_t CanSet = REGF_ALL>
    struct load_X
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
        {
            if((CanSet & REGF_X) == 0)
            {
                assert(false);
                return;
            }

            assert(v);
            assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            if(regs[REG_X] == v)
                cont(regs, prev);
            else if(regs[REG_A] == v)
                def_op<TAX, CanSet>{v}(regs, prev, cont);
            else
            {
                def_op<LDX, CanSet>{v, v}(regs, prev, cont);
                if(CanSet & REGF_A)
                    def_op<LAX, CanSet>{v, v}(regs, prev, cont);
            }
        }
    };

    template<regs_t CanSet = REGF_ALL>
    struct load_Y
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
        {
            if((CanSet & REGF_Y) == 0)
            {
                assert(false);
                return;
            }

            assert(v);
            assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            if(regs[REG_Y] == v)
                cont(regs, prev);
            else if(regs[REG_A] == v)
                def_op<TAY, CanSet>{v}(regs, prev, cont);
            else
                def_op<LDY, CanSet>{v, v}(regs, prev, cont);
        }
    };

    template<regs_t CanSet = REGF_ALL>
    struct load_AX
    {
        ssa_value_t v;

        template<typename Cont> 
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
        {
            if((CanSet & REGF_AX) != REGF_AX)
            {
                assert(false);
                return;
            }

            assert(v);
            assert(!v.holds_ref() || v->type() == TYPE_BYTE);

            if(regs[REG_A] == v && regs[REG_X] == v)
                cont(regs, prev);
            else if(regs[REG_X] == v)
                def_op<TXA, CanSet>{v}(regs, prev, cont);
            else if(regs[REG_A] == v)
                def_op<TAX, CanSet>{v}(regs, prev, cont);
            else if(v.is_num())
                (def_op<LDA, CanSet>{v, v}
                 >>= def_op<TAX, CanSet>{v}
                 >>= cont)(regs, prev);
            else
                def_op<LAX, CanSet>{v, v}(regs, prev, cont);
        }
    };

    template<regs_t CanSet = REGF_ALL>
    struct load_CA
    {
        ssa_value_t c;
        ssa_value_t a;

        template<typename Cont> 
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
        {
            if((CanSet & REGF_AC) != REGF_AC)
            {
                assert(false);
                return;
            }

            assert(a && c);

            if(regs[REG_C] != c)
            {
                if(c.is_handle())
                {
                    assert(c.handle());

                    ([=,*this](regs_array_t regs, sel_t const* prev, auto cont)
                    { cont(regs, &alloc_sel<LDA_ABSOLUTE>(prev, 
                        locator_t::carry(c.handle()))); }
                    >>= def_op<PHA, CanSet>{}
                    >>= def_op<PLP, CanSet>{c}
                    >>= load_A<CanSet & ~REGF_C>{a}
                    >>= cont)(regs, prev);
                }
                else if(c.is_num())
                {
                    // TODO: use constraints instead
                    if(c.carry())
                    {
                        (def_op<SEC, CanSet>{c}
                        >>= load_A<CanSet & ~REGF_C>{a}
                        >>= cont)(regs, prev);

                        if((CanSet & REGF_A) && a.is_num() 
                           && regs[REG_A].is_num())
                        {
                            unsigned const r = regs[REG_A].whole();
                            if((r & 1) == 1 && (r >> 1) == a.whole())
                                (def_op<LSR, CanSet>{a}
                                 >>= set_regs<REGF_C, CanSet>{1u}
                                 >>= cont)(regs, prev);
                        }
                    }
                    else
                    {
                        (def_op<CLC, CanSet>{c}
                        >>= load_A<CanSet & ~REGF_C>{a}
                        >>= cont)(regs, prev);

                        if((CanSet & REGF_A) && a.is_num() 
                           && regs[REG_A].is_num())
                        {
                            unsigned const r = regs[REG_A].whole();
                            if((r >> 1) == a.whole())
                            {
                                if((r & 1) == 0)
                                    (def_op<LSR, CanSet>{a}
                                     >>= set_regs<REGF_C, CanSet>{0u}
                                     >>= cont)(regs, prev);
                                else
                                    (def_op<ALR, CanSet>{a, 0xFEu}
                                     >>= set_regs<REGF_C, CanSet>{0u}
                                     >>= cont)(regs, prev);
                            }

                            if(a.whole() == 0)
                                def_op<ANC, CanSet>{0u, 0u}(regs, prev, cont);
                        }
                    }
                }
                else
                    assert(false);
            }
            else
                return load_A<CanSet>{a}(regs, prev, cont);
        }
    };

    // TODO!!!

    template<op_name_t OpName, regs_t CanSet, 
        bool Enable = (CanSet & REGF_X) 
        && get_op(OpName, MODE_ABSOLUTE_X) != BAD_OP>
    struct impl_addr_X
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, regs_array_t regs, 
                         sel_t const* prev, Cont cont)
        {
            constexpr op_t absolute_X = get_op(OpName, MODE_ABSOLUTE_X);
            static_assert(absolute_X);

            load_X<REGF_X>{h->input(1)}(regs, prev, 
            [=](regs_array_t regs, sel_t const* prev)
            {
                set_regs_for<absolute_X, CanSet>{def}(
                    regs, &alloc_sel<absolute_X>(prev, h->input(0)), cont);
            });
        }
    };

    template<op_name_t OpName, regs_t CanSet>
    struct impl_addr_X<OpName, CanSet, false>
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, regs_array_t regs, 
                         sel_t const* prev, Cont cont)
        {}
    };

    template<op_name_t OpName, regs_t CanSet, 
        bool Enable = (CanSet & REGF_Y) 
        && get_op(OpName, MODE_ABSOLUTE_Y) != BAD_OP>
    struct impl_addr_Y
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, regs_array_t regs, 
                         sel_t const* prev, Cont cont)
        {
            constexpr op_t absolute_Y = get_op(OpName, MODE_ABSOLUTE_Y);
            static_assert(absolute_Y);

            load_Y<REGF_Y>{h->input(1)}(regs, prev, 
            [=](regs_array_t regs, sel_t const* prev)
            {
                set_regs_for<absolute_Y, CanSet>{def}(
                    regs, &alloc_sel<absolute_Y>(prev, h->input(0)), cont);
            });
        }
    };

    template<op_name_t OpName, regs_t CanSet>
    struct impl_addr_Y<OpName, CanSet, false>
    {
        template<typename Cont>
        static void func(ssa_value_t def, ssa_ht h, regs_array_t regs, 
                         sel_t const* prev, Cont cont)
        {}
    };

    template<op_name_t OpName, regs_t CanSet>
    template<typename Cont>
    void def_op<OpName, CanSet>
    ::operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
    {
        constexpr op_t implied    = get_op(OpName, MODE_IMPLIED);
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);

        if(implied && !arg)
        {
            if(can_set_regs_for<implied, CanSet>)
                (set_regs_for<implied, CanSet>{def} >>= cont)
                    (regs, &alloc_sel<implied>(prev, {}));
            else
                assert(false);
        }
        else if(immediate && arg.is_num())
        {
            if(can_set_regs_for<immediate, CanSet>)
                (set_regs_for<immediate, CanSet>{def} >>= cont)
                    (regs, &alloc_sel<immediate>(prev, arg));
            else
                assert(false);
        }
        else if(absolute && !arg.is_num())
        {
            goto locator_store; // TODO
            if(arg.is_locator())
                goto locator_store;

            assert(!arg.is_num());
            assert(arg.is_handle());

            if(arg->test_flags(FLAG_STORED))
            {
            locator_store:
                if(can_set_regs_for<absolute, CanSet>)
                    (set_regs_for<absolute, CanSet>{def} >>= cont)
                        (regs, &alloc_sel<absolute>(prev, arg));
                else
                    assert(false);
                return;
            }

            ssa_ht h = arg.handle();

            // If the node was stored into a locator, and that locator still
            // holds the node, we can use the locator as the memory address.
            /* TODO
            for(locator_t loc : data(h).stores_into_locators)
            {
                if(loc_live[loc] == h)
                {
                    cont(regs, sel_t<absolute>(prev, loc));
                    return;
                }
            }
            */

            // If the node is an array read, try loading either X or Y
            // with the index, then using an indexed addressing mode.
            if(h->op() == SSA_read_array)
            {
                impl_addr_X<OpName, CanSet>::func(
                    def, h, regs, prev, cont);

                impl_addr_Y<OpName, CanSet>::func(
                    def, h, regs, prev, cont);
            }

            // Penalize loading from unstored memory:
            constexpr unsigned UNSTORED_PENALTY = cost_fn<STA_ABSOLUTE> + 1;
            if(can_set_regs_for<absolute, CanSet>)
                (set_regs_for<absolute, CanSet>{def} >>= cont)
                    (regs, &alloc_sel<absolute>(prev, arg, UNSTORED_PENALTY));
            else
                assert(false);
        }
    }

    /* TODO
    struct  // TODO
    {
        std::pair<ssa_ht, locator_t>* to_write;
        unsigned size;
        unsigned i;

        template<typename Cont>
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont)
        {
            if(i == size)
                return;

            lda
        }
    };

    void write_globals(ssa_ht h, regs_array_t regs, sel_t const* prev)
    {
        fc::small_multimap<locator_t, ssa_value_t, 16> needs_load;

        for_each_written_global(h, [](ssa_value_t v, locator_t loc)
        {
            if(regs[REG_A] == pair.second)
                ptr = &alloc_sel<STA_ABSOLUTE>(*ptr, v));
            else if(regs[REG_X] == pair.second)
                ptr = &alloc_sel<STX_ABSOLUTE>(*ptr, v));
            else if(regs[REG_Y] == pair.second)
                ptr = &alloc_sel<STY_ABSOLUTE>(*ptr, v));
            else
                needs_load.emplace(loc, v);
        });

        sel_t const* ptr = &prev;

        for(auto const& pair : needs_load)
        {
            load_A<false>{pair.second}(regs, *prev,
            [&](regs_array_t new_regs, isel_t const& new_isel)
            {
                regs = new_regs;
                ptr = &alloc_sel<STA_ABSOLUTE>(new_isel, v);
            });
        }
    }
    */

    void finish(regs_array_t regs, sel_t const* sel)
    {
        auto result = next_map.insert({ regs, sel });

        unsigned const sel_cost = get_cost(sel);
        //std::cout << "FINISH " << sel_cost << '\n';

        if(!result.inserted && sel_cost < get_cost(*result.mapped))
            *result.mapped = sel;

        if(sel_cost < next_best_cost)
        {
            next_best_cost = sel_cost;
            best_sel = sel;
        }
    }
    template<op_t StoreOp = BAD_OP>
    struct store
    {
        ssa_ht h;

        template<typename Cont>
        void operator()(regs_array_t regs, sel_t const* prev, Cont cont) const
        {
            cont(regs, alloc_sel<StoreOp>(prev, h));
        }

        void operator()(regs_array_t regs, sel_t const* prev) const
        {
            finish(regs, &alloc_sel<StoreOp>(prev, h));
        }
    };

    void run_isel(ssa_ht h, regs_array_t regs, sel_t const* const prev)
    {
        switch(h->op())
        {
        case SSA_add:
            {
                ssa_value_t const carry = h->input(0);

                for(unsigned i = 0; i < 2; ++i)
                {
                    ssa_value_t const lhs = h->input(1+i);
                    ssa_value_t const rhs = h->input(2-i);

                    (load_CA<>{ carry, lhs }
                    >>= def_op<ADC>{ h, rhs }
                    >>= store<MAYBE_STA>{h})(regs, prev);
                }

                if(!has_output_matching(h, INPUT_CARRY) && carry.is_const())
                {
                    for(unsigned i = 0; i < 2; ++i)
                    {
                        ssa_value_t const lhs = h->input(1+i);
                        ssa_value_t const rhs = h->input(2-i);

                        if(!rhs.is_const())
                            continue;

                        unsigned const axs_arg =
                            (0x100 - rhs.whole() - carry.carry()) & 0xFF;

                        (load_AX<>{ lhs }
                        >>= def_op<AXS>{ h, axs_arg }
                        >>= set_regs<REGF_C>{}
                        >>= store<MAYBE_STX>{h})(regs, prev);

                        (load_X<>{ lhs }
                        >>= load_A<~REGF_X>{ 0xFFu }
                        >>= def_op<AXS>{ h, axs_arg }
                        >>= set_regs<REGF_C>{}
                        >>= store<MAYBE_STX>{h})(regs, prev);

                        (load_A<>{ 0xFFu }
                        >>= load_X<~REGF_A>{ lhs }
                        >>= def_op<AXS>{ h, axs_arg }
                        >>= set_regs<REGF_C>{}
                        >>= store<MAYBE_STX>{h})(regs, prev);
                    }
                }
            }
            break;

        case SSA_and:
            for(unsigned i = 0; i < 2; ++i)
            {
                ssa_value_t const lhs = h->input(i);
                ssa_value_t const rhs = h->input(1-i);

                (load_A<>{ lhs }
                >>= def_op<AND>{ h, rhs }
                >>= store<MAYBE_STA>{h})(regs, prev);

                (load_A<>{ lhs }
                >>= load_X<~REGF_A>{ rhs }
                >>= def_op<AXS>{ h, 0xFFu }
                >>= set_regs<REGF_C>{}
                >>= store<MAYBE_STX>{h})(regs, prev);

                (load_X<>{ lhs }
                >>= load_A<~REGF_X>{ rhs }
                >>= def_op<AXS>{ h, 0xFFu }
                >>= set_regs<REGF_C>{}
                >>= store<MAYBE_STX>{h})(regs, prev);

                (load_A<>{ lhs }
                >>= load_X<~REGF_A>{ rhs }
                >>= store<MAYBE_SAX>{h})(regs, prev);

                (load_X<>{ lhs }
                >>= load_A<~REGF_X>{ rhs }
                >>= store<MAYBE_SAX>{h})(regs, prev);
            }
            break;

        case SSA_read_global:
            (def_op<LDA>{ h, h->input(1) }
            >>= store<MAYBE_STA>{ h })(regs, prev);
            break;

        case SSA_return:
            // TODO
            def_op<RTS>{}(regs, prev, finish);
            break;

        default:
            finish(regs, prev);
            break;
        }
    }

} // namespace isel

sel_t const* select_instructions(ssa_ht const* schedule_begin, 
                                 ssa_ht const* schedule_end)
{
    using namespace isel;

    constexpr unsigned COST_CUTOFF = cost_fn<LDA_ABSOLUTE> * 3;

    sel_pool.clear();
    map.clear();
    assert(map.empty());
    best_cost = ~0 - COST_CUTOFF;
    best_sel = nullptr;

    // Starting state:
    map.insert({ regs_array_t{}, nullptr });

        for(auto const& pair : map)
        {
            std::cout << "- \n";
            sel_t const* sel = pair.second;
            while(sel)
            {
                std::cout << " - - START " << to_string(sel->op) << ' ' << sel->cost << '\n';
                sel = sel->prev;
            }
        }

    for(ssa_ht const* it = schedule_begin; it < schedule_end; ++it)
    {
        assert(it);
        std::cout << "\n----\n";
        std::cout << "SSA OP = " << (*it)->op() << '\n';
        next_map.clear();
        next_best_cost = ~0 - COST_CUTOFF;

        for(auto const& pair : map)
            if(get_cost(pair.second) < best_cost + COST_CUTOFF)
                run_isel(*it, pair.first, pair.second);

        for(auto const& pair : next_map)
        {
            std::cout << "- \n";
            sel_t const* sel = pair.second;
            while(sel)
            {
                std::cout << " - - sel " << to_string(sel->op) << ' ' << sel->cost << '\n';
                sel = sel->prev;
            }
        }


        assert(!next_map.empty());
        map.swap(next_map);
        best_cost = next_best_cost;
    }

    std::cout << "BEST COST = " << best_cost << ' ' << get_cost(best_sel) << '\n';

    //assert(best_sel);
    return best_sel;
}
