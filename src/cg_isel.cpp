#include "cg_isel.hpp"

#include <cstdint>
#include <iostream> // TODO
#include <functional>
#include <type_traits>
#include <vector>
#include <unordered_map> // TODO

#include <boost/container/small_vector.hpp>

#include "robin/hash.hpp"
#include "robin/map.hpp"

#include "array_pool.hpp"
#include "format.hpp"
#include "globals.hpp"
#include "group.hpp"
#include "cg_isel_cpu.hpp"
#include "cg_cset.hpp"
#include "options.hpp"
#include "ir_algo.hpp"
#include "worklist.hpp"

namespace bc = ::boost::container;

namespace isel
{
    // Backbone state of the instruction selection algorithm.
    struct state_t
    {
        //rh::batman_map<cpu_t, sel_t const*> map;
        //rh::batman_map<cpu_t, sel_t const*> next_map;

        using map_t = rh::batman_map<cpu_t, sel_t const*>;
        //using map_t = robin_hood::unordered_map<cpu_t, sel_t const*>;
        //using map_t = std::unordered_map<cpu_t, sel_t const*, T...>;
        //using map_t = std::unordered_map<cpu_t, sel_t const*>;

        map_t map;
        map_t next_map;
        //map_t<isel::approximate_hash_t, isel::approximate_eq_t> approx_map;

        array_pool_t<sel_t> sel_pool;

        unsigned best_cost = ~0;
        unsigned next_best_cost = ~0;
        //sel_t const* best_sel = nullptr;

        cfg_ht cfg_node;
        ssa_ht ssa_node;

        unsigned next_label = 0;
        unsigned next_var = 0;

        fn_ht fn;

        locator_t minor_label() { return locator_t::minor_label(next_label++); }
        locator_t minor_var() { return locator_t::minor_var(fn, next_var++); }
    };

    // Main global state of the instruction selection algorithm.
    thread_local state_t state;

    inline locator_t ssa_to_value(ssa_value_t v)
    {
        return locator_t::from_ssa_value(orig_def(v)).with_byteified(false);
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
            _value = ssa_to_value(v);
            _trans = asm_arg(v).with_byteified(false);
        }

        [[gnu::always_inline]]
        static ssa_value_t node() { return _node; }

        [[gnu::always_inline]]
        static locator_t value() { return _value; }

        [[gnu::always_inline]]
        static locator_t trans() { return _trans; }

        [[gnu::always_inline]]
        static locator_t trans_hi() { return {}; }
    };

    template<typename Param>
    struct array_index
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(2); }

        [[gnu::always_inline]]
        static locator_t value() { return ssa_to_value(node()); }

        [[gnu::always_inline]]
        static locator_t trans() { return asm_arg(node()); }

        [[gnu::always_inline]]
        static locator_t trans_hi() { return {}; }
    };

    template<typename Param>
    struct array_mem
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node()->input(0); }

        [[gnu::always_inline]]
        static locator_t value() { return ssa_to_value(node()); }

        [[gnu::always_inline]]
        static locator_t trans() { return asm_arg(node()); }

        [[gnu::always_inline]]
        static locator_t trans_hi() { return {}; }
    };

    struct null_
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return {}; }

        [[gnu::always_inline]]
        static locator_t value() { return {}; }

        [[gnu::always_inline]]
        static locator_t trans() { return {}; }

        [[gnu::always_inline]]
        static locator_t trans_hi() { return {}; }

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

        [[gnu::always_inline]]
        static locator_t trans_hi() { return {}; }
    };

    template<typename Param, typename PtrHi>
    struct set_ptr_hi
    {
        [[gnu::always_inline]]
        static ssa_value_t node() { return Param::node(); }

        [[gnu::always_inline]]
        static locator_t value() { return Param::value(); }

        [[gnu::always_inline]]
        static locator_t trans() { return Param::trans(); }

        [[gnu::always_inline]]
        static locator_t trans_hi() { return PtrHi::trans(); }
    };

    // TODO: remove this?
    [[gnu::always_inline]]
    unsigned get_cost(sel_t const* sel)
    {
        assert(sel);
        return sel->cost;
    }

    constexpr unsigned op_penalty(op_t op)
    {
        unsigned penalty = 0;

        // Penalize X / Y, preferring A.
        if((op_output_regs(op) | op_input_regs(op)) & (REGF_X | REGF_Y))
            penalty += 2;

        switch(op_name(op))
        {
        default: 
            break;
        // Very slightly penalize ROL/ROR, to prefer LSR/ASL:
        case ROL:
        case ROR:
        // Very slighty penalize LAX, to prefer LDA or LDX:
        case LAX: 
        // Same with SAX and AND:
        case SAX: 
            penalty += 1;
        }

        return penalty;
    }

    constexpr isel_cost_t cost_fn(op_t op) 
    { 
        return (op_cycles(op) * 256ull) + op_size(op) + op_penalty(op);
    }

    template<op_t Op>
    sel_t& alloc_sel(options_t opt, sel_t const* prev, locator_t arg = {}, locator_t ptr_hi = {}, unsigned extra_cost = 0)
    {
        unsigned total_cost = cost_fn(Op);
        if(opt.flags & OPT_CONDITIONAL)
            total_cost = (total_cost * 3) / 4; // Conditional ops are cheaper.
        total_cost += get_cost(prev) + extra_cost;

        return state.sel_pool.emplace(prev, total_cost, asm_inst_t{ Op, state.ssa_node->op(), arg, ptr_hi /*, total_cost */ });
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

    // Marks the node as stored without any cost.
    // This is used when a node has been aliased and doesn't need a MAYBE_STORE at all.
    template<typename Def>
    void ignore_req_store(cpu_t const& cpu, sel_t const* sel, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;

        ssa_value_t const n = Def::node();
        if(n.holds_ref())
            cpu_copy.req_store |= cg_data(n.handle()).isel.store_mask;

        cont->call(cpu_copy, sel);
    }

    // These determine how extensive the search is.
    constexpr unsigned MAX_MAP_SIZE = 128;
    //constexpr unsigned SHRINK_MAP_SIZE = MAX_MAP_SIZE / 2;
    constexpr unsigned cost_cutoff(unsigned size)
    {
        constexpr unsigned BASE = cost_fn(LDA_ABSOLUTE) * 3;
        return BASE >> (size >> 4);
    }

    // Finishes the selection.
    template<bool FinishNode>
    void finish(cpu_t const& cpu, sel_t const* sel, cons_t const*)
    {
        state_t::map_t::value_type insertion = { cpu, sel };

        unsigned const sel_cost = get_cost(sel);

        if(sel_cost > state.next_best_cost + cost_cutoff(state.next_map.size()))
            return;

        // If this completes a node's operations, we'll release 'req_store'.
        if(FinishNode) // TODO: remove cast
            (std::uint64_t&)insertion.first.req_store &= ~cg_data(state.ssa_node).isel.last_use;

        auto result = state.next_map.insert(std::move(insertion));

        if(!result.second && sel_cost < get_cost(result.first->second))
            result.first->second = sel;

        if(sel_cost < state.next_best_cost)
        {
            state.next_best_cost = sel_cost;
            //state.best_sel = sel; // TODO
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
    template<bool FinishNode, typename Fn>
    void select_step(Fn fn)
    {
        //std::cout << state.sel_pool.size() << std::endl;
        state.next_map.clear();
        state.next_best_cost = ~0 - cost_cutoff(0);

        cons_t const cont = { finish<FinishNode> };

        //std::cout << "BEST COST = " << state.best_cost << std::endl;

        unsigned const cutoff = cost_cutoff(state.map.size());
        unsigned count = 0;

        for(auto const& pair : state.map)
            if(get_cost(pair.second) <= state.best_cost + cutoff)
            {
                fn(pair.first, pair.second, &cont);
                count += 1;
            }

        if(state.next_map.empty())
            throw isel_no_progress_error_t{};

        //std::cout << "FINISH count = " << count << ' ' << state.map.size() << ' ' << state.ssa_node->op() << std::endl;

        if(state.next_map.size() > MAX_MAP_SIZE)
        {
            std::cout << "OLD SIZE = " << state.next_map.size() << std::endl;

#if 0
            state.map.clear();

            auto it = state.next_map.begin();
            auto const end = state.next_map.end() - 1;
            for(; it < end; it += 2)
            {
                auto const next = std::next(it);
                if(get_cost(it->second) < get_cost(next->second))
                    state.map.insert(*it);
                else
                    state.map.insert(*next);
            }

            if(it < state.next_map.end())
                state.map.insert(*it);
#else

            auto const rank = [&](auto const& a) -> int
            {
                return (int(get_cost(a.second)) 
                        + int(builtin::popcount(a.first.req_store))
                        - int(builtin::popcount(unsigned(a.first.known_mask))));
            };

            std::sort(state.next_map.begin(), state.next_map.end(), [&](auto const& a, auto const& b)
            {
                return rank(a) < rank(b);
            });

            assert(get_cost(state.next_map.begin()->second) == state.next_best_cost);

            state.map.clear();
            for(unsigned i = 0; i < MAX_MAP_SIZE; ++i)
                state.map.insert(state.next_map.begin()[i]);
#endif


            /*
            state.approx_map.clear();
            for(auto const& pair : state.next_map)
            {
                auto result = state.approx_map.insert(pair);

                if(!result.second && get_cost(pair.second) < get_cost(result.first->second))
                    result.first->second = pair.second;
            }

            state.next_map.clear();
            for(auto const& pair : state.approx_map)
                state.next_map.insert(pair);
                */

            std::cout << "NEW SIZE = " << state.map.size() << std::endl;
        }
        else
            state.map.swap(state.next_map);
        state.best_cost = state.next_best_cost;
    }

    template<typename Opt, regs_t Regs, bool KeepValue, typename Param> [[gnu::noinline]]
    void set_defs(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_defs<Regs>(Opt::to_struct, Param::value(), KeepValue))
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

    template<op_t Op>
    unsigned handle_req_store_penalty(cpu_t& cpu, ssa_value_t def, ssa_value_t arg)
    {
        if(!arg.holds_ref())
            return 0;

        ssa_ht const arg_h = arg.handle();
        auto& d = cg_data(arg_h);

        // Nodes that belong to this CFG node are tracked more
        // precisely using the 'req_store' part of 'cpu_t'.
        if(arg_h->cfg_node() != state.cfg_node)
            return 0; 

        // Determine if this node has been stored yet.
        // If it has, there won't be a penalty.
        unsigned const new_stores = builtin::popcount(~cpu.req_store & d.isel.store_mask);

        // Mark the node as stored.
        cpu.req_store |= d.isel.store_mask;
        assert(!(~cpu.req_store & d.isel.store_mask));

        // If this is a store, mark it immediately without cost.
        if(!xy_addr_mode(op_addr_mode(Op)) && (op_output_regs(Op) & REGF_M) 
           && !(op_flags(Op) & ASMF_MAYBE_STORE) && def.holds_ref() && def.handle() == state.ssa_node)
        {
            assert(def.handle() != arg_h);
            cpu.req_store |= cg_data(def.handle()).isel.store_mask;
        }

        return cost_fn(STA_ABSOLUTE) * new_stores;
    }

    template<op_t Op>
    bool valid_arg(locator_t arg)
    {
#ifdef NDEBUG
        return true;
#else
        switch(op_addr_mode(Op))
        {
        case MODE_IMPLIED: 
            return !arg;
        case MODE_RELATIVE:
        case MODE_LONG:
            return is_label(arg.lclass());
        case MODE_IMMEDIATE:
            return arg.is_immediate();
        case MODE_ZERO_PAGE:
        case MODE_ZERO_PAGE_X:
        case MODE_ZERO_PAGE_Y:
        case MODE_ABSOLUTE:
        case MODE_ABSOLUTE_X:
        case MODE_ABSOLUTE_Y:
        case MODE_INDIRECT:
        case MODE_INDIRECT_X:
        case MODE_INDIRECT_Y:
            return bool(arg);
        default:
            return true;
        }
#endif
    }

    // Spits out the op specified.
    // NOTE: You generally shouldn't use this for any op that uses
    // a memory argument, as that argument can't be a SSA_cg_read_array_direct.
    // Thus, prefer pick_op.
    template<op_t Op> [[gnu::noinline]]
    void exact_op(options_t opt, locator_t def, locator_t arg, locator_t ptr_hi, ssa_value_t ssa_def, ssa_value_t ssa_arg,
                  cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        constexpr auto mode = op_addr_mode(Op);

        assert(valid_arg<Op>(arg));

        cpu_t cpu_copy = cpu;

        if(cpu_copy.set_defs_for<Op>(opt, def, arg))
        {
            unsigned penalty = 0;
            if((op_input_regs(Op) & REGF_M)
               && (mode == MODE_ZERO_PAGE 
                   || mode == MODE_ABSOLUTE 
                   || indirect_addr_mode(mode)))
            {
                penalty = handle_req_store_penalty<Op>(cpu_copy, ssa_def, ssa_arg);
            }

            cont->call(cpu_copy, &alloc_sel<Op>(opt, prev, arg, ptr_hi, penalty));
        }
    }

    template<typename Opt, op_t Op, typename Def = null_, typename Arg = null_> [[gnu::noinline]]
    void exact_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        exact_op<Op>(Opt::to_struct, Def::value(), Arg::trans(), Arg::trans_hi(), Def::node(), Arg::node(), cpu, prev, cont);
    }

    // Like exact_op, but with a simplified set of parameters.
    // Only supports a few addressing modes.
    template<op_t Op>
    void simple_op(options_t opt, locator_t def, locator_t arg, cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
#ifndef NDEBUG
        constexpr auto mode = op_addr_mode(Op);
        assert(mode == MODE_IMPLIED || mode == MODE_RELATIVE || mode == MODE_IMMEDIATE);
        assert(valid_arg<Op>(arg));
#endif
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_defs_for<Op>(opt, def, arg))
            cont->call(cpu_copy, &alloc_sel<Op>(opt, prev, arg, {}, 0));
    }

    // Generates an op using the 0..255 table.
    template<typename Opt, op_t Op, typename Def = null_> [[gnu::noinline]]
    void iota_op(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        static_assert(op_addr_mode(Op) == MODE_ABSOLUTE_X || op_addr_mode(Op) == MODE_ABSOLUTE_Y);
        cpu_t cpu_copy = cpu;
        if(cpu_copy.set_output_defs<Op>(Opt::to_struct, Def::value()))
            cont->call(cpu_copy, &alloc_sel<Op>(Opt::to_struct, prev, locator_t::runtime_rom(RTROM_iota)));
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
            simple_op<EOR_IMMEDIATE>(
                Opt::template unrestrict<REGF_A>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            simple_op<TAX_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);

            simple_op<TAY_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else if(cpu.def_eq(REG_X, v))
        {
            chain
            < exact_op<typename Opt::unrestrict<REGF_X>, INX_IMPLIED>
            , exact_op<typename Opt::unrestrict<REGF_X>, DEX_IMPLIED, Def>
            >(cpu, prev, cont);

            simple_op<CPX_IMMEDIATE>(
                Opt::template valid_for<REGF_NZ>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            simple_op<TXA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        }
        else if(cpu.def_eq(REG_Y, v))
        {
            chain
            < exact_op<typename Opt::template unrestrict<REGF_Y>, INY_IMPLIED>
            , exact_op<typename Opt::template unrestrict<REGF_Y>, DEY_IMPLIED, Def>
            >(cpu, prev, cont);

            simple_op<CPY_IMMEDIATE>(
                Opt::template valid_for<REGF_NZ>::to_struct, 
                v, locator_t::const_byte(0),
                cpu, prev, cont);

            simple_op<TYA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
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
            simple_op<TXA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
        else if(cpu.value_eq(REG_Y, Def::value()))
            simple_op<TYA_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
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
            simple_op<TAX_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
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
            if(cpu_copy.set_defs_for<INY_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_Y, byte))
                cont->call(cpu_copy, &alloc_sel<INY_IMPLIED>(opt, prev));

            cpu_copy = cpu;
            if(cpu_copy.set_defs_for<DEY_IMPLIED>(opt, {}, {}) && cpu_copy.is_known(REG_Y, byte))
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
            simple_op<TAY_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
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
                simple_op<SEC_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
            else
                simple_op<CLC_IMPLIED>(Opt::to_struct, v, {}, cpu, prev, cont);
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

    template<typename Opt, typename Def, typename Arg, op_t AbsoluteX, op_t AbsoluteY, op_t Absolute
            , bool Enable = (AbsoluteX || AbsoluteY) && (Opt::flags & OPT_NO_DIRECT) < OPT_NO_DIRECT>
    struct pick_op_xy
    {
        [[gnu::noinline]]
        static void call(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
        {
            using OptN = typename Opt::inc_no_direct;

            locator_t const index = array_index<Arg>::value();

            if(Absolute != BAD_OP && index.is_const_num())
            {
                locator_t mem = array_mem<Arg>::trans();
                mem.advance_offset(index.data());

                exact_op<Absolute>(
                    OptN::to_struct, Def::value(),  mem, locator_t{}, 
                    Def::node(), array_mem<Arg>::node(), cpu, prev, cont);
            }
            else
            {
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
        }
    };

    template<typename Opt, typename Def, typename Arg, op_t AbsoluteX, op_t AbsoluteY, op_t Absolute>
    struct pick_op_xy<Opt, Def, Arg, AbsoluteX, AbsoluteY, Absolute, false>
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
            simple_op<implied>(Opt::to_struct, Def::value(), {}, cpu, prev, cont);
        else if(relative)
            simple_op<relative>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if(immediate && Arg::trans().is_immediate())
            simple_op<immediate>(Opt::to_struct, Def::value(), Arg::trans(), cpu, prev, cont);
        else if((absolute_X || absolute_Y) && read_direct)
            pick_op_xy<Opt, Def, Arg, absolute_X, absolute_Y, absolute>::call(cpu, prev, cont);
        else if(absolute && !Arg::trans().is_immediate() && !read_direct)
            exact_op<absolute>(Opt::to_struct, Def::value(), Arg::trans(), Arg::trans_hi(), Def::node(), Arg::node(), cpu, prev, cont);
    }

    template<typename Opt, typename Label, bool Sec> [[gnu::noinline]]
    void maybe_carry_label_clear_conditional(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        carry_label_clear_conditional<Opt, Label, Sec>(cpu, prev, cont);

        chain
        < label<Label>
        , clear_conditional
        >(cpu, prev, cont);
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
        static_assert(!(Opt::flags & OPT_CONDITIONAL), "Conditional stores likely break cg_liveness"); 
        // Store the node, locally:
        if(Maybe/* && Param::trans().lclass() == LOC_SSA*/)
        {
            int penalty = 0;
            ssa_value_t const n = Param::node();
            if(n.holds_ref() && n->cfg_node() == state.cfg_node)
            {
                static_assert(cost_fn(LDA_ABSOLUTE) >= cost_fn(MAYBE_STA));

                if(cg_data(n.handle()).isel.likely_store)
                {
                    penalty = cost_fn(LDA_ABSOLUTE) - cost_fn(MAYBE_STA);
                }
            }

            switch(StoreOp)
            {
            case STA: cont->call(cpu, &alloc_sel<MAYBE_STA>(Opt::to_struct, prev, Param::trans(), {}, penalty)); break;
            case STX: cont->call(cpu, &alloc_sel<MAYBE_STX>(Opt::to_struct, prev, Param::trans(), {}, penalty)); break;
            case STY: cont->call(cpu, &alloc_sel<MAYBE_STY>(Opt::to_struct, prev, Param::trans(), {}, penalty)); break;
            case SAX: cont->call(cpu, &alloc_sel<MAYBE_SAX>(Opt::to_struct, prev, Param::trans(), {}, penalty)); break;
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

    template<typename Opt, typename Def, typename Load, typename Store, bool Maybe = true> [[gnu::noinline]]
    void load_then_store(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        if(Load::trans() == Store::trans())
        {
            ignore_req_store<Def>(cpu, prev, cont);
            return;
        }

        chain
        < load_A<Opt, Load>
        , set_defs<Opt, REGF_A, true, Def>
        , store<Opt, STA, Store, Maybe>
        >(cpu, prev, cont);

        chain
        < load_X<Opt, Load>
        , set_defs<Opt, REGF_X, true, Def>
        , store<Opt, STX, Store, Maybe>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, Load>
        , set_defs<Opt, REGF_Y, true, Def>
        , store<Opt, STY, Store, Maybe>
        >(cpu, prev, cont);
    };

    template<typename Opt, typename Def> [[gnu::noinline]]
    void load_B(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const v = Def::value();

        std::uint16_t const bs_addr = bankswitch_addr(mapper().type);

        if(!mapper().bankswitches() || cpu.value_eq(REG_B, v))
            cont->call(cpu, prev);
        else if(!(Opt::can_set & REGF_B))
            return;
        else
        {
            if(has_bus_conflicts(mapper().type))
            {
                chain
                < load_AX<Opt, Def, Def>
                , iota_op<Opt, STA_ABSOLUTE_X, null_>
                , set_defs<Opt, REGF_B, true, Def>
                >(cpu, prev, cont);

                chain
                < load_AY<Opt, Def, Def>
                , iota_op<Opt, STA_ABSOLUTE_Y, null_>
                , set_defs<Opt, REGF_B, true, Def>
                >(cpu, prev, cont);
            }
            else if(state_size(mapper().type))
            {
                using addr = param<struct load_B_addr_tag>;
                addr::set(locator_t::addr(bs_addr));

                using state = param<struct load_B_state_tag>;
                state::set(locator_t::runtime_ram(RTRAM_mapper_state));

                // TODO: track the mapper state as another CPU register.

                chain
                < load_A<Opt, Def>
                , exact_op<Opt, ORA_ABSOLUTE, null_, state>
                , exact_op<Opt, STA_ABSOLUTE, null_, addr>
                , set_defs<Opt, REGF_B, true, Def>
                >(cpu, prev, cont);
            }
            else
            {
                using addr = param<struct load_B_addr_tag>;
                addr::set(locator_t::addr(bs_addr));

                chain
                < load_then_store<Opt, Def, Def, addr, false>
                , set_defs<Opt, REGF_B, true, Def>
                >(cpu, prev, cont);
            }
        }
    }

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
        , set_defs<Opt, REGF_A, true, Def>
        , store<Opt, STA, Def>
        >(cpu, prev, cont);

        chain
        < load_X<Opt, const_<0>>
        , load_NZ_for<typename Opt::restrict_to<~REGF_X>, Value>
        , exact_op<Opt, BMI_RELATIVE, null_, label>
        , exact_op<OptC, DEX_IMPLIED, null_, null_>
        , clear_conditional
        , set_defs<Opt, REGF_X, true, Def>
        , store<Opt, STX, Def>
        >(cpu, prev, cont);

        chain
        < load_Y<Opt, const_<0>>
        , load_NZ_for<typename Opt::restrict_to<~REGF_Y>, Value>
        , exact_op<Opt, BMI_RELATIVE, null_, label>
        , exact_op<OptC, DEY_IMPLIED, null_, null_>
        , clear_conditional
        , set_defs<Opt, REGF_Y, true, Def>
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

            select_step<false>([&, i](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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

        select_step<false>(
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
        select_step<false>(load_X<O, const_<0>>);

        fail::set(state.minor_label());
        success::set(state.minor_label());

        p_def::set(h);

        eq_branch<O::restrict_to<~REGF_X>, BranchOp, fail, success>(h);

        using OC = O::add_flags<OPT_CONDITIONAL>;
        
        select_step<true>([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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
            , set_defs<O, REGF_X, true, p_def>
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

        //std::cout << "LT " << type_t(lt) << ' ' << type_t(rt) << std::endl;

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

                        select_step<false>(
                            chain
                            < load_NZ_for<OptC, p_rhs>
                            , exact_op<OptC, BMI_RELATIVE, null_, FailLabel>
                            , exact_op<OptC, BNE_RELATIVE, null_, SuccessLabel>
                            , if_<OptC, last_comp, exact_op<OptC, BEQ_RELATIVE, null_, FailLabel>>
                            >);
                    }
                    else
                    {
                        select_step<false>(
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

                        select_step<false>(
                            chain
                            < load_NZ_for<OptC, p_lhs>
                            , exact_op<OptC, BMI_RELATIVE, null_, SuccessLabel>
                            , exact_op<OptC, BNE_RELATIVE, null_, FailLabel>
                            , if_<OptC, last_comp, exact_op<OptC, BEQ_RELATIVE, null_, SuccessLabel>, nullptr>
                            >);
                    }
                    else
                    {
                        select_step<false>(
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
                select_step<false>([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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

                select_step<false>([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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
                if(i + 1 == maxwhole && lsigned && rsigned) // If this is the only iteration, and we're signed
                {
                    select_step<false>([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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
                    select_step<false>([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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
                select_step<false>([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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
                select_step<false>(
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
                select_step<false>(
                    chain
                    < exact_op<OptC, BCC_RELATIVE, null_, SuccessLabel>
                    , exact_op<OptC, BCS_RELATIVE, null_, FailLabel>
                    >);
            }
        }
        else
            assert(iteration == 0);

        select_step<false>(clear_conditional);
    }

    template<bool LTE = false>
    void lt_store(ssa_ht h)
    {
        using fail = p_label<0>;
        using success = p_label<1>;
        using O = options<>;

        // For now, this implementation only loads the result in register X.
        select_step<false>(load_X<O, const_<0>>);

        fail::set(state.minor_label());
        success::set(state.minor_label());

        p_def::set(h);

        lt_branch<O::restrict_to<~REGF_X>, fail, success, LTE>(h);

        using OC = O::add_flags<OPT_CONDITIONAL>;
        
        select_step<true>([&](cpu_t const& cpu, sel_t const* const prev, cons_t const* cont)
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
            , set_defs<O, REGF_X, true, p_def>
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

            select_step<true>(load_then_store<Opt, p_def, p_def, p_arg<0>, false>);
        });
    }

    template<typename Opt, typename Def, typename Array, typename Index>
    void read_array(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const index = Index::value();

        if(index.is_const_num())
        {
            using temp = param<struct read_array_tag>;

            locator_t mem = Array::trans();
            mem.advance_offset(index.data());
            mem.set_is(IS_DEREF);
            temp::set(mem);

            load_then_store<Opt, Def, temp, Def>(cpu, prev, cont);
        }
        else
        {
            chain
            < load_X<Opt, Index>
            , exact_op<Opt, LDA_ABSOLUTE_X, Def, Array>
            , store<Opt, STA, Def>
            >(cpu, prev, cont);

            chain
            < load_X<Opt, Index>
            , exact_op<Opt, LDY_ABSOLUTE_X, Def, Array>
            , store<Opt, STY, Def>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, Index>
            , exact_op<Opt, LDA_ABSOLUTE_Y, Def, Array>
            , store<Opt, STA, Def>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, Index>
            , exact_op<Opt, LDX_ABSOLUTE_Y, Def, Array>
            , store<Opt, STX, Def>
            >(cpu, prev, cont);
        }
    }

    template<typename Opt, typename Array, typename Index, typename Assignment>
    void write_array(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        locator_t const index = Index::value();

        if(index.is_const_num())
        {
            using temp = param<struct write_array_tag>;

            locator_t mem = Array::trans();
            mem.advance_offset(index.data());
            mem.set_is(IS_DEREF);
            temp::set(mem);

            load_then_store<Opt, Assignment, Assignment, temp, false>(cpu, prev, cont);
        }
        else
        {
            chain
            < load_AX<Opt, Assignment, Index>
            , exact_op<Opt, STA_ABSOLUTE_X, null_, Array>
            >(cpu, prev, cont);

            chain
            < load_AY<Opt, Assignment, Index>
            , exact_op<Opt, STA_ABSOLUTE_Y, null_, Array>
            >(cpu, prev, cont);
        }
    }

    template<typename Opt, typename Def>
    void store_carry(cpu_t const& cpu, sel_t const* prev, cons_t const* cont)
    {
        using OptC = typename Opt::add_flags<OPT_CONDITIONAL>;

        ssa_value_t v = Def::node();

        if(v.holds_ref() && (v->output_size() > 1 || (v->output_size() == 1 && v->output(0)->cfg_node() != v->cfg_node())))
        {
            p_label<0>::set(state.minor_label());

            chain
            < exact_op<Opt, AND_IMMEDIATE, null_, const_<0>>
            , exact_op<typename Opt::valid_for<REGF_A>, ROL_IMPLIED, p_def>
            , store<Opt, STA, p_def, false>
            >(cpu, prev, cont);

            chain
            < load_X<Opt, const_<0>>
            , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
            , exact_op<OptC, INX_IMPLIED>
            , carry_label_clear_conditional<Opt, p_label<0>, false>
            , set_defs<Opt, REGF_X | REGF_C, true, p_def>
            , store<Opt, STX, p_def, false>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, const_<0>>
            , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
            , exact_op<OptC, INY_IMPLIED>
            , carry_label_clear_conditional<Opt, p_label<0>, false>
            , set_defs<Opt, REGF_Y | REGF_C, true, p_def>
            , store<Opt, STY, p_def, false>
            >(cpu, prev, cont);
        }
        else
        {
            cpu_t new_cpu = cpu;
            if(new_cpu.set_def<REG_C>(Opt::to_struct, Def::value(), true))
                cont->call(new_cpu, &alloc_sel<MAYBE_STORE_C>(Opt::to_struct, prev, Def::trans()));
        }
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
            store_carry<Opt, p_def>(cpu, prev, cont);
            break;

        case SSA_add:
            //p_lhs::set(h->input(0));
            //p_rhs::set(h->input(1));
            p_carry::set(h->input(2));

            // TODO: This should be a math identity, right?
            /* TODO: remove
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
            */

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
                        , maybe_carry_label_clear_conditional<Opt, p_label<0>, false>
                        , set_defs<Opt, REGF_X, true, p_def>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_C<Opt, p_carry>
                        , load_Y<Opt, p_lhs>
                        , exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, INY_IMPLIED>
                        , maybe_carry_label_clear_conditional<Opt, p_label<0>, false>
                        , set_defs<Opt, REGF_Y, true, p_def>
                        , store<Opt, STY, p_def>
                        >(cpu, prev, cont);

                        if(p_def::trans() == p_lhs::trans())
                        {
                            chain
                            < exact_op<Opt, BCC_RELATIVE, null_, p_label<0>>
                            , pick_op<OptC, INC, p_def, p_lhs>
                            , maybe_carry_label_clear_conditional<Opt, p_label<0>, false>
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
                        , set_defs<Opt, REGF_X, true, p_def>
                        , store<Opt, STX, p_def>
                        >(cpu, prev, cont);

                        chain
                        < load_C<Opt, p_carry>
                        , load_Y<Opt, p_lhs>
                        , exact_op<Opt, BCS_RELATIVE, null_, p_label<0>>
                        , exact_op<OptC, DEY_IMPLIED>
                        , label<p_label<0>>
                        , clear_conditional
                        , set_defs<Opt, REGF_Y, true, p_def>
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
                                pick_op<Opt, INC, p_def, p_lhs>(cpu, prev, cont);
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

        case SSA_rol:
            p_lhs::set(h->input(0));
            p_rhs::set(h->input(1));
            if(h->input(1).eq_whole(0u))
            {
                chain
                < load_A<Opt, p_lhs>
                , exact_op<Opt, ASL_IMPLIED, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_def::trans() == p_lhs::trans())
                    pick_op<Opt, ASL, p_def, p_lhs>(cpu, prev, cont);
            }
            else
            {
                chain
                < load_AC<Opt, p_lhs, p_rhs>
                , exact_op<Opt, ROL_IMPLIED, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_def::trans() == p_lhs::trans())
                {
                    chain
                    < load_C<Opt, p_rhs>
                    , pick_op<Opt, ROL, p_def, p_lhs>
                    >(cpu, prev, cont);
                }
            }
            break;

        case SSA_ror:
            p_lhs::set(h->input(0));
            p_rhs::set(h->input(1));
            if(h->input(1).eq_whole(0u))
            {
                chain
                < load_A<Opt, p_lhs>
                , exact_op<Opt, LSR_IMPLIED, p_def>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_def::trans() == p_lhs::trans())
                    pick_op<Opt, LSR, p_def, p_lhs>(cpu, prev, cont);
            }
            else
            {
                chain
                < load_AC<Opt, p_lhs, p_rhs>
                , exact_op<Opt, ROR_IMPLIED, p_lhs>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                if(p_def::trans() == p_lhs::trans())
                {
                    chain
                    < load_C<Opt, p_rhs>
                    , pick_op<Opt, ROR, p_def, p_lhs>
                    >(cpu, prev, cont);
                }
            }
            break;
            
        case SSA_sign_extend:
            {
                p_arg<0>::set(h->input(0));
                sign_extend<Opt, p_def, p_arg<0>>(cpu, prev, cont);
            }
            break;

        case SSA_sign_to_carry:
            {
                p_arg<0>::set(h->input(0));
                chain
                < load_A<Opt, p_lhs>
                , exact_op<Opt, CMP_IMMEDIATE, p_def, const_<0x80>>
                , store_carry<Opt, p_def>
                >(cpu, prev, cont);
            }
            break;

        case SSA_read_hw:
            p_arg<0>::set(h->input(0));

            if(h->output_size() == 0) // TODO: handle linked?
                exact_op<Opt, IGN_ABSOLUTE, p_def, p_arg<0>>(cpu, prev, cont);
            else
            {
                chain
                < exact_op<Opt, LDA_ABSOLUTE, p_def, p_arg<0>>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);

                chain
                < exact_op<Opt, LDX_ABSOLUTE, p_def, p_arg<0>>
                , store<Opt, STX, p_def>
                >(cpu, prev, cont);

                chain
                < exact_op<Opt, LDY_ABSOLUTE, p_def, p_arg<0>>
                , store<Opt, STY, p_def>
                >(cpu, prev, cont);

                chain
                < exact_op<Opt, LAX_ABSOLUTE, p_def, p_arg<0>>
                , store<Opt, STA, p_def>
                >(cpu, prev, cont);
            }

            break;

        case SSA_write_hw:
            p_arg<0>::set(h->input(0));
            p_arg<1>::set(h->input(1));

            chain
            < load_A<Opt, p_arg<1>>
            , exact_op<Opt, STA_ABSOLUTE, null_, p_arg<0>>
            >(cpu, prev, cont);

            chain
            < load_X<Opt, p_arg<1>>
            , exact_op<Opt, STX_ABSOLUTE, null_, p_arg<0>>
            >(cpu, prev, cont);

            chain
            < load_Y<Opt, p_arg<1>>
            , exact_op<Opt, STY_ABSOLUTE, null_, p_arg<0>>
            >(cpu, prev, cont);

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
                ignore_req_store<p_def>(cpu, prev, cont);
            break;

        case SSA_phi_copy:
            if(!h->input(0).holds_ref() || cset_head(h) != cset_head(h->input(0).handle()))
            {
                p_arg<0>::set(h->input(0));
                load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            }
            else
                ignore_req_store<p_def>(cpu, prev, cont);
            break;

        case SSA_phi:
            {
                assert(h->input_size() > 0);
                locator_t const loc = cset_locator(h->input(0).handle());

                if(cset_head(h) != cset_head(h->input(0).handle()))
                {
                    if(cpu.def_eq(REG_A, loc))
                        store<Opt, STA, p_def>(cpu, prev, cont);
                    else if(cpu.def_eq(REG_X, loc))
                        store<Opt, STX, p_def>(cpu, prev, cont);
                    else if(cpu.def_eq(REG_Y, loc))
                        store<Opt, STY, p_def>(cpu, prev, cont);
                    else
                    {
                        p_arg<0>::set(h->input(0));
                        load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
                    }
                }
                else
                {
                    if(cpu.def_eq(REG_A, loc))
                        chain
                        < set_defs<Opt, REGF_A, true, p_def>
                        , ignore_req_store<p_def>
                        >(cpu, prev, cont);
                    else if(cpu.def_eq(REG_X, loc))
                        chain
                        < set_defs<Opt, REGF_X, true, p_def>
                        , ignore_req_store<p_def>
                        >(cpu, prev, cont);
                    else if(cpu.def_eq(REG_Y, loc))
                        chain
                        < set_defs<Opt, REGF_Y, true, p_def>
                        , ignore_req_store<p_def>
                        >(cpu, prev, cont);
                    else
                        ignore_req_store<p_def>(cpu, prev, cont);
                }
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
                ignore_req_store<p_def>(cpu, prev, cont);
            break;

        case SSA_write_array:
            if(h->input(0).holds_ref() && cset_head(h->input(0).handle()) != cset_head(h))
            {
                // TODO! Insert an array copy here.
                throw std::runtime_error("Unimplemented array copy.");
            }
            else
            {
                using p_array = p_arg<0>;
                using p_index = p_arg<1>;
                using p_assignment = p_arg<2>;

                p_array::set(h->input(0));
                p_index::set(h->input(2));
                p_assignment::set(h->input(3));

                write_array<Opt, p_array, p_index, p_assignment>(cpu, prev, cont);
            }
            break;

        case SSA_read_array:
        do_read_array_direct:
            {
                using p_array = p_arg<0>;
                using p_index = p_arg<1>;

                p_array::set(h->input(0));
                p_index::set(h->input(2));

                read_array<Opt, p_def, p_array, p_index>(cpu, prev, cont);
            }

            break;

        case SSA_read_ptr:
            {
                using p_ptr_lo = p_arg<0>;
                using p_ptr_hi = p_arg<1>;
                using p_ptr = set_ptr_hi<p_ptr_lo, p_ptr_hi>;
                using p_index = p_arg<2>;

                using namespace ssai::rw_ptr;

                p_ptr_lo::set(h->input(PTR));
                p_ptr_hi::set(h->input(PTR_HI));
                p_index::set(h->input(INDEX));

                if(h->input(PTR).is_const()) // TODO
                {
                    assert(!h->input(PTR_HI) || h->input(PTR_HI).is_const());
                    read_array<Opt, p_def, p_ptr, p_index>(cpu, prev, cont);
                }
                else
                {
                    if(h->input(INDEX).eq_whole(0))
                    {
                        chain
                        < load_X<Opt, const_<0>>
                        , exact_op<Opt, LDA_INDIRECT_X, p_def, p_ptr>
                        , store<Opt, STA, p_def>
                        >(cpu, prev, cont);
                    }

                    chain
                    < load_Y<Opt, p_index>
                    , exact_op<Opt, LDA_INDIRECT_Y, p_def, p_ptr>
                    , store<Opt, STA, p_def>
                    >(cpu, prev, cont);

                    chain
                    < load_Y<Opt, p_index>
                    , exact_op<Opt, LAX_INDIRECT_Y, p_def, p_ptr>
                    , store<Opt, STA, p_def>
                    >(cpu, prev, cont);
                }
            }
            break;

        case SSA_write_ptr:
            {
                using p_ptr_lo = p_arg<0>;
                using p_ptr_hi = p_arg<1>;
                using p_ptr = set_ptr_hi<p_ptr_lo, p_ptr_hi>;
                using p_index = p_arg<2>;
                using p_assignment = p_arg<3>;

                using namespace ssai::rw_ptr;

                p_ptr_lo::set(h->input(PTR));
                p_ptr_hi::set(h->input(PTR_HI));
                p_index::set(h->input(INDEX));
                p_assignment::set(h->input(ASSIGNMENT));

                if(h->input(PTR).is_const()) // TODO
                {
                    assert(!h->input(PTR_HI) || h->input(PTR_HI).is_const());
                    write_array<Opt, p_ptr_lo, p_index, p_assignment>(cpu, prev, cont);
                }
                else
                {
                    if(h->input(INDEX).eq_whole(0))
                    {
                        chain
                        < load_AX<Opt, p_assignment, const_<0>>
                        , exact_op<Opt, STA_INDIRECT_X, null_, p_ptr>
                        >(cpu, prev, cont);
                    }

                    chain
                    < load_AY<Opt, p_assignment, p_index>
                    , exact_op<Opt, STA_INDIRECT_Y, null_, p_ptr>
                    >(cpu, prev, cont);
                }
            }
            break;

        case SSA_make_ptr_lo:
        case SSA_make_ptr_hi:
            p_arg<0>::set(h->input(1));
            load_then_store<Opt, p_def, p_arg<0>, p_def>(cpu, prev, cont);
            break;

        case SSA_fn_call:
            assert(h->input(0).is_locator());
            p_arg<0>::set(h->input(0));
            p_arg<1>::set(h->input(0).locator().with_is(IS_BANK));
            chain
            < load_Y<Opt, p_arg<1>>
            , exact_op<Opt, BANKED_Y_JSR, null_, p_arg<0>>
            , set_defs<Opt, REGF_CPU & ~REGF_B, false, null_>
            >(cpu, prev, cont);
            break;

        case SSA_return:
            switch(state.fn->fclass)
            {
            case FN_MODE:
                p_label<0>::set(locator_t::runtime_rom(RTROM_reset));
                exact_op<Opt, JMP_ABSOLUTE, null_, p_label<0>>(cpu, prev, cont);
                break;
            case FN_NMI:
                p_label<0>::set(locator_t::runtime_rom(RTROM_nmi_exit));
                exact_op<Opt, JMP_ABSOLUTE, null_, p_label<0>>(cpu, prev, cont);
                break;
            default:
                exact_op<Opt, RTS_IMPLIED>(cpu, prev, cont);
                break;
            }
            break;

        case SSA_jump:
            assert(cfg_node->output_size() == 1);
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
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

        case SSA_wait_nmi:
            p_arg<0>::set(locator_t::runtime_rom(RTROM_wait_nmi));
            chain
            < exact_op<Opt, JSR_ABSOLUTE, null_, p_arg<0>>
            , set_defs<Opt, REGF_CPU & ~(REGF_X | REGF_Y | REGF_B), false, null_>
            >(cpu, prev, cont);
            break;

        case SSA_entry:
        case SSA_uninitialized:
        case SSA_cg_read_array_direct:
            ignore_req_store<p_def>(cpu, prev, cont);
            break;
        default:
            throw std::runtime_error(fmt("Unhandled SSA op in code gen: %", h->op()));
        }
    }

    void isel_node(ssa_ht h)
    {
        p_def::set(h);

        using Opt = options<>;
        using OptC = typename Opt::add_flags<OPT_CONDITIONAL>;

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
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            eq_branch<Opt, BEQ, p_label<0>, p_label<1>>(h);
            break;
        case SSA_branch_not_eq:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            eq_branch<Opt, BNE, p_label<0>, p_label<1>>(h);
            break;

        case SSA_branch_lt:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(0)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(1)));
            lt_branch<Opt, p_label<0>, p_label<1>, false>(h);
            break;

        case SSA_branch_lte:
            p_label<0>::set(locator_t::cfg_label(cfg_node->output(1)));
            p_label<1>::set(locator_t::cfg_label(cfg_node->output(0)));
            lt_branch<Opt, p_label<0>, p_label<1>, true>(h);
            break;

        case SSA_return:
        case SSA_fn_call:
        case SSA_wait_nmi:
            write_globals<Opt>(h);
            goto simple;

        case SSA_fence:
            write_globals<Opt>(h);
            break;

        case SSA_goto_mode:
            {
                write_globals<Opt>(h);

                assert(h->input(0).is_locator());
                assert(h->input(0).locator().lclass() == LOC_FN);

                fn_t const& call = *h->input(0).locator().fn();
                assert(call.fclass == FN_MODE);

                assert(h->input(1).is_locator());
                assert(h->input(1).locator().lclass() == LOC_STMT);

                mods_t const* mods = state.fn->def().mods_of(h->input(1).locator().stmt());
                assert(mods);

                bool did_reset_nmi = false;

                call.precheck_group_vars().for_each([&](group_vars_ht gv)
                {
                    if(!gv->has_init())
                        return;

                    if(!mods->group_vars.count(gv->group.handle()))
                    {
                        if(!did_reset_nmi)
                        {
                            // Reset the nmi handler until we've reset all group vars.
                            p_arg<0>::set(locator_t::runtime_ram(RTRAM_nmi_index));
                            select_step<false>(load_then_store<Opt, null_, const_<0>, p_arg<0>, false>);
                            did_reset_nmi = true;
                        }

                        p_arg<0>::set(locator_t::reset_group_vars(gv));
                        p_arg<1>::set(locator_t::reset_group_vars(gv).with_is(IS_BANK));

                        select_step<false>(
                            chain
                            < load_Y<Opt, p_arg<1>>
                            , exact_op<Opt, BANKED_Y_JSR, null_, p_arg<0>>
                            , set_defs<Opt, REGF_CPU, false, null_>
                            >);
                    }
                });

                bool same_nmi = true;
                for(fn_ht mode : state.fn->precheck_parent_modes())
                {
                    if(mode->mode_nmi() != call.mode_nmi())
                    {
                        same_nmi = false;
                        break;
                    }
                }
                
                // Set the nmi handler to its proper value
                if(did_reset_nmi || !same_nmi)
                {
                    p_arg<0>::set(locator_t::runtime_ram(RTRAM_nmi_index));
                    p_arg<1>::set(locator_t::nmi_index(call.mode_nmi()));
                    select_step<false>(load_then_store<Opt, null_, p_arg<1>, p_arg<0>, false>);
                }

                // Do the jump:
                p_arg<0>::set(h->input(0));
                p_arg<1>::set(h->input(0).locator().with_is(IS_BANK));
                select_step<true>(
                    chain
                    < load_Y<Opt, p_arg<1>>
                    , exact_op<Opt, BANKED_Y_JMP, null_, p_arg<0>>
                    , set_defs<Opt, REGF_CPU, false, null_>
                    >);
            }
            break;

            goto simple;

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

            /* TODO: remove?
        case SSA_shl:
            p_lhs::set(h->input(0));
            p_rhs::set(h->input(1));

            if(h->input(1).is_num())
            {
                unsigned const shift = h->input(1).whole();
                select_step(load_A<Opt, p_lhs>);
                for(unsigned i = 0; i < shift; ++i)
                    select_step(exact_op<Opt, ASL_IMPLIED, p_def>);
                select_step(
                    chain
                    < set_defs<Opt, REGF_A, true, p_def>
                    , store<Opt, STA, p_def>
                    >);
            }
            else
            {
                p_label<0>::set(state.minor_label());
                p_label<1>::set(state.minor_label());

                select_step([h](cpu_t cpu, sel_t const* prev, cons_t const* cont)
                {
                    chain
                    < load_AX<Opt, p_lhs, p_rhs>
                    , load_NZ_for<typename Opt::restrict_to<~REGF_AX>, p_rhs>
                    , exact_op<Opt, BEQ_RELATIVE, null_, p_label<1>>
                    , label<p_label<0>>
                    , exact_op<OptC, ASL_IMPLIED>
                    , exact_op<OptC, DEX_IMPLIED>
                    , exact_op<OptC, BNE_RELATIVE, null_, p_label<0>>
                    , label<p_label<1>>
                    , clear_conditional
                    , set_defs<Opt, REGF_A, true, p_def>
                    , store<Opt, STA, p_def>
                    >(cpu, prev, cont);

                    chain
                    < load_AY<Opt, p_lhs, p_rhs>
                    , load_NZ_for<typename Opt::restrict_to<~REGF_AY>, p_rhs>
                    , exact_op<Opt, BEQ_RELATIVE, null_, p_label<1>>
                    , label<p_label<0>>
                    , exact_op<OptC, ASL_IMPLIED>
                    , exact_op<OptC, DEY_IMPLIED>
                    , exact_op<OptC, BNE_RELATIVE, null_, p_label<0>>
                    , label<p_label<1>>
                    , clear_conditional
                    , set_defs<Opt, REGF_A, true, p_def>
                    , store<Opt, STA, p_def>
                    >(cpu, prev, cont);
                });
            }
            break;
            */

        case SSA_read_ptr:
        case SSA_write_ptr:
            {
                using namespace ssai::rw_ptr;

                // Handle bankswitching here

                if(h->input(BANK) && mapper().bankswitches())
                {
                    p_arg<0>::set(h->input(BANK));
                    select_step<false>(load_B<Opt, p_arg<0>>);
                }
            }
            goto simple;

        default: 
        simple:
            select_step<true>([h](cpu_t cpu, sel_t const* prev, cons_t const* cont)
            {
                isel_node_simple(h, cpu, prev, cont);
            });
            break;
        }
    }

} // namespace isel

static void setup_rolling_window(cfg_ht cfg_node)
{
    auto& cd = cg_data(cfg_node);

    for(ssa_ht h : cd.schedule)
        cg_data(h).isel = {};

    // A bitset is used to track which variables have been stored.
    // To shrink the bitset size down to 64 bits, a rolling window is used
    // based around the live ranges occuring within a single CFG node.

    // This code finds that rolling window and allocates each node to a bit:
    std::uint64_t free = ~0ull; // Set of availible bits to use
    for(ssa_ht h : cd.schedule)
    {
        if(free)
        {
            // Allocate a single bit.
            std::uint64_t const allocated = 1ull << builtin::ctz(free);
            assert((free & allocated) == allocated);

            // Remove the allocated bit from 'free'.
            free ^= allocated;

            // Track the bit with this node:
            cg_data(h).isel.store_mask = allocated;

            // Determine which use occurs latest in the CFG node.
            // If a use
            ssa_ht last_use = h;
            for(unsigned j = 0; j < h->output_size(); ++j)
            {
                auto const oe = h->output_edge(j);

                if(oe.input_class() != INPUT_VALUE)
                    continue;

                if(((ssa_flags(oe.handle->op()) & SSAF_WRITE_GLOBALS) 
                    && oe.index >= write_globals_begin(oe.handle->op()))
                   || oe.handle->op() == SSA_aliased_store)
                {
                    cg_data(h).isel.likely_store = true;
                }

                // Assume that all stores used across basic block
                // boundaries must be stored. 
                // (They may not be in the final generated code.)
                // This is highly pessimistic, but simplifies the code gen.
                if(oe.handle->cfg_node() != h->cfg_node())
                {
                    cg_data(h).isel.likely_store = true;
                    goto skip;
                }

                if(cg_data(oe.handle).schedule.index > cg_data(last_use).schedule.index)
                    last_use = oe.handle;
            }

            cg_data(last_use).isel.last_use |= allocated; // TODO
        }
    skip:
        // Reclaim bits after the last use has been seen:
        assert((free & cg_data(h).isel.last_use) == 0);
        free |= cg_data(h).isel.last_use; // TODO
    }
}

/* TODO: remove

struct isel_t
{
    std::vector<asm_inst_t> code;
    cpu_t cpu;
    unsigned cost;
};
rh::batman_map<cpu_t, isel_result_t> do_isel(fn_t const& fn, cfg_ht cfg_node, map_t const& initial_states)
{
    using namespace isel;

    state.fn = fn.handle();
    state.cfg_node = cfg_node;

    ///////////////////////
    // DO THE SELECTIONS //
    ///////////////////////

do_selections:
    state.sel_pool.clear();
    state.best_cost = ~0 - cost_cutoff(0);
    state.map = std::move(initial_states);

    for(ssa_ht h : cd.schedule)
    {
        try
        {
            state.ssa_node = h;
            isel_node(h);
            //std::cout << "MAP SIZE = " << state.map.size() << std::endl;
        }
        catch(isel_no_progress_error_t const&)
        {
            // We'll try and fix the error.
            bool repaired = false;

            // 
            for_each_node_input(h, [&](ssa_ht input)
            {
                if(input->cfg_node() == cfg_node && input->op() == SSA_cg_read_array_direct)
                {
                    input->unsafe_set_op(SSA_read_array);
                    repaired = true;
                }
            });

            if(repaired)
                goto do_selections;
            throw;
        }
        catch(...) { throw; }
    }

    rh::batman_map<cpu_t, isel_result_t> sels;

    for(auto const& pair : state.map)
    {
        sels.emplace(pair.first, [&] -> isel_result_t
        {
            isel_result_t result;
            result.cost = pair.second->cost;

            // Create the 'code' vector:
            std::size_t size = 1;
            sel_t const* first_sel = pair.second;
            assert(first_sel);
            for(;first_sel->prev; first_sel = first_sel->prev)
                ++size;
            result.code.resize(size);
            for(sel_t const* sel = pair.second; sel; sel = sel->prev)
                result.code[size--] = sel->inst;
            assert(size == 0);
            assert(result.code[0].op == ASM_PRUNED);
            result.code[0] = { ASM_LABEL, SSA_null, locator_t::cfg_label(cfg_node) };

            // Determine the final 'in_state'.
            // This is the in_state we started with, 
            // ignoring any input register not actually used.

            regs_t used = 0;
            regs_t live = REGF_A | REGF_X | REGF_Y;
            for(asm_inst_t const& inst : result.code)
            {
                used |= op_input_regs(inst.op)
                live &= ~op_output_regs(inst.op);
                if(!live)
                    break;
            }

            assert(first_sel->inst.op == ASM_PRUNED);
            assert(first_sel->inst.arg.lclass() == LOC_HANDLE);

            unsigned const in_i = first_sel->inst.arg.handle();
            assert(in_i < in_states.size());

            result.in_state = in_states.begin()[in_i].first;

            if(!(used & ASMF_A))
                in_state.a = LOC_NONE;
            if(!(used & ASMF_X))
                in_state.x = LOC_NONE;
            if(!(used & ASMF_Y))
                in_state.y = LOC_NONE;

            return result;
        });
    }

    return sels;
}
*/

namespace isel
{

static cpu_t to_cpu(cross_cpu_t const& cross)
{
    static_assert(REGF_CROSS == (REGF_A | REGF_X | REGF_Y | REGF_C));
    cpu_t ret = {};
    ret.set_def<REG_A>({}, cross.defs[REG_A]);
    ret.set_def<REG_X>({}, cross.defs[REG_X]);
    ret.set_def<REG_Y>({}, cross.defs[REG_Y]);
    ret.set_def<REG_C>({}, cross.defs[REG_C]);
    return ret;
}

static cross_cpu_t make_cross_cpu(cpu_t const& cpu)
{
    auto const convert = [](locator_t l) -> locator_t
    {
        if(l.lclass() == LOC_PHI)
            return LOC_NONE;

        if(l.lclass() == LOC_SSA)
        {
            ssa_ht const h = l.ssa_node();
            

            if(h->op() == SSA_phi_copy)
            {
                assert(0);
                assert(cset_locator(h).lclass() == LOC_PHI);
                return cset_locator(h);
            }
        }

        return l;
    };

    cross_cpu_t cross = {};
    cross.defs[REG_A] = convert(cpu.defs[REG_A]);
    cross.defs[REG_X] = convert(cpu.defs[REG_X]);
    cross.defs[REG_Y] = convert(cpu.defs[REG_Y]);
    if(cpu.defs[REG_C].lclass() == LOC_CONST_BYTE)
        cross.defs[REG_C] = cpu.defs[REG_C];
    return cross;
}

static cross_cpu_t prepare_cross_cpu_for(cross_cpu_t const& cross, cfg_ht cfg)
{
    auto const convert = [cfg](locator_t l) -> locator_t
    {
        assert(l.lclass() != LOC_PHI);

        if(l.lclass() == LOC_SSA)
        {
            ssa_ht const h = l.ssa_node();

            // Setup incoming phis:
            locator_t const cset_loc = cset_locator(h);
            if(cset_loc.lclass() == LOC_PHI)
            {
                for(ssa_ht phi = cfg->phi_begin(); phi; ++phi)
                {
                    assert(phi->op() == SSA_phi);
                    if(cset_loc.ssa_node() == phi)
                        return locator_t::phi(phi);
                }
            }

            // Values can't loop:
            if(h->cfg_node() == cfg)
                return LOC_NONE;

            // If the node has no output used through 'cfg', ignore it.
            if(for_each_output_matching(h, INPUT_VALUE, [&](ssa_ht output)
            {
                return !dominates(cfg, output->cfg_node());
            }))
            {
                return LOC_NONE;
            }
        }

        return l;
    };

    cross_cpu_t ret = {};
    ret.defs[REG_A] = convert(cross.defs[REG_A]);
    ret.defs[REG_X] = convert(cross.defs[REG_X]);
    ret.defs[REG_Y] = convert(cross.defs[REG_Y]);
    return ret;
}

}

void select_instructions(fn_t const& fn, ir_t& ir)
{
    using namespace isel;

    state.fn = fn.handle();

    build_loops_and_order(ir);
    build_dominators_from_order(ir);

    _data_vec.clear();
    _data_vec.resize(cfg_pool::array_size());

    ///////////////////////////////////////////////
    // GENERATE SELECTION LIST FOR EACH CFG NODE //
    ///////////////////////////////////////////////

    rh::batman_map<cross_cpu_t, result_t> rebuilt;

    // Create the initial worklist:
    assert(cfg_worklist.empty());
    for(cfg_ht cfg : postorder | std::views::reverse)
        cfg_worklist.push(cfg);

    // Setup initial 'in_state's:
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);
        d.in_states.insert({});
        d.to_compute.push_back(0);
    }

    // Run until completion:
    while(!cfg_worklist.empty())
    {
        cfg_ht const cfg = cfg_worklist.pop();
        auto& d = data(cfg);

        if(d.to_compute.empty())
            continue;

        state.cfg_node = cfg;
        setup_rolling_window(cfg);
    do_selections:

        // Init the state:
        state.sel_pool.clear();
        state.best_cost = ~0 - cost_cutoff(0);
        state.map.clear();
        for(unsigned index : d.to_compute)
        {
            state.map.insert({ 
                to_cpu(d.in_states.begin()[index]),
                &state.sel_pool.emplace(nullptr, 0, 
                    asm_inst_t{ .op = ASM_PRUNED, .arg = locator_t::index(index) }) });
        }

        // Generate every selection:
        for(ssa_ht h : cg_data(cfg).schedule)
        {
            try
            {
                state.ssa_node = h;
                isel_node(h); // This creates all the selections.
            }
            catch(isel_no_progress_error_t const&)
            {
                // We'll try and fix the error.
                bool repaired = false;

                // Maybe the addressing mode was impossible,
                // so let's make it simpler.
                for_each_node_input(h, [&](ssa_ht input)
                {
                    if(input->cfg_node() == cfg && input->op() == SSA_cg_read_array_direct)
                    {
                        input->unsafe_set_op(SSA_read_array);
                        repaired = true;
                    }
                });

                if(repaired)
                    goto do_selections;
                throw;
            }
            catch(...) { throw; }
        }

        // Clear after computing:
        d.to_compute.clear();

        // Assemble those selections:
        for(auto const& pair : state.map)
        {
            rh::apair<cross_cpu_t, result_t> new_sel = { make_cross_cpu(pair.first) };
            result_t& result = new_sel.second;
            result.cost = pair.second->cost;

            // Create the 'code' vector:
            std::size_t size = 1;
            sel_t const* first_sel = pair.second;
            assert(first_sel);
            for(;first_sel->prev; first_sel = first_sel->prev)
                ++size;
            result.code.resize(size);
            for(sel_t const* sel = pair.second; sel; sel = sel->prev)
                result.code[--size] = sel->inst;
            assert(size == 0);
            assert(result.code[0].op == ASM_PRUNED);
            result.code[0] = { ASM_LABEL, SSA_null, locator_t::cfg_label(cfg) };

            // Determine the final 'start state'.
            // This is the cpu state we started with, 
            // ignoring any input register not actually used.

            regs_t used = 0;
            regs_t written = 0;
            for(asm_inst_t const& inst : result.code)
            {
                used |= op_input_regs(inst.op) & ~written;
                written |= op_output_regs(inst.op);
                if((written & REGF_CROSS) == REGF_CROSS)
                    break;
            }

            assert(first_sel->inst.op == ASM_PRUNED);
            assert(first_sel->inst.arg.lclass() == LOC_INDEX);

            unsigned const in_i = first_sel->inst.arg.data();
            assert(in_i < d.in_states.size());

            result.in_state = d.in_states.begin()[in_i];

            for(unsigned i = 0; i < NUM_CROSS_REGS; ++i)
                if(~used & written & (1 << i))
                    result.in_state.defs[i] = LOC_NONE;

            // Insert the 'new_sel' into 'd':
            auto insert_result = d.sels.insert(new_sel);
            if(!insert_result.second)
            {
                // Keep the lowest cost:
                if(insert_result.first->second.cost > new_sel.second.cost)
                    *insert_result.first = new_sel;
            }
        }

        // For efficiency, cap the maximum number of selections tracked:
        constexpr unsigned MAX_SELS_PER_CFG = 256;
        if(d.sels.size() > MAX_SELS_PER_CFG)
        {
            std::sort(d.sels.begin(), d.sels.end(), [&](auto const& a, auto const& b)
            {
                return a.second.cost < b.second.cost;
            });

            // Reuse 'rebuilt':
            rebuilt.clear();
            rebuilt.reserve(MAX_SELS_PER_CFG);

            for(unsigned i = 0; i < MAX_SELS_PER_CFG; ++i)
                rebuilt.insert(d.sels.begin()[i]);

            d.sels.swap(rebuilt);
        }

        // Pass our output CPU states to our output CFG nodes.
        unsigned const depth = loop_depth(cfg);
        unsigned const output_size = cfg->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            cfg_ht const output = cfg->output(i);
            if(depth < loop_depth(output))
                continue;
            auto& od = data(output);

            for(auto const& new_sel : d.sels)
            {
                auto result = od.in_states.insert(prepare_cross_cpu_for(new_sel.first, output));
                if(result.second)
                {
                    od.to_compute.push_back(result.first - od.in_states.begin());
                    cfg_worklist.push(output);
                }
            }
        }
    }

#ifndef NDEBUG
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);
        assert(d.sels.size() > 0);
    }
#endif

    /////////////////////////////////////////
    // PICK THE BEST SELECTION COMBINATION //
    /////////////////////////////////////////

    auto const gen_load = [&](cross_cpu_t const& cross, regs_t reg, locator_t loc) -> asm_inst_t
    {
        //assert(loc);

        switch(reg)
        {
        case REG_A:
            if(cross.defs[REG_X] == loc)
                return { .op = TXA_IMPLIED };
            else if(cross.defs[REG_Y] == loc)
                return { .op = TYA_IMPLIED };
            else if(loc.is_immediate())
                return { .op = LDA_IMMEDIATE, .arg = loc };
            else
                return { .op = LDA_ABSOLUTE, .arg = loc };

        case REG_X:
            if(cross.defs[REG_A] == loc)
                return { .op = TAX_IMPLIED };
            else if(loc.is_immediate())
                return { .op = LDX_IMMEDIATE, .arg = loc };
            else
                return { .op = LDX_ABSOLUTE, .arg = loc };

        case REG_Y:
            if(cross.defs[REG_A] == loc)
                return { .op = TAY_IMPLIED };
            else if(loc.is_immediate())
                return { .op = LDY_IMMEDIATE, .arg = loc };
            else
                return { .op = LDY_ABSOLUTE, .arg = loc };

        case REG_C:
            assert(loc.lclass() == LOC_CONST_BYTE);
            return { .op = loc.data() ? SEC_IMPLIED : CLC_IMPLIED };

        default:
            return { .op = ASM_PRUNED };
        }
    };

    {
        pbqp_t pbqp;

        for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
        {
            auto& d = data(cfg);

            isel_cost_t const multiplier = loop_depth_exp(cfg, 4);

            for(auto const& pair : d.sels)
                d.cost_vector.push_back(pair.second.cost * multiplier);
        }

        for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
        {
            auto& d = data(cfg);

            isel_cost_t const multiplier = loop_depth_exp(cfg, 4);

            unsigned const output_size = cfg->output_size();
            for(unsigned i = 0; i < output_size; ++i)
            {
                cfg_ht const output = cfg->output(i);
                auto& od = data(output);

                std::vector<pbqp_cost_t> cost_matrix(d.sels.size() * od.sels.size());
                for(unsigned y = 0; y < od.sels.size(); ++y)
                for(unsigned x = 0; x < d.sels.size(); ++x)
                {
                    cross_cpu_t const& out = prepare_cross_cpu_for(d.sels.begin()[x].first, output);
                    cross_cpu_t const& in  = od.sels.begin()[y].second.in_state;

                    pbqp_cost_t cost = 0;

                    for(regs_t reg = 0; reg < NUM_CROSS_REGS; ++reg)
                    {
                        if(!in.defs[reg])
                            continue;
                        if(in.defs[reg] == out.defs[reg])
                            continue;
                        op_t const op = gen_load(out, reg, LOC_NONE).op;
                        cost += cost_fn(op);
                    }

                    cost_matrix[x + y * d.sels.size()] = cost * multiplier;
                }

                pbqp.add_edge(d, od, std::move(cost_matrix));
            }
        }

        std::vector<pbqp_node_t*> pbqp_order;
        for(cfg_ht cfg : postorder)
            pbqp_order.push_back(&data(cfg));
        pbqp.solve(std::move(pbqp_order));
    }

#ifndef NDEBUG
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);
        assert(d.sel >= 0 && d.sel < d.sels.size());
    }
#endif

    ////////////////////////////////////
    // INSERT ADDITIONAL INSTRUCTIONS //
    ////////////////////////////////////

    bc::small_vector<regs_t, 16> input_loads;
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        auto& d = data(cfg);

        regs_t to_load = 0;
        unsigned const input_size = cfg->input_size();
        input_loads.resize(input_size);

        for(unsigned i = 0; i < input_size; ++i)
        {
            cfg_ht input = cfg->input(i);
            auto const& id = data(input);

            cross_cpu_t const out_state = prepare_cross_cpu_for(id.final_out_state(), cfg);

            regs_t input_load = 0;
            for(unsigned reg = 0; reg < NUM_CROSS_REGS; ++reg)
            {
                if(!d.final_in_state().defs[reg])
                    continue;
                if(out_state.defs[reg] == d.final_in_state().defs[reg])
                    continue;
                to_load |= 1 << reg;
                input_load |= 1 << reg;
            }
            input_loads[i] = input_load;
        }

        auto const initial_cross = [&](regs_t loads)
        {
            cross_cpu_t cross = d.final_in_state();
            bitset_for_each(loads, [&](regs_t reg){ cross.defs[reg] = {}; });
            return cross;
        };

        auto const replace_labels = [&](regs_t loads, locator_t label)
        {
            for(unsigned i = 0; i < input_size; ++i)
            {
                if(!input_loads[i] || (input_loads[i] &= ~loads))
                    continue;

                cfg_ht input = cfg->input(i);
                auto& id = data(input);

                // Replace the labels of incoming jumps with the new label.
                for(asm_inst_t& inst : id.final_code())
                    if(inst.arg == locator_t::cfg_label(cfg))
                        inst.arg = label;
            }
        };

        // Find loads that all incoming paths (that need loads) will use.

        regs_t always_load = to_load;
        for(unsigned i = 0; i < input_size; ++i)
            if(input_loads[i])
                always_load &= input_loads[i];

        bc::small_vector<asm_inst_t, NUM_CROSS_REGS * 2> always_load_code;
        unsigned next_label_index = 1;

        if(always_load)
        {
            locator_t const label = locator_t::cfg_label(cfg, next_label_index++);
            always_load_code.push_back({ .op = ASM_LABEL, .arg = label });

            cross_cpu_t cross = initial_cross(always_load);
            bitset_for_each(always_load, [&](regs_t reg)
            {
                always_load_code.push_back(gen_load(cross, reg, d.final_in_state().defs[reg]));
                cross.defs[reg] = d.final_in_state().defs[reg];
            });

            replace_labels(always_load, label);
        }

        // Now find loads that can be appended onto an input's CFG.

        for(unsigned i = 0; i < input_size; ++i)
        {
            cfg_ht input = cfg->input(i);
            auto& id = data(input);

            if(input->output_size() != 1)
                continue;

            regs_t const load_now = input_loads[i];

            if(!load_now)
                continue;

            input_loads[i] = 0;

            auto& code = id.final_code();
            assert(code.size());
            assert(op_flags(code.back().op) & ASMF_BRANCH);
            asm_inst_t const jump = code.back();
            code.pop_back();

            cross_cpu_t cross = prepare_cross_cpu_for(id.final_out_state(), cfg);
            bitset_for_each(load_now, [&](regs_t reg)
            {
                code.push_back(gen_load(cross, reg, d.final_in_state().defs[reg]));
                cross.defs[reg] = d.final_in_state().defs[reg];
            });

            code.push_back(jump);
            std::cout << "ISEL CODE 1OUT " << input << std::endl;
        }

        // Now handle all the remaining loads.

        regs_t remaining_load = 0;
        for(unsigned i = 0; i < input_size; ++i)
            remaining_load |= input_loads[i];

        bc::small_vector<asm_inst_t, NUM_CROSS_REGS * 2> remaining_load_code;

        if(remaining_load)
        {
            // We'll try various permutations, searching for an optimal one.

            unsigned lowest_misses = ~0u;
            bc::small_vector<regs_t, NUM_CROSS_REGS> order, best_order;
            bitset_for_each(remaining_load, [&](regs_t r){ order.push_back(r); });

            do
            {
                unsigned misses = 0;

                for(unsigned i = 0; i < input_size; ++i)
                {
                    cfg_ht input = cfg->input(i);

                    unsigned const cost = loop_depth_exp(input, 4);
                    regs_t input_load = input_loads[i];

                    for(unsigned j = 0; j < order.size(); ++j)
                    {
                        regs_t const regf = 1 << order[i];

                        if(!(input_load & regf))
                            misses += cost;

                        input_load &= ~regf;

                        if(!input_load)
                            break;
                    }
                }

                if(misses < lowest_misses)
                {
                    lowest_misses = misses;
                    best_order = order;
                }
            }
            while(std::next_permutation(order.begin(), order.end()));

            // OK! 'best_order' is built. Now generate code:

            cross_cpu_t const cross = initial_cross(remaining_load);
            for(regs_t reg : best_order)
            {
                locator_t const label = locator_t::cfg_label(cfg, next_label_index++);
                remaining_load_code.push_back(gen_load(cross, reg, d.final_in_state().defs[reg]));
                remaining_load_code.push_back({ .op = ASM_LABEL, .arg = label });

                replace_labels(1 << reg, label);
            }
            std::reverse(remaining_load_code.begin(), remaining_load_code.end());
        }

        // Prepend to code:
        std::cout << "ISEL CODE " << remaining_load_code.size() << ' ' << always_load_code.size() << std::endl;
        std::vector<asm_inst_t> new_code(d.final_code().size() + always_load_code.size() + remaining_load_code.size());
        std::copy(remaining_load_code.begin(), remaining_load_code.end(), 
                  new_code.begin());
        std::copy(always_load_code.begin(), always_load_code.end(), 
                  new_code.begin() + remaining_load_code.size());
        std::copy(d.final_code().begin(), d.final_code().end(), 
                  new_code.begin() + always_load_code.size() + remaining_load_code.size());
        d.final_code() = std::move(new_code);
    }

    // Store the code
    std::cout << "ISEL\n";
    for(cfg_ht cfg = ir.cfg_begin(); cfg; ++cfg)
    {
        cg_data(cfg).code = std::move(data(cfg).final_code());

        auto const& d = data(cfg);
        std::cout << "ISEL " << cfg << ' ' << d.in_states.size() << ' ' << d.sels.size() << std::endl;
    }
}

