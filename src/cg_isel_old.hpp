
{
    constexpr unsigned COST_CUTOFF = 9;

    rh::batman_map<cpu_state_t, step_t*> maps[2];

    rh::batman_map<cpu_state_t, step_t*> map;
    rh::batman_map<cpu_state_t, step_t*> next_map;

    unsigned best_cost = ~0;
    unsigned next_best_cost = ~0;

    {
        next_map.clear();
        next_best_cost = ~0;

        for(auto const& pair : *map)
        {
            if(pair.second.cost > best_cost + COST_CUTOFF)
                continue;

            for each choice
            {

                // Clone the state
                cpu_state_t state = pair.first;

                run_choice(); // TODO

                step_t next_step = 
                {
                    .prev = &pair.second,
                    .cost = pair.second.cost + TODO,
                    .sel = sel,
                };

                auto result = next_map->emplace(state, next_step);
                if(!result.inserted && result.mapped->cost > next_step.cost)
                    *result.mapped = next_step;

                if(next_step.cost < best_cost)
                    next_best_cost = next_step.cost;
            }
        }
    }

    map.swap(next_map);
    std::swap(best_cost, next_best_cost);


    // Enumerate all choices
        // Calculate costs and new cpu state of each
        // insert into new map


    // SPLIT:
    // - run all paths from the split seed

    // MERGE:
    // - pick best node of all the joining nodes
    // - continue from there
}



template<typename Derived>
struct asm_builder_t
{
    cpu_state_t state;

    Derived* derived() { return static_cast<Derived*>(this); }

    asm_builder_t& load_CA(ssa_value_t c, ssa_value_t a)
    {
        assert(c);
        assert(a);
        assert(!c->holds_ref() || c->type() == TYPE_BYTE);
        assert(!a->holds_ref() || a->type() == TYPE_BYTE);

        if(state.C != c)
        {
            derived()->flag_local_store_C(c);
            derived()->append_load_C<LDA>(c);
            append_op<PHA>();
            append_op<PLP>();
            derived()->on_change_C(v);
        }
        return load_A(a); // sets A
    }

    asm_builder_t& load_A(ssa_value_t v)
    {
        assert(v);
        assert(!v->holds_ref() || v->type() == TYPE_BYTE);

        if(state[REG_A] == v)
            return *this;

        if(state[REG_X] == v)
            append_op<TXA>();
        else if(state[REG_Y] == v)
            append_op<TYA>();
        else
            append_op<LDA>(v);

        //derived()->on_change_A(v);
        state[REG_A] = v;
        return *this;
    }

    asm_builder_t& load_X(ssa_value_t v)
    {
        assert(v);
        assert(!v->holds_ref() || v->type() == TYPE_BYTE);

        if(state[REG_X] == v)
            return *this;

        if(state[REG_A] == v)
            append_op<TAX>();
        else
            append_op<LDX>(v);

        //derived()->on_change_X(v);
        state[REG_X] = v;
        return *this;
    }

    asm_builder_t& load_Y(ssa_value_t v)
    {
        assert(v);
        assert(!v->holds_ref() || v->type() == TYPE_BYTE);

        if(state[REG_Y] == v)
            return *this;

        if(state[REG_A] == v)
            append_op<TAY>();
        else
            append_op<LDY>(v);

        //derived()->on_change_Y(v);
        state[REG_Y] = v;
        return *this;
    }

    asm_builder_t& load_AX(ssa_value_t v)
    {
        assert(v);
        assert(!v->holds_ref() || v->type() == TYPE_BYTE);

        if(state.A == v && state.X == v)
            return *this;
        else if(state[REG_A] == v)
        {
            append_op<TAX>();
            //derived()->on_change_X(v);
        }
        else if(state.X == v)
        {
            append_op<TXA>();
            //derived()->on_change_A(v);
        }
        else if(v.is_const())
        {
            append_op<LDA>(v);
            append_op<TAX>();
            //derived()->on_change_AX(v);
        }
        else
        {
            append_op<LAX>(v);
            //derived()->on_change_AX(v);
        }

        state[REG_A] = v;
        state[REG_X] = v;
        return *this;
    }

    template<op_name_t OpName>
    void get_mem(ssa_ht h)
    {
        assert(h);
        assert(get_op(OpName, MODE_ABSOLUTE));

        if(h->test_flags(FLAG_STORE))
            return h;

        // If the node was stored into a locator, and that locator
        // still holds the node, we can use the locator as the memory address.
        for(locator_t loc : data(h).stores_into_locators)
            if(loc_live[loc] == h)
                return loc;

        // TODO
        derived()->flag_local_store(h);
        return h;
    }
    
    template<op_name_t OpName> [[gnu::always_inline]]
    asm_builder_t& append_op()
    {
        constexpr op_t implied = get_op(OpName, MODE_IMPLIED);

        if(implied)
        {
            derived()->append_implied_op<implied>();
            return true;
        }

        return false;
    }

    template<op_name_t OpName> [[gnu::always_inline]]
    bool append_op(ssa_value_t v)
    {
        constexpr op_t immediate  = get_op(OpName, MODE_IMMEDIATE);
        constexpr op_t absolute   = get_op(OpName, MODE_ABSOLUTE);

        if(immediate && v.is_const())
        {
            derived()->append_immediate_op<immediate>(v);
            return true;
        }

        if(absolute)
        {
            assert(v.holds_ref());
            derived()->append_absolute_op<absolute>(get_mem(v.handle()));
            return true;
        }

        return false;
    }

    template<op_name_t OpName, bool XelseY> [[gnu::always_inline]]
    bool append_array_op(ssa_ht h)
    {
        constexpr op_t absolute_xy = get_op(OpName, XelseY ? MODE_ABSOLUTE_X 
                                                           : MODE_ABSOLUTE_Y);
        if(absolute_xy && h->op() == SSA_read_array)
        {
            if(XelseY)
                load_X(h->input(1));
            else
                load_Y(h->input(1));

            derived()->append_absolute_indexed_op<absolute_xy>(
                get_mem(h->input(0)));

            return true;
        }

        return false;
    }
};

class cycle_estimator_t : asm_builder_t<cycle_estimator_t>
{
public:
    unsigned cost = 0;

    ////////////
    // LOCALS //
    ////////////

    // Force the node's result to be stored locally.
    void flag_local_store(ssa_ht)
        {}

    // If the node's result will be, or is stored in a local variable.
    bool locally_stored(ssa_ht h)
        { return h->test_flags(FLAG_STORED); }

    /////////////
    // CARRIES //
    /////////////

    // Loads the previously stored carry.
    template<op_name_t OpName> [[gnu::always_inline]]
    void append_load_C() 
        {}

    // Force the node's carry result to be stored locally.
    void flag_local_store_C(ssa_ht)
        {}

    // If the node's carry result will be, or is stored in a local variable.
    bool locally_stored_C(ssa_ht h)
        { return h->test_flags(FLAG_STORED_C); }

    /////////////
    // GLOBALS //
    /////////////

    // If the node's result is currently stored in global variable 'loc'.
    bool global_live(ssa_ht h, locator_ht loc);

    /////////////
    // ON LOAD //
    /////////////

    void on_change_C(ssa_value_t) {}
    void on_change_A(ssa_value_t) {}
    void on_change_X(ssa_value_t) {}
    void on_change_Y(ssa_value_t) {}
    void on_change_AX(ssa_value_t) {}

    template<regs_t Reg> [[gnu::always_inline]]
    void on_change_impl(ssa_value_t new_value, ssa_ht from_op) 
    {
        // If the node in 'Reg' has unscheduled outputs, 
        // it will need to be stored.

        if(!state[Reg].holds_ref())
            return;

        ssa_ht regv = state[Reg].handle();
        auto& d = data(regv);

        if(Reg == REG_C && d.single_carry_use 
           && d.single_carry_use != new_value
           && !scheduled(d.single_carry_use))
        {
            cost += 20;
        }
        
        if(Reg != REG_C && d.single_value_use 
           && d.single_value_use != new_value
           && !scheduled(d.single_value_use))
        {
            cost += 20;
        }

        // we care about nodes with 1 output


        if(Reg == REG_C && locally_stored_C(regv))
            return;

        if(Reg != REG_C && locally_stored(regv))
            return;

        unsigned const output_size = regv->output_size();
        for(unsigned i = 0; i < output_size; ++i)
        {
            ssa_ht output = state[reg]->output(i);
            if(output == from_op)
                continue;
            if(output->test_flags(FLAG_PROCESSED))
                continue;
            if(reg == REG_C && carry_used_on_output(*state[reg], i);
                store(state[reg]);
            if(reg != REG_C && value_used_on_output(*state[reg], i);
                store(state[reg]);
        }
    }

    ////////////////
    // APPEND OPS //
    ////////////////

    // If stores are required, this appends all the necessary stores.
    template<op_name_t OpName>
    asm_builder_t& append_stores(ssa_ht h);

    template<op_t Op> [[gnu::always_inline]]
    void append_implied_op() { cycles += op_cycles(Op); }

    template<op_t Op> [[gnu::always_inline]]
    void append_immediate_op(std::uint8_t c); { cycles += op_cycles(Op); }

    template<op_t Op> [[gnu::always_inline]]
    void append_absolute_op(ssa_ht mem) { cycles += op_cycles(Op); }

    template<op_t Op> [[gnu::always_inline]]
    void append_absolute_indexed_op(ssa_ht mem) { cycles += op_cycles(Op); }

    template<op_t Op> [[gnu::always_inline]]
    void append_rmw_op(ram_ref_t) { cycles += op_cycles(Op); }
};

#define SEL(x) SEL_##x, SEL_##x##_FLIPPED
enum selection_t
{
    SELL(NULL),
    SEL(ADC),
    SEL(INC),
    SEL(DEC),
    SEL(INX),
    SEL(DEX),
    SEL(INY),
    SEL(DEY),
    SEL(ADD_AXS),
    SEL(AND),
    SEL(ORA),
};
#undef SEL

inline constexpr bool flipped(selection_t sel)
    { return sel & 1; }

inline constexpr selection_t mask_flipped(selection_t sel)
    { return static_cast<selection_t>((sel & ~1); }

inline constexpr selection_t make_sel(selection_t sel, bool flipped)
    { return static_cast<selection_t>(sel | (unsigned)flipped); }

template<typename T>
T run_selection(ssa_ht node, selection_t sel)
{
    // TODO: assert sel is valid for node

    ssa_value_t const a0 = flipped(sel) ? node->input(0) : node->input(1);
    ssa_value_t const a1 = flipped(sel) ? node->input(1) : node->input(0);

    T builder;
    switch(mask_flipped(sel))
    {
    case SEL_ADC:
        return true
        && builder.load_CA(node->input(2), a0)
        && builder.append_op<ADC>(a1)
        && builder.append_stores<STA>(node)
        ; break;
    case SEL_INC:
        builder.append_rmw_op<INC>(a0);
        break;
    case SEL_DEC:
        builder.append_rmw_op<DEC>(a0);
        break;
    case SEL_INX:
        builder.load_X(a0);
        builder.append_op<INX>();
        builder.append_stores<STX>(node);
        break;
    case SEL_DEX:
        builder.load_X(a0);
        builder.append_op<DEX>();
        builder.append_stores<STX>(node);
        break;
    case SEL_INY:
        builder.load_Y(a0);
        builder.append_op<INY>();
        builder.append_stores<STY>(node);
        break;
    case SEL_DEY:
        builder.load_Y(a0);
        builder.append_op<DEY>();
        builder.append_stores<STY>(node);
        break;
    case SEL_ADD_AXS:
        {
            std::uint8_t const negated =
                (0x100 - a1.whole() - node.input(2).carry());
            builder.load_AX(a0);
            builder.append_immediate_op<AXS>(negated);
            builder.append_stores<STX>(node);
        }
        break;
    case SEL_AND:
        builder.load_A(a0);
        builder.append_op<AND>(a1);
        builder.append_stores<STA>(node);
        break;
    case SEL_ORA:
        builder.load_A(a0);
        builder.append_op<ORA>(a1);
        builder.append_stores<STA>(node);
        break;
    }
    return builder;
}


template<typename Fn> [[gnu::always_inline, gnu::flatten]]
void for_each_flip(ssa_ht node, Fn fn)
{
    fn(false, node->input(0), node->input(1));
    fn(true, node->input(1), node->input(0));
}

struct choice_t
{
    selection_t sel;
    unsigned cost;
};

choice_t best_flip(selection_t sel)
{
    selection_t flipped = make_sel(sel, true);

    unsigned sel_cost     = run_selection(node, flipped).cost;
    unsigned flipped_cost = run_selection(node, flipped).cost;

    if(sel_cost < flipped_cost)
        return { sel, sel_cost };
    else
        return { flipped, flipped_cost };
};

choice_t make_choice(ssa_ht node, selection_t sel, bool flipped)
{
    sel = make_sel(sel, flipped);
    return { sel, run_selection(node, sel).cost };
}


// This should be big enough to hold anything 'get_choices' returns:
using selections_array_t = std::array<choice_t, 8>;

template<typename Policy>
selections_array_t possible_selections(ssa_ht node)
{
    choices_array_t array = {};

    auto push_choice = [&](selection_t sel, bool flipped = false)
    {
        sel = make_sel(sel, flipped);
        vec.push_back({ sel, run_selection(node, sel).cost });
    };

    switch(node->op())
    {
    default: assert(false); break;
    case SSA_add:
        choices[0] = best_flip(SEL_ADD);

        if(!carry_output(node) && node->input(2).is_const())
        {
            unsigned const carry = node->input(2).carry();
            for_each_flip(node, 
            [&](bool flipped, ssa_value_t a0, ssa_value_t a1)
            {
                if(!a1.is_const())
                    return;

                if(a1.whole() == 1 - carry)
                {
                    // RMW are only valid when nothing else uses their input.
                    // TODO: make this an intereference check instead
                    if(a0.output_size() == 1)
                        choices[1] = make_choice(node, SEL_INC, flipped);
                    choices[2] = make_choice(node, SEL_INX, flipped);
                    choices[3] = make_choice(node, SEL_INY, flipped);
                }
                else if(a1.whole() == 255 - carry)
                {
                    // RMW are only valid when nothing else uses their input.
                    // TODO: make this an intereference check instead
                    if(a0.output_size() == 1)
                        choices[4] = make_choice(node, SEL_DEC, flipped);
                    choices[5] = make_choice(node, SEL_DEX, flipped);
                    choices[6] = make_choice(node, SEL_DEY, flipped);
                }
                else
                    choices[7] = make_choice(node, SEL_AXS);
            });

        }
        break;

    case SSA_and: choices[0] = best_flip(SEL_AND); break;
    case SSA_or:  choices[0] = best_flip(SEL_ORA); break;
    }
    return choices;
}
