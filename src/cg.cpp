#include "cg.hpp"


struct cg_key_t
{
    unsigned cycle_cost;
    ssa_value_t a; 
    ssa_value_t x; 
    ssa_value_t y; 
    ssa_value_t c; 

    bool operator==(cg_key_t const& o) const
    {
        return (cycle_cost == o.cycle_cost
                && a == o.a && x == o.x
                && y == o.y && c == o.c);
    }

    bool operator<(cg_key_t const& o) const
    {
        if(cycle_cost != o.cycle_cost)
            return cycle_cost < o.cycle_cost;
        if(a != o.a)
            return a < o.a;
        if(x != o.x)
            return x < o.x;
        if(y != o.y)
            return y < o.y;
        if(c != o.c)
            return c < o.c;
    }
};

namespace std
{
    template<> 
    struct hash<cg_key_t>
    {
        std::size_t operator()(cg_key_t const& key) const noexcept
        {
            std::size_t ret = key.a.value;
            ret = rh::hash_combine(ret, key.x.value);
            ret = rh::hash_combine(ret, key.y.value);
            ret = rh::hash_combine(ret, key.c.value);
            return ret;
        }
    }
}

struct instr_t
{
    instr_t const* prev;
    op_t op;
    unsigned total_cycles;
    ssa_value_t arg;
    bitset_uint_t const* loaded;
};

rh::batman_map<cg_key_t, instr_t>; //TODO

class code_gen_t
{
    instr_t const* append_instr(instr_t const* prev, op_t op, ssa_value_t arg);
    instr_t const* load_A(instr_t const* prev, cg_key_t& key, ssa_value_t arg);

    std::deque<instr_t> instr_pool;
};

instr_t const* code_gen_t::append_instr(instr_t const* prev, 
                                        op_t op, ssa_value_t arg)
{
    return &instr_pool.emplace_back(instr_t{ 
        .prev = prev, 
        .op = op,
        .total_cycles = op_cycles(op) + prev->total_cycles,
        .arg = arg,
        .loaded = prev->loaded });
}

instr_t const* code_gen_t::append_load(instr_t const* prev, 
                                       op_t op, ssa_value_t arg)
{
    bitset_uint_t const* loaded = prev->loaded;
    unsigned added_cycles = op_cycles(op);

    int load_i = -1;

    if(ag.holds_ref())
        load_i = arg.handle().data<ssa_ai_d>().load_i;

    if(load_i > 0 && !bitset_test(prev->loaded, load_i))
    {
        bitset_uint_t* bitset = bitset_pool.alloc(bitset_size);
        bitset_copy(bitset_size, prev->loaded, bitset);
        bitset_set(bitset, load_i);
        loaded = bitset;
        added_cycles += op_cycles(STA_ABSOLUTE);
    }

    return &instr_pool.emplace_back(instr_t{ 
        .prev = prev, 
        .op = op,
        .total_cycles = op_cycles(op) + prev->total_cycles,
        .arg = arg,
        .loaded = loaded });
}

template<op_t Op>
void code_gen_t::mod_key(cg_key_t& key)
{
    constexpr regs_t oreg = op_output_regs(Op);
    if(oreg & REG_A)
        key.a = arg;
    if(oreg & REG_X)
        key.x = arg;
    if(oreg & REG_Y)
        key.y = arg;
    if(oreg & REG_C)
        key.c = arg;
}


instr_t const* code_gen_t::load_A(instr_t const* prev, cg_key_t& key,
                                  ssa_value_t arg)
{
    if(key.x == key.a)
        prev = append_instr(prev, TXA, arg);
    else if(key.y == key.a)
        prev = append_instr(prev, TYA, arg);
    else
    {
        if(arg.is_const())
            prev = append_load(prev, LDA_IMMEDIATE, arg);
        else
            prev = append_load(prev, LDA_ABSOLUTE, arg);
    }
    key.a = arg;
    return prev;
}

void code_gen_t::map_insert(instr_t const* prev, cg_key_t key)
{
    next_map.insert({ key, prev });
}


if(node.input(1).is_const() && node.input(1).whole() == 1)
{
    cg_key_t key = last_key;
    instr_t const* head = prev;
    head = append_instr(head, INC_, arg);
    mod_key<INC_TODO>(key);
    map_insert(prev, key);
}


template<op_name_t OpName, bool Comm>
void TODO(ssa_node_t& node)
{
    addr_mode_t mode;

    if(get_op(OpName, MODE_IMMEDIATE))
    {
        if(node.input(1).is_const())
            mode = MODE_IMMEDIATE;
        else if(Comm && node.input(0).is_const())
            mode = MODE_IMMEDIATE;
    }

    if(get_op(OpName, MODE_ABSOLUTE))
        mode = MODE_ABSOLUTE;

    if(get_op(OpName, MODE_ABSOLUTE_X))
    {
        if(node.input(0)->op() == ARRAY_READ
           && key.x == READ_INDEX
        {

        }


    }

    if(node.input(0) == key.a)
        output_asm(OpName, mode, node.input(1));
    else if(Comm && node.input(1) == key.a)
        output_asm(OpName, mode, node.input(0));
    else
    {
        load_A(node.input(0));
        output_asm(OpName, mode, node.input(1));
    }
}

        if(node.input(0).is_const())
        {
            output_asm(ADC_IMMEDIATE);
        }
        else if(node.input(1).is_const())
        {
            load_A(node.input(0));
            output_asm(ADC_IMMEDIATE);
        }

{
    cg_key_t key;

    switch(OP)
    {
    case SSA_add:
        else

        addr_mode_table_t const& modes = get_addr_modes(ADC);
        for(unsigned m = 0; m < NUM_ADDR_MODES; ++m)
        {
            if(modes[m] == BAD_OP)
                continue;

            addr_mode_t const mode = static_cast<addr_mode_t>(m);

ADDR_MODE(IMPLIED)
ADDR_MODE(IMMEDIATE)
ADDR_MODE(ZERO_PAGE)
ADDR_MODE(ZERO_PAGE_X)
ADDR_MODE(ZERO_PAGE_Y)
ADDR_MODE(ABSOLUTE)
ADDR_MODE(ABSOLUTE_X)
ADDR_MODE(ABSOLUTE_Y)
ADDR_MODE(INDIRECT)
ADDR_MODE(INDIRECT_X)
ADDR_MODE(INDIRECT_Y)
ADDR_MODE(RELATIVE)


        }
    }
}
