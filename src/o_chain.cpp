#include "o_id.hpp"

#include <cstdint>
#ifndef NDEBUG
#include <iostream>
#endif

#include <boost/container/small_vector.hpp>
#include <boost/container/static_vector.hpp>

#include "flat/small_map.hpp"

#include "ir.hpp"
#include "type.hpp"
#include "type_mask.hpp"
#include "alloca.hpp"

namespace bc = ::boost::container;


namespace
{
    struct entry_t
    {
        type_t type;
        fixed_uint_t value;
        ssa_ht handle;
        unsigned index;
    };
} // end anonymous namespace

using ssa_chain_d = fc::vector_map<ssa_op_t, std::vector<entry_t>>;

static ssa_chain_d& chain_data(ssa_ht h) { return h.data<ssa_chain_d>(); }

bool o_chain(log_t* log, ir_t& ir)
{
    ssa_data_pool::scope_guard_t<ssa_chain_d> sg(ssa_pool::array_size());

    bool modified = false;

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        if(ssa_it->in_daisy())
            continue;

        switch(ssa_it->op())
        {
        case SSA_add:
        case SSA_sub:
            if(!ssa_it->input(2).is_num())
                continue;
            for(unsigned i = 0; i < (ssa_it->op() == SSA_sub ? 1 : 2); i += 1)
            {
                if(!ssa_it->input(!i).is_num() || !ssa_it->input(i).holds_ref() 
                   || ssa_it->input(i)->cfg_node() != cfg_it || carry_used(*ssa_it))
                {
                    continue;
                }
                fixed_uint_t f = ssa_it->input(!i).fixed().value;
                fixed_uint_t const mask = numeric_bitmask(ssa_it->type().name());
                if(ssa_it->op() == SSA_add && ssa_it->input(2).whole())
                    f += low_bit_only(mask);
                else if(ssa_it->op() == SSA_sub && !ssa_it->input(2).whole())
                    f -= low_bit_only(mask);
                f &= mask;
                chain_data(ssa_it->input(i).handle())[ssa_it->op()].push_back(
                    { .type = ssa_it->type(), .value = f, .handle = ssa_it, .index = i });
                break;
            }
            break;

        default: 
            continue;
        }
    }

    for(cfg_ht cfg_it = ir.cfg_begin(); cfg_it; ++cfg_it)
    for(ssa_ht ssa_it = cfg_it->ssa_begin(); ssa_it; ++ssa_it)
    {
        auto& d = chain_data(ssa_it);
        for(auto& pair : d.container)
        {
            auto& vec = pair.second;

            if(vec.size() < 2)
                continue;

            for(unsigned i = 0; i < vec.size(); i += 1)
            {
                auto& entry = vec[i];
                type_t const type = entry.type;
                fixed_uint_t const mask = numeric_bitmask(type.name());
                fixed_uint_t desired = entry.value;

                switch(pair.first)
                {
                case SSA_add:
                    desired -= low_bit_only(mask);
                    break;
                case SSA_sub:
                    desired += low_bit_only(mask);
                    break;
                default:
                    assert(false);
                }
                desired &= mask;

                for(unsigned j = 0; j < vec.size(); j += 1)
                {
                    auto const& other = vec[j];
                    if(i == j || other.value != desired || other.type != type)
                        continue;

                    ssa_ht const h = entry.handle;
                    h->link_change_input(entry.index, other.handle);
                    h->link_change_input(!entry.index, ssa_value_t(fixed_t{low_bit_only(mask)}, type.name()));

                    switch(pair.first)
                    {
                    case SSA_add:
                        h->link_change_input(2, ssa_value_t(0u, TYPE_BOOL));
                        break;
                    case SSA_sub:
                        h->link_change_input(2, ssa_value_t(1u, TYPE_BOOL));
                        break;
                    default:
                        assert(false);
                    }

                    modified = true;
                    break;
                }

            next:;
            }
        }
    }

    return modified;
}
