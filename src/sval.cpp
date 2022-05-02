#include "sval.hpp"

#include "locator.hpp"

bool is_ct(sval_t const& sval)
{
    for(auto const& v : sval)
        if(ssa_value_t const* ssa = std::get_if<ssa_value_t>(&v))
            if(ssa->holds_ref())
                return false;

    return true;
}

/* TODO
sval_t make_sval(type_t type, std::function<ssa_value_t(type_t)> const& gen)
{
    unsigned const num = num_members(type);
    assert(num > 0);

    sval_t sval;
    sval.reserve(num);

    for(unsigned i = 0; i < num; ++i)
    {
        type_t const mt = member_type(type, i);
        if(mt.name() == TYPE_ARRAY)
        {
            unsigned const length = mt.array_length();
            auto ct_array = make_ct_array(length);
            for(unsigned j = 0; j < length; ++j)
                ct_array[j] = gen(mt);
            sval.emplace_back(std::move(ct_array));
        }
        else
            sval.emplace_back(gen(mt));
    }
    
    return sval;
}

sval_t make_sval(type_t type)
{
    return make_sval(type, [](type_t) { return ssa_value_t(); });
}

locator_t ct_manager_t::insert(fn_ht fn, ct_pair_t const& pair)
{
    auto result = m_ct_pair_map.insert({ pair, m_ct_pairs.size() });
    if(result.second)
        m_ct_pairs.push_back(pair);
    return locator_t::ct_pair(fn, result.first->second);
}

*/
