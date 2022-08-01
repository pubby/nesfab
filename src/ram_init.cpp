#include "ram_init.hpp"

#include <algorithm>
#include <vector>

#include "group.hpp"
#include "globals.hpp"
#include "span.hpp"
#include "asm_proc.hpp"
#include "rom.hpp"

namespace  // anonymous
{

struct init_span_t
{
    gmember_ht gmember = {};
    unsigned atom = 0;
    init_span_t* merged_with = nullptr;

    span_t span() const { return gmember->span(atom); }
};

struct merged_span_t
{
    span_t span = {};
    init_span_t* head = nullptr;
};

// Merges non-overlapping spans together into a smaller set
std::vector<merged_span_t> reduce_spans(std::vector<init_span_t>& vec)
{
    if(vec.size() == 0)
        return {};

    if(vec.size() == 1)
        return {{ vec[0].span(), &vec[0] }};

    // Sorting by address first means we can find mergable spans in linear time next.
    std::sort(vec.begin(), vec.end(), 
        [](init_span_t const& l, init_span_t const& r) { return l.span().addr < r.span().addr; });

    std::vector<merged_span_t> merged;
    merged.reserve(vec.size());

    merged.push_back({ vec[0].span(), &vec[0] });
    for(unsigned i = 1; i < vec.size(); ++i)
    {
        span_t const span = vec[i].span();

        if(span.addr == merged.back().span.end())
        {
            vec[i-1].merged_with = &vec[i];
            merged.back().span.size += span.size;
        }
        else
            merged.push_back({ span, &vec[i] });
    }

    return merged;
}

} // end anonymous namespace


void gen_group_var_inits()
{
    for(group_vars_t& group : group_vars_ht::values())
    {
        std::cout << "group " << group.group.name << std::endl;

        // Gather lists of gmembers that need inits

        std::vector<init_span_t> zero_init;
        std::vector<init_span_t> value_init;

        for(gvar_ht v : group.gvars())
        {
            if(!v->init_expr)
                continue;

            for(gmember_ht m = v->begin_gmember(); m != v->end_gmember(); ++m)
            {
                unsigned const num_atoms = ::num_atoms(m->type(), 0);
                assert(num_atoms > 0);
                for(unsigned a = 0; a < num_atoms; ++a)
                {
                    std::cout << m->span(a) << std::endl;

                    if(!m->span(a))
                        continue;

                    //std::cout << m->span(a) << std::endl;

                    if(m->zero_init(a))
                        zero_init.push_back({ m, a });
                    else
                        value_init.push_back({ m, a });
                }
            }
        }

        if(zero_init.empty() && value_init.empty())
            continue;

        // Merge gmember RAM spans together, minimizing the number of spans

        std::vector<merged_span_t> zero_merged  = reduce_spans(zero_init);
        std::vector<merged_span_t> value_merged = reduce_spans(value_init);

        // Split some of those spans back apart so that all spans have size <= 256.
        // For value inits, we'll also combine their data into rom_arrays.

        struct combined_zero_t { span_t span; };
        std::vector<combined_zero_t> zero_combined;
        zero_combined.reserve(zero_merged.size() * 2);

        for(merged_span_t const& ms : zero_merged)
        {
            span_t span = ms.span;
            assert(span.size);

            while(span.size > 256)
            {
                zero_combined.push_back({{ .addr = span.addr, .size = 256 }});
                span.addr += 256;
                span.size -= 256;
            }
            assert(span.size);

            zero_combined.push_back({ span });
        }

        struct combined_value_t
        {
            span_t span;
            rom_array_ht rom_array;
        };

        std::vector<combined_value_t> value_combined;
        value_combined.reserve(value_merged.size() * 2);

        for(merged_span_t const& ms : value_merged)
        {
            span_t span_left = ms.span;

            loc_vec_t vec;

            auto const reset_vec = [&]()
            {
                vec.clear();
                vec.reserve(std::min<unsigned>(256, span_left.size));
            };

            reset_vec();

            for(init_span_t const* is = ms.head; is; is = is->merged_with)
            {
                gmember_t const& gmember = *is->gmember;
                gvar_t const& gvar = gmember.gvar;

                unsigned const size = gmember.init_size();
                locator_t const* data = gmember.init_data(is->atom);

                assert(vec.size() < 256);

                if(vec.size() + size >= 256)
                {
                    assert(span_left.size >= 256);

                    vec.insert(vec.end(), data, data + (256 - vec.size()));
                    assert(vec.size() == 256);

                    value_combined.push_back({ span_t{ span_left.addr, 256 }, rom_array_t::make(std::move(vec)) });
                    reset_vec();

                    span_left.addr += 256;
                    span_left.size -= 256;
                }
                else
                    vec.insert(vec.end(), data, data + size);
            }

            assert(span_left.size <= 256);
            assert(vec.size() == span_left.size);

            if(vec.size() > 0)
                value_combined.push_back({ span_left, rom_array_t::make(std::move(vec)) });
        }

        assert(!zero_combined.empty() || !value_combined.empty());

        // Create the proc

        asm_proc_t proc;
        unsigned next_label = 0;
        bool x_is_zero = false;

        auto const gen_proc_loop = [&](auto& vec, bool zero, auto const& indexed, auto const& direct)
        {
            if(vec.empty())
                return;

            std::sort(vec.begin(), vec.end(),
                      [](auto const& l, auto const& r) { return l.span.size > r.span.size; });

            constexpr unsigned cutoff = 2;

            unsigned const loop_range = vec[0].span.size;
            unsigned prev_range = loop_range;

            unsigned i = 0; // index into 'vec'

            if(loop_range > cutoff)
            {
                if(!x_is_zero)
                    proc.push_inst(LDX, 0);
                if(zero)
                    proc.push_inst(TXA);

                locator_t const loop_start = proc.push_label(next_label++);
                locator_t const loop_end = proc.make_label(next_label++);

                for(; i < vec.size(); ++i)
                {
                    auto const& v = vec[i];

                    if(v.span.size <= cutoff)
                        break;

                    if(prev_range != v.span.size)
                    {
                        prev_range = v.span.size;
                        proc.push_inst(CPX, prev_range);
                        proc.push_inst(BCS_LONG, loop_end);
                    }

                    indexed(v);
                }

                proc.push_inst(ASM_LABEL, loop_end);
                proc.push_inst(INX);
                if(loop_range == 256)
                    x_is_zero = true;
                else
                    proc.push_inst(CPX, loop_range);
                proc.push_inst(BNE_LONG, loop_start);
            }
            else if(zero)
                proc.push_inst(LDA, 0);

            for(; i < vec.size(); ++i)
            {
                auto const& v = vec[i];

                assert(v.span.size <= cutoff);

                for(unsigned j = 0; j < v.span.size; ++j)
                    direct(v, j);
            }
        };

        gen_proc_loop(zero_combined, true, [&](combined_zero_t const& v)
        {
            proc.push_inst(STA_ABSOLUTE_X, locator_t::addr(v.span.addr)); 
        }, [&](combined_zero_t const& v, unsigned offset)
        {
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(v.span.addr, offset));
        });

        gen_proc_loop(value_combined, false, [&](combined_value_t const& v)
        { 
            proc.push_inst(LDA_ABSOLUTE_X, locator_t::rom_array(v.rom_array));
            proc.push_inst(STA_ABSOLUTE_X, locator_t::addr(v.span.addr));
        }, [&](combined_value_t const& v, unsigned offset)
        {
            proc.push_inst(LDA_ABSOLUTE, locator_t::rom_array(v.rom_array, offset));
            proc.push_inst(STA_ABSOLUTE, locator_t::addr(v.span.addr, offset));
        });

        proc.push_inst(RTS);

        // Tag the proc
        // TODO
        proc.initial_optimize();

        std::cout << "MODE ASM:\n" << group.group.name << ":\n";
        proc.write_assembly(std::cout);
        std::cout << "\n";
    }
}

