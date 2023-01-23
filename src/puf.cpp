#include "puf.hpp"

#include <cstdint>
#include <array>
#include <vector>
#include <map>
#include <string_view>
#include <charconv>
#include <iostream> // TODO

#include "globals.hpp"
#include "group.hpp"
#include "format.hpp"
#include "hex.hpp"
#include "asm_proc.hpp"
#include "eternal_new.hpp"

using penguin_pattern_t = std::vector<std::uint8_t>;
using asm_vec_t = std::vector<asm_inst_t>;

struct bucket_t
{
    std::size_t size;
    std::list<asm_vec_t> code;
};

enum channel_t
{
    CHAN_SQUARE1 = 0,
    CHAN_SQUARE2,
    CHAN_TRIANGLE,
    CHAN_NOISE,
    CHAN_DPCM,
    NUM_CHAN,
};

struct macro_t
{
    //int type;
    //int index;
    int loop;
    //int release;
    //int setting;
    std::vector<int> sequence;

    auto operator<=>(macro_t const&) const = default;

    static constexpr int volume   = 0;
    static constexpr int arpeggio = 1;
    static constexpr int pitch    = 2;
    static constexpr int hi_pitch = 3;
    static constexpr int duty     = 4;
};

struct dpcm_key_t
{
    int sample;
    int pitch;
    int loop;
    int loop_point;

    auto operator<=>(dpcm_key_t const&) const = default;
};

struct instrument_t
{
    int seq_vol;
    int seq_arp;
    int seq_pit;
    int seq_hpi;
    int seq_dut;

    int pseq_vol_duty;
    int pseq_arp;
    int pseq_pit;

    std::map<int, dpcm_key_t> dpcm_keys;

    int id = -1;
    const_ht gconst = {};
};

struct channel_data_t
{
    int note;
    int instrument;

    auto operator<=>(channel_data_t const&) const = default;
};

struct row_t
{
    int number;
    std::array<channel_data_t, NUM_CHAN> chan;
    std::uint8_t d00; // Bitset with one bit per channel.
    channel_data_t dpcm;
};

struct track_t
{
    int pattern_length;
    int speed;
    int tempo;
    std::array<int, NUM_CHAN> columns;
    std::vector<std::array<int, NUM_CHAN>> order;
    std::vector<std::vector<row_t>> patterns;

    unsigned num_columns = 0;
    const_ht gconst = {};
};

int parse_hex_pair(std::string_view str)
{
    if(str.size() < 2)
        throw std::runtime_error(fmt("Invalid hex pair. (%)", str));

    int d1 = char_to_int(str[0]);
    int d2 = char_to_int(str[1]);
    if(d1 < 0 || d2 < 0)
        return -1;
    return (d1 * 16) | d2;
}

char const* parse_line(char const* const ptr, char const* const end, 
                       std::vector<std::string_view>& words)
{
    words.clear();
    for(char const* last = ptr; last != end;)
    {
        while(last != end && std::isspace(*last))
            if(*(last++) == '\n')
                return last;
        char const* first = last;
        while(last != end && *last != ' ' && *last != '\n')
            ++last;
        words.emplace_back(first, last);
    }
    return end;
}

constexpr int convert_note(int note, int octave)
{
    note += 12 * octave;
    return (note - 9) * 2;
}

int parse_note(std::string_view str)
{
    using namespace std::literals;

    if(str == "..."sv)
        return -1;
    if(str == "---"sv)
        return -2;
    if(str == "==="sv)
        return -2;

    if(str[2] == '#')
    {
        int r = char_to_int(str[0]);
        if(r < 0)
            return -1;
        return r ^ 0b1111;
    }

    int note = -1;

    switch(str[0])
    {
    case 'C': note = 0; break;
    case 'D': note = 2; break;
    case 'E': note = 4; break;
    case 'F': note = 5; break;
    case 'G': note = 7; break;
    case 'A': note = 9; break;
    case 'B': note = 11; break;
    default: throw std::runtime_error("bad note");
    }

    switch(str[1])
    {
    case '#':
    case '+': ++note; break;
    case 'b':
    case 'f': --note; break;
    }

    return convert_note(note, str[2] - '0');
}


macro_t combine_vol_duty(macro_t volume, macro_t duty)
{
    if(volume.loop < 0 || duty.loop < 0)
        throw std::runtime_error("Can't combine non-looping macros.");

    if(std::size_t(volume.loop) >= volume.sequence.size() || std::size_t(duty.loop) >= duty.sequence.size())
        throw std::runtime_error("Invalid loop value.");

    // TODO: remove
    //std::size_t const max_size = std::max(volume.sequence.size(), duty.sequence.size());

    std::size_t const volume_loop_size = volume.sequence.size() - volume.loop;
    std::size_t const duty_loop_size = duty.sequence.size() - duty.loop;

    std::size_t const max_loop = std::max(volume.loop, duty.loop);
    std::size_t const max_loop_size = std::max(volume_loop_size, duty_loop_size);

    if(duty_loop_size % volume_loop_size != 0 && volume_loop_size % duty_loop_size != 0)
        std::fprintf(stderr, "The length of duty and volume loops must be  multiples of each other.\n");

    macro_t combined = {};
    //combined.type = macro_t::vol_duty; TODO

    for(std::size_t i = 0; i < max_loop + max_loop_size; ++i)
    {
        unsigned v = 0b00110000;

        std::size_t j = i;
        while(j >= volume.sequence.size())
            j -= (volume.sequence.size() - volume.loop);
        v |= volume.sequence[j];

        std::size_t k = i;
        while(k >= duty.sequence.size())
            k -= (duty.sequence.size() - duty.loop);
        v |= duty.sequence[k] << 6;

        combined.sequence.push_back(v);
        combined.loop = max_loop;
    }

    return combined;
}

void convert_puf(char const* const begin, std::size_t size, pstring_t at)
{
    using namespace std::literals;

    char const* ptr = begin;
    char const* end = begin + size;

    std::vector<std::string_view> words;
    std::map<std::pair<int, int>, macro_t> macros;
    std::map<int, instrument_t> instruments;
    std::vector<track_t> tracks;

    track_t* active_track = nullptr;
    std::vector<row_t>* active_pattern = nullptr;

    std::map<int, std::vector<std::uint8_t>> dpcms;
    std::vector<std::uint8_t>* active_dpcm = nullptr;

    auto const parse_int = [](std::string_view sv, int base = 10) -> int
    {
        int value;
        auto result = std::from_chars(&*sv.begin(), &*sv.end(), value, base);
        if(result.ec != std::errc())
            throw std::runtime_error("Unable to parse integer.");
        return value;
    };

    while(ptr != end)
    {
        ptr = parse_line(ptr, end, words);
        if(words.empty())
            continue;

        if(words[0] == "MACRO"sv)
        {
            macro_t macro = {};
            int const type  = parse_int(words[1]);
            int const index = parse_int(words[2]);
            macro.loop  = parse_int(words[3]);

            // Unused:
            // int const release = parse_int(words[4]);
            // int const setting = parse_int(words[5]);

            for(int i = 7; i < words.size(); ++i)
                macro.sequence.push_back(parse_int(words[i]));

            // Force macro to loop.
            if(macro.loop < 0 || std::size_t(macro.loop) >= macro.sequence.size())
            {
                if(type == macro_t::pitch && macro.sequence.size() && macro.sequence.back() != 0)
                    macro.sequence.push_back(0);
                macro.loop = macro.sequence.size() - 1;
            }

            macros[std::make_pair(type, index)] = macro;
        }
        else if(words[0] == "INST2A03"sv)
        {
            instrument_t instrument = {};
            instrument.seq_vol = parse_int(words[2]);
            instrument.seq_arp = parse_int(words[3]);
            instrument.seq_pit = parse_int(words[4]);
            instrument.seq_hpi = parse_int(words[5]);
            instrument.seq_dut = parse_int(words[6]);
            instruments.emplace(parse_int(words[1]), instrument);
        }
        else if(words[0] == "KEYDPCM"sv)
        {
            int const instrument = parse_int(words[1]);
            int const octave     = parse_int(words[2]);
            int const note       = parse_int(words[3]);

            dpcm_key_t key =
            {
                .sample     = parse_int(words[4]),
                .pitch      = parse_int(words[5]),
                .loop       = parse_int(words[6]),
                .loop_point = parse_int(words[7]),
            };

            instruments[instrument].dpcm_keys[convert_note(note, octave)] = std::move(key);
        }
        else if(words[0] == "TRACK"sv)
        {
            track_t track;
            track.pattern_length = parse_int(words[1]);
            track.speed = parse_int(words[2]);
            track.tempo = parse_int(words[3]);
            tracks.push_back(track);
            active_track = &tracks.back();
        }
        else if(words[0] == "COLUMNS"sv)
        {
            for(int i = 0; i < NUM_CHAN; ++i)
                active_track->columns[i] = parse_int(words[i+2]);
        }
        else if(words[0] == "ORDER"sv)
        {
            active_track->order.push_back({});
            for(int i = 0; i < NUM_CHAN; ++i)
                active_track->order.back()[i] = parse_hex_pair(words[i+3]);
        }
        else if(words[0] == "DPCMDEF"sv)
        {
            int const index = parse_int(words[1]);
            int const size  = parse_int(words[2]);
            active_dpcm = &dpcms[index];
            active_dpcm->reserve(size);
        }
        else if(active_dpcm && words[0] == "DPCM")
        {
            for(unsigned i = 2; i < words.size(); ++i)
                active_dpcm->push_back(parse_hex_pair(words[i]));
        }
        else if(words[0] == "PATTERN"sv)
        {
            active_track->patterns.push_back({});
            active_pattern = &active_track->patterns.back();
        }
        else if(active_pattern && words[0] == "ROW"sv)
        {
            row_t row = {};

            row.number = parse_hex_pair(words[1]);

            row.chan[0].note = parse_note(words[3]);
            row.chan[0].instrument = parse_hex_pair(words[4]);

            row.chan[1].note = parse_note(words[8]);
            row.chan[1].instrument = parse_hex_pair(words[9]);

            row.chan[2].note = parse_note(words[13]);
            row.chan[2].instrument = parse_hex_pair(words[14]);

            row.chan[3].note = parse_note(words[18]);
            row.chan[3].instrument = parse_hex_pair(words[19]);

            row.chan[4].note = parse_note(words[23]);
            row.chan[4].instrument = parse_hex_pair(words[24]);

            for(unsigned i = 0; i < NUM_CHAN; ++i)
                if(words[6+5*i] == "D00")
                    row.d00 |= 1 << i;

            active_pattern->push_back(row);
        }
    }

    if(tracks.empty())
        throw std::runtime_error("No tracks.");

    // Add blank macros.
    {
        macro_t macro = {};
        macro.loop = 0;
        macro.sequence.push_back(0);
        macros[std::make_pair(macro_t::pitch, -1)]    = macro;
        macros[std::make_pair(macro_t::arpeggio, -1)] = macro;
        macros[std::make_pair(macro_t::duty, -1)]     = macro;
        macros[std::make_pair(macro_t::volume, -10)]  = macro;
        macro.sequence[0] = 15;
        macros[std::make_pair(macro_t::volume, -1)] = macro;
    }

    // Add blank instruments
    {
        instrument_t instrument = {};
        instrument.seq_vol = -10;
        instrument.seq_arp = -1;
        instrument.seq_pit = -1;
        instrument.seq_hpi = -1;
        instrument.seq_dut = -1;
        instrument.pseq_vol_duty = -1;
        instrument.pseq_arp = -1;
        instrument.pseq_pit = -1;
        instrument.id = 0;
        instruments.emplace(-1, instrument);
    }

    std::vector<int> penguin_instrument_vector = { -1 };

    std::vector<std::map<penguin_pattern_t, int>> penguin_pattern_maps(tracks.size());
    //std::vector<penguin_pattern_t> penguin_pattern_vector;

    std::map<dpcm_key_t, int> penguin_dpcm_map;
    std::vector<dpcm_key_t const*> penguin_dpcm_vector;

    //

    // std::fprintf(fp, ".align 256\n");TODO

    std::vector<bucket_t> buckets;
    std::vector<bucket_t> allocated;

    auto const define = [&](std::string_view name, asm_proc_t&& proc, std::pair<group_data_t*, group_data_ht> group, bool align) -> const_ht
    {
        using namespace lex;

        std::unique_ptr<mods_t> mods;
        if(align)
            mods = std::make_unique<mods_t>(MOD_align);

        std::cout << "BYTE SUB OG1 " << proc.code.size() << std::endl;

        ast_node_t* sub_proc = eternal_emplace<ast_node_t>(ast_node_t{
            .token = token_t::make_ptr(TOK_byte_block_sub_proc, at, 
                                       eternal_emplace<asm_proc_t>(std::move(proc))),
            .children = nullptr,
        });

        ast_node_t* expr = eternal_emplace<ast_node_t>(ast_node_t{
            .token = { .type = TOK_byte_block_proc, .pstring = at, .value = 1 },
            .children = sub_proc,
        });

        auto paa_def = std::make_unique<paa_def_t>();

        std::cout << "BYTE SUB OG " << sub_proc->token.ptr<asm_proc_t>()->size() << std::endl;

        global_t& global = global_t::lookup_sourceless(at, name);
        const_ht gconst = global.define_const(
            at, {}, { at, type_t::paa(0, group.first->group.handle()) }, group, 
            expr, std::move(paa_def), std::move(mods));

        assert(gconst);
        return gconst;
    };

    group_t& data_group = group_t::lookup_sourceless(at, "puf_data"sv);
    auto data_group_pair = data_group.define_data({}, true);

    group_t& omni_group = group_t::lookup_sourceless(at, "puf_omni"sv);
    auto omni_group_pair = omni_group.define_data({}, false);

    for(std::size_t t = 0; t < tracks.size(); ++t)
    {
        unsigned next_label = 0;
        buckets.clear();

        track_t& track = tracks[t];
        std::array<std::vector<int>, NUM_CHAN> penguin_channels;

        for(auto const& pattern_array : track.order)
        {
            std::size_t ps = -1;

            for(std::size_t k = 0; k < NUM_CHAN; ++k)
            {
                auto const& pv = track.patterns.at(pattern_array[k]);
                unsigned size = 0;
                for(row_t const& row : pv)
                {
                    ++size;
                    if(row.d00 & (1 << k))
                        break;
                }
                if(size % 8 != 0)
                    throw std::runtime_error("Pattern size must be a multiple of 8.");
                if(size / 8 < ps)
                    ps = size / 8;
            }

            for(std::size_t k = 0; k < NUM_CHAN; ++k)
            {
                auto const& pv = track.patterns.at(pattern_array[k]);
                for(std::size_t i = 0; i < ps; ++i)
                {
                    penguin_pattern_t penguin_pattern = { 0 };

                    for(std::size_t j = 0; j < 8; ++j)
                    {
                        channel_data_t cd = pv[i*8+j].chan[k];

                        if(cd.note == -2)
                        {
                            if(k != CHAN_DPCM)
                                penguin_pattern.push_back(0);
                            penguin_pattern.push_back(0);
                        }
                        else if(cd.instrument >= 0 && cd.note >= 0)
                        {
                            if(k == CHAN_DPCM)
                            {
                                auto const& key = instruments.at(cd.instrument).dpcm_keys.at(cd.note);
                                auto result = penguin_dpcm_map.emplace(key, penguin_dpcm_vector.size());
                                if(result.second)
                                    penguin_dpcm_vector.push_back(&result.first->first);
                                penguin_pattern.push_back(result.first->second + 1);
                            }
                            else
                            {
                                auto& instrument = instruments[cd.instrument];
                                if(instrument.id < 0)
                                {
                                    instrument.id = penguin_instrument_vector.size();
                                    penguin_instrument_vector.push_back(cd.instrument);
                                }
                                penguin_pattern.push_back(instrument.id);
                                penguin_pattern.push_back(cd.note);
                            }
                        }
                        else
                            continue;

                        penguin_pattern[0] |= 1 << j;
                    }

                    auto pair = penguin_pattern_maps[t].emplace(
                        penguin_pattern,
                        penguin_pattern_maps[t].size());
                    /* TODO
                    if(pair.second)
                        penguin_pattern_vector.push_back(penguin_pattern);
                        */
                    penguin_channels[k].push_back(pair.first->second);
                }
            }
        }

        //track_code.push_back({ .op = ASM_LABEL, .arg = minor_label(TRACK_LABEL, t) });

        track.num_columns = penguin_channels[0].size();


        // TODO
        {
            bucket_t bucket = {};
            asm_vec_t& code = bucket.code.emplace_back();

            //code.push_back({ .op = ASM_LABEL, .arg = locator_t::minor_label(ENTRY_LABEL) });

            for(std::size_t i = 0; i < penguin_channels[0].size(); ++i)
            for(std::size_t k = 0; k < NUM_CHAN; ++k)
            {
                if(penguin_channels[k].size() != penguin_channels[0].size())
                   throw std::runtime_error("Channels are not the same length.");

                assert(penguin_channels[k][i] >= 0);
                locator_t const label = locator_t::minor_label(penguin_channels[k][i]);

                code.push_back({ .op = ASM_DATA, .arg = label.with_is(IS_PTR) });
                code.push_back({ .op = ASM_DATA, .arg = label.with_is(IS_PTR_HI) });
                bucket.size += 2;
            }

            buckets.push_back(std::move(bucket));
        }

        for(auto const& pair : penguin_pattern_maps[t])
        {
            bucket_t bucket = {};
            asm_vec_t& code = bucket.code.emplace_back();

            code.push_back({ .op = ASM_LABEL, .arg = locator_t::minor_label(pair.second) });
            for(auto byte : pair.first)
                push_byte(code, byte);

            bucket.size = pair.first.size();
            buckets.push_back(std::move(bucket)); // TODO
        }

        std::sort(
            buckets.begin(), buckets.end(),
            [](bucket_t const& a, bucket_t const& b) { return a.size > b.size; });

        allocated.clear();
        allocated.reserve(buckets.size());

        for(bucket_t& bucket : buckets)
        {
            if(bucket.size <= 0)
                throw std::runtime_error("Bad bucket size.");

            for(bucket_t& a : allocated)
            {
                if(bucket.size + a.size <= 256)
                {
                    a.size += bucket.size;
                    a.code.splice(a.code.end(), bucket.code);
                    goto inserted;
                }
            }
            allocated.push_back(std::move(bucket));
        inserted:;
        }

        // TODO:
        asm_proc_t proc;

        for(bucket_t const& bucket : allocated)
            for(auto const& vec : bucket.code)
                proc.code.insert(proc.code.end(), vec.begin(), vec.end());

        // TODO: do something with the proc
        track.gconst = define(fmt("puf_track_%", t), std::move(proc), data_group_pair, true);
    }

    //std::fprintf(fp, "tracks_end:\n");

    // TODO
    //unsigned prev = ~0u;
    //int next_id = 0;

    // NEW STUFF

    for(auto& pair : instruments)
    {
        auto& instrument = pair.second;

        if(instrument.id < 0)
            continue; // Unused instrument.

        //std::fprintf(fp, "instrument_%u:\n", pair.first);

        // TODO: check for missing, handle gracefully

        if(!macros.count(std::make_pair(macro_t::volume, instrument.seq_vol))
        || !macros.count(std::make_pair(macro_t::duty, instrument.seq_dut)))
        {
            throw std::runtime_error("Missing instrument macro.");
        }

        macro_t vol_duty = combine_vol_duty(
            macros.at(std::make_pair(macro_t::volume, instrument.seq_vol)),
            macros.at(std::make_pair(macro_t::duty, instrument.seq_dut)));

        macro_t const& pit = macros.at(std::make_pair(macro_t::pitch, instrument.seq_pit));

        macro_t const& arp = macros.at(std::make_pair(macro_t::arpeggio, instrument.seq_arp));

        // Append the data:

        asm_proc_t proc;

        auto const append = [&](auto const& macro, int scale = 1)
        {
            push_byte(proc.code, macro.sequence.size() + 2);
            push_byte(proc.code, macro.loop + 2);
            for(auto b : macro.sequence)
                push_byte(proc.code, b * scale);
        };

        append(vol_duty);
        append(pit);
        append(arp, 2);

        instrument.gconst = define(fmt("puf_instrument_%", instrument.id), std::move(proc), data_group_pair, false);
    }

    {
        // instrument_lo
        asm_proc_t proc;
        for(int i : penguin_instrument_vector)
        {
            passert(instruments[i].gconst, i);
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(instruments[i].gconst).with_is(IS_PTR) });
        }

        define("puf_instrument_lo"sv, std::move(proc), omni_group_pair, true);
    }

    {
        // instrument_hi
        asm_proc_t proc;
        for(int i : penguin_instrument_vector)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(instruments[i].gconst).with_is(IS_PTR_HI) });

        define("puf_instrument_hi"sv, std::move(proc), omni_group_pair, true);
    }

    {
        // instrument_bank
        asm_proc_t proc;
        for(int i : penguin_instrument_vector)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(instruments[i].gconst).with_is(IS_BANK) });

        define("puf_instrument_bank"sv, std::move(proc), omni_group_pair, true);
    }

    {
        // dpcm_rate
        asm_proc_t proc;
        for(auto* ptr : penguin_dpcm_vector)
            push_byte(proc.code, (ptr->pitch & 0xF) | (ptr->loop == 1 ? 0b01000000 : 0));

        define("puf_dpcm_rate"sv, std::move(proc), omni_group_pair, true);
    }

    {
        // dpcm_length
        asm_proc_t proc;
        for(auto* ptr : penguin_dpcm_vector)
            push_byte(proc.code, (dpcms[ptr->sample].size() - 1) / 16);

        define("puf_dpcm_length"sv, std::move(proc), omni_group_pair, true);
    }

    /* TODO
    {
        // dpcm_addr
        asm_proc_t proc;
        for(auto* ptr : penguin_dpcm_vector)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::dpcm_byte(ptr->sample) });

        define("puf_dpcm_addr"sv, std::move(proc), omni_group_pair, true);
    }
    */

    /* TODO
    std::fprintf(fp, "dpcm_addr:\n");
    for(auto* ptr : penguin_dpcm_vector)
        std::fprintf(fp, ".byt .lobyte((dpcm_sample_%u - $C000) / 64)\n", ptr->sample);

    // TODO
    for(auto const& p : dpcms)
    {
        std::fprintf(fdpcm, ".align 64\n", p.first);
        std::fprintf(fdpcm, "dpcm_sample_%u:\n", p.first);
        for(auto b : p.second)
            std::fprintf(fdpcm, ".byt %u\n", b);
    }
        */

    std::puts("DONE NEW");

    {
        // tracks_begin_lo
        asm_proc_t proc;
        for(int i = 0; i < tracks.size(); ++i)
        {
            passert(tracks[i].gconst, i, tracks[i].gconst);
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR) });
        }

        define("puf_tracks_begin_lo"sv, std::move(proc), omni_group_pair, false);
    }

    {
        // tracks_begin_hi
        asm_proc_t proc;
        for(int i = 0; i < tracks.size(); ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR_HI) });

        define("puf_tracks_begin_hi"sv, std::move(proc), omni_group_pair, false);
    }

    {
        // tracks_end_lo
        asm_proc_t proc;
        for(int i = 0; i < tracks.size(); ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg
                = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR).with_offset(tracks[i].num_columns * 2) });
        define("puf_tracks_end_lo"sv, std::move(proc), omni_group_pair, false);
    }

    {
        // tracks_end_hi
        asm_proc_t proc;
        for(int i = 0; i < tracks.size(); ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg
                = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR_HI).with_offset(tracks[i].num_columns * 2) });
        define("puf_tracks_end_hi"sv, std::move(proc), omni_group_pair, false);
    }

    {
        // tracks_speed
        asm_proc_t proc;
        for(int i = 0; i < tracks.size(); ++i)
            push_byte(proc.code, tracks[i].speed);
        define("puf_tracks_speed"sv, std::move(proc), omni_group_pair, false);
    }



    /*
    std::fprintf(fp, "tracks_lo:\n");
    for(int i = 0; i < tracks.size(); ++i)
        std::fprintf(fp, ".byt .lobyte(track_%i)\n", i);
    std::fprintf(fp, ".byt .lobyte(tracks_end)\n");

    std::fprintf(fp, "tracks_hi:\n");
    for(int i = 0; i < tracks.size(); ++i)
        std::fprintf(fp, ".byt .hibyte(track_%i)\n", i);
    std::fprintf(fp, ".byt .hibyte(tracks_end)\n");

    std::fprintf(fp, "tracks_speed:\n");
    for(int i = 0; i < tracks.size(); ++i)
        std::fprintf(fp, ".byt %i\n", tracks[i].speed);

    std::printf("pattern size   = %u\n", pattern_size);
    std::printf("pattern size o = %u\n", pattern_size_o);
    std::printf("macro size = %u\n", macro_size);
    std::printf("fn size = %u\n", fn_size);
    std::printf("duped= %u\n", duped);

    std::fclose(fdpcm);
    */
}
