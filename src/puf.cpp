#include "puf.hpp"

#include <cstdint>
#include <array>
#include <vector>
#include <map>
#include <string_view>
#include <charconv>

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
    NUM_SFX_CHAN = CHAN_DPCM,
    NUM_CHAN,
};

struct macro_t
{
    int loop;
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
    std::string name;
    int pattern_length;
    int speed;
    int tempo;
    std::array<int, NUM_CHAN> columns;
    std::vector<std::array<int, NUM_CHAN>> order;
    std::vector<std::vector<row_t>> patterns;

    int pattern_size = 0;
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

constexpr int convert_note(int note, int octave, int scale = 1)
{
    note += 12 * octave;
    return (note - 9) * scale;
}

int parse_note(std::string_view str, int scale)
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

    return convert_note(note, str[2] - '0', scale);
}

// Converts a string to a valid identifier suffix.
std::string convert_name(std::string const& in)
{
    std::string out;
    unsigned quotes = 0;

    for(char c : in)
    {
        if(std::isalpha(c))
            out.push_back(std::tolower(c));
        else if(std::isdigit(c))
            out.push_back(c);
        else if(c == '"')
        {
            ++quotes;
            if(quotes == 2)
                break;
        }
        else
            out.push_back('_');
    }

    return out;
}

const_ht define_const(pstring_t at, std::string_view name, asm_proc_t&& proc, 
                      std::pair<group_data_t*, group_data_ht> group, mod_flags_t flags)
{
    using namespace lex;

    std::unique_ptr<mods_t> mods;
    if(flags)
        mods = std::make_unique<mods_t>(flags);

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

    global_t& global = global_t::lookup_sourceless(at, name);
    const_ht gconst = global.define_const(
        at, {}, { at, type_t::paa(0, group.first->group.handle()) }, group, 
        expr, std::move(paa_def), std::move(mods));

    assert(gconst);
    return gconst;
}

const_ht define_ct(pstring_t at, std::string_view name, std::uint8_t value)
{
    using namespace lex;

    ast_node_t* expr = eternal_emplace<ast_node_t>(ast_node_t{
        .token = { .type = TOK_int, .pstring = at, .value = fixed_uint_t(value) << fixed_t::shift },
    });

    global_t& global = global_t::lookup_sourceless(at, name);
    const_ht gconst = global.define_const(
        at, {}, { at, TYPE_U }, {}, expr, {}, {});

    assert(gconst);
    return gconst;
}

macro_t combine_vol_duty(macro_t volume, macro_t duty)
{
    if(volume.loop < 0 || duty.loop < 0)
        throw std::runtime_error("Can't combine non-looping macros.");

    if(std::size_t(volume.loop) >= volume.sequence.size() || std::size_t(duty.loop) >= duty.sequence.size())
        throw std::runtime_error("Invalid loop value.");

    std::size_t const volume_loop_size = volume.sequence.size() - volume.loop;
    std::size_t const duty_loop_size = duty.sequence.size() - duty.loop;

    std::size_t const max_loop = std::max(volume.loop, duty.loop);
    std::size_t const max_loop_size = std::max(volume_loop_size, duty_loop_size);

    if(duty_loop_size % volume_loop_size != 0 && volume_loop_size % duty_loop_size != 0)
        throw std::runtime_error("The length of duty and volume loops must be  multiples of each other.");

    macro_t combined = {};

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

void convert_puf_music(char const* const begin, std::size_t size, pstring_t at)
{
    using namespace std::literals;

    std::map<std::pair<int, int>, macro_t> macros;
    std::map<int, instrument_t> instruments;
    std::vector<track_t> tracks;

    track_t* active_track = nullptr;
    std::vector<row_t>* active_pattern = nullptr;

    std::map<int, std::vector<std::uint8_t>> dpcms;
    std::vector<std::uint8_t>* active_dpcm = nullptr;

    std::vector<int> penguin_instrument_vector = { -1 };

    std::vector<std::map<penguin_pattern_t, int>> penguin_pattern_maps;

    std::map<dpcm_key_t, int> penguin_dpcm_map;
    std::vector<dpcm_key_t const*> penguin_dpcm_vector;

    std::vector<bucket_t> buckets;
    std::vector<bucket_t> allocated;

    group_t& data_group = *group_t::lookup_sourceless(at, "puf_data"sv);
    auto data_group_pair = data_group.define_data({}, true);

    group_t& omni_group = *group_t::lookup_sourceless(at, "puf_omni"sv);
    auto omni_group_pair = omni_group.define_data({}, false);

    if(begin)
    {
        char const* ptr = begin;
        char const* end = begin + size;

        std::vector<std::string_view> words;

        auto const parse_int = [](std::string_view sv, int base = 10) -> int
        {
            int value;
            auto result = std::from_chars(&*sv.begin(), &*sv.end(), value, base);
            if(result.ec != std::errc())
                throw std::runtime_error(fmt("Unable to parse integer. (%)", sv));
            return value;
        };

        auto const req_words = [&](unsigned amount)
        {
            if(words.size() < amount)
                throw std::runtime_error("Unable to parse file.");
        };

        while(ptr != end)
        {
            ptr = parse_line(ptr, end, words);
            if(words.empty())
                continue;

            if(words[0] == "MACRO"sv)
            {
                req_words(8);

                macro_t macro = {};
                int const type  = parse_int(words[1]);
                int const index = parse_int(words[2]);
                macro.loop  = parse_int(words[3]);

                // Unused:
                // int const release = parse_int(words[4]);
                // int const setting = parse_int(words[5]);

                for(unsigned i = 7; i < words.size(); ++i)
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
                req_words(7);

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
                req_words(8);

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

                instruments[instrument].dpcm_keys[convert_note(note, octave, 2)] = std::move(key);
            }
            else if(words[0] == "TRACK"sv)
            {
                req_words(4);

                track_t track;
                track.pattern_length = parse_int(words[1]);
                track.speed = parse_int(words[2]);
                track.tempo = parse_int(words[3]);
                for(unsigned i = 4; i < words.size(); ++i)
                    track.name += words[i];
                tracks.push_back(track);
                active_track = &tracks.back();
            }
            else if(words[0] == "COLUMNS"sv)
            {
                req_words(NUM_CHAN + 2);

                for(int i = 0; i < NUM_CHAN; ++i)
                    active_track->columns[i] = parse_int(words[i+2]);
            }
            else if(words[0] == "ORDER"sv)
            {
                req_words(NUM_CHAN + 3);

                active_track->order.push_back({});
                for(int i = 0; i < NUM_CHAN; ++i)
                    active_track->order.back()[i] = parse_hex_pair(words[i+3]);
            }
            else if(words[0] == "DPCMDEF"sv)
            {
                req_words(3);

                int const index = parse_int(words[1]);
                int const size  = parse_int(words[2]);
                active_dpcm = &dpcms[index];
                active_dpcm->reserve(size);
            }
            else if(active_dpcm && words[0] == "DPCM")
            {
                req_words(3);

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
                req_words(27);

                row_t row = {};

                row.number = parse_hex_pair(words[1]);

                row.chan[0].note = parse_note(words[3], 2);
                row.chan[0].instrument = parse_hex_pair(words[4]);

                row.chan[1].note = parse_note(words[8], 2);
                row.chan[1].instrument = parse_hex_pair(words[9]);

                row.chan[2].note = parse_note(words[13], 2);
                row.chan[2].instrument = parse_hex_pair(words[14]);

                row.chan[3].note = parse_note(words[18], 2);
                row.chan[3].instrument = parse_hex_pair(words[19]);

                row.chan[4].note = parse_note(words[23], 2);
                row.chan[4].instrument = parse_hex_pair(words[24]);

                for(unsigned i = 0; i < NUM_CHAN; ++i)
                    if(words[6+5*i] == "D00")
                        row.d00 |= 1 << i;

                active_pattern->push_back(row);
            }
        }

        if(tracks.empty())
            throw std::runtime_error("No tracks.");
    }

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

    if(begin)
    {
        for(std::size_t t = 0; t < tracks.size(); ++t)
        {
            buckets.clear();

            track_t& track = tracks[t];
            std::array<std::vector<int>, NUM_CHAN> penguin_channels;

            // Determine the pattern size:
            std::uint8_t pattern_sizes = 0b11111100;

            for(auto const& pattern_array : track.order)
            {
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

                    for(unsigned i = 3; i < 8; ++i)
                        if(size % (i+1))
                            pattern_sizes &= ~(1 << i);
                }
            }

            if(pattern_sizes == 0)
                throw std::runtime_error("Pattern sizes must share a common multiple between 3 and 8.");

            track.pattern_size = builtin::rclz((unsigned)pattern_sizes);

            penguin_pattern_maps.resize(tracks.size());

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

                    passert(size % track.pattern_size == 0, size, track.pattern_size);

                    if(size / track.pattern_size < ps)
                        ps = size / track.pattern_size;
                }

                for(std::size_t k = 0; k < NUM_CHAN; ++k)
                {
                    auto const& pv = track.patterns.at(pattern_array[k]);
                    for(std::size_t i = 0; i < ps; ++i)
                    {
                        penguin_pattern_t penguin_pattern = { 0 };

                        for(int j = 0; j < track.pattern_size; ++j)
                        {
                            channel_data_t cd = pv[i*track.pattern_size+j].chan[k];

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

                        penguin_channels[k].push_back(pair.first->second);
                    }
                }
            }

            track.num_columns = penguin_channels[0].size();

            allocated.clear();

            {
                bucket_t bucket = {};
                asm_vec_t& code = bucket.code.emplace_back();

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

                allocated.push_back(std::move(bucket));
            }

            for(auto const& pair : penguin_pattern_maps[t])
            {
                bucket_t bucket = {};
                asm_vec_t& code = bucket.code.emplace_back();

                code.push_back({ .op = ASM_LABEL, .arg = locator_t::minor_label(pair.second) });
                for(auto byte : pair.first)
                    push_byte(code, byte);

                bucket.size = pair.first.size();
                buckets.push_back(std::move(bucket));
            }

            std::sort(
                buckets.begin(), buckets.end(),
                [](bucket_t const& a, bucket_t const& b) { return a.size > b.size; });

            allocated.reserve(buckets.size() + 1);

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

            asm_proc_t proc;

            for(bucket_t const& bucket : allocated)
            {
                for(auto const& vec : bucket.code)
                    proc.code.insert(proc.code.end(), vec.begin(), vec.end());

                // Padding:
                if(&bucket != &allocated.back())
                    for(unsigned i = 0; i < unsigned((256 - bucket.size) % 256); ++i)
                        push_byte(proc.code, 0);
            }

            track.gconst = define_const(at, fmt("puf_trackid_%", t), std::move(proc), data_group_pair, MOD_align);

            std::string name = convert_name(track.name);
            if(!name.empty())
                define_ct(at, fmt("puf_track_%", name), t);
        }

        for(auto& pair : instruments)
        {
            auto& instrument = pair.second;

            if(instrument.id < 0)
                continue; // Unused instrument.

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

            instrument.gconst = define_const(at, fmt("puf_instrument_%", instrument.id), std::move(proc), data_group_pair, 0);
        }
    }

    {
        // instrument_lo
        asm_proc_t proc;
        for(int i : penguin_instrument_vector)
        {
            passert(instruments[i].gconst, i);
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(instruments[i].gconst).with_is(IS_PTR) });
        }

        define_const(at, "puf_instrument_lo"sv, std::move(proc), omni_group_pair, MOD_align);
    }

    {
        // instrument_hi
        asm_proc_t proc;
        for(int i : penguin_instrument_vector)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(instruments[i].gconst).with_is(IS_PTR_HI) });

        define_const(at, "puf_instrument_hi"sv, std::move(proc), omni_group_pair, MOD_align);
    }

    {
        // instrument_bank
        asm_proc_t proc;
        for(int i : penguin_instrument_vector)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(instruments[i].gconst).with_is(IS_BANK) });

        define_const(at, "puf_instrument_bank"sv, std::move(proc), omni_group_pair, MOD_align);
    }

    {
        // dpcm_rate
        asm_proc_t proc;
        for(auto* ptr : penguin_dpcm_vector)
            push_byte(proc.code, (ptr->pitch & 0xF) | (ptr->loop == 1 ? 0b01000000 : 0));

        define_const(at, "puf_dpcm_rate"sv, std::move(proc), omni_group_pair, MOD_align);
    }

    {
        // dpcm_length
        asm_proc_t proc;
        for(auto* ptr : penguin_dpcm_vector)
            push_byte(proc.code, (dpcms[ptr->sample].size() - 1) / 16);

        define_const(at, "puf_dpcm_length"sv, std::move(proc), omni_group_pair, MOD_align);
    }

    {
        std::map<int, const_ht> dpcm_map;

        for(auto const& p : dpcms)
        {
            asm_proc_t dpcm_proc;
            for(auto b : p.second)
                push_byte(dpcm_proc.code, b);
            const_ht c = define_const(at, fmt("puf_dpcm_%", p.first), std::move(dpcm_proc), omni_group_pair, MOD_dpcm);

            dpcm_map[p.first] = c;
        }

        // dpcm_addr
        asm_proc_t proc;
        for(auto* ptr : penguin_dpcm_vector)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::dpcm(dpcm_map[ptr->sample]) });

        define_const(at, "puf_dpcm_addr"sv, std::move(proc), omni_group_pair, MOD_align);
    }

    {
        // tracks_begin_lo
        asm_proc_t proc;
        for(unsigned i = 0; i < tracks.size(); ++i)
        {
            passert(tracks[i].gconst, i, tracks[i].gconst);
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR) });
        }

        define_const(at, "puf_tracks_begin_lo"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // tracks_begin_hi
        asm_proc_t proc;
        for(unsigned i = 0; i < tracks.size(); ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR_HI) });

        define_const(at, "puf_tracks_begin_hi"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // tracks_bank
        asm_proc_t proc;
        for(unsigned i = 0; i < tracks.size(); ++i)
        {
            passert(tracks[i].gconst, i, tracks[i].gconst);
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(tracks[i].gconst).with_is(IS_BANK) });
        }

        define_const(at, "puf_tracks_bank"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // tracks_end_lo
        asm_proc_t proc;
        for(unsigned i = 0; i < tracks.size(); ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg
                = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR).with_offset(tracks[i].num_columns * 2 * NUM_CHAN) });
        define_const(at, "puf_tracks_end_lo"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // tracks_end_hi
        asm_proc_t proc;
        for(unsigned i = 0; i < tracks.size(); ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg
                = locator_t::gconst(tracks[i].gconst).with_is(IS_PTR_HI).with_offset(tracks[i].num_columns * 2 * NUM_CHAN) });
        define_const(at, "puf_tracks_end_hi"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // tracks_speed
        asm_proc_t proc;
        for(unsigned i = 0; i < tracks.size(); ++i)
            push_byte(proc.code, tracks[i].speed);
        define_const(at, "puf_tracks_speed"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // tracks_pattern_size
        asm_proc_t proc;
        for(unsigned i = 0; i < tracks.size(); ++i)
            push_byte(proc.code, tracks[i].pattern_size - 1);
        define_const(at, "puf_tracks_pattern_size"sv, std::move(proc), omni_group_pair, 0);
    }
}

constexpr int ntsc_notes[] = 
{
    0x07F1,0x077F,0x0713,0x06AD,0x064D,0x05F3,0x059D,0x054C,0x0500,0x04B8,0x0474,0x0434,
    0x03F8,0x03BF,0x0389,0x0356,0x0326,0x02F9,0x02CE,0x02A6,0x0280,0x025C,0x023A,0x021A,
    0x01FB,0x01DF,0x01C4,0x01AB,0x0193,0x017C,0x0167,0x0152,0x013F,0x012D,0x011C,0x010C,
    0x00FD,0x00EF,0x00E1,0x00D5,0x00C9,0x00BD,0x00B3,0x00A9,0x009F,0x0096,0x008E,0x0086,
    0x007E,0x0077,0x0070,0x006A,0x0064,0x005E,0x0059,0x0054,0x004F,0x004B,0x0046,0x0042,
    0x003F,0x003B,0x0038,0x0034,0x0031,0x002F,0x002C,0x0029,0x0027,0x0025,0x0023,0x0021,
    0x001F,0x001D,0x001B,0x001A,0x0018,0x0017,0x0015,0x0014,0x0013,0x0012,0x0011,0x0010,
    0x000F,0x000E,0x000D
};

struct nsf_track_t
{
    std::string name;
    std::array<std::vector<int>, NUM_SFX_CHAN> notes;
    std::array<bool, NUM_SFX_CHAN> empty;
    std::array<const_ht, NUM_SFX_CHAN> gconsts;
};

struct nsf_t
{
    unsigned songs;
    unsigned load_addr;
    unsigned init_addr;
    unsigned play_addr;
};

bool fill_blank_notes(std::vector<int>& notes)
{
    if(notes.empty())
        return true;

    for(unsigned i = 0; i < notes.size(); ++i)
    {
        if(notes[i] >= 0)
        {
            notes.front() = notes[i];
            goto foundNote;
        }
    }
    return true;
foundNote:
    int prev = notes.front();
    for(unsigned i = 0; i < notes.size(); ++i)
    {
        if(notes[i] < 0)
            notes[i] = prev;
        else
            prev = notes[i];
    }
    return false;
}

unsigned char mem_rd(unsigned address);
void mem_wr(unsigned address, unsigned char data);

#include "cpu_2a03.hpp"

std::array<unsigned char, 1 << 16> memory;
std::array<int, 32> apu_registers;
std::array<int, 32> prev_apu_registers;
std::array<int, NUM_SFX_CHAN> volume;
bool log_cpu;
bool effect_stop;

bool register_allowed(unsigned address)
{
    switch(address)
    {
    case 0x4000:
    case 0x4002:
    case 0x4003:
    case 0x4004:
    case 0x4006:
    case 0x4007:
    case 0x4008:
    case 0x400A:
    case 0x400B:
    case 0x400C:
    case 0x400E:
        return true;
    default:
        return false;
    }
}

unsigned char mem_rd(unsigned address)
{
    return address < 0x2000 ? memory[address & 0x7FF] : memory[address];
}

void mem_wr(unsigned address, unsigned char data)
{
    // RAM writes:
    if(address < 0x2000)
    {
        memory[address & 0x7FF] = data;
        return;
    } 
    
    // Expansion memory.
    if(address >= 0x5C00 && address < 0x8000)
    {
        memory[address] = data;
        return;
    } 

    // APU registers:
    if(log_cpu && address < 0x4018)
    {
        if((address == 0x4001 || address == 0x4005) && (data & 0x80))
            throw std::runtime_error("sweep effects are not supported.\n");

        if(address >= 0x4010 && address <= 0x4013)
            throw std::runtime_error("DMC is not supported.\n");

        if(register_allowed(address) && apu_registers[address-0x4000] != data)
        {
            switch(address)
            {
            case 0x4000: volume[CHAN_SQUARE1]  = data & 0x0F; break;
            case 0x4004: volume[CHAN_SQUARE2]  = data & 0x0F; break;
            case 0x4008: volume[CHAN_TRIANGLE] = data & 0x7F; break;
            case 0x400C: volume[CHAN_NOISE]    = data & 0x0F; break;
            }

            apu_registers[address - 0x4000] = data;
        }

        // Catch the C00 effect.
        if(address == 0x4015 && data == 0)
            effect_stop = true;
    }
}

const_ht convert_effect(pstring_t at,
                        std::uint8_t const* const nsf_data, std::size_t nsf_size,
                        nsf_t const& nsf, unsigned song, unsigned mode,
                        std::deque<nsf_track_t>& nsf_tracks,
                        std::pair<group_data_t*, group_data_ht> group_pair)
{
    thread_local std::mutex cpu_mutex;
    std::lock_guard<std::mutex> lock(cpu_mutex);

    assert(nsf_data);
    assert(nsf_size >= 128);
    assert(nsf_size-128 <= memory.size() - nsf.load_addr);
    assert(nsf.load_addr < memory.size());

    memory.fill(0);
    std::memcpy(&memory[nsf.load_addr], nsf_data+128, nsf_size-128);

    apu_registers.fill(-1);
    apu_registers[0x00] = 0x30;
    apu_registers[0x04] = 0x30;
    apu_registers[0x08] = 0x30;
    apu_registers[0x0C] = 0x30;

    volume.fill(0);

    // Init nsf code.
    cpu_reset();
    CPU.A = song;
    CPU.X = mode;
    CPU.PC.hl = nsf.init_addr;
    log_cpu = false;
    for(unsigned i = 0; i < 2000; ++i) 
        cpu_tick(); // 2000 is enough for FT init
    cpu_reset();

    std::vector<std::array<int, 32>> apu_register_log;
    std::vector<std::array<int, 4>> volume_log;

    log_cpu = true;

    for(effect_stop = false; !effect_stop;)
    {
        CPU.PC.hl = nsf.play_addr;
        CPU.jam = false;
        CPU.S = 0xFF;

        for(unsigned i = 0; i < 30000/4 && !effect_stop; ++i)
            cpu_tick();

        apu_register_log.push_back(apu_registers);
        volume_log.push_back(volume);
    }

    for(unsigned k = 0; k < NUM_SFX_CHAN; ++k)
    {
        asm_proc_t proc;

        if(!nsf_tracks[song].empty[k])
        {
            // Volume & Duty
            push_byte(proc.code, apu_register_log.size() + 2);
            push_byte(proc.code, 0);
            for(unsigned i = 0; i < apu_register_log.size(); ++i)
            {
                unsigned vol_duty = apu_register_log[i][0x00+k*4] | 0b110000;
                if(k == 2 && (vol_duty & 0b1111))
                    vol_duty |= 0b1111;
                push_byte(proc.code, vol_duty);
            }

            // Pitch
            if(k == 3)
            {
                push_byte(proc.code, 3);
                push_byte(proc.code, 2);
                push_byte(proc.code, 0);
            }
            else
            {
                push_byte(proc.code, apu_register_log.size() + 2);
                push_byte(proc.code, 0);

                int pitch_bend = 0;
                for(unsigned i = 0; i < apu_register_log.size(); ++i)
                {
                    int note = nsf_tracks[song].notes[k][i];
                    unsigned pitch = apu_register_log[i][0x02 + k*4];
                    pitch |= (apu_register_log[i][0x03 + k*4] & 0b111) << 8;
                    int diff = pitch - ntsc_notes[note] - pitch_bend;
                    pitch_bend += diff;
                    push_byte(proc.code, diff);
                }
            }

            // Arpeggio
            push_byte(proc.code, apu_register_log.size() + 2);
            push_byte(proc.code, 0);
            for(unsigned i = 0; i < apu_register_log.size(); ++i)
            {
                int change = nsf_tracks[song].notes[k][i];
                change -= nsf_tracks[song].notes[k].front();
                change *= 2;
                push_byte(proc.code, change);
            }
        }

        nsf_tracks[song].gconsts[k] = define_const(at, fmt("puf_sfxid_%_%", song, k), std::move(proc), group_pair, 0);
    }

    {
        asm_proc_t proc;

        unsigned mask = 0;
        for(unsigned k = 0; k < 4; ++k)
            if(!nsf_tracks[song].empty[k])
                mask |= 1 << k;
        assert((mask & 0b1111) == mask);

        push_byte(proc.code, mask);

        for(unsigned k = 0; k < 4; ++k)
        {
            if(nsf_tracks[song].empty[k])
                continue;

            locator_t const loc = locator_t::gconst(nsf_tracks[song].gconsts[k]);

            proc.code.push_back({ .op = ASM_DATA, .arg = loc.with_is(IS_PTR) });
            proc.code.push_back({ .op = ASM_DATA, .arg = loc.with_is(IS_PTR_HI) });
            proc.code.push_back({ .op = ASM_DATA, .arg = loc.with_is(IS_BANK) });
            push_byte(proc.code, nsf_tracks[song].notes[k].front()*2);
        }

        std::string name = convert_name(nsf_tracks[song].name);
        if(!name.empty())
            define_ct(at, fmt("puf_sfx_%", name), song);

        return define_const(at, fmt("puf_sfxid_%", song), std::move(proc), group_pair, 0);
    }
}

void convert_puf_sfx(char const* const txt_data, std::size_t txt_size, 
                     std::uint8_t const* const nsf_data, std::size_t nsf_size,
                     pstring_t at)
{
    using namespace std::literals;

    std::deque<nsf_track_t> nsf_tracks;
    nsf_track_t* active_track = nullptr;

    nsf_t nsf = {};

    if(txt_data && nsf_data)
    {

        char const* ptr = txt_data;
        char const* end = txt_data + txt_size;
        std::vector<std::string_view> words;

        while(ptr != end)
        {
            ptr = parse_line(ptr, end, words);
            if(words.empty())
                continue;

            if(words[0] == "TRACK"sv)
            {
                nsf_track_t track;

                for(unsigned i = 4; i < words.size(); ++i)
                    track.name += words[i];

                nsf_tracks.push_back(track);
                active_track = &nsf_tracks.back();
            }
            else if(words[0] == "ROW"sv)
            {
                auto it = words.begin();
                for(std::vector<int>& n : active_track->notes)
                {
                    it = std::find(it, words.end(), ":");
                    ++it;
                    n.push_back(parse_note(*it, 1));
                }
            }
        }

        for(nsf_track_t& t : nsf_tracks)
            for(unsigned i = 0; i < 4; ++i)
                t.empty[i] = fill_blank_notes(t.notes[i]);

        if(nsf_size < 128)
            throw std::runtime_error("Invalid NSF file.");

        nsf.songs = nsf_data[0x06];
        nsf.load_addr = nsf_data[0x08] + (nsf_data[0x09] << 8);
        nsf.init_addr = nsf_data[0x0A] + (nsf_data[0x0B] << 8);
        nsf.play_addr = nsf_data[0x0C] + (nsf_data[0x0D] << 8);

        for(unsigned i = 0x70; i < 0x78; ++i)
            if(nsf_data[i])
                throw std::runtime_error("Bankswitching in NSF file is not supported.");

        if(nsf_data[0x7B])
            throw std::runtime_error("Expansion chips in NSF file are not supported.");
    }

    group_t& data_group = *group_t::lookup_sourceless(at, "puf_data"sv);
    auto data_group_pair = data_group.define_data({}, true);

    group_t& omni_group = *group_t::lookup_sourceless(at, "puf_omni"sv);
    auto omni_group_pair = omni_group.define_data({}, false);

    std::vector<const_ht> gconsts;

    for(unsigned i = 0; i < nsf.songs; ++i)
        gconsts.push_back(convert_effect(at, nsf_data, nsf_size, nsf, i, 0, nsf_tracks, data_group_pair));

    {
        // puf_sfx_lo
        asm_proc_t proc;
        for(unsigned i = 0; i < nsf.songs; ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(gconsts[i]).with_is(IS_PTR) });

        define_const(at, "puf_sfx_lo"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // puf_sfx_hi
        asm_proc_t proc;
        for(unsigned i = 0; i < nsf.songs; ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(gconsts[i]).with_is(IS_PTR_HI) });

        define_const(at, "puf_sfx_hi"sv, std::move(proc), omni_group_pair, 0);
    }

    {
        // puf_sfx_bank
        asm_proc_t proc;
        for(unsigned i = 0; i < nsf.songs; ++i)
            proc.code.push_back({ .op = ASM_DATA, .arg = locator_t::gconst(gconsts[i]).with_is(IS_BANK) });

        define_const(at, "puf_sfx_bank"sv, std::move(proc), omni_group_pair, 0);
    }
}
