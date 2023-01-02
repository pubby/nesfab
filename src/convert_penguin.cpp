#include "convert_penguin.hpp"

#include <iostream>
#include <algorithm>
#include <array>
#include <cstdio>
#include <cctype>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>
#include <charconv>

#include "asm_proc.hpp"
#include "hex.hpp"

namespace // inline namespace
{

using asm_vec_t = std::vector<asm_inst_t>;

struct bucket_t
{
    std::size_t size;
    std::list<asm_vec_t> code;
};

struct macro_t
{
    //int type;
    //int index;
    int loop;
    //int release;
    //int setting;
    std::vector<int> sequence;

    static constexpr int volume   = 0;
    static constexpr int arpeggio = 1;
    static constexpr int pitch    = 2;
    static constexpr int hi_pitch = 3;
    static constexpr int duty     = 4;
};

constexpr int macro_t::volume;
constexpr int macro_t::arpeggio;
constexpr int macro_t::pitch;
constexpr int macro_t::hi_pitch;
constexpr int macro_t::duty;

constexpr std::size_t NUM_CHAN = 4;
constexpr std::size_t CHAN_SQUARE1  = 0;
constexpr std::size_t CHAN_SQUARE2  = 1;
constexpr std::size_t CHAN_TRIANGLE = 2;
constexpr std::size_t CHAN_NOISE    = 3;

bool operator==(macro_t const& a, macro_t const& b)
{
    return (a.loop == b.loop 
            && a.sequence.size() == b.sequence.size()
            && std::equal(a.sequence.begin(), a.sequence.end(), 
                          b.sequence.begin()));
}

bool operator<(macro_t const& a, macro_t const& b)
{
    if(a.loop != b.loop)
        return a.loop < b.loop;
    return std::lexicographical_compare(
        a.sequence.begin(), a.sequence.end(),
        b.sequence.begin(), b.sequence.end());
}

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
    std::array<bool, NUM_CHAN> d00;
    channel_data_t dpcm;
};

struct track_t
{
    int pattern_length;
    int speed;
    int tempo;
    std::array<int, 5> columns;
    std::vector<std::array<int, 5>> order;
    std::vector<std::vector<row_t>> patterns;
};

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

int parse_note(std::string_view str)
{
    using namespace std::literals;

    if(str.size() < 3)
        throw convert_error_t("Invalid note.");

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
    default: throw convert_error_t("Invalid note.");
    }

    switch(str[1])
    {
    case '#':
    case '+': 
        ++note; 
        break;
    case 'b':
    case 'f': 
        --note; 
        break;
    }

    note += 12 * (str[2] - '0');
    return (note - 9) * 2;
}


macro_t combine_vol_duty(macro_t volume, macro_t duty)
{
    if(volume.loop < 0 || duty.loop < 0)
        throw convert_error_t("Can't combine non-looping macros.");

    if(volume.loop >= volume.sequence.size() || duty.loop >= duty.sequence.size())
        throw convert_error_t("Bad loop value.");

    std::size_t const max_size = std::max(volume.sequence.size(), duty.sequence.size());

    std::size_t const volume_loop_size = volume.sequence.size() - volume.loop;
    std::size_t const duty_loop_size = duty.sequence.size() - duty.loop;

    std::size_t const max_loop = std::max(volume.loop, duty.loop);
    std::size_t const max_loop_size = std::max(volume_loop_size, duty_loop_size);

    // TODO: This might be better as a warning.
    if(duty_loop_size % volume_loop_size != 0 && volume_loop_size % duty_loop_size != 0)
        throw convert_error_t("Duty and volume loops are not multiples of each other.");

    macro_t combined = {};
    //combined.type = macro_t::vol_duty;

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

std::uint8_t pattern_mask(std::array<channel_data_t, 8> const& pattern)
{
    std::uint8_t mask = 0;
    for(unsigned j = 0; j < 8; ++j)
        if(pattern[j].note >= 0)
            mask |= 1 << j;
    return mask;
}

enum minor_label_t
{
    TRACK_LABEL,
    PATTERN_LABEL,
    MACRO_LABEL,
    INSTRUMENT_LO_LABEL,
    INSTRUMENT_HI_LABEL,
    INSTRUMENT_PITCH_LABEL,
    INSTRUMENT_VOL_DUTY_LABEL,
    INSTRUMENT_ARPEGGIO_LABEL,
};

struct minor_label_policy_t
{
    using enum_type = minor_label_t;
    static locator_t make_label(unsigned i) { return locator_t::minor_label(i); }
};

template<typename Policy>
struct label_allocator_t
{
    using enum_type = typename Policy::enum_type;

    struct key_t
    {
        enum_type e;
        unsigned a;
        unsigned b;

        auto operator<=>(key_t const&) const = default;
    };

    unsigned next_index = 0;
    std::map<key_t, locator_t> map;

    locator_t operator()() { return Policy::make_label(next_index++); }

    locator_t operator()(enum_type e, unsigned a = 0, unsigned b = 0)
    {
        auto result = map.emplace(key_t{ e, a, b }, Policy::make_label(next_index));
        if(result.second)
            next_index += 1;
        return result.first->second;
    };
};

} // end anonymous namespace

conversion_t convert_penguin(char const* const begin, std::size_t size, global_ht penguin_fn)
{
    using namespace std::literals;

    char const* const end = begin + size;
    char const* ptr = begin;

    std::vector<std::string_view> words;
    std::map<std::pair<int, int>, macro_t> macros;
    std::map<int, instrument_t> instruments;
    std::vector<track_t> tracks;
    track_t* active_track = nullptr;
    std::vector<row_t>* active_pattern = nullptr;

    // First, we'll parse the data.

    auto const parse_int = [](std::string_view sv, int base = 10) -> int
    {
        int value;
        auto result = std::from_chars(&*sv.begin(), &*sv.end(), value, base);
        if(result.ec != std::errc())
            throw convert_error_t("Unable to parse integer.");
        return value;
    };

    auto parse_hex_pair = [](std::string_view str) -> int
    {
        if(str.size() < 2)
            throw convert_error_t(fmt("Invalid hex pair. (%)", str));

        int d1 = char_to_int(str[0]);
        int d2 = char_to_int(str[1]);
        if(d1 < 0 || d2 < 0)
            return -1;
        return (d1 * 16) | d2;
    };

    while(ptr != end)
    {
        ptr = parse_line(ptr, end, words);

        if(words.empty())
            continue;

        if(words[0] == "MACRO"sv)
        {
            if(words.size() < 6)
                throw convert_error_t("Invalid macro.");

            macro_t macro = {};
            int type    = parse_int(words[1]);
            int index   = parse_int(words[2]);
            macro.loop  = parse_int(words[3]);
            int release = parse_int(words[4]);
            int setting = parse_int(words[5]);
            for(unsigned i = 7; i < words.size(); ++i)
                macro.sequence.push_back(parse_int(words[i]));

            // Force macro to loop.
            if(macro.loop < 0 || macro.loop >= macro.sequence.size())
            {
                if(type == macro_t::pitch && macro.sequence.size() && macro.sequence.back() != 0)
                    macro.sequence.push_back(0);
                macro.loop = macro.sequence.size() - 1;
            }

            macros[std::make_pair(type, index)] = macro;
        }
        else if(words[0] == "INST2A03"sv)
        {
            if(words.size() < 7)
                throw convert_error_t("Invalid instrument.");

            instrument_t instrument = {};
            instrument.seq_vol = parse_int(words[2]);
            instrument.seq_arp = parse_int(words[3]);
            instrument.seq_pit = parse_int(words[4]);
            instrument.seq_hpi = parse_int(words[5]);
            instrument.seq_dut = parse_int(words[6]);
            instrument.seq_dut = parse_int(words[6]);
            instruments.emplace(parse_int(words[1]), instrument);
        }
        else if(words[0] == "TRACK"sv)
        {
            if(words.size() < 4)
                throw convert_error_t("Invalid track.");

            track_t track;
            track.pattern_length = parse_int(words[1]);
            track.speed = parse_int(words[2]);
            track.tempo = parse_int(words[3]);
            tracks.push_back(track);
            active_track = &tracks.back();
        }
        else if(words[0] == "COLUMNS"sv)
        {
            for(unsigned i = 0; i < 5; ++i)
                active_track->columns[i] = parse_int(words[i+2]);
        }
        else if(words[0] == "ORDER"sv)
        {
            if(words.size() < 7)
                throw convert_error_t("Invalid order.");

            active_track->order.push_back({});
            for(unsigned i = 0; i < 5; ++i)
                active_track->order.back()[i] = parse_hex_pair(words[i+3]);
        }
        else if(words[0] == "PATTERN"sv)
        {
            active_track->patterns.push_back({});
            active_pattern = &active_track->patterns.back();
        }
        else if(active_pattern && words[0] == "ROW"sv)
        {
            if(words.size() < 22)
                throw convert_error_t("Invalid order.");

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

            for(unsigned i = 0; i < 4; ++i)
                row.d00[i] = (words[6+5*i] == "D00"sv);

            active_pattern->push_back(row);

        }
    }

    // Add blank macros.
    {
        macro_t macro = {};
        macro.loop = 0;
        macro.sequence.push_back(0);
        macros[std::make_pair(macro_t::pitch, -1)] = macro;
        macros[std::make_pair(macro_t::arpeggio, -1)] = macro;
        macros[std::make_pair(macro_t::duty, -1)] = macro;
        macros[std::make_pair(macro_t::volume, -10)] = macro;
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
        instruments.emplace(-1, instrument);
    }

    // Prepare labels

    label_allocator_t<minor_label_policy_t> minor_label;

    locator_t const tracks_end = minor_label();

    auto const instrument_assign_return = [&](unsigned i) -> locator_t
        { return locator_t::penguin(penguin_fn, penguin_label_t(PENGUIN_square1_instrument_assign_return + i)); };

    auto const pitch_return_label = [&](unsigned i) -> locator_t
        { return locator_t::penguin(penguin_fn, penguin_label_t(PENGUIN_square1_instrument_pitch_return + i)); };

    auto const vol_duty_return_label = [&](unsigned i) -> locator_t
        { return locator_t::penguin(penguin_fn, penguin_label_t(PENGUIN_square1_instrument_vol_duty_return + i)); };

    auto const arpeggio_return_label = [&](unsigned k, unsigned i) -> locator_t
        { return locator_t::penguin(penguin_fn, penguin_label_t(PENGUIN_square1_instrument_arpeggio_return + i)); };

    // Now convert and output the data.

    /* TODO
    fp = std::fopen(argv[2], "w");
    if(!fp)
    {
        std::fprintf(stderr, "can't open %s", argv[2]);
        return 1;
    }
    */

    std::vector<bucket_t> buckets;
    asm_vec_t arpeggio_code;
    asm_vec_t track_code;

    std::array<std::map<int, int>, NUM_CHAN> penguin_instrument_map;
    std::array<std::vector<int>, NUM_CHAN> penguin_instrument_vector;

    std::map<std::array<channel_data_t, 8>, int> penguin_pattern_map;
    std::vector<std::array<channel_data_t, 8>> penguin_pattern_vector;

    for(unsigned k = 0; k < NUM_CHAN; ++k)
    {
        penguin_instrument_map[k][-1] = 0;
        penguin_instrument_vector[k].push_back(-1);
    }

    //std::fprintf(fp, ".align 256\n"); TODO

    for(std::size_t t = 0; t < tracks.size(); ++t)
    {
        track_t const& track = tracks[t];
        std::vector<int> penguin_channels[NUM_CHAN];

        for(auto const& pattern_array : track.order)
        {
            // Calculate pattern size.

            std::size_t ps = -1; // Pattern size divided by 8.

            for(std::size_t k = 0; k < NUM_CHAN; ++k)
            {
                auto const& pv = track.patterns.at(pattern_array[k]);
                std::size_t size = 0;

                for(row_t const& row : pv)
                {
                    ++size;
                    if(row.d00[k])
                        break;
                }

                if(size % 8 != 0)
                    throw convert_error_t("Pattern length is not a multiple of 8.");

                ps = std::min(ps, size / 8);
            }

            // Calculate tracks:

            for(std::size_t k = 0; k < NUM_CHAN; ++k)
            {
                auto const& pv = track.patterns.at(pattern_array[k]);
                for(std::size_t i = 0; i < ps; ++i)
                {
                    std::array<channel_data_t, 8> penguin_pattern;
                    for(std::size_t j = 0; j < 8; ++j)
                    {
                        channel_data_t cd = pv[i*8+j].chan[k];

                        if(cd.note == -2) // TODO magic numbers
                            penguin_pattern[j] = { 0, 0 };
                        else if(cd.instrument >= 0 && cd.note >= 0)
                        {
                            auto pair = penguin_instrument_map[k].emplace(
                                cd.instrument, 
                                penguin_instrument_map[k].size());

                            if(pair.second)
                                penguin_instrument_vector[k].push_back(cd.instrument);

                            penguin_pattern[j] = { cd.note, pair.first->second};
                        }
                        else
                            penguin_pattern[j] = { -1, -1 }; // TODO magic numbers

                    }

                    auto pair = penguin_pattern_map.emplace(
                        penguin_pattern,
                        penguin_pattern_map.size());

                    if(pair.second)
                        penguin_pattern_vector.push_back(penguin_pattern);

                    penguin_channels[k].push_back(pair.first->second);
                }
            }
        }

        // Output tracks:

        track_code.push_back({ .op = ASM_LABEL, .arg = minor_label(TRACK_LABEL, t) });

        for(std::size_t i = 0; i < penguin_channels[0].size(); ++i)
        for(std::size_t k = 0; k < NUM_CHAN; ++k)
        {
            if(penguin_channels[k].size() != penguin_channels[0].size())
               throw convert_error_t("Channels are not the same length.");
            track_code.push_back({ .op = ASM_DATA, .arg = minor_label(PATTERN_LABEL, penguin_channels[k][i]).with_is(IS_PTR) });
            track_code.push_back({ .op = ASM_DATA, .arg = minor_label(PATTERN_LABEL, penguin_channels[k][i]).with_is(IS_PTR_HI) });
        }
    }

    track_code.push_back({ .op = ASM_LABEL, .arg = tracks_end });

    //Tracks:
    // !!! THIS SHOULD BE THE FIRST BUCKET !!!
    {
        bucket_t bucket = {};
        asm_vec_t& code = bucket.code.emplace_back();

        // First
        push_byte(code, tracks.size());

        // tracks_lo:
        for(unsigned i = 0; i < tracks.size(); ++i)
            code.push_back({ .op = ASM_DATA, .arg = minor_label(TRACK_LABEL, i).with_is(IS_PTR) });
        code.push_back({ .op = ASM_DATA, .arg = tracks_end.with_is(IS_PTR) });

        // tracks_hi:
        for(unsigned i = 0; i < tracks.size(); ++i)
            code.push_back({ .op = ASM_DATA, .arg = minor_label(TRACK_LABEL, i).with_is(IS_PTR_HI) });
        code.push_back({ .op = ASM_DATA, .arg = tracks_end.with_is(IS_PTR_HI) });

        // tracks_speed:
        for(unsigned i = 0; i < tracks.size(); ++i)
            push_byte(code, tracks[i].speed);

        bucket.size += tracks.size() * 3 + 3;
        assert(bucket.size);
        buckets.push_back(std::move(bucket));
    }

    // Output patterns:

    for(std::size_t i = 0; i < penguin_pattern_vector.size(); ++i)
    {
        bucket_t bucket = {};
        asm_vec_t& code = bucket.code.emplace_back();

        auto const& pattern = penguin_pattern_vector[i];

        code.push_back({ .op = ASM_LABEL, .arg = minor_label(PATTERN_LABEL, i) });
        push_byte(code, pattern_mask(pattern));
        bucket.size += 1;

        for(std::size_t j = 0; j < 8; ++j)
        {
            // TODO: fix negative notes
            if(pattern[j].instrument >= 0 && pattern[j].note >= 0)
            {
                push_byte(code, pattern[j].instrument);
                push_byte(code, pattern[j].note);
                bucket.size += 2;
            }
        }

        assert(bucket.size);
        buckets.push_back(std::move(bucket));
    }

    // Now for instrument macros.

    std::map<macro_t, int> penguin_macro_map;
    std::vector<macro_t> penguin_macro_vector;

    std::map<macro_t, int> penguin_arpeggio_map;
    std::vector<macro_t> penguin_arpeggio_vector;

    // Find used sequences
    for(unsigned k = 0; k < NUM_CHAN; ++k)
    for(unsigned i = 0; i < penguin_instrument_vector[k].size(); ++i)
    {
        int const j = penguin_instrument_vector.at(k).at(i);

        {
            auto it = macros.find(std::make_pair(macro_t::volume, instruments.at(j).seq_vol));
            if(it == macros.end()) // TODO
            {
                std::cout << "fuck" << std::endl;
                std::cout << "seq_vol" << instruments[j].seq_vol << std::endl;
            }

            macro_t vol_duty = combine_vol_duty(
                macros.at(std::make_pair(macro_t::volume, instruments[j].seq_vol)),
                macros.at(std::make_pair(macro_t::duty, instruments[j].seq_dut)));

            auto pair = penguin_macro_map.emplace(
                vol_duty, 
                penguin_macro_map.size());

            if(pair.second)
                penguin_macro_vector.push_back(vol_duty);

            instruments[j].pseq_vol_duty = pair.first->second;
        }

        if(k != CHAN_NOISE)
        {
            macro_t const& pit = macros.at(
                std::make_pair(macro_t::pitch, instruments[j].seq_pit));

            auto pair = penguin_macro_map.emplace(
                pit, 
                penguin_macro_map.size());

            if(pair.second)
                penguin_macro_vector.push_back(pit);

            instruments[j].pseq_pit = pair.first->second;
        }

        {
            macro_t const& arp = macros.at(
                std::make_pair(macro_t::arpeggio, instruments.at(j).seq_arp));

            auto pair = penguin_arpeggio_map.emplace(
                arp, penguin_arpeggio_map.size());

            if(pair.second)
                penguin_arpeggio_vector.push_back(arp);

            instruments[j].pseq_arp = pair.first->second;
        }
    }

    // Addresses to instrument subroutin will be stored in two tables.
    // (One for the lo-byte, one for the hi-byte.)

    for(unsigned k = 0; k < NUM_CHAN; ++k)
    {
        {
            bucket_t bucket = {};
            asm_vec_t& code = bucket.code.emplace_back();

            code.push_back({ .op = ASM_LABEL, .arg = minor_label(INSTRUMENT_LO_LABEL, k) });

            for(unsigned i = 0; i < penguin_instrument_vector[k].size(); ++i)
            {
                code.push_back({ .op = ASM_DATA, .arg = minor_label(INSTRUMENT_ARPEGGIO_LABEL, k, i).with_is(IS_PTR) });
                bucket.size += 1;
            }

            assert(bucket.size);
            buckets.push_back(std::move(bucket));
        }

        {
            bucket_t bucket = {};
            asm_vec_t& code = bucket.code.emplace_back();

            code.push_back({ .op = ASM_LABEL, .arg = minor_label(INSTRUMENT_HI_LABEL, k) });
            for(unsigned i = 0; i < penguin_instrument_vector[k].size(); ++i)
            {
                code.push_back({ .op = ASM_DATA, .arg = minor_label(INSTRUMENT_ARPEGGIO_LABEL, k, i).with_is(IS_PTR_HI) });
                bucket.size += 1;
            }

            assert(bucket.size);
            buckets.push_back(std::move(bucket));
        }
    }

    // Instrument subroutines:

    for(unsigned k = 0; k < NUM_CHAN; ++k)
    for(unsigned i = 0; i < penguin_instrument_vector[k].size(); ++i)
    {
        int const j = penguin_instrument_vector[k][i];

        {
            macro_t const* macro = &penguin_macro_vector[instruments[j].pseq_vol_duty];
            bucket_t bucket = {};
            asm_vec_t& code = bucket.code.emplace_back();

            code.push_back({ .op = ASM_LABEL,      .arg = minor_label(INSTRUMENT_VOL_DUTY_LABEL, k, i) });
            code.push_back({ .op = JSR_ABSOLUTE,   .arg = instrument_assign_return(k) });
            code.push_back({ .op = CPY_IMMEDIATE,  .arg = locator_t::const_byte(macro->sequence.size()) });
            code.push_back({ .op = BCC_RELATIVE,   .arg = locator_t::const_byte(2) });
            code.push_back({ .op = LDY_IMMEDIATE,  .arg = locator_t::const_byte(macro->loop) });
            code.push_back({ .op = LDA_ABSOLUTE_Y, .arg = minor_label(MACRO_LABEL, instruments[j].pseq_vol_duty) });
            code.push_back({ .op = JMP_ABSOLUTE,   .arg = vol_duty_return_label(k) });

            bucket.size += (3+2+2+2+3+3);

            assert(bucket.size);
            buckets.push_back(std::move(bucket));
        }

        if(k != CHAN_NOISE)
        {
            macro_t const* macro = &penguin_macro_vector[instruments[j].pseq_pit];
            bucket_t bucket = {};
            asm_vec_t& code = bucket.code.emplace_back();

            code.push_back({ .op = ASM_LABEL,      .arg = minor_label(INSTRUMENT_PITCH_LABEL, k, i) });
            code.push_back({ .op = JSR_ABSOLUTE,   .arg = minor_label(INSTRUMENT_VOL_DUTY_LABEL, k, i) });
            code.push_back({ .op = CPX_IMMEDIATE,  .arg = locator_t::const_byte(macro->sequence.size()) });
            code.push_back({ .op = BCC_RELATIVE,   .arg = locator_t::const_byte(2) });
            code.push_back({ .op = LDX_IMMEDIATE,  .arg = locator_t::const_byte(macro->loop) });
            code.push_back({ .op = LDA_ABSOLUTE_X, .arg = minor_label(MACRO_LABEL, instruments[j].pseq_pit) });
            code.push_back({ .op = JMP_ABSOLUTE,   .arg = pitch_return_label(k) });

            bucket.size += (3+2+2+2+3+3);

            assert(bucket.size);
            buckets.push_back(std::move(bucket));
        }

        {
            macro_t const* macro = &penguin_arpeggio_vector[instruments[j].pseq_arp];

            arpeggio_code.push_back({ .op = ASM_LABEL, .arg = minor_label(INSTRUMENT_ARPEGGIO_LABEL, k, i) });

            if(k != CHAN_NOISE)
                arpeggio_code.push_back({ .op = JSR_ABSOLUTE, .arg = minor_label(INSTRUMENT_PITCH_LABEL, k, i) });
            else
                arpeggio_code.push_back({ .op = JSR_ABSOLUTE, .arg = minor_label(INSTRUMENT_VOL_DUTY_LABEL, k, i) });

            locator_t loop_label = minor_label();

            for(unsigned i = 0; i < macro->sequence.size(); ++i)
            {
                if(i == macro->loop)
                {
                    push_byte(arpeggio_code, 0x04);
                    push_byte(arpeggio_code, 0x00);
                    arpeggio_code.push_back({ .op = ASM_LABEL, .arg = loop_label });
                }

                if(macro->sequence[i])
                {
                    if(k == CHAN_NOISE)
                        arpeggio_code.push_back({ .op = AXS_IMMEDIATE, .arg = locator_t::const_byte(macro->sequence[i]) });
                    else
                        arpeggio_code.push_back({ .op = SBC_IMMEDIATE, .arg = locator_t::const_byte(macro->sequence[i] * -2) });
                }
                else
                    arpeggio_code.push_back({ .op = NOP_IMPLIED });

                if(i == macro->loop)
                    arpeggio_code.push_back({ .op = JSR_ABSOLUTE, .arg = arpeggio_return_label(k, i).with_advance_offset(2) });
                else
                    arpeggio_code.push_back({ .op = JSR_ABSOLUTE, .arg = arpeggio_return_label(k, i) });
            }

            arpeggio_code.push_back({ .op = JMP_ABSOLUTE, .arg = loop_label });
        }
    }

    // Macros:
    
    for(unsigned i = 0; i < penguin_macro_vector.size(); ++i)
    {
        bucket_t bucket = {};
        asm_vec_t& code = bucket.code.emplace_back();

        macro_t const& macro = penguin_macro_vector[i];

        code.push_back({ .op = ASM_LABEL, .arg = minor_label(MACRO_LABEL, i) });

        for(unsigned j = 0; j < macro.sequence.size(); ++j)
        {
            push_byte(code, macro.sequence[j]);
            bucket.size += 1;
        }

        assert(bucket.size);
        buckets.push_back(std::move(bucket));
    }

    std::sort(
        buckets.begin(), buckets.end(),
        [](bucket_t const& a, bucket_t const& b) { return a.size > b.size; });

    std::vector<bucket_t> allocated;
    allocated.reserve(buckets.size());

    for(bucket_t& bucket : buckets)
    {
        if(bucket.size <= 0)
            throw convert_error_t("Invalid bucket size.");

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

    buckets.clear();

    if(allocated.empty())
        throw convert_error_t("No data.");

    asm_proc_t proc;
    proc.code.reserve(allocated.size() * 256);

    for(bucket_t const& bucket : allocated)
    {
        for(auto const& code : bucket.code)
            proc.code.insert(proc.code.end(), code.begin(), code.end());

        // Padding to keep alignment:
        if(&bucket != &allocated.back())
        {
            std::size_t const old_size = proc.code.size();
            proc.code.resize(old_size + 256 - (bucket.size % 256), { .op = ASM_DATA, .arg = locator_t::const_byte(0) });
        }
    }

    // Arpeggio
    proc.code.insert(proc.code.end(), arpeggio_code.begin(), arpeggio_code.end());

    // Named values
    conversion_t conversion = { std::move(proc) };
    conversion.named_values.push_back({ "tracks", ssa_value_t(tracks.size(), TYPE_INT) });

    return conversion;
}
