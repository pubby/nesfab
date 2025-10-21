#include "convert_wav.hpp"

#include <cstdint>
#include <vector>

#include "AudioFile.h"

std::vector<std::uint8_t> wav_to_dpcm(std::vector<std::uint8_t> const& wav, unsigned quality)
{
    std::vector<std::uint8_t> result;

    AudioFile<double> file;
    file.loadFromMemory(wav);
    double const seconds = file.getLengthInSeconds();
    int const num_samples = file.getNumSamplesPerChannel();

    if(file.getNumChannels() != 1)
        throw std::runtime_error("Audio must have exactly 1 channel.");

    if(quality > 15)
        throw std::runtime_error("Invalid DPCM quality.");

    if(num_samples == 0)
        throw std::runtime_error("Audio file has no samples.");

    constexpr unsigned ntsc_rates[] = { 428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106,  84,  72,  54 };
    constexpr unsigned pal_rates[]  = { 398, 354, 316, 298, 276, 236, 210, 198, 176, 148, 132, 118,  98,  78,  66,  50 };

    double const freq = 1789773.0 / double(ntsc_rates[quality]);
    int const num_iters = seconds * freq;
    result.reserve(num_iters);
    double const scale = double(num_samples) / double(num_iters);

    std::uint8_t byte = 0;
    std::uint8_t bit = 0;
    double level = 0.0;

    for(int i = 0; i < num_iters; ++i)
    {
        int const s = std::clamp<int>(int(double(i) * scale), 0, num_samples-1);

        if(level < file.samples[0][s])
        {
            level += 2.0 / 128.0;
            byte |= 1 << bit;
        }
        else
            level -= 2.0 / 128.0;

        bit += 1;

        if(bit == 8)
        {
            result.push_back(byte);
            byte = 0;
            bit = 0;
        }
    }

    if(bit != 0)
    {
        while(bit < 8)
        {
            if(bit & 1)
                byte |= 1 << bit;
            bit += 1;
        }
    }

    result.push_back(byte);

    while(result.size() % 16 != 1)
        result.push_back(0xAA);

    unsigned const len = (result.size() - 1) / 16;
    if(len > 0xFF)
        throw std::runtime_error(fmt("DPCM sample too long. Length is %.", len));

    return result;
}
