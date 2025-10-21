#ifndef CONVERT_WAV_HPP
#define CONVERT_WAV_HPP

#include <cstdint>
#include <vector>

#include "convert.hpp"

std::vector<std::uint8_t> wav_to_dpcm(std::vector<std::uint8_t> const& wav, unsigned quality);

#endif
