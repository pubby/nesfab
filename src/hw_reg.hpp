#ifndef HW_REG_HPP
#define HW_REG_HPP

#include <cstdint>

constexpr std::uint16_t PPUCTRL   = 0x2000;
constexpr std::uint16_t PPUMASK   = 0x2001;
constexpr std::uint16_t PPUSTATUS = 0x2002;
constexpr std::uint16_t OAMADDR   = 0x2003;
constexpr std::uint16_t OAMDATA   = 0x2004;
constexpr std::uint16_t PPUSCROLL = 0x2005;
constexpr std::uint16_t PPUADDR   = 0x2006;
constexpr std::uint16_t PPUDATA   = 0x2007;
constexpr std::uint16_t OAMDMA    = 0x4014;
constexpr std::uint16_t SNDCHN    = 0x4015;

#endif
