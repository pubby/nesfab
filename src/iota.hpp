#ifndef IOTA_HPP
#define IOTA_HPP

#include <string>
#include <mutex>

#include "robin/map.hpp"

struct iota_map_t
{
    rh::joker_map<std::string, unsigned> map;
    std::mutex mutex;
};

extern iota_map_t iota_map;

#endif
