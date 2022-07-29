#ifndef OPTIONS_HPP
#define OPTIONS_HPP

// Compiler options.

#include "mapper.hpp"

struct options_t
{
    int num_threads = 1;
    int time_limit = 1000;
    bool graphviz = false;
    bool build_time = false;
    mapper_t mapper = mapper_t::bnrom(MIRROR_H, 4);
    std::string output_file = "a.nes";
};

extern options_t _options;
inline options_t const& compiler_options() { return _options; }
inline mapper_t const& mapper() { return _options.mapper; }

#endif
