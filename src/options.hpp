#ifndef OPTIONS_HPP
#define OPTIONS_HPP

// Compiler options.

#include <vector>
#include <filesystem>

#include "mapper.hpp"

namespace fs = ::std::filesystem;

struct options_t
{
    int num_threads = 1;
    int time_limit = 1000;
    bool graphviz = false;
    bool build_time = false;
    bool werror = false;
    mapper_t mapper = mapper_t::bnrom(MIRROR_H, 1);
    std::string output_file = "a.nes";

    std::vector<fs::path> source_names;
    std::vector<fs::path> code_dirs = { fs::current_path() };
    std::vector<fs::path> resource_dirs;
};

extern options_t _options;
inline options_t const& compiler_options() { return _options; }
inline mapper_t const& mapper() { return _options.mapper; }

#endif
