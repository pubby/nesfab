#ifndef OPTIONS_HPP
#define OPTIONS_HPP

// Compiler options.

#include <vector>
#include <filesystem>

#include "mapper.hpp"
#include "nes_system.hpp"

namespace fs = ::std::filesystem;

struct options_t
{
    int num_threads = 1;
    int time_limit = 1000;
    bool graphviz = false;
    bool build_time = false;
    bool werror = false;

    nes_system_t nes_system = NES_SYSTEM_UNKNOWN;
    std::string raw_system;

    // Raw mapper settings.
    // (These will get converted to a 'mapper_t' eventually)
    std::string raw_mn;
    std::string raw_mm;
    unsigned raw_mc = 0;
    unsigned raw_mp = 0;

    mapper_t mapper;
    std::string output_file = "a.nes";

    std::vector<fs::path> source_names;
    std::vector<fs::path> code_dirs = { fs::current_path() };
    std::vector<fs::path> resource_dirs;
};

extern options_t _options;
inline options_t const& compiler_options() { return _options; }
inline mapper_t const& mapper() { return _options.mapper; }

#endif
