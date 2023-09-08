#ifndef OPTIONS_HPP
#define OPTIONS_HPP

// Compiler options.

#include <vector>
#include <string>
#include <filesystem>

#include "robin/map.hpp"

#include "mapper.hpp"
#include "nes_system.hpp"

namespace fs = ::std::filesystem;

struct source_t
{
    fs::path file;
    fs::path dir;
};

struct options_t
{
    int num_threads = 1;
    int time_limit = 1000;
    bool graphviz = false;
    bool ir_info = false;
    bool ram_info = false;
    bool rom_info = false;
    bool build_time = false;
    bool werror = false;
    bool pause = false;
    bool unsafe_bank_switch = false;
    bool assert_valid = true;
    bool sloppy = false;
    bool action53 = false;

    // Label files, etc:
    std::string raw_mlb;
    std::string raw_ctags;

    nes_system_t nes_system = NES_SYSTEM_UNKNOWN;
    std::string raw_system;

    // Raw mapper settings.
    // (These will get converted to a 'mapper_t' eventually)
    std::string raw_mn;
    std::string raw_mm;
    unsigned raw_mc = 0;
    unsigned raw_mp = 0;
    mapper_bus_conflicts_t raw_bus_conflicts = {};
    mapper_sram_t raw_sram = {};

    mapper_t mapper;
    std::string output_file = "a.nes";

    unsigned num_fab = 0;
    std::vector<source_t> source_names;
    rh::batman_map<std::string, source_t> macro_names;
    std::vector<fs::path> code_dirs;
    std::vector<fs::path> resource_dirs;
    std::vector<fs::path> nesfab_dirs;
};

extern options_t _options;
inline options_t const& compiler_options() { return _options; }
inline mapper_t const& mapper() { return _options.mapper; }

#endif
