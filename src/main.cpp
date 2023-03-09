// This project is licensed under the Boost Software License.
// See license.txt for details.

#include <cstdlib>
#include <chrono>
#include <iostream>
#include <fstream>
#include <filesystem>

#include <boost/program_options.hpp>

#include "file.hpp"
#include "options.hpp"
#include "parser.hpp"
#include "pass1.hpp"
#include "thread.hpp"
#include "ram_alloc.hpp"
#include "rom_alloc.hpp"
#include "rom_prune.hpp"
#include "runtime.hpp"
#include "rom_link.hpp"
#include "ram_init.hpp"
#include "cg_isel.hpp"
#include "text.hpp"
#include "compiler_error.hpp"

extern char __GIT_COMMIT;

namespace po = boost::program_options;
namespace fs = std::filesystem;

void handle_options(fs::path dir, po::options_description const& cfg_desc, po::variables_map const& vm, int depth = 0)
{
    if(depth > 16)
        throw std::runtime_error("Configuration files nested too deeply.");

    if(vm.count("input"))
    {
        for(std::string const& name : vm["input"].as<std::vector<std::string>>())
        {
            fs::path const path = dir / fs::path(name);
            std::string const ext = fs::path(path).extension().string();

            if(ext == ".cfg")
            {
                std::ifstream ifs(path.string(), std::ios::in);
                if(ifs)
                {
                    fs::path cfg_dir = path;
                    cfg_dir.remove_filename();

                    po::variables_map cfg_vm;
                    po::store(po::parse_config_file(ifs, cfg_desc), cfg_vm);
                    po::notify(cfg_vm);

                    handle_options(cfg_dir, cfg_desc, cfg_vm, depth + 1);
                }
                else
                    throw std::runtime_error(fmt("Unable to open configuration file: %", name.c_str()));
            }
            else if(ext == ".fab")
                _options.source_names.push_back(path);
            else
                throw std::runtime_error(fmt("Unknown file type: %", name.c_str()));
        }
    }

    if(vm.count("code-dir"))
        for(std::string const& str : vm["code-dir"].as<std::vector<std::string>>())
            _options.code_dirs.push_back(dir / fs::path(str));

    if(vm.count("resource-dir"))
        for(std::string const& str : vm["resource-dir"].as<std::vector<std::string>>())
            _options.resource_dirs.push_back(dir / fs::path(str));

    if(vm.count("output"))
        _options.output_file = vm["output"].as<std::string>();

    if(vm.count("graphviz"))
        _options.graphviz = true;

    if(vm.count("build-time"))
        _options.build_time = true;

    if(vm.count("error-on-warning"))
        _options.werror = true;

    if(vm.count("threads"))
    {
        _options.num_threads = std::clamp(vm["threads"].as<int>(), 1, 1024); // Clamp to some sufficiently high value
#ifdef NO_THREAD
        if(_options.num_threads > 1)
            compiler_warning("Multiple threads are not supported on this version and platform.");
#endif
    }

    if(vm.count("timelimit"))
        _options.time_limit = std::max(vm["timelimit"].as<int>(), 0);

    if(vm.count("mapper"))
        _options.raw_mn = vm["mapper"].as<std::string>();

    if(vm.count("mirroring"))
        _options.raw_mm = vm["mirroring"].as<std::string>();

    if(vm.count("prg-size"))
        _options.raw_mp = vm["prg-size"].as<unsigned>();

    if(vm.count("chr-size"))
        _options.raw_mc = vm["chr-size"].as<unsigned>();

    if(vm.count("system"))
        _options.raw_system = vm["system"].as<std::string>();

    if(vm.count("info") || vm.count("ir-info"))
        _options.ir_info = true;

    if(vm.count("info") || vm.count("ram-info"))
        _options.ram_info = true;

    if(vm.count("info") || vm.count("rom-info"))
        _options.rom_info = true;

    if(vm.count("pause"))
        _options.pause = true;
}

int main(int argc, char** argv)
{
    auto entry_time = std::chrono::system_clock::now();

#ifdef NDEBUG
    try
#endif
    {
        /////////////////////////////
        // Handle program options: //
        /////////////////////////////
        {
            po::options_description cmdline("Instructional Flags");
            cmdline.add_options()
                ("help,h", "produce help message")
                ("version,v", "version")
            ;

            po::options_description cmdline_hidden("Hidden command line options");
            cmdline_hidden.add_options()
                ("print-cpp-sizes", "print size of C++ objects")
            ;

            po::options_description basic("Options");
            basic.add_options()
                ("code-dir,I", po::value<std::vector<std::string>>(), "search directory for code files")
                ("resource-dir,R", po::value<std::vector<std::string>>(), "search directory for resource files")
                ("output,o", po::value<std::string>(), "output file")
                ("threads,j", po::value<int>(), "number of compiler threads")
                ("error-on-warning,W", "turn warnings into errors")
                ("pause", "await input on stdin before exiting")
            ;

            po::options_description mapper_opt("Mapper options");
            mapper_opt.add_options()
                ("mapper,M", po::value<std::string>(), "name of cartridge mapper")
                ("mirroring,m", po::value<std::string>(), "mirroring of mapper (V, H, 4)")
                ("prg-size,p", po::value<unsigned>(), "size of mapper PRG in KiB")
                ("chr-size,c", po::value<unsigned>(), "size of mapper CHR in KiB")
                ("system,S", po::value<std::string>(), "target NES system")
            ;

            po::options_description basic_hidden("Hidden options");
            basic_hidden.add_options()
                ("input,i", po::value<std::vector<std::string>>()->multitoken(), "input file")
                ("graphviz,g", "output graphviz files")
                ("info", "output every info")
                ("ir-info", "output intermediate info")
                ("ram-info", "output RAM info")
                ("rom-info", "output ROM info")
                ("time-limit,T", po::value<int>(), "interpreter execution time limit (in ms, 0 is off)")
                ("build-time,B", "print compiler execution time")
            ;

            po::options_description cmdline_full;
            cmdline_full.add(cmdline).add(cmdline_hidden).add(basic).add(basic_hidden).add(mapper_opt);

            po::options_description config_full;
            config_full.add(basic).add(basic_hidden).add(mapper_opt);

            po::positional_options_description p;
            p.add("input", -1);

            po::variables_map vm;        
            po::store(po::command_line_parser(argc, argv).options(cmdline_full).positional(p).run(), vm);
            po::notify(vm);

            if(vm.count("help")) 
            {
                po::options_description visible;
                visible.add(cmdline).add(basic).add(mapper_opt);
                std::cout << visible << std::endl;
                return EXIT_SUCCESS;
            }

            if(vm.count("version")) 
            {
                std::cout << "NesFab " << VERSION << " (" << GIT_COMMIT << ", " << __DATE__ << ")\n";
                std::cout << 
                    "Copyright (C) 2022, Patrick Bene\n"
                    "This is free software. "
                    "There is no warranty.\n";
                return EXIT_SUCCESS;
            }

            if(vm.count("print-cpp-sizes"))
            {
#define PRINT_SIZE(x) std::printf(#x ": %u\n", unsigned(sizeof(x)));
                PRINT_SIZE(cfg_buffer_t);
                PRINT_SIZE(ssa_buffer_t);
                PRINT_SIZE(cfg_node_t);
                PRINT_SIZE(ssa_node_t);
                PRINT_SIZE(global_t);
                PRINT_SIZE(fn_t);
                PRINT_SIZE(const_t);
                PRINT_SIZE(type_t);
                PRINT_SIZE(mods_t);
                PRINT_SIZE(token_t);
                PRINT_SIZE(ast_node_t);
                PRINT_SIZE(asm_inst_t);
                PRINT_SIZE(isel::cpu_t);
                PRINT_SIZE(isel::sel_t);
#undef PRINT_SIZE
                return EXIT_SUCCESS;
            }

            handle_options(fs::path(), config_full, vm);

            if(compiler_options().source_names.empty())
                throw std::runtime_error("No input files.");

            using namespace std::literals;

            // Handle mapper:

            auto const get_mirroring = [&]() -> mapper_mirroring_t
            {
                if(_options.raw_mm.empty())
                    return MIRROR_NONE;
                if(_options.raw_mm == "H"sv)
                    return MIRROR_H;
                if(_options.raw_mm == "V"sv)
                    return MIRROR_V;
                if(_options.raw_mm == "4"sv)
                    return MIRROR_4;
                throw std::runtime_error(fmt("Invalid mapper mirroring: \"%\"", _options.raw_mm));
            };

            mapper_params_t const mapper_params = 
            {
                .mirroring = get_mirroring(),
                .prg_size = _options.raw_mp,
                .chr_size = _options.raw_mc,
            };

            auto const to_lower = [](std::string str)
            {
                std::transform(str.begin(), str.end(), str.begin(),
                    [](unsigned char c){ return std::tolower(c); });
                return str;
            };

            {
                std::string const str = to_lower(compiler_options().raw_mn);

                if(str.empty() || str == "nrom"sv)
                    _options.mapper = mapper_t::nrom(mapper_params);
                else if(str == "cnrom"sv)
                    _options.mapper = mapper_t::cnrom(mapper_params);
                else if(str == "anrom"sv)
                    _options.mapper = mapper_t::anrom(mapper_params);
                else if(str == "bnrom"sv)
                    _options.mapper = mapper_t::bnrom(mapper_params);
                else if(str == "gnrom"sv)
                    _options.mapper = mapper_t::gnrom(mapper_params);
                else if(str == "gtrom"sv)
                    _options.mapper = mapper_t::gtrom(mapper_params);
                else
                    throw std::runtime_error(fmt("Invalid mapper: '%'", compiler_options().raw_mn));
            }

            {
                std::string const str = to_lower(compiler_options().raw_system);

                if(str.empty() || str == "detect"sv)
                    _options.nes_system = NES_SYSTEM_DETECT;
                else if(str == "ntsc"sv)
                    _options.nes_system = NES_SYSTEM_NTSC;
                else if(str == "pal"sv)
                    _options.nes_system = NES_SYSTEM_PAL;
                else if(str == "dendy"sv)
                    _options.nes_system = NES_SYSTEM_DENDY;
                else if(str == "unknown"sv)
                    _options.nes_system = NES_SYSTEM_UNKNOWN;
                else
                    throw std::runtime_error(fmt("Invalid system: '%'", compiler_options().raw_system));
            }
        }

        ////////////////////////////////////
        // OK! Now to do the actual work: //
        ////////////////////////////////////

        auto time = std::chrono::system_clock::now();

        auto const output_time = [&time](char const* desc)
        {
            if(compiler_options().build_time)
            {
                auto now = std::chrono::system_clock::now();
                unsigned long long const ms = std::chrono::duration_cast<std::chrono::milliseconds>(now - time).count();
                std::printf("time %s %8lli ms\n", desc, ms);
                time = std::chrono::system_clock::now();
            }
        };

        global_t::init();
        output_time("init:     ");

        // Parse the files, loading everything into globals:
        set_compiler_phase(PHASE_PARSE);
        std::atomic<unsigned> next_file_i = 0;
        parallelize(compiler_options().num_threads,
        [&next_file_i](std::atomic<bool>& exception_thrown)
        {
            while(!exception_thrown)
            {
                unsigned const file_i = next_file_i++;
                if(file_i >= compiler_options().source_names.size())
                    return;

                file_contents_t file(file_i);
                parse<pass1_t>(file);
            }
        }, []{});

        // Fix various things after parsing:
        set_compiler_phase(PHASE_PARSE_CLEANUP);
        get_main_mode(); // This throws an error if 'main' isn't proper.

        global_t::parse_cleanup();
        output_time("parse:    ");

        // Count and arrange struct members:
        set_compiler_phase(PHASE_COUNT_MEMBERS);
        global_t::count_members();

        set_compiler_phase(PHASE_GROUP_MEMBERS);
        group_t::group_members();
        output_time("members:  ");

        // Load standard data:
        set_compiler_phase(PHASE_RUNTIME);
        auto static_used_ram = alloc_runtime_ram();
        auto rom_allocator = alloc_runtime_rom();
        output_time("runtime:  ");

        set_compiler_phase(PHASE_CHARMAP_GROUPS);
        charmap_t::set_all_group_data();
        set_compiler_phase(PHASE_CONVERT_STRINGS);
        sl_manager.convert_all();
        set_compiler_phase(PHASE_COMPRESS_STRINGS);
        sl_manager.compress_all();
        output_time("strings:  ");

        set_compiler_phase(PHASE_ORDER_RESOLVE);
        global_t::build_order();
        output_time("order1:   ");

        set_compiler_phase(PHASE_RESOLVE);
        global_t::resolve_all();
        output_time("resolve:  ");

        set_compiler_phase(PHASE_ORDER_PRECHECK);
        global_t::build_order();
        output_time("order2:   ");

        set_compiler_phase(PHASE_PRECHECK);
        global_t::precheck_all();
        output_time("precheck: ");

        set_compiler_phase(PHASE_ORDER_COMPILE);
        global_t::build_order();
        output_time("order3:   ");

        // Compile each global:
        set_compiler_phase(PHASE_COMPILE);
        global_t::compile_all();
        output_time("compile:  ");

        set_compiler_phase(PHASE_ALLOC_RAM);
        alloc_ram(nullptr, ~static_used_ram);

        if(compiler_options().ram_info)
        {
            std::filesystem::create_directory("info/");

            std::ofstream of(fmt("info/RAM_info.txt"));
            if(of.is_open())
                print_ram(of);
        }
        output_time("alloc ram:");

        set_compiler_phase(PHASE_RESET_PROC);
        create_reset_proc();
        set_reset_proc();
        output_time("reset:    ");

        set_compiler_phase(PHASE_ASM_GOTO_MODES);
        fn_t::implement_asm_goto_modes();
        output_time("goto mode:");

        set_compiler_phase(PHASE_INITIAL_VALUES);
        gen_group_var_inits();
        output_time("init vals:");

        set_compiler_phase(PHASE_PREPARE_ALLOC_ROM);
        prune_rom_data();
        alloc_rom(nullptr, rom_allocator, mapper().num_32k_banks);
        if(compiler_options().ram_info)
        {
            std::filesystem::create_directory("info/");

            std::ofstream of(fmt("info/ROM_info.txt"));
            if(of.is_open())
                print_rom(of);
        }
        output_time("alloc rom:");

        set_compiler_phase(PHASE_LINK);
        auto rom = write_rom();
        FILE* of = std::fopen(compiler_options().output_file.c_str(), "wb");
        if(!of)
            throw std::runtime_error(fmt("Unable to open file %", compiler_options().output_file));
        if(!std::fwrite(rom.data(), rom.size(), 1, of))
        {
            std::fclose(of);
            throw std::runtime_error(fmt("Unable to write to file %", compiler_options().output_file));
        }
        std::fclose(of);
        output_time("link:     ");

        for(fn_t const& fn : fn_ht::values())
        {
            std::filesystem::create_directory("info/");

            if(std::stringstream const* ss = fn.info_stream())
            {
                std::ofstream of(fmt("info/%.txt", fn.global.name));
                if(of.is_open())
                    of << ss->str() << std::endl;
            }
        }
    }
#ifdef NDEBUG // In debug mode, we get better stack traces without catching.
    catch(std::exception& e)
    {
        std::fprintf(stderr, "%s\n", e.what());

        if(compiler_options().pause)
            std::fgetc(stdin);

        return EXIT_FAILURE;
    }
#endif

    if(compiler_options().build_time)
    {
        auto now = std::chrono::system_clock::now();
        unsigned long long const ms = std::chrono::duration_cast<std::chrono::milliseconds>(now - entry_time).count();
        std::printf("time total:     %8lli ms\n", ms);
    }

    if(compiler_options().pause)
        std::fgetc(stdin);

    return EXIT_SUCCESS;
}

