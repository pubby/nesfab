// This project is licensed under the Boost Software License.
// See license.txt for details.

#include <cstdlib>
#include <chrono>
#include <iostream>

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
#include "eternal_new.hpp" // TODO: remove?
#include "ir.hpp" // TODO: remove?

#include "eval.hpp" // TODO: remove?

#include "assert.hpp" // TODO: remove?

extern char __GIT_COMMIT;

namespace po = boost::program_options;

int main(int argc, char** argv)
{
#ifdef NDEBUG
    try
#endif
    {
        /////////////////////////////
        // Handle program options: //
        /////////////////////////////
        {
            po::options_description desc("Allowed options");
            desc.add_options()
                ("help,h", "produce help message")
                ("version,v", "version")
                ("input-file,i", po::value<std::vector<std::string>>(), "input file")
                ("graphviz,g", "output graphviz files")
                ("threads,j", po::value<int>(), "number of compiler threads")
                ("timelimit,T", po::value<int>(), "interpreter execution time limit (in ms, 0 is off)")
                ("build-time,B", "print compiler execution time")
                ("error-on-warning,W", "turn warnings into errors")
                ("output-file,o", po::value<std::string>(), "output file")
                ("print-cpp-sizes", "print size of C++ objects")
            ;

            po::positional_options_description p;
            p.add("input-file", -1);

            po::variables_map vm;        
            po::store(po::command_line_parser(argc, argv) .options(desc).positional(p).run(), vm);
            po::notify(vm);

            if(vm.count("help")) 
            {
                std::cout << desc << '\n';
                return EXIT_SUCCESS;
            }

            if(vm.count("version")) 
            {
                std::cout << "NesFab " << VERSION << " (" << GIT_COMMIT << ", " << __DATE__ << ")\n";
                std::cout << 
                    "Copyright (C) 2022, Pubby\n"
                    "This is free software. "
                    "There is no warranty.\n";
                return EXIT_SUCCESS;
            }

            if(vm.count("input-file"))
                source_file_names = vm["input-file"].as<std::vector<std::string>>();

            if(vm.count("output-file"))
                _options.output_file = vm["output-file"].as<std::string>();

            if(vm.count("graphviz"))
                _options.graphviz = true;

            if(vm.count("build-time"))
                _options.build_time = true;

            if(vm.count("error-on-warning"))
                _options.werror = true;

            if(vm.count("threads"))
                _options.num_threads = std::clamp(vm["threads"].as<int>(), 1, 1024); // Clamp to some sufficiently high value

            if(vm.count("timelimit"))
                _options.time_limit = std::max(vm["timelimit"].as<int>(), 0);

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

            if(source_file_names.empty())
                throw std::runtime_error("No input files.");
        }

        ////////////////////////////////////
        // OK! Now to do the actual work: //
        ////////////////////////////////////

        auto time = std::chrono::system_clock::now();

        auto const output_time = [&time](char const* desc)
        {
            auto now = std::chrono::system_clock::now();
            unsigned long long const ms = std::chrono::duration_cast<std::chrono::milliseconds>(now - time).count();
            if(compiler_options().build_time)
                std::printf("time %s %lli ms\n", desc, ms);
            time = std::chrono::system_clock::now();
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
                if(file_i >= source_file_names.size())
                    return;

                file_contents_t file(file_i);
                parse<pass1_t>(file);
            }
        });

        // Fix various things after parsing:
        set_compiler_phase(PHASE_PARSE_CLEANUP);
        get_main_entry(); // This throws an error if 'main' isn't proper.
        global_t::parse_cleanup();
        output_time("parse:  ");

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

        set_compiler_phase(PHASE_ORDER_PRECHECK);
        global_t::build_order(true);
        output_time("order1:   ");

        set_compiler_phase(PHASE_PRECHECK);
        global_t::precheck_all();
        output_time("precheck: ");

        // Create an ordering of all the globals:
        set_compiler_phase(PHASE_ORDER_COMPILE);
        global_t::build_order(false);
        output_time("order2:   ");

        // Compile each global:
        set_compiler_phase(PHASE_COMPILE);
        global_t::compile_all();
        output_time("compile:  ");

        set_compiler_phase(PHASE_ALLOC_RAM);
        alloc_ram(nullptr, ~static_used_ram);
        // TODO: remove
        //for(fn_t const& fn : impl_deque<fn_t>)
            //fn.proc().write_assembly(std::cout, fn.handle());
        print_ram(std::cout);
        output_time("alloc ram:");

        set_compiler_phase(PHASE_INITIAL_VALUES);
        gen_group_var_inits();
        output_time("init vals:");

        set_compiler_phase(PHASE_PREPARE_ALLOC_ROM);
        prune_rom_data();
        alloc_rom(nullptr, rom_allocator, mapper().num_32k_banks);
        print_rom(std::cout);
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

        //std::cout << "init at " << static_span(RTROM_reset).addr << std::endl;

        //for(unsigned i = 0; i < 1; ++i)
        //{
            //globals.debug_print();
            //globals.finish();
        //}

    }
#ifdef NDEBUG // In debug mode, we get better stack traces without catching.
    catch(std::exception& e)
    {
        std::fprintf(stderr, "%s\n", e.what());
        return EXIT_FAILURE;
    }
#endif

    return EXIT_SUCCESS;
}

