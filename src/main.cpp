// This project is licensed under the Boost Software License.
// See license.txt for details.

#include <cstdlib>
#include <iostream>

#include <boost/program_options.hpp>

#include "file.hpp"
#include "options.hpp"
#include "parser.hpp"
#include "pass1.hpp"
#include "thread.hpp"

extern char __GIT_COMMIT;

namespace po = boost::program_options;

int main(int argc, char** argv)
{
    // TODO: remove
    /*
    std::cout << "type_t size = " << sizeof(type_t) << '\n';
    std::cout << "ssa_op_t size = " << sizeof(ssa_op_t) << '\n';
    std::cout << "ssa_node_t size = " << sizeof(ssa_node_t) << '\n';
    std::cout << "cfg_node_t size = " << sizeof(cfg_node_t) << '\n';
    */

    try
    {
        /////////////////////////////
        // Handle program options: //
        /////////////////////////////
        {
            po::options_description desc("Allowed options");
            desc.add_options()
                ("help,h", "produce help message")
                ("version,v", "version")
                ("input-file,i", po::value<std::vector<std::string>>(), 
                 "input file")
                ("graphviz,g", "output graphviz files")
                ("optimize,O", "optimize code")
                ("threads,j", po::value<int>(), "number of compiler threads")
            ;

            po::positional_options_description p;
            p.add("input-file", -1);

            po::variables_map vm;        
            po::store(po::command_line_parser(argc, argv)
                      .options(desc).positional(p).run(), vm);
            po::notify(vm);

            if(vm.count("help")) 
            {
                std::cout << desc << '\n';
                return EXIT_SUCCESS;
            }

            if(vm.count("version")) 
            {
                std::cout << "MOSBOL version " << VERSION << '\n';
                std::cout << "commit " << GIT_COMMIT << '\n';
                std::cout << 
                    "Copyright (C) 2022, Pubby\n"
                    "This is free software. "
                    "There is no warranty.\n";
                return EXIT_SUCCESS;
            }

            if(vm.count("input-file"))
                source_file_names = 
                    vm["input-file"].as<std::vector<std::string>>();

            if(source_file_names.empty())
                throw std::runtime_error("No input files.");

            if(vm.count("optimize"))
                _options.optimize = true;

            if(vm.count("graphviz"))
                _options.graphviz = true;

            if(vm.count("threads"))
                _options.num_threads = 
                    std::clamp(vm["threads"].as<int>(), 1, 64);
        }

        ////////////////////////////////////
        // OK! Now to do the actual work: //
        ////////////////////////////////////

        global_t::init();

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
        global_t::parse_cleanup();

        // Create an ordering of all the globals:
        set_compiler_phase(PHASE_ORDER_GLOBALS);
        global_t::build_order();

        // Compile each global:
        set_compiler_phase(PHASE_COMPILE);
        global_t::compile_all();

        set_compiler_phase(PHASE_ALLOC_RAM);
        global_t::alloc_ram();

        //for(unsigned i = 0; i < 1; ++i)
        //{
            //globals.debug_print();
            //globals.finish();
        //}
    }
    catch(std::exception& e)
    {
        std::fprintf(stderr, "%s\n", e.what());
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}


