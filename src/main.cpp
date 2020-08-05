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
    std::cout << "type_t size = " << sizeof(type_t) << '\n';
    std::cout << "ssa_op_t size = " << sizeof(ssa_op_t) << '\n';
    std::cout << "ssa_node_t size = " << sizeof(ssa_node_t) << '\n';
    std::cout << "cfg_node_t size = " << sizeof(cfg_node_t) << '\n';

    try
    {
        // Handle program options
        {
            po::options_description desc("Allowed options");
            desc.add_options()
                ("help,h", "produce help message")
                ("version,v", "version")
                ("graph,g", "outputs graphviz files")
                ("input-file", po::value<std::vector<std::string>>(), 
                 "input file")
                ("optimize,O", "optimize code")
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
                std::cout << "version: " << VERSION << '\n';
                return EXIT_SUCCESS;
            }

            if (vm.count("input-file"))
            {
                for(auto& str : vm["input-file"].as< std::vector<std::string> >())
                std::cout << "Input files are: " << str << '\n';
            }
        }

        ////////////////////////////////////
        // OK! Now to do the actual work: //
        ////////////////////////////////////

        // Load every source file
        set_compiler_phase(PHASE_LOAD_FILES);
        std::vector<std::string> file_names = { "file.robust" };
        load_files(&*file_names.begin(), &*file_names.end());

        // Parse the files, loading everything into globals:
        set_compiler_phase(PHASE_PARSE);
        std::atomic<unsigned> next_file_i = 0;
        parallelize(compiler_options().num_threads,
        [&next_file_i](std::atomic<bool>& exception_thrown)
        {
            while(!exception_thrown)
            {
                unsigned const file_i = next_file_i++;
                if(file_i >= num_files())
                    return;

                parse<pass1_t>(file_i);
            }
        });

        // Create an ordering of all the globals:
        set_compiler_phase(PHASE_ORDER_GLOBALS);
        global_t::build_order();

        // Compile each global:
        set_compiler_phase(PHASE_COMPILE);
        global_t::compile_all();



        //for(unsigned i = 0; i < 1; ++i)
        //{
            //globals.debug_print();
            //globals.finish();
        //}
    }
    catch(std::exception& e)
    {
        std::fprintf(stderr, "%s", e.what());
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}


