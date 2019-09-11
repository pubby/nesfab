// This project is licensed under the Boost Software License.
// See license.txt for details.

#include <iostream>

#include "file.hpp"
#include "parser.hpp"
#include "pass1.hpp"

int main()
{
    assert(false);
    /*
    ssa_t ssa;
    ssa.const_ref(1);
    ssa.debug_print();
    ssa.const_ref(2);
    ssa.debug_print();
    ssa.const_ref(3);
    ssa.append_ssa_n(SSA_add, 2);
    ssa.debug_print();
    */

    try
    {
        unsigned file_i = open_file("file.robust");

        for(unsigned i = 0; i < 1000; ++i)
        {
            global_manager_t globals;
            pass1_t pass1(globals);
            parser_t<pass1_t> parser(pass1, file_i);
            parser.parse();
            //globals.debug_print();
            globals.finish();
        }
        //globals.gv_deps(std::cout);
        /*
        pass1_t pass1(file, data);
        {
            parser_t parser(file, data, pass1);
            parser.parse();
            data.debug_print();
        }
        pass2_t pass2(file, data);
        {
            parser_t parser(file, data, pass2);
            parser.parse();
        }
        pass2.debug_print();
        */
    }
    catch(std::exception& e)
    {
        std::fprintf(stderr, "%s", e.what());
    }
}


