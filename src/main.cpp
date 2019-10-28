// This project is licensed under the Boost Software License.
// See license.txt for details.

#include <iostream>

#include "file.hpp"
#include "parser.hpp"
#include "pass1.hpp"

template<std::size_t StorageSize, typename T, typename SizeT> 
[[gnu::always_inline]]
void sso_resize(SizeT& size, T*& ptr, T* storage, SizeT new_size)
{
    if(size > StorageSize)
    {
        if(new_size > StorageSize)
        {
            if(builtin::clz(size) != builtin::clz(new_size))
            {
                T* old_ptr = ptr;
                ptr = new T[(SizeT)1 << builtin::rclz(size)];
                std::copy(old_ptr, old_ptr + size, ptr);
                delete[] old_ptr;
            }
        }
        else
        {
            delete[] ptr;
            ptr = storage;
        }
    }
    else if(new_size > StorageSize)
    {
        ptr = new T[(SizeT)1 << builtin::rclz(size)];
        std::copy(storage, storage + size, ptr);
    }
    else
        ptr = storage;
    size = new_size;
}

template<std::size_t StorageSize, typename T, typename SizeT> 
[[gnu::always_inline]]
void sso_free(SizeT size, T* ptr)
{
    if(size > StorageSize)
        delete[] ptr;
}

struct foo
{
    std::uint64_t data;
};


/*
class pool
{
    T* alloc()
    {

    }

    unsigned free;
    std::vector<> memory;
};
*/

class alignas(2) ssa_node_t2
{
    unsigned next;
    unsigned prev;
    unsigned cfg_node_;
    unsigned type_;

    std::uint32_t op_;
    std::uint32_t flags;

    std::uint32_t input_size;
    std::uint32_t output_size;


    void* input;
    void* output;

    std::uint64_t storage[6];
};

class alignas(2) cfg_node_t2
{
    unsigned next;
    unsigned prev;

    unsigned list[3];

    std::uint32_t flags;

    std::uint32_t input_size;
    std::uint32_t output_size;

    void* input;
    void* output;

    std::uint64_t storage[4];

    union 
    {
        //ai_ssa_t ai_data;
        //phi_ssa_t phi_data;
    };
};

int main()
{
    std::cout << "edge size = " << sizeof(ssa_reverse_edge_t) << '\n';
    std::cout << "type_t size = " << sizeof(type_t) << '\n';
    std::cout << "ssa_op_t size = " << sizeof(ssa_op_t) << '\n';
    std::cout << "ssa_node_t size = " << sizeof(ssa_node_t) << '\n';
    std::cout << "cfg_node_t size = " << sizeof(cfg_node_t) << '\n';
    std::cout << "ssa_node_t2 size = " << sizeof(ssa_node_t2) << '\n';
    std::cout << "cfg_node_t2 size = " << sizeof(cfg_node_t2) << '\n';
    std::cout << "constraints_t size = " << sizeof(constraints_t) << '\n';
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

        for(unsigned i = 0; i < 1; ++i)
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


