#ifndef THREAD_HPP
#define THREAD_HPP

#include <atomic>
#include <exception>
#include <thread>
#include <vector>

// Launches a bunch of threads and waits until they finish.
template<typename Fn>
void parallelize(unsigned const num_threads, Fn fn)
{
    std::vector<std::thread> threads;
    threads.reserve(num_threads);

    std::vector<std::exception_ptr> exception_ptrs;
    exception_ptrs.resize(num_threads, nullptr);

    std::atomic<bool> exception_thrown = false;

    if(num_threads == 1)
    {
        fn(exception_thrown);
        return;
    }

    for(unsigned i = 0; i < num_threads; ++i)
    {
        threads.emplace_back(
        [fn, &exception_thrown](std::exception_ptr& exception_ptr)
        {
            try
            {
                fn(exception_thrown);
            }
            catch(...)
            {
                exception_ptr = std::current_exception();
                exception_thrown = true;
            }
        }, std::ref(exception_ptrs[i]));
    }

    for(unsigned i = 0; i < num_threads; ++i)
    {
        threads[i].join();
        if(exception_ptrs[i])
            std::rethrow_exception(exception_ptrs[i]);
    }
}

#endif
