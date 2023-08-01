#ifndef THREAD_HPP
#define THREAD_HPP

#if defined(__MINGW32__) || defined(__MINGW64__)
#define NO_THREAD
#endif

#include <exception>
#include <vector>
#include <atomic>
#ifndef NO_THREAD
  #include <thread>
#endif

// MinGW has a buggy thread_local implementation.
#ifdef NO_THREAD
#define TLS
#else
#define TLS thread_local
#endif

// Launches a bunch of threads and waits until they finish.
template<typename Fn, typename OnError>
void parallelize(unsigned const num_threads, Fn const& fn, OnError const& on_error)
{
    std::atomic<bool> exception_thrown = false;

#ifdef NO_THREAD
    fn(exception_thrown);
    return;
#else
    if(num_threads == 1)
    {
        fn(exception_thrown);
        return;
    }

    std::vector<std::thread> threads;
    threads.reserve(num_threads);

    std::vector<std::exception_ptr> exception_ptrs;
    exception_ptrs.resize(num_threads, nullptr);

    for(unsigned i = 0; i < num_threads; ++i)
    {
        threads.emplace_back(
        [&fn, &exception_thrown, &on_error](std::exception_ptr& exception_ptr)
        {
            try
            {
                fn(exception_thrown);
            }
            catch(...)
            {
                exception_ptr = std::current_exception();
                exception_thrown = true;
                on_error();
            }
        }, std::ref(exception_ptrs[i]));
    }

    for(unsigned i = 0; i < threads.size(); ++i)
        threads[i].join();

    threads.clear();

    for(unsigned i = 0; i < num_threads; ++i)
        if(exception_ptrs[i])
            std::rethrow_exception(exception_ptrs[i]);
#endif
}

#endif
