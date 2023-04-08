#ifndef DEBUG_PRINT_HPP
#define DEBUG_PRINT_HPP

// Logging, used for debugging purposes.

#include <cstdio>
#include <mutex>

#include "format.hpp"

struct log_t
{
    FILE* stream;
    std::mutex mutex;

    void write(std::string const& msg)
    {
        std::lock_guard<std::mutex> lock(mutex);
        std::fputs(msg.c_str(), stream);
        std::fputc('\n', stream);
        std::fflush(stream);
    }
};

inline log_t stdout_log = { stdout };
inline log_t stderr_log = { stderr };

#define DEBUG_PRINT

#ifdef DEBUG_PRINT
#define debug_printf(...) std::printf(__VA_ARGS__)
#define dprint(stream, ...) ((void)((stream) ? ((stream)->write(::ezcat(" ",__VA_ARGS__)), 0) : 0))
#else
#define debug_printf(...) ((void)0)
#define dprint(...) ((void)0)
#endif

#endif
