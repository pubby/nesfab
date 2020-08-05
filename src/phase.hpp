#ifndef PHASES_HPP
#define PHASES_HPP

// Tracks which phase the compiler is on, to mostly be used for debugging.

#include <atomic>

enum compiler_phase_t
{
    PHASE_INIT,
    PHASE_LOAD_FILES,
    PHASE_PARSE,
    PHASE_ORDER_GLOBALS,
    PHASE_COMPILE,
};

#ifndef NDEBUG
inline std::atomic<compiler_phase_t> _compiler_phase = PHASE_INIT;
inline compiler_phase_t compiler_phase() { return _compiler_phase; }
#endif

inline void set_compiler_phase(compiler_phase_t p) 
{ 
#ifndef NDEBUG
    assert(p > _compiler_phase);
    _compiler_phase = p; 
#endif
}

#endif
