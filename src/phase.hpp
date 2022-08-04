#ifndef PHASES_HPP
#define PHASES_HPP

// Tracks which phase the compiler is on, to mostly be used for debugging.

#include <atomic>

enum compiler_phase_t
{
    PHASE_NONE = 0,
    PHASE_INIT,
    PHASE_PARSE, // threaded
    PHASE_PARSE_CLEANUP,
    PHASE_COUNT_MEMBERS,
    PHASE_GROUP_MEMBERS,
    PHASE_FINISH_MEMBERS,
    PHASE_STD, 
    PHASE_PRECHECK,
    PHASE_ORDER_GLOBALS,
    PHASE_COMPILE, // threaded
    PHASE_ALLOC_RAM,
    PHASE_INITIAL_VALUES,
    PHASE_PREPARE_ALLOC_ROM,
    PHASE_ALLOC_ROM,
    PHASE_LINK,
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
#else
    // Preserve memory order, just in case.
    std::atomic_thread_fence(std::memory_order_seq_cst);
#endif
}

#endif
