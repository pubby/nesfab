#ifndef PHASES_HPP
#define PHASES_HPP

// Tracks which phase the compiler is on, to mostly be used for debugging.

#include <atomic>
#include <mutex>
#include <list>

enum compiler_phase_t
{
    PHASE_NONE = 0,
    PHASE_INIT,
    PHASE_PARSE, // threaded
    PHASE_PARSE_CLEANUP,
    PHASE_COUNT_MEMBERS,
    PHASE_GROUP_MEMBERS,
    PHASE_FINISH_MEMBERS,
    PHASE_RUNTIME, 
    PHASE_ORDER_PRECHECK,
    PHASE_PRECHECK,
    PHASE_ORDER_COMPILE,
    PHASE_COMPILE, // threaded
    PHASE_ALLOC_RAM,
    PHASE_INITIAL_VALUES,
    PHASE_PREPARE_ALLOC_ROM,
    PHASE_ALLOC_ROM,
    PHASE_LINK,
};

inline std::atomic<compiler_phase_t> _compiler_phase = PHASE_INIT;
inline compiler_phase_t compiler_phase() { return _compiler_phase; }

// This class listens for phase changes, letting you run code when it happens.
// Derive from it!
class on_phase_change_t
{
friend void set_compiler_phase(compiler_phase_t to);
public:
    on_phase_change_t() 
    { 
        std::lock_guard<std::mutex> lock(mutex);
        iter = list.insert(list.end(), this); 
    }

    virtual ~on_phase_change_t()
    {
        list.erase(iter);
    }

    virtual void on_change(compiler_phase_t from, compiler_phase_t to) = 0;

private:
    static void change(compiler_phase_t from, compiler_phase_t to)
    {
        std::lock_guard<std::mutex> lock(mutex);
        for(on_phase_change_t* ptr : list)
            ptr->on_change(from, to);
    }

    inline static std::mutex mutex;
    inline static std::list<on_phase_change_t*> list;

    std::list<on_phase_change_t*>::iterator iter;
};

inline void set_compiler_phase(compiler_phase_t to) 
{ 
    assert(to > _compiler_phase);
    compiler_phase_t from = _compiler_phase;
    _compiler_phase = to; 
    on_phase_change_t::change(from, to);
}

#endif
