#ifndef GUARD_HPP
#define GUARD_HPP

#include <functional>
#include <utility>

// Always calls the stored function on scope exit.
template<typename OnThrow = std::function<void()>>
class scope_guard_t
{
public:
    scope_guard_t() = default;

    template<typename F>
    scope_guard_t(F&& on_exit)
    : on_exit(std::forward<F>(on_exit))
    {}

    scope_guard_t(scope_guard_t const&) = delete;

    scope_guard_t(scope_guard_t&& o)
    : on_exit(std::move(o.on_exit))
    {}

    scope_guard_t& operator=(scope_guard_t const&) = delete;

    scope_guard_t& operator=(scope_guard_t&& o)
    {
        on_exit = std::move(o.on_exit);
    }

    ~scope_guard_t()
    {
        on_exit();
    }
    
public:
    OnThrow on_exit;
};

template<typename T>
scope_guard_t<std::remove_reference_t<T>> make_scope_guard(T&& t)
{
    return std::forward<T>(t);
}

// A scope_guard-like class which wraps a function and calls it when
// destroyed due to exception stack unwinding.
template<typename OnThrow = std::function<void()>>
class throw_guard_t
{
public:
    throw_guard_t() : m_uncaught(std::uncaught_exceptions()) {}

    template<typename F>
    throw_guard_t(F&& on_throw)
    : m_uncaught(std::uncaught_exceptions())
    , on_throw(std::forward<F>(on_throw))
    {}

    throw_guard_t(throw_guard_t const&) = delete;

    throw_guard_t(throw_guard_t&& o)
    : m_uncaught(std::uncaught_exceptions())
    , on_throw(std::move(o.on_throw))
    {}

    throw_guard_t& operator=(throw_guard_t const&) = delete;

    throw_guard_t& operator=(throw_guard_t&& o)
    {
        on_throw = std::move(o.on_throw);
    }

    ~throw_guard_t()
    {
        if(std::uncaught_exceptions() > m_uncaught)
            on_throw();
    }
    
private:
    int m_uncaught;
public:
    OnThrow on_throw;
};

template<typename T>
throw_guard_t<std::remove_reference_t<T>> make_throw_guard(T&& t)
{
    return std::forward<T>(t);
}

#endif
