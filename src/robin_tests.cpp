#include "catch/catch.hpp"
#include "robin/set.hpp"

#include <cstdlib>
#include <cstdio>
#include <set>

template<template<class...> class Set>
void test_set()
{
    std::set<unsigned> stdset;
    Set<unsigned> rhset;

    REQUIRE(rhset.size() == 0);

    for(int i = 0; i < 10000; ++i)
    {
        unsigned const x = std::rand() % 1024;
        auto stdpair = stdset.insert(x);
        auto rhpair  = rhset.insert(x);

        REQUIRE(stdpair.second == rhpair.second);

        REQUIRE(stdset.size() == rhset.size());

        rhpair = rhset.insert(x);
        REQUIRE(rhpair.second == false);
    }

    for(int i = 0; i < 10000; ++i)
    {
        unsigned const x = std::rand() % 1024;
        REQUIRE(stdset.count(x) == rhset.count(x));

        if(unsigned const* y = rhset.find(x))
            REQUIRE(*y == x);
    }

    for(int i = 0; i < 10000; ++i)
    {
        unsigned const x = std::rand() % 2048;
        std::size_t size = rhset.size();
        if(rhset.remove(x))
            REQUIRE(size == rhset.size() + 1);
    }
}

template<template<class...> class Set>
void test_set_iteration()
{
    std::set<unsigned> stdset;
    Set<unsigned> rhset;
    std::vector<unsigned> order;

    for(int i = 0; i < 10000; ++i)
    {
        unsigned const x = std::rand();
        auto stdpair = stdset.insert(x);
        rhset.insert(x);
        if(stdpair.second)
            order.push_back(x);
    }

    REQUIRE((std::size_t)(rhset.end() - rhset.begin()) == rhset.size());

    for(std::size_t i = 0; i < rhset.size(); ++i)
    {
        REQUIRE(stdset.count(rhset.begin()[i]));
        REQUIRE(rhset.begin()[i] == order[i]);
    }
}


TEST_CASE("robin_set", "[rh]")
{
    test_set<rh::robin_set>();
}

TEST_CASE("batman_set", "[rh]")
{
    test_set<rh::batman_set>();
    test_set_iteration<rh::batman_set>();
}

TEST_CASE("joker_set", "[rh]")
{
    test_set<rh::batman_set>();
    test_set_iteration<rh::batman_set>();
}
