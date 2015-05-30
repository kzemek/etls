/**
 * @file testUtils.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_TEST_UTILS_HPP
#define ONE_ETLS_TEST_UTILS_HPP

#include <algorithm>
#include <atomic>
#include <chrono>
#include <random>
#include <thread>
#include <vector>

namespace {

using namespace std::literals;

std::default_random_engine &engine()
{
    thread_local std::random_device rd;
    thread_local std::default_random_engine engine{rd()};
    return engine;
}

unsigned short randomPort()
{
    static thread_local std::uniform_int_distribution<unsigned short> dist{
        1025, 65535};
    return dist(engine());
}

std::vector<char> randomData()
{
    static thread_local std::uniform_int_distribution<std::size_t> lenDist{
        1, 255};
    static thread_local std::uniform_int_distribution<char> dist;

    const auto len = lenDist(engine());
    std::vector<char> data;
    data.reserve(len);
    std::generate_n(
        std::back_inserter(data), len, [&] { return dist(engine()); });

    return data;
}

template <typename Pred> bool waitFor(Pred &&predicate)
{
    const auto timeout = std::chrono::steady_clock::now() + 5s;
    while (!predicate() && std::chrono::steady_clock::now() < timeout)
        std::this_thread::sleep_for(1ms);

    return predicate();
}

template <> bool waitFor<>(std::atomic<bool> &predicate)
{
    return waitFor([&]() { return static_cast<bool>(predicate); });
}

} // namespace

#endif // ONE_ETLS_TEST_UTILS_HPP
