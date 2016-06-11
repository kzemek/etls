/**
 * @file tlsApplication.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#include "tlsApplication.hpp"

#include "utils.hpp"

#include <algorithm>
#include <functional>

namespace one {
namespace etls {

TLSApplication::TLSApplication(std::size_t n)
    : m_threadsNum{n}
{
    std::generate_n(std::back_inserter(m_ioServices), m_threadsNum,
        [] { return std::make_unique<asio::io_service>(1); });

    for (auto &ios : m_ioServices) {
        m_works.emplace_back(asio::make_work(*ios));
        m_threads.emplace_back([&] {
            utils::nameThread("TLSApplication");
            ios->run();
        });
    }
}

TLSApplication::~TLSApplication()
{
    for (auto &ios : m_ioServices)
        ios->stop();

    for (auto &thread : m_threads)
        thread.join();
}

asio::io_service &TLSApplication::ioService()
{
    return *m_ioServices[m_nextService++ % m_threadsNum];
}

} // namespace etls
} // namespace one
