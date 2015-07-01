/**
 * @file tlsApplication.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "tlsApplication.hpp"

#include <algorithm>
#include <functional>

namespace one {
namespace etls {

TLSApplication::TLSApplication()
{
    std::generate_n(std::back_inserter(m_threads), m_threadsNum,
        [=] { return std::thread{[=] { m_ioService.run(); }}; });
}

TLSApplication::~TLSApplication()
{
    m_ioService.stop();
    for (auto &thread : m_threads)
        thread.join();
}

asio::io_service &TLSApplication::ioService() { return m_ioService; }

} // namespace etls
} // namespace one
