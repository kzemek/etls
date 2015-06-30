/**
 * @file tlsApplication.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "tlsApplication.hpp"

#include <algorithm>

namespace one {
namespace etls {

TLSApplication::TLSApplication()
{
    m_thread = std::thread{[this] {
        while (!m_ioService.stopped()) {
            try {
                m_ioService.run();
            }
            catch (...) {
            }
        }
    }};
}

TLSApplication::~TLSApplication()
{
    m_ioService.stop();
    m_thread.join();
}

asio::io_service &TLSApplication::ioService() { return m_ioService; }

} // namespace etls
} // namespace one
