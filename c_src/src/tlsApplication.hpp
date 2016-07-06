/**
 * @file tlsApplication.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#ifndef ONE_ETLS_TLS_APPLICATION_HPP
#define ONE_ETLS_TLS_APPLICATION_HPP

#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <asio/ssl/context.hpp>

#include <atomic>
#include <cstdint>
#include <memory>
#include <thread>
#include <vector>

namespace one {
namespace etls {

/**
 * The @c TLSApplication class stores persistent objects required for
 * uninterrupted application functioning.
 */
class TLSApplication {
public:
    /**
     * Constructor.
     * Starts N threads where N is by default the result of @c
     * std::hardware_concurrency().
     */
    TLSApplication(std::size_t n = std::thread::hardware_concurrency());

    /**
     * Destructor.
     * Stops the @c io_service and joins all managed threads.
     */
    ~TLSApplication();

    /**
     * @returns An @c io_service object managed by this.
     */
    asio::io_service &ioService();

private:
    std::size_t m_threadsNum;
    std::vector<std::unique_ptr<asio::io_service>> m_ioServices;
    std::vector<asio::executor_work<asio::io_service::executor_type>> m_works;
    std::vector<std::thread> m_threads;
    std::atomic<std::size_t> m_nextService{0};
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_TLS_APPLICATION_HPP
