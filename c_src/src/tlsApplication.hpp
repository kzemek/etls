/**
 * @file tlsApplication.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_SSL2_APPLICATION_HPP
#define ONE_ETLS_SSL2_APPLICATION_HPP

#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <asio/ssl/context.hpp>

#include <vector>
#include <thread>

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
     * Starts N threads where N is the result of @c std::hardware_concurrency().
     */
    TLSApplication();

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
    std::size_t m_threadsNum = 1;
    asio::io_service m_ioService{m_threadsNum};
    std::vector<std::thread> m_threads;
    asio::executor_work<asio::io_service::executor_type> m_work =
        asio::make_work(m_ioService);
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_SSL2_APPLICATION_HPP
