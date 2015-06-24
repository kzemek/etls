/**
 * @file tlsApplication.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_ssl2_appLICATION_HPP
#define ONE_ETLS_ssl2_appLICATION_HPP

#include <boost/asio/io_service.hpp>
#include <boost/asio/ssl/context.hpp>

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
    boost::asio::io_service &ioService();

private:
    boost::asio::io_service m_ioService{1};
    boost::asio::io_service::work m_work{m_ioService};
    std::thread m_thread;
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_ssl2_appLICATION_HPP
