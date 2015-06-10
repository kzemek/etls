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

#include <vector>
#include <thread>

namespace one {
namespace etls {

class TLSApplication {
public:
    TLSApplication();
    ~TLSApplication();

    boost::asio::io_service &ioService();

private:
    const std::size_t m_threadsNo{std::thread::hardware_concurrency()};

    boost::asio::io_service m_ioService{m_threadsNo};
    boost::asio::io_service::work m_work{m_ioService};

    std::vector<std::thread> m_threads;
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_ssl2_appLICATION_HPP
