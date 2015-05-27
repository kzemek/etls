/**
 * @file tlsApplication.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ERLANG_TLS_TLS_APPLICATION_HPP
#define ERLANG_TLS_TLS_APPLICATION_HPP

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
    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_work{m_ioService};

    std::vector<std::thread> m_threads;
};

} // namespace etls
} // namespace one

#endif // ERLANG_TLS_TLS_APPLICATION_HPP
