/**
 * @file tlsAcceptor.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_TLS_ACCEPTOR_HPP
#define ONE_ETLS_TLS_ACCEPTOR_HPP

#include "commonDefs.hpp"
#include "tlsSocket.hpp"

#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/context.hpp>

#include <memory>
#include <string>

namespace one {
namespace etls {

class TLSAcceptor {
public:
    using Ptr = std::shared_ptr<TLSAcceptor>;

    TLSAcceptor(boost::asio::io_service &ioService, const unsigned short port,
        const std::string &certPath, const std::string &keyPath);

    void acceptAsync(
        Ptr self, SuccessFun<TLSSocket::Ptr> success, ErrorFun error);

private:
    boost::asio::io_service &m_ioService;
    boost::asio::io_service::strand m_strand{m_ioService};
    boost::asio::ip::tcp::acceptor m_acceptor;
    boost::asio::ssl::context m_context{
        boost::asio::ssl::context::tlsv12_server};
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_TLS_ACCEPTOR_HPP
