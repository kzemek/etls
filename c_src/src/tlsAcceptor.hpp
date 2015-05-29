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

#include <memory>

namespace one {
namespace etls {

class TLSAcceptor {
public:
    using Ptr = std::shared_ptr<TLSAcceptor>;

    TLSAcceptor(boost::asio::io_service &ioService, const unsigned short port);

    void acceptAsync(Ptr self, SuccessFun<TLSSocket::Ptr> success,
        ErrorFun error = [](auto) {});

private:
    boost::asio::io_service &m_ioService;
    boost::asio::io_service::strand m_strand{m_ioService};
    boost::asio::ip::tcp::acceptor m_acceptor;
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_TLS_ACCEPTOR_HPP
