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

/**
 * The @c TLSAcceptor class is responsible for representing an acceptor socket
 * and its interface methods.
 */
class TLSAcceptor {
public:
    /**
     * A shortcut alias for frequent usage.
     */
    using Ptr = std::shared_ptr<TLSAcceptor>;

    /**
     * Constructor.
     * Opens the acceptor socket.
     * @param ioService @c io_service object to use for this object's
     * asynchronous operations.
     * @param port Port on which to listen.
     * @param certPath Path to a PEM certificate file to use for the TLS
     * connection.
     * @param keyPath Path to a PEM keyfile to use for the TLS connection.
     */
    TLSAcceptor(boost::asio::io_service &ioService, const unsigned short port,
        const std::string &certPath, const std::string &keyPath);

    /**
     * Asynchronously accepts a single pending connection.
     * Calls success callback with a new instance of @c TLSSocket that is a
     * result of the connection.
     * @param self Shared pointer to this.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
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
