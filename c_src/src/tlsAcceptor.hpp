/**
 * @file tlsAcceptor.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#ifndef ONE_ETLS_TLS_ACCEPTOR_HPP
#define ONE_ETLS_TLS_ACCEPTOR_HPP

#include "callback.hpp"
#include "detail.hpp"
#include "tlsSocket.hpp"

#include <asio/io_service.hpp>
#include <asio/ip/tcp.hpp>
#include <asio/ssl/context.hpp>

#include <memory>
#include <string>

namespace one {
namespace etls {

class TLSApplication;

/**
 * The @c TLSAcceptor class is responsible for representing an acceptor socket
 * and its interface methods.
 */
class TLSAcceptor : public detail::WithSSLContext {
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
    TLSAcceptor(TLSApplication &m_app, const unsigned short port,
        const std::string &certPath, const std::string &keyPath,
        std::string rfc2818Hostname = "");

    /**
     * Asynchronously accepts a single pending connection.
     * Calls success callback with a new instance of @c TLSSocket that is a
     * result of the connection.
     * @param self Shared pointer to this.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void acceptAsync(Ptr self, Callback<TLSSocket::Ptr> callback);

    /**
     * Asynchronously retrieve the local endpoint information.
     * Calls success callback with the retrieved endpoint.
     * @param self Shared pointer to this.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void localEndpointAsync(
        Ptr self, Callback<const asio::ip::tcp::endpoint &> callback);

private:
    TLSApplication &m_app;
    asio::io_service &m_ioService;
    asio::ip::tcp::acceptor m_acceptor;
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_TLS_ACCEPTOR_HPP
