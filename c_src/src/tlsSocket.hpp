/**
 * @file tlsSocket.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#ifndef ONE_ETLS_TLS_SOCKET_HPP
#define ONE_ETLS_TLS_SOCKET_HPP

#include "callback.hpp"
#include "detail.hpp"

#include <asio.hpp>
#include <asio/io_service.hpp>
#include <asio/ip/tcp.hpp>
#include <asio/ssl/stream.hpp>

#include <memory>
#include <string>
#include <vector>

namespace one {
namespace etls {

class TLSApplication;

/**
 * The @c TLSSocket class is responsible for handling a single TLS socket.
 */
class TLSSocket : public detail::WithSSLContext {
    friend class TLSAcceptor;

public:
    /**
     * A shortcut alias for frequent usage.
     */
    using Ptr = std::shared_ptr<TLSSocket>;

    /**
     * Constructor.
     * Prepares a new @c asio socket with a local @c ssl::context.
     * @param app @c TLSApplication object to retrieve @c io_service for this
     * object's asynchronous operations.
     * @param certPath Path to a PEM certificate file to use for the TLS
     * connection.
     * @param keyPath Path to a PEM keyfile to use for the TLS connection.
     */
    TLSSocket(TLSApplication &app, const std::string &keyPath = "",
        const std::string &certPath = "", std::string rfc2818Hostname = "");

    /**
     * Constructor.
     * Prepares a new @c asio socket with a given SSL context.
     * @param app @c TLSApplication object to retrieve @c io_service for this
     * object's asynchronous operations.
     * @param acceptor a @c context handler to use for this socket's
     * configuration.
     */
    TLSSocket(TLSApplication &app, std::shared_ptr<asio::ssl::context> context);

    /**
     * Asynchronously connects the socket to a remote service.
     * Calls success callback with @c self.
     * @param self Shared pointer to this.
     * @param host Host to connect to.
     * @param port TCP port to connect to.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void connectAsync(Ptr self, std::string host, const unsigned short port,
        Callback<Ptr> callback);

    /**
     * Asynchronously sends a message through the socket.
     * @param self Shared pointer to this.
     * @param buffer The message to send.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    template <typename BufferSequence>
    void sendAsync(Ptr self, const BufferSequence &buffer, Callback<> callback);

    /**
     * Asynchronously receives a message from the socket.
     * Calls success callback with @c buffer. Either all of the buffer will
     * be filled with received data, or error will be called.
     * @param self Shared pointer to this.
     * @param buffer Buffer to save the received message to.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void recvAsync(Ptr self, asio::mutable_buffer buffer,
        Callback<asio::mutable_buffer> callback);

    /**
     * Asynchronously receive a message from the socket.
     * Calls success callback with a given @c buffer resized to the size of
     * received message. Consequently, @c recvAnyAsync returns with success
     * after any data is received.
     * @param self Shared pointer to this.
     * @param buffer Buffer to save the received message to.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void recvAnyAsync(Ptr self, asio::mutable_buffer buffer,
        Callback<asio::mutable_buffer> callback);

    /**
     * Asynchronously perform a handshake for an incoming connection.
     * @param self Shared pointer to this.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void handshakeAsync(Ptr self, Callback<> callback);

    /**
     * Asynchronously shutdown the TCP connection on the socket.
     * @param self Shared pointer to this.
     * @param type Type of the shutdown (read, write or both).
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void shutdownAsync(Ptr self, const asio::socket_base::shutdown_type type,
        Callback<> callback);

    /**
     * Asynchronously retrieve the local endpoint information.
     * Calls success callback with the retrieved endpoint.
     * @param self Shared pointer to this.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void localEndpointAsync(
        Ptr self, Callback<const asio::ip::tcp::endpoint &> callback);

    /**
     * Asynchronously retrieve the remote endpoint information.
     * Calls success callback with the retrieved endpoint.
     * @param self Shared pointer to this.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void remoteEndpointAsync(
        Ptr self, Callback<const asio::ip::tcp::endpoint &> callback);

    /**
     * @returns A DER-encoded list of certificates that form peer's certificate
     * chain.
     */
    const std::vector<std::vector<unsigned char>> &certificateChain() const;

    /**
     * Asynchronously close the socket.
     * @param self Shared pointer to this.
     * @param success Callback function to call on success.
     * @param error Callback function to call on error.
     */
    void closeAsync(Ptr self, Callback<> callback);

    void setVerifyMode(const asio::ssl::verify_mode mode) override;

private:
    void saveChain(bool server);

    std::vector<asio::ip::basic_resolver_entry<asio::ip::tcp>> shuffleEndpoints(
        asio::ip::tcp::resolver::iterator iterator);

    asio::io_service &m_ioService;
    asio::ip::tcp::resolver m_resolver;
    asio::ssl::stream<asio::ip::tcp::socket> m_socket;
    std::vector<std::vector<unsigned char>> m_certificateChain;
};

template <typename BufferSequence>
void TLSSocket::sendAsync(
    Ptr self, const BufferSequence &buffers, Callback<> callback)
{
    asio::post(m_ioService, [
        =, self = std::move(self), callback = std::move(callback)
    ]() mutable {
        asio::async_write(m_socket, buffers,
            [ =, self = std::move(self), callback = std::move(callback) ](
                              const auto ec, const auto read) {
                if (ec)
                    callback(ec);
                else
                    callback();
            });
    });
}

} // namespace etls
} // namespace one

#endif // ONE_ETLS_TLS_SOCKET_HPP
