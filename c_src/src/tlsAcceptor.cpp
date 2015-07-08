/**
 * @file tlsAcceptor.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "tlsAcceptor.hpp"

#include "tlsApplication.hpp"

namespace one {
namespace etls {

TLSAcceptor::TLSAcceptor(TLSApplication &app, const unsigned short port,
    const std::string &certPath, const std::string &keyPath)
    : m_app{app}
    , m_ioService{app.ioService()}
    , m_acceptor{m_ioService, {asio::ip::tcp::v4(), port}}
{
    m_context.set_options(asio::ssl::context::default_workarounds);
    m_context.use_certificate_file(certPath, asio::ssl::context::pem);
    m_context.use_private_key_file(keyPath, asio::ssl::context::pem);
}

void TLSAcceptor::acceptAsync(Ptr self, Callback<TLSSocket::Ptr> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable {
        auto sock = std::make_shared<TLSSocket>(m_app, m_context);
        m_acceptor.async_accept(sock->m_socket.lowest_layer(),
            [ =, self = std::move(self), callback = std::move(callback) ](
                                    const auto ec) mutable {
                if (ec) {
                    callback(ec);
                }
                else {
                    sock->m_socket.lowest_layer().set_option(
                        asio::ip::tcp::no_delay{true});

                    callback(sock);
                }
            });
    });
}

void TLSAcceptor::localEndpointAsync(
    Ptr self, Callback<const asio::ip::tcp::endpoint &> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable { callback(m_acceptor.local_endpoint()); });
}

} // namespace etls
} // namespace one
