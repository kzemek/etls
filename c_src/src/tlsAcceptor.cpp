/**
 * @file tlsAcceptor.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "tlsAcceptor.hpp"

namespace one {
namespace etls {

TLSAcceptor::TLSAcceptor(asio::io_service &ioService, const unsigned short port,
    const std::string &certPath, const std::string &keyPath)
    : m_ioService{ioService}
    , m_acceptor{m_ioService, {asio::ip::tcp::v4(), port}}
{
    m_context.set_options(asio::ssl::context::default_workarounds);
    m_context.use_certificate_file(certPath, asio::ssl::context::pem);
    m_context.use_private_key_file(keyPath, asio::ssl::context::pem);
}

void TLSAcceptor::acceptAsync(
    Ptr self, SuccessFun<TLSSocket::Ptr> success, ErrorFun error)
{
    m_ioService.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        auto sock = std::make_shared<TLSSocket>(m_ioService, m_context);
        m_acceptor.async_accept(sock->m_socket.lowest_layer(), [
            =,
            self = std::move(self),
            success = std::move(success),
            error = std::move(error)
        ](const auto ec) {
            if (ec) {
                error(ec);
            }
            else {
                sock->m_socket.lowest_layer().set_option(
                    asio::ip::tcp::no_delay{true});
                success(sock);
            }
        });
    });
}

void TLSAcceptor::localEndpointAsync(Ptr self,
    SuccessFun<const asio::ip::tcp::endpoint &> success, ErrorFun error)
{
    m_ioService.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable { success(m_acceptor.local_endpoint()); });
}

} // namespace etls
} // namespace one
