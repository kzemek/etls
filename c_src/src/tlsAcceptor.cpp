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

TLSAcceptor::TLSAcceptor(
    boost::asio::io_service &ioService, const unsigned short port)
    : m_ioService{ioService}
    , m_acceptor{m_ioService, {boost::asio::ip::tcp::v4(), port}}
{
}

void TLSAcceptor::acceptAsync(
    Ptr self, SuccessFun<TLSSocket::Ptr> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        auto sock = std::make_shared<TLSSocket>(m_ioService);
        m_acceptor.async_accept(sock->m_socket.lowest_layer(), [
            =,
            self = std::move(self),
            success = std::move(success),
            error = std::move(error)
        ](const auto ec) {
            if (ec)
                error(ec.message());
            else
                success(sock);
        });
    });
}

} // namespace etls
} // namespace one
