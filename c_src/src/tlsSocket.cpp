/**
 * @file tlsSocket.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "tlsSocket.hpp"

#include <boost/asio.hpp>

#include <algorithm>
#include <functional>
#include <random>
#include <vector>

namespace one {
namespace etls {

TLSSocket::TLSSocket(boost::asio::io_service &ioService)
    : m_strand{ioService}
    , m_resolver{ioService}
    , m_socket{ioService, m_clientContext}
{
    namespace p = std::placeholders;
    m_socket.set_verify_mode(boost::asio::ssl::verify_none);
    m_socket.set_verify_callback(
        std::bind(&TLSSocket::saveCertificate, this, p::_1, p::_2));
}

TLSSocket::TLSSocket(
    boost::asio::io_service &ioService, boost::asio::ssl::context &context)
    : m_strand{ioService}
    , m_resolver{ioService}
    , m_socket{ioService, context}
{
    namespace p = std::placeholders;
    m_socket.set_verify_mode(boost::asio::ssl::verify_none);
    m_socket.set_verify_callback(
        std::bind(&TLSSocket::saveCertificate, this, p::_1, p::_2));
}

void TLSSocket::connectAsync(Ptr self, std::string host,
    const unsigned short port, SuccessFun<Ptr> success, ErrorFun error)
{
    m_resolver.async_resolve(
        {std::move(host), std::to_string(port)}, m_strand.wrap([
            =,
            self = std::move(self),
            success = std::move(success),
            error = std::move(error)
        ](const auto ec, auto iterator) mutable {

            auto endpoints = this->shuffleEndpoints(std::move(iterator));

            if (ec) {
                error(ec);
                return;
            }

            boost::asio::async_connect(m_socket.lowest_layer(),
                endpoints.begin(), endpoints.end(), m_strand.wrap([
                    =,
                    self = std::move(self),
                    success = std::move(success),
                    error = std::move(error)
                ](const auto ec, auto iterator) mutable {

                    if (ec) {
                        error(ec);
                        return;
                    }

                    m_socket.lowest_layer().set_option(
                        boost::asio::ip::tcp::no_delay{true});

                    m_socket.async_handshake(
                        boost::asio::ssl::stream_base::client, [
                            =,
                            self = std::move(self),
                            success = std::move(success),
                            error = std::move(error)
                        ](const auto ec) mutable {
                            if (ec)
                                error(ec);
                            else
                                success(std::move(self));
                        });
                }));
        }));
}

void TLSSocket::sendAsync(Ptr self, boost::asio::const_buffer buffer,
    SuccessFun<> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        boost::asio::async_write(
            m_socket, boost::asio::const_buffers_1{buffer}, [
                =,
                self = std::move(self),
                success = std::move(success),
                error = std::move(error)
            ](const auto ec, const auto read) {
                if (ec)
                    error(ec);
                else
                    success();
            });
    });
}

void TLSSocket::recvAsync(Ptr self, boost::asio::mutable_buffer buffer,
    SuccessFun<boost::asio::mutable_buffer> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        boost::asio::async_read(
            m_socket, boost::asio::mutable_buffers_1{buffer}, [
                =,
                self = std::move(self),
                success = std::move(success),
                error = std::move(error)
            ](const auto ec, const auto read) {
                if (ec)
                    error(ec);
                else
                    success(buffer);
            });
    });
}

void TLSSocket::recvAnyAsync(Ptr self, boost::asio::mutable_buffer buffer,
    SuccessFun<boost::asio::mutable_buffer> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        m_socket.async_read_some(boost::asio::mutable_buffers_1{buffer}, [
            =,
            self = std::move(self),
            success = std::move(success),
            error = std::move(error)
        ](const auto ec, const auto read) {
            if (ec)
                error(ec);
            else
                success(boost::asio::buffer(buffer, read));
        });
    });
}

void TLSSocket::handshakeAsync(Ptr self, SuccessFun<> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        m_socket.async_handshake(boost::asio::ssl::stream_base::server, [
            =,
            self = std::move(self),
            success = std::move(success),
            error = std::move(error)
        ](const auto ec) {
            if (ec)
                error(ec);
            else
                success();
        });
    });
}

void TLSSocket::shutdownAsync(Ptr self,
    const boost::asio::socket_base::shutdown_type type, SuccessFun<> success,
    ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        boost::system::error_code ec;
        m_socket.lowest_layer().shutdown(type, ec);
        if (ec)
            error(ec);
        else
            success();
    });
}

void TLSSocket::closeAsync(Ptr self, SuccessFun<> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable {
        boost::system::error_code ec;
        m_socket.lowest_layer().shutdown(
            boost::asio::ip::tcp::socket::shutdown_both, ec);

        m_socket.lowest_layer().close(ec);
        if (ec)
            error(ec);
        else
            success();
    });
}

void TLSSocket::localEndpointAsync(Ptr self,
    SuccessFun<const boost::asio::ip::tcp::endpoint &> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable { success(m_socket.lowest_layer().local_endpoint()); });
}

void TLSSocket::remoteEndpointAsync(Ptr self,
    SuccessFun<const boost::asio::ip::tcp::endpoint &> success, ErrorFun error)
{
    m_strand.post([
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ]() mutable { success(m_socket.lowest_layer().remote_endpoint()); });
}

const std::vector<std::vector<unsigned char>> &
TLSSocket::certificateChain() const
{
    return m_certificateChain;
}

std::vector<boost::asio::ip::basic_resolver_entry<boost::asio::ip::tcp>>
TLSSocket::shuffleEndpoints(boost::asio::ip::tcp::resolver::iterator iterator)
{
    static thread_local std::random_device rd;
    static thread_local std::default_random_engine engine{rd()};

    std::vector<decltype(iterator)::value_type> endpoints;
    std::move(iterator, decltype(iterator){}, std::back_inserter(endpoints));
    std::shuffle(endpoints.begin(), endpoints.end(), engine);

    return endpoints;
}

bool TLSSocket::saveCertificate(bool, boost::asio::ssl::verify_context &ctx)
{
    auto cert = X509_STORE_CTX_get_current_cert(ctx.native_handle());
    if (!cert)
        return true;

    const auto dataLen = i2d_X509(cert, nullptr);
    if (dataLen < 0)
        return true;

    std::vector<unsigned char> certificateData(dataLen);
    auto p = certificateData.data();
    if (i2d_X509(cert, &p) < 0)
        return true;

    m_certificateChain.emplace_back(std::move(certificateData));

    return true;
}

} // namespace etls
} // namespace one
