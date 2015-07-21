/**
 * @file tlsSocket.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "tlsSocket.hpp"

#include "tlsApplication.hpp"

#include <asio.hpp>

#include <algorithm>
#include <functional>
#include <random>
#include <system_error>
#include <vector>

namespace one {
namespace etls {

TLSSocket::TLSSocket(TLSApplication &app)
    : m_ioService{app.ioService()}
    , m_resolver{m_ioService}
    , m_socket{m_ioService, m_clientContext}
{
    namespace p = std::placeholders;
    m_socket.set_verify_mode(asio::ssl::verify_none);
    m_socket.set_verify_callback(
        std::bind(&TLSSocket::saveCertificate, this, p::_1, p::_2));
}

TLSSocket::TLSSocket(TLSApplication &app, asio::ssl::context &context)
    : m_ioService{app.ioService()}
    , m_resolver{m_ioService}
    , m_socket{m_ioService, context}
{
    namespace p = std::placeholders;
    m_socket.set_verify_mode(asio::ssl::verify_none);
    m_socket.set_verify_callback(
        std::bind(&TLSSocket::saveCertificate, this, p::_1, p::_2));
}

void TLSSocket::connectAsync(Ptr self, std::string host,
    const unsigned short port, Callback<Ptr> callback)
{
    m_resolver.async_resolve({std::move(host), std::to_string(port)}, [
        this,
        self = std::move(self),
        callback = std::move(callback)
    ](const auto ec1, auto iterator) mutable {

        auto endpoints = this->shuffleEndpoints(std::move(iterator));

        if (ec1) {
            callback(ec1);
            return;
        }

        asio::async_connect(m_socket.lowest_layer(), endpoints.begin(),
            endpoints.end(),
            [ this, self = std::move(self), callback = std::move(callback) ](
                                const auto ec2, auto) mutable {

                if (ec2) {
                    callback(ec2);
                    return;
                }

                m_socket.lowest_layer().set_option(
                    asio::ip::tcp::no_delay{true});

                m_socket.async_handshake(asio::ssl::stream_base::client, [
                    this,
                    self = std::move(self),
                    callback = std::move(callback)
                ](const auto ec3) mutable {
                    if (ec3)
                        callback(ec3);
                    else
                        callback(std::move(self));
                });
            });
    });
}

void TLSSocket::recvAsync(Ptr self, asio::mutable_buffer buffer,
    Callback<asio::mutable_buffer> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable {
        asio::async_read(m_socket, asio::mutable_buffers_1{buffer},
            [ =, self = std::move(self), callback = std::move(callback) ](
                             const auto ec, const auto read) mutable {
                if (ec)
                    callback(ec);
                else
                    callback(std::move(buffer));
            });
    });
}

void TLSSocket::recvAnyAsync(Ptr self, asio::mutable_buffer buffer,
    Callback<asio::mutable_buffer> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable {
        m_socket.async_read_some(asio::mutable_buffers_1{buffer},
            [ =, self = std::move(self), callback = std::move(callback) ](
                                     const auto ec, const auto read) {
                if (ec)
                    callback(ec);
                else
                    callback(asio::buffer(buffer, read));
            });
    });
}

void TLSSocket::handshakeAsync(Ptr self, Callback<> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable {
        m_socket.async_handshake(asio::ssl::stream_base::server,
            [ =, self = std::move(self), callback = std::move(callback) ](
                                     const auto ec) {
                if (ec)
                    callback(ec);
                else
                    callback();
            });
    });
}

void TLSSocket::shutdownAsync(
    Ptr self, const asio::socket_base::shutdown_type type, Callback<> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable {
        std::error_code ec;
        m_socket.lowest_layer().shutdown(type, ec);
        if (ec)
            callback(ec);
        else
            callback();
    });
}

void TLSSocket::closeAsync(Ptr self, Callback<> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable {
        std::error_code ec;
        m_socket.lowest_layer().shutdown(
            asio::ip::tcp::socket::shutdown_both, ec);

        m_socket.lowest_layer().close(ec);
        if (ec)
            callback(ec);
        else
            callback();
    });
}

void TLSSocket::localEndpointAsync(
    Ptr self, Callback<const asio::ip::tcp::endpoint &> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable { callback(m_socket.lowest_layer().local_endpoint()); });
}

void TLSSocket::remoteEndpointAsync(
    Ptr self, Callback<const asio::ip::tcp::endpoint &> callback)
{
    asio::post(m_ioService, [
        =,
        self = std::move(self),
        callback = std::move(callback)
    ]() mutable { callback(m_socket.lowest_layer().remote_endpoint()); });
}

const std::vector<std::vector<unsigned char>> &
TLSSocket::certificateChain() const
{
    return m_certificateChain;
}

std::vector<asio::ip::basic_resolver_entry<asio::ip::tcp>>
TLSSocket::shuffleEndpoints(asio::ip::tcp::resolver::iterator iterator)
{
    static thread_local std::random_device rd;
    static thread_local std::default_random_engine engine{rd()};

    std::vector<decltype(iterator)::value_type> endpoints;
    std::move(iterator, decltype(iterator){}, std::back_inserter(endpoints));
    std::shuffle(endpoints.begin(), endpoints.end(), engine);

    return endpoints;
}

bool TLSSocket::saveCertificate(bool, asio::ssl::verify_context &ctx)
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
