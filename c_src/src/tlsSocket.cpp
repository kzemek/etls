/**
 * @file tlsSocket.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.md'
 */

#include "tlsSocket.hpp"

#include "detail.hpp"
#include "tlsApplication.hpp"

#include <asio.hpp>

#include <algorithm>
#include <functional>
#include <random>
#include <system_error>
#include <vector>

namespace {

std::vector<unsigned char> certToDer(X509 *cert)
{
    if (!cert)
        return {};

    const auto dataLen = i2d_X509(cert, nullptr);
    if (dataLen < 0)
        return {};

    std::vector<unsigned char> certificateData(dataLen);
    auto p = certificateData.data();
    if (i2d_X509(cert, &p) < 0)
        return {};

    return certificateData;
}

} // namespace

namespace one {
namespace etls {

TLSSocket::TLSSocket(TLSApplication &app, const std::string &keyPath,
    const std::string &certPath, std::string rfc2818Hostname)
    : detail::WithSSLContext{asio::ssl::context::tlsv12_client, keyPath,
          certPath, std::move(rfc2818Hostname)}
    , m_ioService{app.ioService()}
    , m_resolver{m_ioService}
    , m_socket{m_ioService, *m_context}
{
}

TLSSocket::TLSSocket(
    TLSApplication &app, std::shared_ptr<asio::ssl::context> context)
    : detail::WithSSLContext{std::move(context)}
    , m_ioService{app.ioService()}
    , m_resolver{m_ioService}
    , m_socket{m_ioService, *m_context}
{
}

void TLSSocket::connectAsync(Ptr self, std::string host,
    const unsigned short port, Callback<Ptr> callback)
{
    m_resolver.async_resolve({std::move(host), std::to_string(port)}, [
        this, self = std::move(self), callback = std::move(callback)
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
                    this, self = std::move(self), callback = std::move(callback)
                ](const auto ec3) mutable {
                    if (ec3) {
                        callback(ec3);
                    }
                    else {
                        this->saveChain(false);
                        callback(std::move(self));
                    }
                });
            });
    });
}

void TLSSocket::recvAsync(Ptr self, asio::mutable_buffer buffer,
    Callback<asio::mutable_buffer> callback)
{
    asio::post(m_ioService, [
        =, self = std::move(self), callback = std::move(callback)
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
        =, self = std::move(self), callback = std::move(callback)
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
        =, self = std::move(self), callback = std::move(callback)
    ]() mutable {
        m_socket.async_handshake(asio::ssl::stream_base::server,
            [ =, self = std::move(self), callback = std::move(callback) ](
                                     const auto ec) {
                if (ec) {
                    callback(ec);
                }
                else {
                    this->saveChain(true);
                    callback();
                }
            });
    });
}

void TLSSocket::shutdownAsync(
    Ptr self, const asio::socket_base::shutdown_type type, Callback<> callback)
{
    asio::post(m_ioService, [
        =, self = std::move(self), callback = std::move(callback)
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
        =, self = std::move(self), callback = std::move(callback)
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

void TLSSocket::setVerifyMode(const asio::ssl::verify_mode mode)
{
    m_socket.set_verify_mode(mode);
}

void TLSSocket::saveChain(bool server)
{
    auto ssl = m_socket.native_handle();
    if (!ssl)
        return;

    auto chain = SSL_get_peer_cert_chain(ssl);
    if (!chain)
        return;

    decltype(m_certificateChain) certChain;

    auto numCerts = sk_X509_num(chain);
    for (auto i = 0u; i < numCerts; ++i) {
        auto cert = sk_X509_value(chain, i);
        auto certificateData = certToDer(cert);
        if (certificateData.empty())
            return;

        certChain.emplace_back(std::move(certificateData));
    }

    if (server) {
        auto cert = SSL_get_peer_certificate(ssl);
        auto certificateData = certToDer(cert);
        if (certificateData.empty())
            return;

        certChain.emplace_back(std::move(certificateData));
    }

    std::swap(m_certificateChain, certChain);
}

void TLSSocket::localEndpointAsync(
    Ptr self, Callback<const asio::ip::tcp::endpoint &> callback)
{
    asio::post(m_ioService, [
        =, self = std::move(self), callback = std::move(callback)
    ]() mutable { callback(m_socket.lowest_layer().local_endpoint()); });
}

void TLSSocket::remoteEndpointAsync(
    Ptr self, Callback<const asio::ip::tcp::endpoint &> callback)
{
    asio::post(m_ioService, [
        =, self = std::move(self), callback = std::move(callback)
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

} // namespace etls
} // namespace one
