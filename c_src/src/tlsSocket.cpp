/**
 * @file tlsSocket.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "tlsSocket.hpp"

#include <boost/asio.hpp>
#include <boost/asio/spawn.hpp>

#include <algorithm>
#include <random>
#include <vector>

namespace one {
namespace etls {

TLSSocket::TLSSocket(boost::asio::io_service &ioService)
    : m_context{boost::asio::ssl::context::tlsv12_client}
    , m_strand{ioService}
    , m_resolver{ioService}
    , m_socket{ioService, m_context}
{
}

void TLSSocket::connectAsync(Ptr self, std::string host,
    const unsigned short port, std::function<void(Ptr)> success,
    std::function<void(std::string)> error)
{
    boost::asio::spawn(m_strand, [
        =,
        host = std::move(host),
        success = std::move(success),
        error = std::move(error)
    ](boost::asio::yield_context yield) mutable {
        try {
            connect(self, std::move(host), port, yield);
            success(std::move(self));
        }
        catch (const std::exception &e) {
            error(e.what());
        }
    });
}

void TLSSocket::connect(Ptr self, std::string host, const unsigned short port,
    boost::asio::yield_context yield)
{
    auto iterator = m_resolver.async_resolve(
        {std::move(host), std::to_string(port)}, yield);

    auto endpoints = shuffleEndpoints(std::move(iterator));

    boost::asio::async_connect(
        m_socket.lowest_layer(), endpoints.begin(), endpoints.end(), yield);

    m_socket.async_handshake(boost::asio::ssl::stream_base::client, yield);
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

} // namespace etls
} // namespace one
