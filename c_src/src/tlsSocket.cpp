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

template <typename Res, typename... Args1, typename... Args2>
void TLSSocket::notifying(SuccessFun<Res> &&success, ErrorFun &&error,
    Res (TLSSocket::*method)(Args1...), Args2 &&... args)
{
    try {
        Res &&result = (this->*method)(std::forward<Args2>(args)...);
        success(std::forward<Res>(result));
    }
    catch (const std::exception &e) {
        error(e.what());
    }
}

template <typename Res, typename... Args1, typename... Args2>
void TLSSocket::notifying(SuccessFun<> &&success, ErrorFun &&error,
    Res (TLSSocket::*method)(Args1...), Args2 &&... args)
{
    try {
        (this->*method)(std::forward<Args2>(args)...);
        success();
    }
    catch (const std::exception &e) {
        error(e.what());
    }
}

TLSSocket::TLSSocket(boost::asio::io_service &ioService)
    : m_context{boost::asio::ssl::context::tlsv12_client}
    , m_strand{ioService}
    , m_resolver{ioService}
    , m_socket{ioService, m_context}
{
}

void TLSSocket::connectAsync(Ptr self, std::string host,
    const unsigned short port, SuccessFun<Ptr> success, ErrorFun error)
{
    boost::asio::spawn(m_strand, [
        =,
        self = std::move(self),
        host = std::move(host),
        success = std::move(success),
        error = std::move(error)
    ](boost::asio::yield_context yield) mutable {
        notifying(std::move(success), std::move(error), &TLSSocket::connect,
            std::move(self), std::move(host), port, yield);
    });
}

void TLSSocket::sendAsync(Ptr self, boost::asio::const_buffer buffer,
    SuccessFun<> success, ErrorFun error)
{
    boost::asio::spawn(m_strand, [
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ](boost::asio::yield_context yield) mutable {
        notifying(std::move(success), std::move(error), &TLSSocket::send,
            std::move(self), buffer, yield);
    });
}

void TLSSocket::recvAsync(Ptr self, boost::asio::mutable_buffer buffer,
    SuccessFun<boost::asio::mutable_buffer> success, ErrorFun error)
{
    boost::asio::spawn(m_strand, [
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ](boost::asio::yield_context yield) mutable {
        notifying(std::move(success), std::move(error), &TLSSocket::recv,
            std::move(self), buffer, yield);
    });
}

void TLSSocket::recvAnyAsync(TLSSocket::Ptr self,
    boost::asio::mutable_buffer buffer,
    SuccessFun<boost::asio::mutable_buffer> success, TLSSocket::ErrorFun error)
{
    boost::asio::spawn(m_strand, [
        =,
        self = std::move(self),
        success = std::move(success),
        error = std::move(error)
    ](boost::asio::yield_context yield) mutable {
        notifying(std::move(success), std::move(error), &TLSSocket::recvAny,
            std::move(self), buffer, yield);
    });
}

void TLSSocket::close() { m_socket.lowest_layer().close(); }

TLSSocket::Ptr TLSSocket::connect(Ptr self, std::string host,
    const unsigned short port, boost::asio::yield_context yield)
{
    auto iterator = m_resolver.async_resolve(
        {std::move(host), std::to_string(port)}, yield);

    auto endpoints = shuffleEndpoints(std::move(iterator));

    boost::asio::async_connect(
        m_socket.lowest_layer(), endpoints.begin(), endpoints.end(), yield);

    m_socket.async_handshake(boost::asio::ssl::stream_base::client, yield);

    return std::move(self);
}

void TLSSocket::send(TLSSocket::Ptr self, boost::asio::const_buffer buffer,
    boost::asio::yield_context yield)
{
    boost::asio::async_write(
        m_socket, boost::asio::const_buffers_1{buffer}, yield);
}

boost::asio::mutable_buffer TLSSocket::recv(Ptr self,
    boost::asio::mutable_buffer buffer, boost::asio::yield_context yield)
{
    boost::asio::async_read(
        m_socket, boost::asio::mutable_buffers_1{buffer}, yield);

    return buffer;
}

boost::asio::mutable_buffer TLSSocket::recvAny(Ptr self,
    boost::asio::mutable_buffer buffer, boost::asio::yield_context yield)
{
    if(m_socket.lowest_layer().available() == 0)
        return {};

    const auto read =
        m_socket.async_read_some(boost::asio::mutable_buffers_1{buffer}, yield);

    return boost::asio::buffer(buffer, read);
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
