/**
 * @file tlsSocket.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ERLANG_TLS_TLS_SOCKET_HPP
#define ERLANG_TLS_TLS_SOCKET_HPP

#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/spawn.hpp>
#include <boost/asio/ssl/stream.hpp>

#include <memory>
#include <string>

namespace one {
namespace etls {

class TLSSocket {
public:
    using Ptr = std::shared_ptr<TLSSocket>;
    template <typename... Args> using SuccessFun = std::function<void(Args...)>;
    using ErrorFun = std::function<void(std::string)>;

    TLSSocket(boost::asio::io_service &ioService);

    void connectAsync(Ptr self, std::string host, const unsigned short port,
        SuccessFun<Ptr> success = [](auto) {}, ErrorFun error = [](auto) {});

    void sendAsync(Ptr self, boost::asio::const_buffer buffer,
        SuccessFun<> success = [] {}, ErrorFun error = [](auto) {});

    void recvAsync(Ptr self, boost::asio::mutable_buffer buffer,
        SuccessFun<boost::asio::mutable_buffer> success = [](auto) {},
        ErrorFun error = [](auto) {});

    void recvAnyAsync(Ptr self, boost::asio::mutable_buffer buffer,
        SuccessFun<boost::asio::mutable_buffer> success = [](auto) {},
        ErrorFun error = [](auto) {});

    void close();

private:
    template <typename Res, typename... Args1, typename... Args2>
    void notifying(SuccessFun<Res> &&success, ErrorFun &&error,
        Res (TLSSocket::*method)(Args1...), Args2 &&... args);

    template <typename Res, typename... Args1, typename... Args2>
    void notifying(SuccessFun<> &&success, ErrorFun &&error,
        Res (TLSSocket::*method)(Args1...), Args2 &&... args);

    Ptr connect(Ptr self, std::string host, const unsigned short port,
        boost::asio::yield_context yield);

    void send(Ptr self, boost::asio::const_buffer buffer,
        boost::asio::yield_context yield);

    boost::asio::mutable_buffer recv(Ptr self,
        boost::asio::mutable_buffer buffer, boost::asio::yield_context yield);

    boost::asio::mutable_buffer recvAny(Ptr self,
        boost::asio::mutable_buffer buffer, boost::asio::yield_context yield);

    std::vector<boost::asio::ip::basic_resolver_entry<boost::asio::ip::tcp>>
    shuffleEndpoints(boost::asio::ip::tcp::resolver::iterator iterator);

    boost::asio::ssl::context m_context;
    boost::asio::io_service::strand m_strand;
    boost::asio::ip::tcp::resolver m_resolver;
    boost::asio::ssl::stream<boost::asio::ip::tcp::socket> m_socket;
};

} // namespace etls
} // namespace one

#endif // ERLANG_TLS_TLS_SOCKET_HPP
