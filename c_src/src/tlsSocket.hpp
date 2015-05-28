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
    using SuccessFun = std::function<void()>;
    using ErrorFun = std::function<void(std::string)>;

    TLSSocket(boost::asio::io_service &ioService);

    void connectAsync(Ptr self, std::string host, const unsigned short port,
        SuccessFun success = [] {}, ErrorFun error = [](auto) {});

    void sendAsync(Ptr self, boost::asio::const_buffer buffer,
        SuccessFun success = [] {}, ErrorFun error = [](auto) {});

    void close();

private:
    template <typename... Args1, typename... Args2>
    void notifying(SuccessFun &&success, ErrorFun &&error,
        void (TLSSocket::*method)(Args1...), Args2 &&... args);

    void connect(Ptr self, std::string host, const unsigned short port,
        boost::asio::yield_context yield);

    void send(Ptr self, boost::asio::const_buffer buffer,
        boost::asio::yield_context yield);

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
