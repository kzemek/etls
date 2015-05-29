/**
 * @file tlsSocket.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_TLS_SOCKET_HPP
#define ONE_ETLS_TLS_SOCKET_HPP

#include "commonDefs.hpp"

#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/spawn.hpp>
#include <boost/asio/ssl/stream.hpp>

#include <memory>
#include <string>

namespace one {
namespace etls {

class TLSSocket {
    friend class TLSAcceptor;

public:
    using Ptr = std::shared_ptr<TLSSocket>;

    TLSSocket(boost::asio::io_service &ioService);

    void connectAsync(Ptr self, std::string host, const unsigned short port,
        SuccessFun<Ptr> success, ErrorFun error);

    void sendAsync(Ptr self, boost::asio::const_buffer buffer,
        SuccessFun<> success, ErrorFun error);

    void recvAsync(Ptr self, boost::asio::mutable_buffer buffer,
        SuccessFun<boost::asio::mutable_buffer> success, ErrorFun error);

    void recvAnyAsync(Ptr self, boost::asio::mutable_buffer buffer,
        SuccessFun<boost::asio::mutable_buffer> success, ErrorFun error);

    void close();

private:
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

#endif // ONE_ETLS_TLS_SOCKET_HPP
