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
#include <boost/asio/ssl/stream.hpp>
#include <boost/asio/strand.hpp>

#include <memory>
#include <string>
#include <vector>

namespace one {
namespace etls {

class TLSSocket {
    friend class TLSAcceptor;

public:
    using Ptr = std::shared_ptr<TLSSocket>;

    TLSSocket(boost::asio::io_service &ioService);

    TLSSocket(
        boost::asio::io_service &ioService, boost::asio::ssl::context &context);

    void connectAsync(Ptr self, std::string host, const unsigned short port,
        SuccessFun<Ptr> success, ErrorFun error);

    void sendAsync(Ptr self, boost::asio::const_buffer buffer,
        SuccessFun<> success, ErrorFun error);

    void recvAsync(Ptr self, boost::asio::mutable_buffer buffer,
        SuccessFun<boost::asio::mutable_buffer> success, ErrorFun error);

    void recvAnyAsync(Ptr self, boost::asio::mutable_buffer buffer,
        SuccessFun<boost::asio::mutable_buffer> success, ErrorFun error);

    void handshakeAsync(Ptr self, SuccessFun<> success, ErrorFun error);

    boost::asio::ip::tcp::endpoint localEndpoint() const;

    boost::asio::ip::tcp::endpoint remoteEndpoint() const;

    const std::vector<std::vector<unsigned char>> &certificateChain() const;

    void close();

private:
    std::vector<boost::asio::ip::basic_resolver_entry<boost::asio::ip::tcp>>
    shuffleEndpoints(boost::asio::ip::tcp::resolver::iterator iterator);

    bool saveCertificate(bool, boost::asio::ssl::verify_context &ctx);

    boost::asio::ssl::context m_clientContext{
        boost::asio::ssl::context::tlsv12_client};

    boost::asio::io_service::strand m_strand;
    boost::asio::ip::tcp::resolver m_resolver;
    boost::asio::ssl::stream<boost::asio::ip::tcp::socket> m_socket;
    std::vector<std::vector<unsigned char>> m_certificateChain;
};

} // namespace etls
} // namespace one

#endif // ONE_ETLS_TLS_SOCKET_HPP
