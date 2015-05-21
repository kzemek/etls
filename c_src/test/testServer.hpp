/**
 * @file testServer.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ERLANG_TLS_TEST_SERVER_HPP
#define ERLANG_TLS_TEST_SERVER_HPP

#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/context.hpp>
#include <boost/asio/ssl/stream.hpp>
#include <boost/asio/spawn.hpp>

#include <atomic>
#include <chrono>
#include <memory>
#include <thread>
#include <vector>

class TestServer {
    using SSLSocket = boost::asio::ssl::stream<boost::asio::ip::tcp::socket>;

public:
    TestServer(const unsigned short port);
    ~TestServer();

    bool waitForConnections(
        std::size_t number, std::chrono::milliseconds timeout);

    void send(boost::asio::const_buffer buffer);
    void receive(boost::asio::mutable_buffer buffer);

private:
    void startAccept(boost::asio::yield_context yield);

    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_work;
    boost::asio::ip::tcp::acceptor m_acceptor;
    boost::asio::ssl::context m_context;
    std::thread m_thread;
    std::vector<std::unique_ptr<SSLSocket>> m_sessions;

    std::atomic<std::size_t> m_sessionsSize{0};
};

#endif // ERLANG_TLS_TEST_SERVER_HPP
