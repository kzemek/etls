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
    void fail();

private:
    void startAccept(boost::asio::yield_context yield);

    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_work;
    boost::asio::ip::tcp::acceptor m_acceptor;
    boost::asio::ssl::context m_context;
    std::thread m_thread;
    std::vector<std::unique_ptr<SSLSocket>> m_sessions;

    std::atomic<std::size_t> m_sessionsSize{0};
    std::atomic<bool> m_fail{false};
};

TestServer::TestServer(const unsigned short port)
    : m_work{m_ioService}
    , m_acceptor{m_ioService,
          boost::asio::ip::tcp::endpoint{boost::asio::ip::tcp::v4(), port}}
    , m_context{boost::asio::ssl::context::tlsv12_server}
{
    m_acceptor.set_option(boost::asio::ip::tcp::acceptor::reuse_address(true));

    m_context.set_options(boost::asio::ssl::context::default_workarounds);
    m_context.set_verify_mode(boost::asio::ssl::context::verify_none);
    m_context.use_certificate_chain_file("server.pem");
    m_context.use_private_key_file(
        "server.key", boost::asio::ssl::context::pem);

    m_thread = std::thread{[this] {
        try {
            m_ioService.run();
        }
        catch (...) {
        }
    }};

    boost::asio::spawn(m_ioService, [=](auto y) { this->startAccept(y); });
}

TestServer::~TestServer()
{
    m_ioService.stop();
    m_thread.join();
}

bool TestServer::waitForConnections(
    std::size_t number, std::chrono::milliseconds timeout)
{
    const auto start = std::chrono::steady_clock::now();
    while (m_sessionsSize < number) {
        if (std::chrono::steady_clock::now() > start + timeout)
            return false;

        std::this_thread::yield();
    }

    return true;
}

void TestServer::send(boost::asio::const_buffer buffer)
{
    std::atomic<bool> done{false};
    boost::asio::spawn(m_ioService, [&, this](auto yield) mutable {
        for (auto &session : this->m_sessions) {
            boost::asio::async_write(
                *session, boost::asio::const_buffers_1{buffer}, yield);
        }

        done = true;
    });

    while (!done)
        std::this_thread::yield();
}

void TestServer::receive(boost::asio::mutable_buffer buffer)
{
    std::atomic<bool> done{false};
    boost::asio::spawn(m_ioService, [&, this](auto yield) mutable {
        boost::asio::async_write(*this->m_sessions.front(),
            boost::asio::mutable_buffers_1{buffer}, yield);

        done = true;
    });

    while (!done)
        std::this_thread::yield();
}

void TestServer::fail() { m_fail = true; }

void TestServer::startAccept(boost::asio::yield_context yield)
{
    while (true) {
        auto socket = std::make_unique<SSLSocket>(m_ioService, m_context);

        m_acceptor.async_accept(socket->lowest_layer(), yield);
        socket->lowest_layer().set_option(boost::asio::ip::tcp::no_delay{});

        if (m_fail) {
            socket->shutdown();
            continue;
        }

        socket->async_handshake(boost::asio::ssl::stream_base::server, yield);
        m_sessions.emplace_back(std::move(socket));
        ++m_sessionsSize;
    }
}

#endif // ERLANG_TLS_TEST_SERVER_HPP
