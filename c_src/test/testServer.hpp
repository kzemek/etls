/**
 * @file testServer.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_TEST_SERVER_HPP
#define ONE_ETLS_TEST_SERVER_HPP

#include <boost/asio/read.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/context.hpp>
#include <boost/asio/ssl/stream.hpp>

#include <atomic>
#include <chrono>
#include <memory>
#include <thread>

namespace {
boost::asio::ssl::context createContext()
{
    boost::asio::ssl::context context{boost::asio::ssl::context::tlsv12_server};
    context.set_options(boost::asio::ssl::context::default_workarounds);
    context.set_verify_mode(boost::asio::ssl::context::verify_none);
    context.use_certificate_chain_file("server.pem");
    context.use_private_key_file("server.key", boost::asio::ssl::context::pem);

    return context;
}
}

class TestServer {
    using SSLSocket = boost::asio::ssl::stream<boost::asio::ip::tcp::socket>;

public:
    TestServer(const unsigned short port);
    ~TestServer();

    bool waitForConnection(std::chrono::milliseconds timeout);
    void send(boost::asio::const_buffer buffer);
    void receive(boost::asio::mutable_buffer buffer);
    void failConnection();
    void fail();

private:
    void accept();

    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_work;
    boost::asio::ip::tcp::acceptor m_acceptor;
    boost::asio::ssl::context m_context;
    std::thread m_thread;
    SSLSocket m_session;

    std::atomic<bool> m_connected{false};
    std::atomic<bool> m_failConnection{false};
};

TestServer::TestServer(const unsigned short port)
    : m_work{m_ioService}
    , m_acceptor{m_ioService,
          boost::asio::ip::tcp::endpoint{boost::asio::ip::tcp::v4(), port}}
    , m_context{createContext()}
    , m_session{m_ioService, m_context}
{
    m_acceptor.set_option(boost::asio::ip::tcp::acceptor::reuse_address(true));

    m_thread = std::thread{[this] {
        try {
            m_ioService.run();
        }
        catch (...) {
        }
    }};

    accept();
}

TestServer::~TestServer()
{
    m_ioService.stop();
    m_thread.join();
}

bool TestServer::waitForConnection(std::chrono::milliseconds timeout)
{
    const auto start = std::chrono::steady_clock::now();
    while (!m_connected) {
        if (std::chrono::steady_clock::now() > start + timeout)
            return false;

        std::this_thread::yield();
    }

    return true;
}

void TestServer::send(boost::asio::const_buffer buffer)
{
    std::atomic<bool> done{false};
    m_ioService.post([&, this] {
        boost::asio::async_write(m_session,
            boost::asio::const_buffers_1{buffer},
            [&](auto, auto) { done = true; });
    });

    while (!done)
        std::this_thread::yield();
}

void TestServer::receive(boost::asio::mutable_buffer buffer)
{
    std::atomic<bool> done{false};
    m_ioService.post([&, this] {
        boost::asio::async_read(m_session,
            boost::asio::mutable_buffers_1{buffer},
            [&](auto, auto) { done = true; });
    });

    while (!done)
        std::this_thread::yield();
}

void TestServer::failConnection() { m_failConnection = true; }

void TestServer::fail() { m_session.lowest_layer().close(); }

void TestServer::accept()
{
    m_ioService.post([this] {
        m_acceptor.async_accept(m_session.lowest_layer(), [this](auto ec) {
            this->m_session.lowest_layer().set_option(
                boost::asio::ip::tcp::no_delay{true});

            if (this->m_failConnection) {
                this->m_session.lowest_layer().close();
                return;
            }

            this->m_session.async_handshake(
                boost::asio::ssl::stream_base::server,
                [this](auto ec) { this->m_connected = true; });
        });
    });
}

#endif // ONE_ETLS_TEST_SERVER_HPP
