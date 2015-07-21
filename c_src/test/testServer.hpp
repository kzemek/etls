/**
 * @file testServer.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONE_ETLS_TEST_SERVER_HPP
#define ONE_ETLS_TEST_SERVER_HPP

#include <asio/read.hpp>
#include <asio/io_service.hpp>
#include <asio/ip/tcp.hpp>
#include <asio/ssl/context.hpp>
#include <asio/ssl/stream.hpp>

#include <atomic>
#include <chrono>
#include <iostream>
#include <memory>
#include <thread>

namespace {
asio::ssl::context createContext()
{
    asio::ssl::context context{asio::ssl::context::tlsv12_server};
    context.set_options(asio::ssl::context::default_workarounds);
    context.set_verify_mode(asio::ssl::context::verify_none);
    context.use_certificate_chain_file("server.pem");
    context.use_private_key_file("server.key", asio::ssl::context::pem);

    return context;
}
}

class TestServer {
    using SSLSocket = asio::ssl::stream<asio::ip::tcp::socket>;

public:
    TestServer(const unsigned short port);
    ~TestServer();

    bool waitForConnection(std::chrono::milliseconds timeout);
    void send(asio::const_buffer buffer);
    void receive(asio::mutable_buffer buffer);
    void failConnection();
    void fail();

private:
    void accept();

    asio::io_service m_ioService;
    asio::io_service::work m_work;
    asio::ip::tcp::acceptor m_acceptor;
    asio::ssl::context m_context;
    std::thread m_thread;
    SSLSocket m_session;

    std::atomic<bool> m_connected{false};
    std::atomic<bool> m_failConnection{false};
};

TestServer::TestServer(const unsigned short port)
    : m_work{m_ioService}
    , m_acceptor{m_ioService,
          asio::ip::tcp::endpoint{asio::ip::tcp::v4(), port}}
    , m_context{createContext()}
    , m_session{m_ioService, m_context}
{
    m_acceptor.set_option(asio::ip::tcp::acceptor::reuse_address(true));
    m_thread = std::thread{[this] {
        try {
            m_ioService.run();
        }
        catch (const std::exception &e) {
            std::cerr << e.what() << std::endl;
            throw;
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

        std::this_thread::sleep_for(std::chrono::milliseconds{1});
    }

    return true;
}

void TestServer::send(asio::const_buffer buffer)
{
    std::atomic<bool> done{false};
    asio::post(m_ioService, [&] {
        asio::async_write(
            m_session, asio::const_buffers_1{buffer}, [&](auto ec, auto) {
                if (ec)
                    throw std::system_error{ec};

                done = true;
            });
    });

    while (!done)
        std::this_thread::sleep_for(std::chrono::milliseconds{1});
}

void TestServer::receive(asio::mutable_buffer buffer)
{
    std::atomic<bool> done{false};
    asio::post(m_ioService, [&] {
        asio::async_read(
            m_session, asio::mutable_buffers_1{buffer}, [&](auto ec, auto) {
                if (ec)
                    throw std::system_error{ec};

                done = true;
            });
    });

    while (!done)
        std::this_thread::sleep_for(std::chrono::milliseconds{1});
}

void TestServer::failConnection() { m_failConnection = true; }

void TestServer::fail() { m_session.lowest_layer().close(); }

void TestServer::accept()
{
    asio::post(m_ioService, [this] {
        m_acceptor.async_accept(
            m_session.lowest_layer(), [this](const std::error_code &ec) {
                if (ec)
                    throw std::system_error{ec};

                m_session.lowest_layer().set_option(
                    asio::ip::tcp::no_delay{true});

                if (m_failConnection) {
                    m_session.lowest_layer().close();
                    return;
                }

                m_session.async_handshake(asio::ssl::stream_base::server,
                    [this](const std::error_code &ec1) {
                        if (ec1)
                            throw std::system_error{ec1};

                        m_connected = true;
                    });
            });
    });
}

#endif // ONE_ETLS_TEST_SERVER_HPP
