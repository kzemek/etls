/**
 * @file testServer.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testServer.hpp"

TestServer::TestServer(const unsigned short port)
    : m_work{m_ioService}
    , m_acceptor{m_ioService,
          boost::asio::ip::tcp::endpoint{boost::asio::ip::tcp::v4(), port}}
    , m_context{boost::asio::ssl::context::tlsv12_server}
    , m_thread{[=] { m_ioService.run(); }}
{
    m_context.set_options(boost::asio::ssl::context::default_workarounds);
    m_context.set_verify_mode(boost::asio::ssl::context::verify_none);
    m_context.use_certificate_chain_file("server.pem");
    m_context.use_private_key_file(
        "server.pem", boost::asio::ssl::context::pem);

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
        if (std::chrono::steady_clock::now() < start + timeout)
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

void TestServer::startAccept(boost::asio::yield_context yield)
{
    while (true) {
        auto socket = std::make_unique<SSLSocket>(m_ioService, m_context);

        m_acceptor.async_accept(socket->lowest_layer(), yield);
        socket->lowest_layer().set_option(boost::asio::ip::tcp::no_delay{});

        socket->async_handshake(boost::asio::ssl::stream_base::server, yield);
        m_sessions.emplace_back(std::move(socket));
        ++m_sessionsSize;
    }
}
