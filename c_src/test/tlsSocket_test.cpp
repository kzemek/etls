/**
 * @file tlsSocket_test.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testServer.hpp"
#include "tlsSocket.hpp"

#include <boost/asio/io_service.hpp>
#include <boost/asio/ssl/context.hpp>
#include <gtest/gtest.h>

#include <atomic>
#include <chrono>
#include <string>
#include <thread>

using namespace std::literals;
using namespace testing;

unsigned short randomPort()
{
    static thread_local std::random_device rd;
    static thread_local std::default_random_engine engine{rd()};
    static thread_local std::uniform_int_distribution<unsigned short> dist{
        49152, 65535};

    return dist(engine);
}

struct TLSSocketTest : public Test {
    std::string host{"127.0.0.1"};
    unsigned short port{randomPort()};
    TestServer server{port};

    boost::asio::io_service ioService;
    boost::asio::io_service::work work{ioService};
    boost::asio::ssl::context context{boost::asio::ssl::context::tlsv12_client};
    one::etls::TLSSocket::Ptr socket;
    std::thread thread;

    TLSSocketTest()
        : socket{std::make_shared<one::etls::TLSSocket>(ioService)}
    {
        thread = std::thread{[this] { ioService.run(); }};
    }

    ~TLSSocketTest()
    {
        ioService.stop();
        thread.join();
    }
};

TEST_F(TLSSocketTest, shouldConnectToTheServer)
{
    socket->connectAsync(socket, "127.0.0.1"s, port, [](auto) {}, [](auto) {});
    ASSERT_TRUE(server.waitForConnections(1, 5s));
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionSuccess)
{
    std::atomic<bool> called{false};

    socket->connectAsync(
        socket, "127.0.0.1"s, port, [&](auto) { called = true; }, [](auto) {});

    const auto timeout = std::chrono::steady_clock::now() + 5s;
    while (!called && std::chrono::steady_clock::now() < timeout)
        std::this_thread::yield();

    ASSERT_TRUE(called);
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionError)
{
    server.fail();
    std::atomic<bool> called{false};

    socket->connectAsync(
        socket, "127.0.0.1"s, port, [](auto) {}, [&](auto) { called = true; });

    const auto timeout = std::chrono::steady_clock::now() + 5s;
    while (!called && std::chrono::steady_clock::now() < timeout)
        std::this_thread::yield();

    ASSERT_TRUE(called);
}
