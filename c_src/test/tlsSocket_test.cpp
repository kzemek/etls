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

#include <algorithm>
#include <atomic>
#include <chrono>
#include <string>
#include <thread>
#include <vector>

using namespace std::literals;
using namespace testing;

namespace {

std::default_random_engine &engine()
{
    thread_local std::random_device rd;
    thread_local std::default_random_engine engine{rd()};
    return engine;
}

unsigned short randomPort()
{
    static thread_local std::uniform_int_distribution<unsigned short> dist{
        49152, 65535};
    return dist(engine());
}

std::vector<char> randomData()
{
    static thread_local std::uniform_int_distribution<std::size_t> lenDist{
        1, 255};
    static thread_local std::uniform_int_distribution<char> dist;

    const auto len = lenDist(engine());
    std::vector<char> data;
    data.reserve(len);
    std::generate_n(
        std::back_inserter(data), len, [&] { return dist(engine()); });

    return data;
}

bool waitFor(std::atomic<bool> &predicate)
{
    const auto timeout = std::chrono::steady_clock::now() + 5s;
    while (!predicate && std::chrono::steady_clock::now() < timeout)
        std::this_thread::yield();

    return predicate;
}

} // namespace

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

struct TLSSocketTestC : public TLSSocketTest {
    TLSSocketTestC()
    {
        socket->connectAsync(socket, "127.0.0.1"s, port);
        server.waitForConnections(1, 5s);
    }
};

TEST_F(TLSSocketTest, shouldConnectToTheServer)
{
    socket->connectAsync(socket, "127.0.0.1"s, port);
    ASSERT_TRUE(server.waitForConnections(1, 5s));
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionSuccess)
{
    std::atomic<bool> called{false};

    socket->connectAsync(socket, "127.0.0.1"s, port, [&] { called = true; });
    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionError)
{
    server.failConnection();
    std::atomic<bool> called{false};

    socket->connectAsync(
        socket, "127.0.0.1"s, port, [] {}, [&](auto) { called = true; });

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldSendMessages)
{
    const auto data = randomData();
    socket->sendAsync(socket, boost::asio::buffer(data));

    std::vector<char> received(data.size());
    server.receive(boost::asio::buffer(received));

    ASSERT_EQ(data, received);
}

TEST_F(TLSSocketTestC, shouldNotifyOnSuccessfulSend)
{
    std::atomic<bool> called{false};
    const auto data = randomData();

    socket->sendAsync(
        socket, boost::asio::buffer(data), [&] { called = true; });

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldNotifyOnSendError)
{
    std::atomic<bool> called{false};
    const auto data = randomData();

    socket->close();
    socket->sendAsync(
        socket, boost::asio::buffer(data), [] {}, [&](auto) { called = true; });

    ASSERT_TRUE(waitFor(called));
}
