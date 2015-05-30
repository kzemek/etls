/**
 * @file tlsSocket_test.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testServer.hpp"
#include "testUtils.hpp"
#include "tlsSocket.hpp"

#include <boost/asio/io_service.hpp>
#include <boost/asio/ssl/context.hpp>
#include <gtest/gtest.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstring>
#include <string>
#include <thread>
#include <vector>

using namespace std::literals;
using namespace testing;

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
        socket->connectAsync(socket, host, port, [](auto) {}, [](auto) {});
        server.waitForConnection(5s);
    }
};

TEST_F(TLSSocketTest, shouldConnectToTheServer)
{
    socket->connectAsync(socket, host, port, [](auto) {}, [](auto) {});
    ASSERT_TRUE(server.waitForConnection(5s));
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionSuccess)
{
    std::atomic<bool> called{false};

    socket->connectAsync(
        socket, host, port, [&](auto) { called = true; }, [](auto) {});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionError)
{
    server.failConnection();
    std::atomic<bool> called{false};

    socket->connectAsync(
        socket, host, port, [](auto) {}, [&](auto) { called = true; });

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldSendMessages)
{
    const auto data = randomData();
    socket->sendAsync(socket, boost::asio::buffer(data), []() {}, [](auto) {});

    std::vector<char> received(data.size());
    server.receive(boost::asio::buffer(received));

    ASSERT_EQ(data, received);
}

TEST_F(TLSSocketTestC, shouldNotifyOnSuccessfulSend)
{
    std::atomic<bool> called{false};
    const auto data = randomData();

    socket->sendAsync(
        socket, boost::asio::buffer(data), [&] { called = true; }, [](auto) {});

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

TEST_F(TLSSocketTestC, shouldReceiveMessages)
{
    std::atomic<bool> called{false};

    const auto data = randomData();
    server.send(boost::asio::buffer(data));

    std::vector<char> received(data.size());
    socket->recvAsync(socket, boost::asio::buffer(received),
        [&](auto) { called = true; }, [](auto) {});

    waitFor(called);

    ASSERT_EQ(data, received);
}

TEST_F(TLSSocketTestC, shouldNotifyOnRecvSuccess)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    server.send(boost::asio::buffer(data));

    socket->recvAsync(socket, boost::asio::buffer(data),
        [&](auto) { called = true; }, [](auto) {});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldNotifyOnRecvError)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    server.fail();

    socket->recvAsync(socket, boost::asio::buffer(data), [](auto) {},
        [&](auto) { called = true; });

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldReceiveMessagesAsTheyCome)
{
    std::atomic<bool> called{false};
    boost::asio::mutable_buffer buffer;

    const auto data = randomData();
    server.send(boost::asio::buffer(data));

    std::vector<char> received(data.size() + 100);

    socket->recvAnyAsync(socket, boost::asio::buffer(received),
        [&](auto b) {
            buffer = b;
            called = true;
        },
        [](auto) {});

    waitFor(called);

    ASSERT_NE(data, received);
    ASSERT_LT(0u, boost::asio::buffer_size(buffer));
    ASSERT_EQ(0, memcmp(boost::asio::buffer_cast<char *>(buffer), data.data(),
                     boost::asio::buffer_size(buffer)));
}

TEST_F(TLSSocketTestC, shouldNotifyOnRecvAnySuccess)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    server.send(boost::asio::buffer(data));

    socket->recvAnyAsync(socket, boost::asio::buffer(data),
        [&](auto) { called = true; }, [](auto) {});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTest, shouldNotifyOnRecvAnyError)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    socket->recvAnyAsync(socket, boost::asio::buffer(data), [](auto) {},
        [&](auto) { called = true; });

    ASSERT_TRUE(waitFor(called));
}
