/**
 * @file tlsSocket_test.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testServer.hpp"
#include "testUtils.hpp"
#include "tlsApplication.hpp"
#include "tlsSocket.hpp"

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

    one::etls::TLSApplication app;
    one::etls::TLSSocket::Ptr socket;

    TLSSocketTest()
        : socket{std::make_shared<one::etls::TLSSocket>(app)}
    {
    }
};

struct TLSSocketTestC : public TLSSocketTest {
    TLSSocketTestC()
    {
        std::atomic<bool> connected{false};
        socket->connectAsync(
            socket, host, port, {[&](auto) { connected = true; }, [](auto) {}});
        waitFor(connected);
    }
};

TEST_F(TLSSocketTest, shouldConnectToTheServer)
{
    socket->connectAsync(socket, host, port, {[](auto) {}, [](auto) {}});
    ASSERT_TRUE(server.waitForConnection(5s));
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionSuccess)
{
    std::atomic<bool> called{false};

    socket->connectAsync(
        socket, host, port, {[&](auto) { called = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTest, shouldNotifyOnConnectionError)
{
    server.failConnection();
    std::atomic<bool> called{false};

    socket->connectAsync(
        socket, host, port, {[](auto) {}, [&](auto) { called = true; }});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldSendMessages)
{
    const auto data = randomData();
    socket->sendAsync(socket, asio::buffer(data), {[] {}, [](auto) {}});

    std::vector<char> received(data.size());
    server.receive(asio::buffer(received));

    ASSERT_EQ(data, received);
}

TEST_F(TLSSocketTestC, shouldNotifyOnSuccessfulSend)
{
    std::atomic<bool> called{false};
    const auto data = randomData();

    socket->sendAsync(
        socket, asio::buffer(data), {[&] { called = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldNotifyOnSendError)
{
    std::atomic<bool> closed{false};
    std::atomic<bool> called{false};
    const auto data = randomData();

    socket->closeAsync(socket, {[&] { closed = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(closed));

    socket->sendAsync(
        socket, asio::buffer(data), {[] {}, [&](auto) { called = true; }});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldReceiveMessages)
{
    std::atomic<bool> called{false};

    const auto data = randomData();
    server.send(asio::buffer(data));

    std::vector<char> received(data.size());
    socket->recvAsync(socket, asio::buffer(received),
        {[&](auto) { called = true; }, [](auto) {}});

    waitFor(called);

    ASSERT_EQ(data, received);
}

TEST_F(TLSSocketTestC, shouldNotifyOnRecvSuccess)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    server.send(asio::buffer(data));

    socket->recvAsync(socket, asio::buffer(data),
        {[&](auto) { called = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldNotifyOnRecvError)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    server.fail();

    socket->recvAsync(socket, asio::buffer(data),
        {[](auto) {}, [&](auto) { called = true; }});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldReceiveMessagesAsTheyCome)
{
    std::atomic<bool> called{false};
    asio::mutable_buffer buffer;

    const auto data = randomData();
    server.send(asio::buffer(data));

    std::vector<char> received(data.size() + 100);

    socket->recvAnyAsync(socket, asio::buffer(received), {[&](auto b) {
        buffer = b;
        called = true;
    },
                                                          [](auto) {}});

    waitFor(called);

    ASSERT_NE(data, received);
    ASSERT_LT(0u, asio::buffer_size(buffer));
    ASSERT_EQ(0, memcmp(asio::buffer_cast<char *>(buffer), data.data(),
                     asio::buffer_size(buffer)));
}

TEST_F(TLSSocketTestC, shouldNotifyOnRecvAnySuccess)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    server.send(asio::buffer(data));

    socket->recvAnyAsync(socket, asio::buffer(data),
        {[&](auto) { called = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTest, shouldNotifyOnRecvAnyError)
{
    std::atomic<bool> called{false};
    auto data = randomData();

    socket->recvAnyAsync(socket, asio::buffer(data),
        {[](auto) {}, [&](auto) { called = true; }});

    ASSERT_TRUE(waitFor(called));
}

TEST_F(TLSSocketTestC, shouldReturnLocalEndpoint)
{
    asio::ip::tcp::endpoint endpoint;
    std::atomic<bool> called{false};

    socket->localEndpointAsync(socket, {[&](auto e) {
        endpoint = e;
        called = true;
    },
                                        [](auto) {}});

    ASSERT_TRUE(waitFor(called));
    ASSERT_EQ("127.0.0.1", endpoint.address().to_string());
    ASSERT_NE(port, endpoint.port());
}

TEST_F(TLSSocketTestC, shouldReturnRemoteEndpoint)
{
    asio::ip::tcp::endpoint endpoint;
    std::atomic<bool> called{false};

    socket->remoteEndpointAsync(socket, {[&](auto e) {
        endpoint = e;
        called = true;
    },
                                         [](auto) {}});

    ASSERT_TRUE(waitFor(called));
    ASSERT_EQ("127.0.0.1", endpoint.address().to_string());
    ASSERT_EQ(port, endpoint.port());
}

TEST_F(TLSSocketTestC, shouldBeShutdownable)
{
    std::atomic<bool> shutdownCalled{false};
    socket->shutdownAsync(socket, asio::socket_base::shutdown_send,
        {[&]() { shutdownCalled = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(shutdownCalled));

    std::atomic<bool> sendCalled{false};
    auto data = randomData();

    socket->sendAsync(socket, asio::buffer(data),
        {[] {},
         [&](auto ec) {
             ASSERT_EQ(std::string{"Broken pipe"}, std::string{ec.message()});
             sendCalled = true;
         }});

    ASSERT_TRUE(waitFor(sendCalled));
}
