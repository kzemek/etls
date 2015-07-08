/**
 * @file tlsAcceptor_test.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "callback.hpp"
#include "testUtils.hpp"
#include "tlsAcceptor.hpp"
#include "tlsApplication.hpp"
#include "tlsSocket.hpp"

#include <gtest/gtest.h>

#include <thread>
#include <vector>

using namespace testing;

template <typename... Args, typename SF>
one::etls::Callback<Args...> successCallback(SF &&success)
{
    return {std::forward<SF>(success), [](auto) { FAIL(); }};
}

template <typename... Args, typename EF>
one::etls::Callback<Args...> errorCallback(EF &&error)
{
    return {{}, std::forward<EF>(error)};
}

struct TLSAcceptorTest : public Test {
    std::string host{"127.0.0.1"};
    unsigned short port{randomPort()};

    one::etls::TLSApplication app{1};
    one::etls::TLSAcceptor::Ptr acceptor;

    TLSAcceptorTest()
        : acceptor{std::make_shared<one::etls::TLSAcceptor>(
              app, port, "server.pem", "server.key")}
    {
    }
};

struct TLSAcceptorTestC : public TLSAcceptorTest {
    one::etls::TLSSocket::Ptr ssock;
    one::etls::TLSSocket::Ptr csock;

    TLSAcceptorTestC()
        : csock{std::make_shared<one::etls::TLSSocket>(app)}
    {
        std::atomic<bool> connectCalled{false};
        std::atomic<bool> handshakeCalled{false};

        acceptor->acceptAsync(acceptor, {[&](one::etls::TLSSocket::Ptr s) {
            s->handshakeAsync(s, {[&, s] {
                ssock = s;
                handshakeCalled = true;
            },
                                  [](auto) {}});
        },
                                         [](auto) {}});

        csock->connectAsync(csock, host, port,
            {[&](one::etls::TLSSocket::Ptr) { connectCalled = true; },
             [](auto) {}});

        waitFor(connectCalled);
        waitFor(handshakeCalled);
    }
};

TEST_F(TLSAcceptorTest, shouldAcceptConnections)
{
    std::atomic<bool> acceptCalled{false};

    one::etls::TLSSocket::Ptr ssock;

    acceptor->acceptAsync(acceptor, {[&](one::etls::TLSSocket::Ptr s) {
        ssock = std::move(s);
        acceptCalled = true;
    },
                                     [](auto) {}});

    auto csock = std::make_shared<one::etls::TLSSocket>(app);
    csock->connectAsync(csock, host, port, {[](auto) {}, [](auto) {}});

    ASSERT_TRUE(waitFor(acceptCalled));
}

TEST_F(TLSAcceptorTest, shouldReturnHandshakableSockets)
{
    std::atomic<bool> connectCalled{false};
    std::atomic<bool> handshakeCalled{false};

    acceptor->acceptAsync(acceptor, {[&](one::etls::TLSSocket::Ptr s) {
        s->handshakeAsync(s, {[&] { handshakeCalled = true; }, [](auto) {}});
    },
                                     [](auto) {}});

    auto csock = std::make_shared<one::etls::TLSSocket>(app);
    csock->connectAsync(csock, host, port,
        {[&](one::etls::TLSSocket::Ptr) { connectCalled = true; },
         [](auto) {}});

    ASSERT_TRUE(waitFor(connectCalled));
    ASSERT_TRUE(waitFor(handshakeCalled));
}

TEST_F(TLSAcceptorTestC, shouldReturnServer_ClientCommunicableSockets)
{
    std::atomic<bool> dataReceived{false};

    const auto sentData = randomData();
    auto recvData = std::vector<char>(sentData.size());

    ssock->sendAsync(ssock, asio::buffer(sentData), {[] {}, [](auto) {}});
    csock->recvAsync(csock, asio::buffer(recvData),
        {[&](auto) { dataReceived = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(dataReceived));
    ASSERT_EQ(recvData, sentData);
}

TEST_F(TLSAcceptorTestC, shouldReturnClient_ServerCommunicableSockets)
{
    std::atomic<bool> dataReceived{false};

    const auto sentData = randomData();
    auto recvData = std::vector<char>(sentData.size());

    csock->sendAsync(csock, asio::buffer(sentData), {[] {}, [](auto) {}});
    ssock->recvAsync(ssock, asio::buffer(recvData),
        {[&](auto) { dataReceived = true; }, [](auto) {}});

    ASSERT_TRUE(waitFor(dataReceived));
    ASSERT_EQ(recvData, sentData);
}

TEST_F(TLSAcceptorTest, shouldReturnLocalEndpoint)
{
    asio::ip::tcp::endpoint endpoint;
    std::atomic<bool> called{false};

    acceptor->localEndpointAsync(acceptor, {[&](auto e) {
        endpoint = e;
        called = true;
    },
                                            [](auto) {}});

    ASSERT_TRUE(waitFor(called));
    ASSERT_EQ("0.0.0.0", endpoint.address().to_string());
    ASSERT_EQ(port, endpoint.port());
}
