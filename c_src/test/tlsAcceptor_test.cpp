/**
 * @file tlsAcceptor_test.hpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "testUtils.hpp"
#include "tlsAcceptor.hpp"
#include "tlsSocket.hpp"

#include <boost/asio/io_service.hpp>
#include <boost/asio/ssl/context.hpp>
#include <gtest/gtest.h>

#include <thread>

using namespace testing;

struct TLSAcceptorTest : public Test {
    std::string host{"127.0.0.1"};
    unsigned short port{randomPort()};

    boost::asio::io_service ioService;
    boost::asio::io_service::work work{ioService};

    one::etls::TLSAcceptor::Ptr acceptor;
    std::thread thread;

    TLSAcceptorTest()
        : acceptor{std::make_shared<one::etls::TLSAcceptor>(ioService, port)}
    {
        thread = std::thread{[=] { ioService.run(); }};
    }

    ~TLSAcceptorTest()
    {
        ioService.stop();
        thread.join();
    }
};

TEST_F(TLSAcceptorTest, shouldAcceptConnections)
{
    std::atomic<bool> acceptCalled{false};
    std::atomic<bool> connectCalled{false};

    one::etls::TLSSocket::Ptr ssock;

    acceptor->acceptAsync(acceptor, [&](one::etls::TLSSocket::Ptr s) {
        ssock = std::move(s);
        acceptCalled = true;
    });

    auto csock = std::make_shared<one::etls::TLSSocket>(ioService);
    csock->connectAsync(csock, host, port,
        [&](one::etls::TLSSocket::Ptr) { connectCalled = true; }, [](auto) {});

    ASSERT_TRUE(waitFor(acceptCalled));
}
