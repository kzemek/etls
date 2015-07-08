/**
 * @file bandwidthCap.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 *
 * This file contains a simple benchmark of possible single-connection bandwidth
 * and message throughput using ASIO + SSL.
 * Client and server sockets are run from dedicated threads. The time is
 * measured between [start sending messages] and [stop receiving message].
 * Bandwidth is measured on a relatively small number of 100 MB messages,
 * message throughput is measured by sending a large number of 1 B messages.
 */

#include <asio.hpp>
#include <asio/ssl.hpp>

#include <atomic>
#include <chrono>
#include <thread>
#include <vector>
#include <string>
#include <future>
#include <iostream>
#include <functional>

using namespace asio;
using namespace std::literals;

template <typename F> auto withIoService(F &&f)
{
    io_service ioService{1};
    io_service::work work{ioService};
    std::thread thread{[&] { ioService.run(); }};

    auto r = f(ioService);

    ioService.stop();
    thread.join();

    return r;
}

auto writer(std::size_t messageSize, std::size_t messages)
{
    return withIoService([&](auto &ioService) {
        ssl::context context{ssl::context::tlsv12_client};
        ssl::stream<ip::tcp::socket> socket{ioService, context};

        ip::tcp::resolver resolver{ioService};
        ip::tcp::resolver::query query{"localhost", "5555"};
        ip::tcp::resolver::iterator iterator = resolver.resolve(query);

        std::vector<char> data(messageSize, 'a');

        connect(socket.lowest_layer(), iterator);
        socket.handshake(ssl::stream_base::client);

        auto ret = std::chrono::steady_clock::now();
        for (auto i = 0u; i < messages; ++i)
            write(socket, buffer(data));

        return ret;
    });
}

auto reader(
    std::size_t messageSize, std::size_t messages, std::atomic<bool> &listening)
{
    return withIoService([&](auto &ioService) {
        ssl::context context{ssl::context::tlsv12_server};
        context.use_certificate_file("server.pem", ssl::context::pem);
        context.use_private_key_file("server.key", ssl::context::pem);

        ssl::stream<ip::tcp::socket> socket{ioService, context};

        ip::tcp::acceptor acceptor{
            ioService, ip::tcp::endpoint(ip::tcp::v4(), 5555)};

        listening = true;

        acceptor.accept(socket.lowest_layer());

        socket.handshake(ssl::stream_base::server);

        std::vector<char> data(messageSize);
        for (auto i = 0u; i < messages; ++i)
            read(socket, buffer(data));

        return std::chrono::steady_clock::now();
    });
}

constexpr std::size_t b = 1;
constexpr std::size_t kb = 1024 * b;
constexpr std::size_t mb = 1024 * kb;
constexpr std::size_t gb = 1024 * mb;

std::chrono::seconds measure(size_t messageSize, size_t send)
{
    std::atomic<bool> listening{false};
    const auto messages = send / messageSize;

    auto stoppedReadingF = std::async(std::launch::async,
        [&] { return reader(messageSize, messages, listening); });

    while (!listening)
        std::this_thread::yield();

    auto startWritingF = std::async(
        std::launch::async, [&] { return writer(messageSize, messages); });

    const auto duration = stoppedReadingF.get() - startWritingF.get();
    const auto seconds =
        std::chrono::duration_cast<std::chrono::seconds>(duration);
    return seconds;
}

int main()
{
    auto bandwidthTestSize = 20 * gb;
    auto bandwidthSeconds = measure(100 * mb, bandwidthTestSize);
    std::cout << static_cast<long double>(bandwidthTestSize / mb) /
            bandwidthSeconds.count() << " MB/s" << std::endl;

    auto messagesTestSize = 1 * mb;
    auto messagesSeconds = measure(1 * b, messagesTestSize);
    std::cout << static_cast<long double>(messagesTestSize) /
            messagesSeconds.count() << " messages/s" << std::endl;
}
