/**
 * @file wrapper.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "commonDefs.hpp"
#include "nifpp.h"
#include "tlsAcceptor.hpp"
#include "tlsApplication.hpp"
#include "tlsSocket.hpp"

#include <memory>
#include <tuple>

class Env {
public:
    Env()
        : env{enif_alloc_env(), enif_free_env}
    {
    }

    operator ErlNifEnv *() { return env.get(); }

private:
    std::shared_ptr<ErlNifEnv> env;
};

namespace {

nifpp::str_atom ok{"ok"};
nifpp::str_atom error{"error"};

one::etls::TLSApplication app;

one::etls::ErrorFun onError(Env localEnv, ErlNifPid pid, nifpp::TERM ref)
{
    return [=](std::string reason) mutable {
        auto message = nifpp::make(
            localEnv, std::make_tuple(ref, std::make_tuple(error, reason)));

        enif_send(nullptr, &pid, localEnv, message);
    };
}

one::etls::ErrorFun onError(Env localEnv, ErlNifPid pid)
{
    return [=](std::string reason) mutable {
        auto message = nifpp::make(localEnv, std::make_tuple(error, reason));
        enif_send(nullptr, &pid, localEnv, message);
    };
}

} // namespace

extern "C" {

static int load(ErlNifEnv *env, void ** /*priv*/, ERL_NIF_TERM /*load_info*/)
{
    nifpp::register_resource<one::etls::TLSSocket::Ptr>(
        env, nullptr, "TLSSocket");

    nifpp::register_resource<one::etls::TLSAcceptor::Ptr>(
        env, nullptr, "TLSAcceptor");

    return 0;
}

static ERL_NIF_TERM connect_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        Env localEnv;

        nifpp::TERM ref{enif_make_copy(localEnv, argv[0])};
        auto host = nifpp::get<std::string>(env, argv[1]);
        unsigned short port = nifpp::get<int>(env, argv[2]);

        ErlNifPid pid;
        enif_self(env, &pid);

        auto onSuccess = [=](one::etls::TLSSocket::Ptr socket) mutable {
            auto resource =
                nifpp::construct_resource<one::etls::TLSSocket::Ptr>(socket);

            auto message = nifpp::make(localEnv,
                std::make_tuple(ref, std::make_tuple(
                                         ok, nifpp::make(localEnv, resource))));

            enif_send(nullptr, &pid, localEnv, message);
        };

        auto sock = std::make_shared<one::etls::TLSSocket>(app.ioService());
        sock->connectAsync(sock, host, port, std::move(onSuccess),
            onError(localEnv, pid, ref));

        return nifpp::make(env, ok);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ERL_NIF_TERM send_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        Env localEnv;

        auto sock = *nifpp::get<one::etls::TLSSocket::Ptr *>(env, argv[0]);
        nifpp::TERM data{enif_make_copy(localEnv, argv[1])};

        ErlNifBinary bin;
        enif_inspect_iolist_as_binary(localEnv, data, &bin);

        boost::asio::const_buffer buffer{bin.data, bin.size};

        ErlNifPid pid;
        enif_self(env, &pid);

        auto onSuccess = [=]() mutable {
            auto message = nifpp::make(localEnv, ok);
            enif_send(nullptr, &pid, localEnv, message);
        };

        sock->sendAsync(
            sock, buffer, std::move(onSuccess), onError(localEnv, pid));

        return nifpp::make(env, ok);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ERL_NIF_TERM recv_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        Env localEnv;

        auto sock = *nifpp::get<one::etls::TLSSocket::Ptr *>(env, argv[0]);
        auto size = nifpp::get<std::size_t>(env, argv[1]);

        ErlNifPid pid;
        enif_self(env, &pid);

        auto bin = std::make_shared<nifpp::binary>(size == 0 ? 1024 : size);

        auto onSuccess = [=](boost::asio::mutable_buffer buffer) mutable {
            if (bin->size != boost::asio::buffer_size(buffer))
                enif_realloc_binary(
                    bin.get(), boost::asio::buffer_size(buffer));

            auto message = nifpp::make(
                localEnv, std::make_tuple(ok, nifpp::make(localEnv, *bin)));

            enif_send(nullptr, &pid, localEnv, message);
        };

        boost::asio::mutable_buffer buffer{bin->data, bin->size};
        if (size == 0) {
            sock->recvAnyAsync(
                sock, buffer, std::move(onSuccess), onError(localEnv, pid));
        }
        else {
            sock->recvAsync(
                sock, buffer, std::move(onSuccess), onError(localEnv, pid));
        }

        return nifpp::make(env, ok);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ERL_NIF_TERM listen_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        const unsigned short port = nifpp::get<int>(env, argv[0]);
        const auto certPath = nifpp::get<std::string>(env, argv[1]);
        const auto keyPath = nifpp::get<std::string>(env, argv[2]);

        auto acceptor = std::make_shared<one::etls::TLSAcceptor>(
            app.ioService(), port, certPath, keyPath);

        auto res =
            nifpp::construct_resource<one::etls::TLSAcceptor::Ptr>(acceptor);

        return nifpp::make(env, std::make_tuple(ok, res));
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ERL_NIF_TERM accept_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        Env localEnv;

        nifpp::TERM ref{enif_make_copy(localEnv, argv[0])};
        auto acceptor =
            *nifpp::get<one::etls::TLSAcceptor::Ptr *>(env, argv[1]);

        ErlNifPid pid;
        enif_self(env, &pid);

        auto onSuccess = [=](one::etls::TLSSocket::Ptr sock) mutable {
            auto resource =
                nifpp::construct_resource<one::etls::TLSSocket::Ptr>(sock);

            auto message = nifpp::make(localEnv,
                std::make_tuple(ref, std::make_tuple(
                                         ok, nifpp::make(localEnv, resource))));

            enif_send(nullptr, &pid, localEnv, message);
        };

        acceptor->acceptAsync(
            acceptor, std::move(onSuccess), onError(localEnv, pid, ref));

        return nifpp::make(env, ok);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ERL_NIF_TERM handshake_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        Env localEnv;

        nifpp::TERM ref{enif_make_copy(localEnv, argv[0])};
        auto sock = *nifpp::get<one::etls::TLSSocket::Ptr *>(env, argv[1]);

        ErlNifPid pid;
        enif_self(env, &pid);

        auto onSuccess = [=]() mutable {
            auto message = nifpp::make(localEnv, std::make_tuple(ref, ok));
            enif_send(nullptr, &pid, localEnv, message);
        };

        sock->handshakeAsync(
            sock, std::move(onSuccess), onError(localEnv, pid, ref));

        return nifpp::make(env, ok);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ERL_NIF_TERM peername_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        auto sock = *nifpp::get<one::etls::TLSSocket::Ptr *>(env, argv[0]);
        auto endpoint = sock->remoteEndpoint();
        auto address = endpoint.address().to_string();
        auto port = endpoint.port();

        return nifpp::make(
            env, std::make_tuple(ok, std::make_tuple(address, port)));
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ERL_NIF_TERM sockname_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        auto sock = *nifpp::get<one::etls::TLSSocket::Ptr *>(env, argv[0]);
        auto endpoint = sock->localEndpoint();
        auto address = endpoint.address().to_string();
        auto port = endpoint.port();

        return nifpp::make(
            env, std::make_tuple(ok, std::make_tuple(address, port)));
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

static ErlNifFunc nif_funcs[] = {{"connect", 3, connect_nif},
    {"send", 2, send_nif}, {"recv", 2, recv_nif}, {"listen", 3, listen_nif},
    {"accept", 2, accept_nif}, {"handshake", 2, handshake_nif},
    {"peername", 1, peername_nif}, {"sockname", 1, sockname_nif}};

ERL_NIF_INIT(ssl2_nif, nif_funcs, load, NULL, NULL, NULL)

} // extern C
