/**
 * @file wrapper.cpp
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "nifpp.h"
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
}

extern "C" {

static int load(ErlNifEnv *env, void ** /*priv*/, ERL_NIF_TERM /*load_info*/)
{
    nifpp::register_resource<one::etls::TLSSocket::Ptr>(
        env, nullptr, "TLSSocket");

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

        auto sock = std::make_shared<one::etls::TLSSocket>(app.ioService());

        auto onSuccess = [=]() mutable {
            auto resource =
                nifpp::construct_resource<one::etls::TLSSocket::Ptr>(sock);

            auto message = nifpp::make(localEnv,
                std::make_tuple(ref, std::make_tuple(
                                         ok, nifpp::make(localEnv, resource))));

            enif_send(nullptr, &pid, localEnv, message);
        };

        auto onError = [=](std::string reason) mutable {
            auto message = nifpp::make(
                localEnv, std::make_tuple(ref, std::make_tuple(error, reason)));

            enif_send(nullptr, &pid, localEnv, message);
        };

        sock->connectAsync(
            sock, host, port, std::move(onSuccess), std::move(onError));

        return nifpp::make(env, ok);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM send_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        Env localEnv;

        nifpp::TERM ref{enif_make_copy(localEnv, argv[0])};
        auto sock = *nifpp::get<one::etls::TLSSocket::Ptr *>(env, argv[1]);
        nifpp::TERM data{enif_make_copy(localEnv, argv[2])};

        ErlNifBinary bin;
        enif_inspect_iolist_as_binary(localEnv, data, &bin);

        boost::asio::const_buffer buffer{bin.data, bin.size};

        ErlNifPid pid;
        enif_self(env, &pid);

        auto onSuccess = [=]() mutable {
            auto message = nifpp::make(localEnv, std::make_tuple(ref, ok));
            enif_send(nullptr, &pid, localEnv, message);
        };

        auto onError = [=](std::string reason) mutable {
            auto message = nifpp::make(
                localEnv, std::make_tuple(ref, std::make_tuple(error, reason)));

            enif_send(nullptr, &pid, localEnv, message);
        };

        sock->sendAsync(sock, buffer, std::move(onSuccess), std::move(onError));
        return nifpp::make(env, ok);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"connect_nif", 3, connect_nif}, {"send_nif", 3, send_nif}};

ERL_NIF_INIT(tls, nif_funcs, load, NULL, NULL, NULL)

} // extern C
