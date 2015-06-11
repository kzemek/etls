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

#include <boost/asio/error.hpp>

#include <algorithm>
#include <memory>
#include <string>
#include <tuple>
#include <vector>
#include <unordered_map>

using namespace std::literals;

/**
 * A shared pointer wrapper to help with Erlang NIF environment management.
 */
class Env {
public:
    /**
     * Creates a new environment by creating a @c shared_ptr with a custom
     * deleter.
     */
    Env()
        : env{enif_alloc_env(), enif_free_env}
    {
    }

    /**
     * Implicit conversion operator to @cErlNifEnv* .
     */
    operator ErlNifEnv *() { return env.get(); }

private:
    std::shared_ptr<ErlNifEnv> env;
};

namespace {

using basic_errors = boost::asio::error::basic_errors;
using netdb_errors = boost::asio::error::netdb_errors;
using addrinfo_errors = boost::asio::error::addrinfo_errors;
using misc_errors = boost::asio::error::misc_errors;

/**
 * @defgroup StaticAtoms Statically created atoms for ease of usage.
 * @{
 */
nifpp::str_atom ok{"ok"};
nifpp::str_atom error{"error"};
nifpp::str_atom unknown_error{"unknown_error"};
/** @} */

/**
 * The @c TLSApplication object has a static lifetime; it will live as long
 * as the shared library is loaded into the memory and can be simultaneously
 * used by multiple multi-threaded applications.
 * The object has no external dependencies, including any lifetime dependencies.
 */
one::etls::TLSApplication app;

/**
 * An error code - atom map used to return atom errors for basic error types.
 */
const std::unordered_map<int, nifpp::str_atom> basicErrorAtom{
    {basic_errors::access_denied, {"access_denied"}},
    {basic_errors::address_family_not_supported,
     {"address_family_not_supported"}},
    {basic_errors::address_in_use, {"address_in_use"}},
    {basic_errors::already_connected, {"already_connected"}},
    {basic_errors::already_started, {"already_started"}},
    {basic_errors::broken_pipe, {"broken_pipe"}},
    {basic_errors::connection_aborted, {"connection_aborted"}},
    {basic_errors::connection_refused, {"connection_refused"}},
    {basic_errors::connection_reset, {"connection_reset"}},
    {basic_errors::bad_descriptor, {"bad_descriptor"}},
    {basic_errors::fault, {"fault"}},
    {basic_errors::host_unreachable, {"host_unreachable"}},
    {basic_errors::in_progress, {"in_progress"}},
    {basic_errors::interrupted, {"interrupted"}},
    {basic_errors::invalid_argument, {"invalid_argument"}},
    {basic_errors::message_size, {"message_size"}},
    {basic_errors::name_too_long, {"name_too_long"}},
    {basic_errors::network_down, {"network_down"}},
    {basic_errors::network_reset, {"network_reset"}},
    {basic_errors::network_unreachable, {"network_unreachable"}},
    {basic_errors::no_descriptors, {"no_descriptors"}},
    {basic_errors::no_buffer_space, {"no_buffer_space"}},
    {basic_errors::no_memory, {"no_memory"}},
    {basic_errors::no_permission, {"no_permission"}},
    {basic_errors::no_protocol_option, {"no_protocol_option"}},
    {basic_errors::not_connected, {"not_connected"}},
    {basic_errors::not_socket, {"not_socket"}},
    {basic_errors::operation_aborted, {"operation_aborted"}},
    {basic_errors::operation_not_supported, {"operation_not_supported"}},
    {basic_errors::shut_down, {"shut_down"}},
    {basic_errors::timed_out, {"timed_out"}},
    {basic_errors::try_again, {"try_again"}},
    {basic_errors::would_block, {"would_block"}}};

/**
 * An error code - atom map used to return atom errors for netdb error types.
 */
const std::unordered_map<int, nifpp::str_atom> netdbErrorAtom{
    {netdb_errors::host_not_found, {"host_not_found"}},
    {netdb_errors::host_not_found_try_again, {"host_not_found_try_again"}},
    {netdb_errors::no_data, {"no_data"}},
    {netdb_errors::no_recovery, {"no_recovery"}}};

/**
 * An error code - atom map used to return atom errors for addrinfo error types.
 */
const std::unordered_map<int, nifpp::str_atom> addrinfoErrorAtom{
    {addrinfo_errors::service_not_found, {"service_not_found"}},
    {addrinfo_errors::socket_type_not_supported,
     {"socket_type_not_supported"}}};

/**
 * An error code - atom map used to return atom errors for misc error types.
 */
const std::unordered_map<int, nifpp::str_atom> miscErrorAtom{
    {misc_errors::already_open, {"already_open"}}, {misc_errors::eof, {"eof"}},
    {misc_errors::not_found, {"not_found"}},
    {misc_errors::fd_set_failure, {"fd_set_failure"}}};

/**
 * Translates @c error_code to an atom.
 * @param ec The error code to translate.
 * @return An atom describing the error.
 */
nifpp::str_atom translateError(const boost::system::error_code &ec)
{
    std::unordered_map<int, nifpp::str_atom>::const_iterator it, end;
    if (ec.category().name() == "asio.netdb"s) {
        it = netdbErrorAtom.find(ec.value());
        end = netdbErrorAtom.end();
    }
    else if (ec.category().name() == "asio.addrinfo"s) {
        it = addrinfoErrorAtom.find(ec.value());
        end = addrinfoErrorAtom.end();
    }
    else if (ec.category().name() == "asio.misc"s) {
        it = miscErrorAtom.find(ec.value());
        end = miscErrorAtom.end();
    }
    else {
        it = basicErrorAtom.find(ec.value());
        end = basicErrorAtom.end();
    }

    return it == end ? unknown_error : it->second;
}

/**
 * Creates an error callback.
 * An error callback implementation is shared between multiple NIF functions.
 * @param localEnv A local NIF environment.
 * @param pid A PID to which an error message will be sent.
 * @return An instance of created error callback for the parameters.
 */
one::etls::ErrorFun onError(Env localEnv, ErlNifPid pid)
{
    return [localEnv, pid](const boost::system::error_code &ec) mutable {
        auto reason = translateError(ec);
        auto message = nifpp::make(localEnv, std::make_tuple(error, reason));
        enif_send(nullptr, &pid, localEnv, message);
    };
}

/**
 * @copydoc onError(Env, ErlNifPid)
 * @param ref A custom Erlang reference for this call.
 */
one::etls::ErrorFun onError(Env localEnv, ErlNifPid pid, nifpp::TERM ref)
{
    return [localEnv, pid, ref](const boost::system::error_code &ec) mutable {
        auto reason = translateError(ec);
        auto message = nifpp::make(
            localEnv, std::make_tuple(ref, std::make_tuple(error, reason)));

        enif_send(nullptr, &pid, localEnv, message);
    };
}

template <typename... Args, std::size_t... I>
ERL_NIF_TERM wrap_helper(
    ERL_NIF_TERM (*fun)(ErlNifEnv *, Env, ErlNifPid, Args...), ErlNifEnv *env,
    const ERL_NIF_TERM args[], std::index_sequence<I...>)
{
    try {
        ErlNifPid pid;
        enif_self(env, &pid);
        return fun(env, Env{}, pid, nifpp::get<Args>(env, args[I])...);
    }
    catch (const nifpp::badarg &) {
        return enif_make_badarg(env);
    }
    catch (const boost::system::system_error &e) {
        return nifpp::make(
            env, std::make_tuple(error, translateError(e.code())));
    }
    catch (const std::exception &e) {
        return nifpp::make(env, std::make_tuple(error, std::string{e.what()}));
    }
}

template <typename... Args>
ERL_NIF_TERM wrap(ERL_NIF_TERM (*fun)(ErlNifEnv *, Env, ErlNifPid, Args...),
    ErlNifEnv *env, const ERL_NIF_TERM args[])
{
    return wrap_helper(fun, env, args, std::index_sequence_for<Args...>{});
}

ERL_NIF_TERM connect(ErlNifEnv *env, Env localEnv, ErlNifPid pid, nifpp::TERM r,
    std::string host, int port)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

    auto onSuccess = [=](one::etls::TLSSocket::Ptr socket) mutable {
        auto resource =
            nifpp::construct_resource<one::etls::TLSSocket::Ptr>(socket);

        auto message = nifpp::make(localEnv,
            std::make_tuple(ref, std::make_tuple(
                                     ok, nifpp::make(localEnv, resource))));

        enif_send(nullptr, &pid, localEnv, message);
    };

    auto sock = std::make_shared<one::etls::TLSSocket>(app.ioService());
    sock->connectAsync(
        sock, host, port, std::move(onSuccess), onError(localEnv, pid, ref));

    return nifpp::make(env, ok);
}

ERL_NIF_TERM send(ErlNifEnv *env, Env localEnv, ErlNifPid pid,
    one::etls::TLSSocket::Ptr sock, nifpp::TERM d)
{
    nifpp::TERM data{enif_make_copy(localEnv, d)};

    ErlNifBinary bin;
    enif_inspect_iolist_as_binary(localEnv, data, &bin);

    boost::asio::const_buffer buffer{bin.data, bin.size};

    auto onSuccess = [=]() mutable {
        auto message = nifpp::make(localEnv, ok);
        enif_send(nullptr, &pid, localEnv, message);
    };

    sock->sendAsync(sock, buffer, std::move(onSuccess), onError(localEnv, pid));

    return nifpp::make(env, ok);
}

ERL_NIF_TERM recv(ErlNifEnv *env, Env localEnv, ErlNifPid pid,
    one::etls::TLSSocket::Ptr sock, std::size_t size)
{
    auto bin = std::make_shared<nifpp::binary>(size == 0 ? 1024 : size);

    auto onSuccess = [=](boost::asio::mutable_buffer buffer) mutable {
        if (bin->size != boost::asio::buffer_size(buffer))
            enif_realloc_binary(bin.get(), boost::asio::buffer_size(buffer));

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

ERL_NIF_TERM listen(ErlNifEnv *env, Env /*localEnv*/, ErlNifPid /*pid*/,
    int port, std::string certPath, std::string keyPath)
{
    auto acceptor = std::make_shared<one::etls::TLSAcceptor>(
        app.ioService(), port, certPath, keyPath);

    auto res = nifpp::construct_resource<one::etls::TLSAcceptor::Ptr>(acceptor);

    return nifpp::make(env, std::make_tuple(ok, res));
}

ERL_NIF_TERM accept(ErlNifEnv *env, Env localEnv, ErlNifPid pid, nifpp::TERM r,
    one::etls::TLSAcceptor::Ptr acceptor)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

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

ERL_NIF_TERM handshake(ErlNifEnv *env, Env localEnv, ErlNifPid pid,
    nifpp::TERM r, one::etls::TLSSocket::Ptr sock)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

    auto onSuccess = [=]() mutable {
        auto message = nifpp::make(localEnv, std::make_tuple(ref, ok));
        enif_send(nullptr, &pid, localEnv, message);
    };

    sock->handshakeAsync(
        sock, std::move(onSuccess), onError(localEnv, pid, ref));

    return nifpp::make(env, ok);
}

ERL_NIF_TERM peername(ErlNifEnv *env, Env localEnv, ErlNifPid pid,
    nifpp::TERM r, one::etls::TLSSocket::Ptr sock)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

    auto onSuccess = [=](auto &endpoint) mutable {
        auto address = endpoint.address().to_string();
        auto port = endpoint.port();
        auto message = nifpp::make(
            localEnv, std::make_tuple(ref,
                          std::make_tuple(ok, std::make_tuple(address, port))));

        enif_send(nullptr, &pid, localEnv, message);
    };

    sock->remoteEndpointAsync(
        sock, std::move(onSuccess), onError(localEnv, pid, ref));

    return nifpp::make(env, ok);
}

ERL_NIF_TERM sockname(ErlNifEnv *env, Env localEnv, ErlNifPid pid,
    nifpp::TERM r, one::etls::TLSSocket::Ptr sock)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

    auto onSuccess = [=](auto &endpoint) mutable {
        auto address = endpoint.address().to_string();
        auto port = endpoint.port();
        auto message = nifpp::make(
            localEnv, std::make_tuple(ref,
                          std::make_tuple(ok, std::make_tuple(address, port))));

        enif_send(nullptr, &pid, localEnv, message);
    };

    sock->localEndpointAsync(
        sock, std::move(onSuccess), onError(localEnv, pid, ref));

    return nifpp::make(env, ok);
}

ERL_NIF_TERM acceptor_sockname(ErlNifEnv *env, Env localEnv, ErlNifPid pid,
    nifpp::TERM r, one::etls::TLSAcceptor::Ptr acceptor)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

    auto onSuccess = [=](auto &endpoint) mutable {
        auto address = endpoint.address().to_string();
        auto port = endpoint.port();
        auto message = nifpp::make(
            localEnv, std::make_tuple(ref,
                          std::make_tuple(ok, std::make_tuple(address, port))));

        enif_send(nullptr, &pid, localEnv, message);
    };

    acceptor->localEndpointAsync(
        acceptor, std::move(onSuccess), onError(localEnv, pid, ref));

    return nifpp::make(env, ok);
}

ERL_NIF_TERM close(ErlNifEnv *env, Env localEnv, ErlNifPid pid, nifpp::TERM r,
    one::etls::TLSSocket::Ptr sock)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

    auto onSuccess = [=]() mutable {
        auto message = nifpp::make(localEnv, std::make_tuple(ref, ok));
        enif_send(nullptr, &pid, localEnv, message);
    };

    sock->closeAsync(sock, std::move(onSuccess), onError(localEnv, pid, ref));

    return nifpp::make(env, ok);
}

ERL_NIF_TERM certificate_chain(ErlNifEnv *env, Env /*localEnv*/,
    ErlNifPid /*pid*/, one::etls::TLSSocket::Ptr sock)
{
    auto &chain = sock->certificateChain();

    std::vector<nifpp::TERM> terms;
    std::transform(chain.begin(), chain.end(), std::back_inserter(terms),
        [env](auto &cert) {
            nifpp::binary bin{cert.size()};
            std::memcpy(bin.data, cert.data(), bin.size);
            return nifpp::TERM{nifpp::make(env, bin)};
        });

    return nifpp::make(env, std::make_tuple(ok, terms));
}

ERL_NIF_TERM shutdown(ErlNifEnv *env, Env localEnv, ErlNifPid pid,
    nifpp::TERM r, one::etls::TLSSocket::Ptr sock, nifpp::str_atom type)
{
    nifpp::TERM ref{enif_make_copy(localEnv, r)};

    auto onSuccess = [=]() mutable {
        auto message = nifpp::make(localEnv, std::make_tuple(ref, ok));
        enif_send(nullptr, &pid, localEnv, message);
    };

    auto asioType = boost::asio::socket_base::shutdown_both;
    if (type == "read")
        asioType = boost::asio::socket_base::shutdown_receive;
    else if (type == "write")
        asioType = boost::asio::socket_base::shutdown_send;
    else if (type == "read_write")
        asioType = boost::asio::socket_base::shutdown_both;
    else
        throw nifpp::badarg{};

    sock->shutdownAsync(
        sock, asioType, std::move(onSuccess), onError(localEnv, pid, ref));

    return nifpp::make(env, ok);
}

} // namespace

/**
 * NIF functions.
 */
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
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(connect, env, argv);
}

static ERL_NIF_TERM send_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(send, env, argv);
}

static ERL_NIF_TERM recv_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(recv, env, argv);
}

static ERL_NIF_TERM listen_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(listen, env, argv);
}

static ERL_NIF_TERM accept_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(accept, env, argv);
}

static ERL_NIF_TERM handshake_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(handshake, env, argv);
}

static ERL_NIF_TERM peername_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(peername, env, argv);
}

static ERL_NIF_TERM sockname_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(sockname, env, argv);
}

static ERL_NIF_TERM acceptor_sockname_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(acceptor_sockname, env, argv);
}

static ERL_NIF_TERM close_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(close, env, argv);
}

static ERL_NIF_TERM certificate_chain_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(certificate_chain, env, argv);
}

static ERL_NIF_TERM shutdown_nif(
    ErlNifEnv *env, int /*argc*/, const ERL_NIF_TERM argv[])
{
    return wrap(shutdown, env, argv);
}

static ErlNifFunc nif_funcs[] = {{"connect", 3, connect_nif},
    {"send", 2, send_nif}, {"recv", 2, recv_nif}, {"listen", 3, listen_nif},
    {"accept", 2, accept_nif}, {"handshake", 2, handshake_nif},
    {"peername", 2, peername_nif}, {"sockname", 2, sockname_nif},
    {"acceptor_sockname", 2, acceptor_sockname_nif}, {"close", 2, close_nif},
    {"certificate_chain", 1, certificate_chain_nif},
    {"shutdown", 3, shutdown_nif}};

ERL_NIF_INIT(ssl2_nif, nif_funcs, load, NULL, NULL, NULL)

} // extern C
