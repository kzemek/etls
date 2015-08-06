%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Erlang wrapper for SSL2 NIF module.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl2_nif).
-author("Konrad Zemek").

-on_load(init/0).

%% API
-export([connect/12, send/2, recv/2, listen/10, accept/2, handshake/2,
    peername/2, sockname/2, acceptor_sockname/2, close/2,
    certificate_chain/1, shutdown/3]).

-type str() :: binary() | string().
-type socket() :: term().
-type acceptor() :: term().

-export_type([socket/0, acceptor/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a native TCP socket, connects to the given host and port
%% and performs an TLS handshake.
%% When finished, sends {Ref, {ok, Socket} | {error, Reason}} to the
%% calling process.
%% @end
%%--------------------------------------------------------------------
-spec connect(Ref :: reference(), Host :: str(), Port :: inet:port_number(),
    CertPath :: str(), KeyPath :: str(), VerifyType :: str(),
    FailIfNoPeerCert :: boolean(), VerifyClientOnce :: boolean(),
    RFC2818Hostname :: str(), CAs :: [binary()], CRLs :: [binary()],
    Chain :: [binary()]) ->
    ok | {error, Reason :: atom()}.
connect(_Ref, _Host, _Port, _CertPath, _KeyPath, _VerifyType, _FailIfNoPeerCert,
    _VerifyClientOnce, _RFC2818Hostname, _CAs, _CRLs, _Chain) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message through the Socket.
%% When finished, sends ok | {error, Reason} to the calling
%% process.
%% @end
%%--------------------------------------------------------------------
-spec send(Socket :: socket(), Data :: iodata()) ->
    ok | {error, Reason :: atom()}.
send(_Sock, _Data) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Receives a message from the Socket.
%% When Size is 0, waits for any data to arrive on the socket.
%% When finished, sends {ok, Data :: binary()} | {error, Reason} to
%% the calling process.
%% @end
%%--------------------------------------------------------------------
-spec recv(Socket :: socket(), Size :: non_neg_integer()) ->
    ok | {error, Reason :: atom()}.
recv(_Sock, _Size) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Creates an acceptor socket that listens on the given port.
%% @end
%%--------------------------------------------------------------------
-spec listen(Port :: inet:port_number(), CertPath :: str(), KeyPath :: str(),
    VerifyType :: str(), FailIfNoPeerCert :: boolean(),
    VerifyClientOnce :: boolean(), RFC2818Hostname :: str(),
    CAs :: [binary()], CRLs :: [binary()], Chain :: [binary()]) ->
    {ok, Acceptor :: acceptor()} |
    {error, Reason :: atom()}.
listen(_Port, _CertPath, _KeyPath, _VerifyType, _FailIfNoPeerCert,
    _VerifyClientOnce, _RFC2818Hostname, _CAs, _CRLs, _Chain) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Accepts an incoming TCP connection on the acceptor.
%% When finished, sends {Ref, {ok, Socket} | {error, Reason}} to the
%% calling process.
%% @end
%%--------------------------------------------------------------------
-spec accept(Ref :: reference(), Acceptor :: acceptor()) ->
    ok | {error, Reason :: atom()}.
accept(_Ref, _Acceptor) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Performs a TLS handshake on the new TCP connection.
%% When finished, sends {Ref, ok | {error, Reason}} to the
%% calling process.
%% @end
%%--------------------------------------------------------------------
-spec handshake(Ref :: reference(), Socket :: socket()) ->
    ok | {error, Reason :: atom()}.
handshake(_Ref, _Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Returns a tuple {RemoteHostname, RemotePort} describing the peer.
%% When finished, sends {Ref, {ok, Result} | {error, Reason}} to the
%% calling process.
%% @end
%%--------------------------------------------------------------------
-spec peername(Ref :: reference(), Socket :: socket()) ->
    ok | {error, Reason :: atom()}.
peername(_Ref, _Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Returns a tuple {LocalHostname, LocalPort} describing the local
%% socket.
%% When finished, sends {Ref, {ok, Result} | {error, Reason}} to the
%% calling process.
%% @end
%%--------------------------------------------------------------------
-spec sockname(Ref :: reference(), Socket :: socket()) ->
    ok | {error, Reason :: atom()}.
sockname(_Ref, _Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Returns a tuple {LocalHostname, LocalPort} describing the acceptor
%% socket.
%% When finished, sends {Ref, {ok, Result} | {error, Reason}} to the
%% calling process.
%% @end
%%--------------------------------------------------------------------
-spec acceptor_sockname(Ref :: reference(), Socket :: acceptor()) ->
    ok | {error, Reason :: atom()}.
acceptor_sockname(_Ref, _Acceptor) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Closes the socket.
%% When finished, sends {Ref, ok | {error, Reason}} to the calling
%% process.
%% @end
%%--------------------------------------------------------------------
-spec close(Ref :: reference(), Socket :: socket()) ->
    ok | {error, Reason :: atom()}.
close(_Ref, _Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Returns a certificate chain of the peer.
%% This method can only be used after connect / handshake.
%% @end
%%--------------------------------------------------------------------
-spec certificate_chain(Socket :: socket()) ->
    {ok, [binary()]} | {error, Reason :: atom()}.
certificate_chain(_Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Shuts down socket communciation in a chosen direction.
%% When finished, sends {Ref, ok | {error, Reason}} to the calling
%% process.
%% @end
%%--------------------------------------------------------------------
-spec shutdown(Ref :: reference(), Socket :: socket(),
    Type :: read | write | read_write) ->
    ok | {error, Reason :: atom()}.
shutdown(_Ref, _Sock, _Type) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialization function for the module.
%% Loads the NIF native library. The library is first searched for
%% in application priv dir, and then under ../priv and ./priv .
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | {error, Reason :: atom()}.
init() ->
    LibName = "liberlang_tls",
    LibPath =
        case code:priv_dir(ssl2) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, LibName]);
                    _ ->
                        filename:join([priv, LibName])
                end;

            Dir ->
                filename:join(Dir, LibName)
        end,

    erlang:load_nif(LibPath, 0).
