%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Main API module for TLS.
%%%--------------------------------------------------------------------
-module(tls).
-author("Konrad Zemek").

%% API
-export([connect/3, connect/4, send/2, recv/2, recv/3, listen/2,
    accept/1, accept/2, handshake/1, handshake/2]).

-type ipaddress() :: {_, _, _, _} | {_, _, _, _, _, _, _, _}.
-type hostname() :: string().
-type host() :: hostname() | ipaddress().
-opaque tlssocket() :: {}.

-on_load(init/0).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect(host(), port(), Options :: proplists:proplist()) ->
    {ok, tlssocket()} | {error, Reason :: any()}.
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).

-spec connect(host(), port(), Options :: proplists:proplist(),
    Timeout :: non_neg_integer()) ->
    {ok, tlssocket()} | {error, Reason :: any()}.
connect(Host, Port, Options, Timeout) ->
    Ref = make_ref(),
    case connect_nif(Ref, Host, Port) of
        ok ->
            receive
                {Ref, Result} ->
                    Result
            after Timeout ->
                {error, timeout}
            end;

        {error, Reason} ->
            {error, Reason}
    end.

send(Sock, Data) ->
    Ref = make_ref(),
    case send_nif(Ref, Sock, Data) of
        ok ->
            receive
                {Ref, Result} ->
                    Result
            end;

        {error, Reason} ->
            {error, Reason}
    end.


recv(Sock, Size) ->
    recv(Sock, Size, infinity).

recv(Sock, Size, Timeout) ->
    Ref = make_ref(),
    case recv_nif(Ref, Sock, Size) of
        ok ->
            receive
                {Ref, Result} ->
                    Result
            after Timeout ->
                {error, timeout}
            end;

        {error, Reason} ->
            {error, Reason}
    end.

listen(Port, Options) ->
    true = proplists:is_defined(certfile, Options),
    CertPath = proplists:get_value(certfile, Options),
    KeyPath = proplists:get_value(keyfile, Options, CertPath),
    listen_nif(Port, CertPath, KeyPath).

accept(Acceptor) ->
    accept(Acceptor, infinity).

accept(Acceptor, Timeout) ->
    Ref = make_ref(),
    case accept_nif(Ref, Acceptor) of
        ok ->
            receive
                {Ref, Result} ->
                    Result
            after Timeout ->
                {error, timeout}
            end;

        {error, Reason} ->
            {error, Reason}
    end.

handshake(Socket) ->
    handshake(Socket, infinity).

handshake(Sock, Timeout) ->
    Ref = make_ref(),
    case handshake_nif(Ref, Sock) of
        ok ->
            receive
                {Ref, Result} ->
                    Result
            after Timeout ->
                {error, timeout}
            end;

        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% NIF stubs
%%%===================================================================

connect_nif(_Ref, _Host, _Port) ->
    error(tls_nif_not_loaded).

send_nif(_Ref, _Sock, _Data) ->
    error(tls_nif_not_loaded).

recv_nif(_Ref, _Sock, _Size) ->
    error(tls_nif_not_loaded).

listen_nif(_Port, _CertPath, _KeyPath) ->
    error(tls_nif_not_loaded).

accept_nif(_Ref, _Acceptor) ->
    error(tls_nif_not_loaded).

handshake_nif(_Ref, _Sock) ->
    error(tls_nif_not_loaded).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init() ->
    LibName = "liberlang_tls",
    LibPath =
        case code:priv_dir(tls) of
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
