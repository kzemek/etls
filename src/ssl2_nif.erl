%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Erlang wrapper for SSL2 NIF module.
%%%--------------------------------------------------------------------
-module(ssl2_nif).
-author("Konrad Zemek").

-on_load(init/0).

%% API
-export([connect/3, send/2, recv/2, listen/3, accept/2, handshake/2,
    peername/1, sockname/1, close/1, certificate_chain/1]).

-opaque socket() :: binary().
-opaque acceptor() :: binary().

-export_type([socket/0, acceptor/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect(Ref :: reference(), Host :: string(),
    Port :: inet:port_number()) ->
    ok | {error, Reason :: any()}.
connect(_Ref, _Host, _Port) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec send(Socket :: socket(), Data :: iodata()) ->
    ok | {error, Reason :: any()}.
send(_Sock, _Data) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec recv(Socket :: socket(), Size :: non_neg_integer()) ->
    ok | {error, Reason :: any()}.
recv(_Sock, _Size) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec listen(Port :: inet:port_number(), CertPath :: string(),
    KeyPath :: string()) ->
    {ok, Acceptor :: acceptor()} |
    {error, Reason :: any()}.
listen(_Port, _CertPath, _KeyPath) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec accept(Ref :: reference(), Acceptor :: acceptor()) ->
    ok | {error, Reason :: any()}.
accept(_Ref, _Acceptor) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec handshake(Ref :: reference(), Socket :: socket()) ->
    ok | {error, Reason :: any()}.
handshake(_Ref, _Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec peername(Socket :: socket()) ->
    {ok, {StrAddress :: string(), Port :: inet:port_number()}} |
    {error, Reason :: any()}.
peername(_Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec sockname(Socket :: socket()) ->
    {ok, {StrAddress :: string(), Port :: inet:port_number()}} |
    {error, Reason :: any()}.
sockname(_Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec close(Socket :: socket()) -> ok | {error, Reason :: any()}.
close(_Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

-spec certificate_chain(Socket :: socket()) ->
    {ok, [binary()]} | {error, Reason :: any()}.
certificate_chain(_Sock) ->
    erlang:nif_error(ssl2_nif_not_loaded).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec init() -> ok | {error, Reason :: any()}.
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
