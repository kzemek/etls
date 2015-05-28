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
-export([connect/3, connect/4, send/2, recv/2, recv/3]).

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
    ok = connect_nif(Ref, Host, Port),
    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        {error, timeout}
    end.

send(Sock, Data) ->
    Ref = make_ref(),
    ok = send_nif(Ref, Sock, Data),
    receive
        {Ref, Result} ->
            Result
    end.


recv(Sock, Size) ->
    recv(Sock, Size, infinity).

recv(Sock, Size, Timeout) ->
    Ref = make_ref(),
    ok = recv_nif(Ref, Sock, Size),
    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        {error, timeout}
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
