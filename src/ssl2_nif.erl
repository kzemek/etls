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
    peername/1, sockname/1]).

%%%===================================================================
%%% API
%%%===================================================================

connect(_Ref, _Host, _Port) ->
    error(ssl2_nif_not_loaded).

send(_Sock, _Data) ->
    error(ssl2_nif_not_loaded).

recv(_Sock, _Size) ->
    error(ssl2_nif_not_loaded).

listen(_Port, _CertPath, _KeyPath) ->
    error(ssl2_nif_not_loaded).

accept(_Ref, _Acceptor) ->
    error(ssl2_nif_not_loaded).

handshake(_Ref, _Sock) ->
    error(ssl2_nif_not_loaded).

peername(_Sock) ->
    error(ssl2_nif_not_loaded).

sockname(_Sock) ->
    error(ssl2_nif_not_loaded).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
