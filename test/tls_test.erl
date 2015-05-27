%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc tls module tests.
%%%--------------------------------------------------------------------
-module(tls_test).
-author("Konrad Zemek").

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test generators
%%%===================================================================

api_test_() ->
    [
        fun tls_can_be_started/0,
        fun tls_application_can_be_started/0,
        fun tls_can_be_started_with_type/0
    ].

communication_test_() ->
    [{foreach, fun start/0, fun stop/1, [
        fun connect_establishes_a_connection/1,
        fun connect_establishes_a_secure_connection/1
    ]}].

%%%===================================================================
%%% Test functions
%%%===================================================================

tls_can_be_started() ->
    [?_assertEqual(ok, tls:start())].

tls_can_be_started_with_type() ->
    [?_assertEqual(ok, tls:start(temporary))].

tls_application_can_be_started() ->
    [?_assertEqual(ok, application:start(tls))].

connect_establishes_a_connection({Ref, Port}) ->
    Self = self(),

    spawn(fun() ->
        try
            {ok, ListenSocket} = ssl:listen(Port, [{certfile, "server.pem"},
                {keyfile, "server.pem"}, {reuseaddr, true}]),
            Self ! {Ref, ready},
            {ok, _S} = ssl:transport_accept(ListenSocket, timer:seconds(10)),
            Self ! {Ref, ok}
        catch
            A:B -> Self ! {Ref, {A, B}}
        end
    end),

    receive
        {Ref, ready} -> ok
    end,

    tls:connect("localhost", Port, [], timer:seconds(10)),

    receive
        {Ref, Result} ->
            [?_assertEqual(ok, Result)]
    end.

connect_establishes_a_secure_connection({Ref, Port}) ->
    Self = self(),

    io:format(user, "~p~n", [ file:get_cwd()]),

    spawn(fun() ->
        try
            {ok, ListenSocket} = ssl:listen(Port, [{certfile, "server.pem"},
                {keyfile, "server.key"}, {reuseaddr, true}]),
            Self ! {Ref, ready},

            {ok, Sock} = ssl:transport_accept(ListenSocket, timer:seconds(10)),
            ok = ssl:ssl_accept(Sock, timer:seconds(10)),

            Self ! {Ref, ok}
        catch
            A:B -> Self ! {Ref, {A, B}}
        end
    end),

    receive
        {Ref, ready} -> ok
    end,

    tls:connect("localhost", Port, [], timer:seconds(10)),

    receive
        {Ref, Result} ->
            [?_assertEqual(ok, Result)]
    end.

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start() ->
    ssl:start(),
    tls:start(),
    {make_ref(), random_port()}.

stop(_Arg0) ->
    _Arg0.

%%%===================================================================
%%% Helper functions
%%%===================================================================

random_port() ->
    random:uniform(65535 - 49152) + 49151.
