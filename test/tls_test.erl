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

-define(TIMEOUT, timer:seconds(10)).

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
    [{foreach, fun start_communication_test/0, fun stop_communication_test/1, [
        fun connect_establishes_a_secure_connection/1,
        fun send_sends_a_message/1
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

connect_establishes_a_secure_connection({Ref, _Server, Port}) ->
    tls:connect("localhost", Port, [], ?TIMEOUT),
    receive
        {Ref, Result} ->
            [?_assertEqual(connected, Result)]
    end.

send_sends_a_message({Ref, Server, Port}) ->
    Data = random_data(),
    {ok, Sock} = tls:connect("localhost", Port, [], ?TIMEOUT),
    io:format(user, "omg", []),
    ok = tls:send(Sock, Data),
    io:format(user, "omg2", []),
    Server ! {'receive', byte_size(Data)},
    receive
        {Ref, 'receive', Result} ->
            [?_assertEqual({ok, Data}, Result)]
    end.

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start_communication_test() ->
    ssl:start(),
    tls:start(),

    Self = self(),
    Ref = make_ref(),
    Port = random_port(),
    Server =
        spawn(fun() ->
            try
                {ok, ListenSocket} = ssl:listen(Port, [
                    {certfile, "server.pem"},
                    {keyfile, "server.key"},
                    {reuseaddr, true}]),

                Self ! {Ref, ready},

                {ok, Sock} = ssl:transport_accept(ListenSocket, ?TIMEOUT),
                ssl:setopts(Sock, [{active, false}, {mode, binary}]),
                ok = ssl:ssl_accept(Sock, ?TIMEOUT),

                Self ! {Ref, connected},

                Loop = fun Action() ->
                    receive
                        {'receive', Bytes} ->
                            Self ! {Ref, 'receive', ssl:recv(Sock, Bytes, ?TIMEOUT)},
                            Action();

                        stop ->
                            ok
                    end
                end,

                Loop()
            catch
                A:B -> Self ! {Ref, {A, B}}
            end
        end),

    receive
        {Ref, ready} ->
            {Ref, Server, Port}
    end.

stop_communication_test({_Ref, Server, _Port}) ->
    Server ! stop.

%%%===================================================================
%%% Helper functions
%%%===================================================================

random_port() ->
    random:uniform(65535 - 49152) + 49151.

random_data() ->
    Len = crypto:rand_uniform(1, 255),
    crypto:rand_bytes(Len).