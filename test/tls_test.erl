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
    [fun listen_should_be_callable/0].

connection_test_() ->
    [{foreach, fun start_server/0, fun stop_server/1, [
        fun connect_should_establish_a_secure_connection/1
    ]}].

communication_test_() ->
    [{foreach, fun start_connection/0, fun stop_connection/1, [
        fun send_should_send_a_message/1,
        fun receive_should_receive_a_message/1,
        fun receive_should_receive_a_message_when_size_is_zero/1
    ]}].

server_test_() ->
    [{foreach, fun start_server_test/0, fun stop_server_test/1, [
        fun accept_should_accept_connections/1
    ]}].

%%%===================================================================
%%% Test functions
%%%===================================================================

listen_should_be_callable() ->
    tls:listen(12345, []).

connect_should_establish_a_secure_connection({Ref, _Server, Port}) ->
    tls:connect("localhost", Port, [], ?TIMEOUT),
    receive
        {Ref, Result} ->
            [?_assertEqual(connected, Result)]
    end.

send_should_send_a_message({Ref, Server, Sock}) ->
    Data = random_data(),
    ok = tls:send(Sock, Data),
    Server ! {'receive', byte_size(Data)},
    receive
        {Ref, 'receive', Result} ->
            [?_assertEqual({ok, Data}, Result)]
    end.

receive_should_receive_a_message({Ref, Server, Sock}) ->
    Data = random_data(),
    Server ! {send, Data},
    receive
        {Ref, send, Result} ->
            RecvResult = tls:recv(Sock, byte_size(Data), ?TIMEOUT),
            [
                ?_assertEqual(ok, Result),
                ?_assertEqual({ok, Data}, RecvResult)
            ]
    end.

receive_should_receive_a_message_when_size_is_zero({Ref, Server, Sock}) ->
    Data = random_data(),
    Server ! {send, Data},
    receive
        {Ref, send, Result} ->
            RecvResult = tls:recv(Sock, 0, ?TIMEOUT),
            [
                ?_assertEqual(ok, Result),
                ?_assertEqual({ok, Data}, RecvResult)
            ]
    end.

accept_should_accept_connections({Ref, Port}) ->
    Self = self(),

    {ok, ListenSock} = tls:listen(Port, []),

    spawn(
        fun() ->
            Self ! {Ref, gen_tcp:connect("localhost", Port, [], ?TIMEOUT)}
        end),

    {ok, _Sock} = tls:accept(ListenSock, ?TIMEOUT),

    receive
        {Ref, Result} ->
            [?_assertMatch({ok, _}, Result)]
    end.


%%%===================================================================
%%% Test fixtures
%%%===================================================================

start_server() ->
    ssl:start(),

    Self = self(),
    Ref = make_ref(),
    Port = random_port(),
    Server = spawn(fun() -> run_server(Self, Ref, Port) end),

    receive
        {Ref, ready} ->
            {Ref, Server, Port}
    end.

stop_server({_Ref, Server, _Port}) ->
    Server ! stop.

start_connection() ->
    {Ref, Server, Port} = start_server(),
    {ok, Sock} = tls:connect("localhost", Port, [], ?TIMEOUT),
    {Ref, Server, Sock}.

stop_connection({_Ref, Server, _Sock}) ->
    Server ! stop.

start_server_test() ->
    {make_ref(), random_port()}.

stop_server_test({_Ref, _Port}) ->
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================

random_port() ->
    random:uniform(65535 - 49152) + 49151.

random_data() ->
    Len = crypto:rand_uniform(1, 255),
    crypto:rand_bytes(Len).

run_server(Pid, Ref, Port) ->
    try
        {ok, ListenSocket} = ssl:listen(Port, [
            {certfile, "server.pem"},
            {keyfile, "server.key"},
            {reuseaddr, true}]),

        Pid ! {Ref, ready},

        {ok, Sock} = ssl:transport_accept(ListenSocket, ?TIMEOUT),
        ssl:setopts(Sock, [{active, false}, {mode, binary}]),
        ok = ssl:ssl_accept(Sock, ?TIMEOUT),

        Pid ! {Ref, connected},
        server_loop(Pid, Ref, Sock)
    catch
        A:B -> Pid ! {Ref, {A, B}}
    end.

server_loop(Pid, Ref, Sock) ->
    receive
        {'receive', Bytes} ->
            Pid ! {Ref, 'receive', ssl:recv(Sock, Bytes, ?TIMEOUT)},
            server_loop(Pid, Ref, Sock);

        {send, Data} ->
            Pid ! {Ref, send, ssl:send(Sock, Data)},
            server_loop(Pid, Ref, Sock);

        stop ->
            ok
    end.

%% run_client(Ref, Port) ->
%%     try
%%         {ok, Sock} = tls:connect("localhost")
%%         catch
%%         end.
