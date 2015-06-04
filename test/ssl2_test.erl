%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc ssl2 module tests.
%%%--------------------------------------------------------------------
-module(ssl2_test).
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
        fun connect_should_establish_a_secure_connection/1,
        fun connect_should_honor_active_once/1,
        fun connect_should_honor_active_true/1
    ]}].

communication_test_() ->
    [{foreach, fun start_connection/0, fun stop_connection/1, [
        fun send_should_send_a_message/1,
        fun receive_should_receive_a_message/1,
        fun receive_should_receive_a_message_when_size_is_zero/1,
        fun setopts_should_honor_active_once/1,
        fun setopts_should_honor_active_true/1
    ]}].

server_test_() ->
    [{foreach, fun prepare_args/0, fun cleanup/1, [
        fun accept_should_accept_connections/1,
        fun sockets_should_communicate/1
    ]}].

%%%===================================================================
%%% Test functions
%%%===================================================================

listen_should_be_callable() ->
    ssl2:listen(12345, [{certfile, "server.pem"}]).

connect_should_establish_a_secure_connection({Ref, _Server, Port}) ->
    ssl2:connect("localhost", Port, [], ?TIMEOUT),
    receive
        {Ref, Result} ->
            [?_assertEqual(connected, Result)]
    end.

send_should_send_a_message({Ref, Server, Sock}) ->
    Data = random_data(),
    ok = ssl2:send(Sock, Data),
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
            RecvResult = ssl2:recv(Sock, byte_size(Data), ?TIMEOUT),
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
            RecvResult = ssl2:recv(Sock, 0, ?TIMEOUT),
            [
                ?_assertEqual(ok, Result),
                ?_assertEqual({ok, Data}, RecvResult)
            ]
    end.

accept_should_accept_connections({Ref, Port}) ->
    Self = self(),

    {ok, ListenSock} = ssl2:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}]),

    spawn(
        fun() ->
            Self ! {Ref, gen_tcp:connect("localhost", Port, [], ?TIMEOUT)}
        end),

    {ok, _Sock} = ssl2:accept(ListenSock, ?TIMEOUT),

    receive
        {Ref, Result} ->
            [?_assertMatch({ok, _}, Result)]
    end.

sockets_should_communicate({Ref, Port}) ->
    Self = self(),

    {ok, ListenSock} = ssl2:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}]),
    spawn(
        fun() ->
            Self ! {Ref, ssl2:connect("localhost", Port, [], ?TIMEOUT)}
        end),

    {ok, ServerSock} = ssl2:accept(ListenSock, ?TIMEOUT),
    ok = ssl2:handshake(ServerSock, ?TIMEOUT),

    {ok, ClientSock} =
        receive
            {Ref, Result} ->
                Result
        end,

    ServerSend = random_data(),
    ClientSend = random_data(),

    ok = ssl2:send(ServerSock, ServerSend),
    ok = ssl2:send(ClientSock, ClientSend),

    ServerRecv = ssl2:recv(ServerSock, byte_size(ClientSend), ?TIMEOUT),
    ClientRecv = ssl2:recv(ClientSock, byte_size(ServerSend), ?TIMEOUT),

    [
        ?_assertEqual({ok, ClientSend}, ServerRecv),
        ?_assertEqual({ok, ServerSend}, ClientRecv)
    ].

connect_should_honor_active_once({_Ref, Server, Port}) ->
    {ok, Sock} = ssl2:connect("localhost", Port, [{active, once}], ?TIMEOUT),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Result1 =
        receive
            {ssl2, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end,

    Result2 = ssl2:recv(Sock, byte_size(Data2), ?TIMEOUT),

    [
        ?_assertEqual({ok, Data1}, Result1),
        ?_assertEqual({ok, Data2}, Result2)
    ].

connect_should_honor_active_true({_Ref, Server, Port}) ->
    {ok, Sock} = ssl2:connect("localhost", Port, [{active, true}], ?TIMEOUT),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Receive = fun() ->
        receive
            {ssl2, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end
    end,

    Result1 = Receive(),
    Result2 = Receive(),

    [
        ?_assertEqual({ok, Data1}, Result1),
        ?_assertEqual({ok, Data2}, Result2)
    ].

setopts_should_honor_active_once({_Ref, Server, Sock}) ->
    ssl2:setopts(Sock, [{active, once}]),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Result1 =
        receive
            {ssl2, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end,

    Result2 = ssl2:recv(Sock, byte_size(Data2), ?TIMEOUT),

    [
        ?_assertEqual({ok, Data1}, Result1),
        ?_assertEqual({ok, Data2}, Result2)
    ].

setopts_should_honor_active_true({_Ref, Server, Sock}) ->
    ssl2:setopts(Sock, [{active, true}]),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Receive = fun() ->
        receive
            {ssl2, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end
    end,

    Result1 = Receive(),
    Result2 = Receive(),

    [
        ?_assertEqual({ok, Data1}, Result1),
        ?_assertEqual({ok, Data2}, Result2)
    ].

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start_server() ->
    ssl:start(temporary),
    ssl2_app:start(temporary, []),

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
    {ok, Sock} = ssl2:connect("localhost", Port, [], ?TIMEOUT),
    {Ref, Server, Sock}.

stop_connection({_Ref, Server, _Sock}) ->
    Server ! stop.

prepare_args() ->
    ssl2_app:start(temporary, []),
    {make_ref(), random_port()}.

cleanup({_Ref, _Port}) ->
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
%%         {ok, Sock} = ssl2:connect("localhost")
%%         catch
%%         end.
