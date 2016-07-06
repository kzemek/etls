%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc etls module tests.
%%%--------------------------------------------------------------------
-module(etls_test).
-author("Konrad Zemek").

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(TIMEOUT, timer:seconds(10)).

%%%===================================================================
%%% Test generators
%%%===================================================================

connection_test_() ->
    [{foreach, fun start_server/0, fun stop_server/1, [
        fun connect_should_establish_a_secure_connection/1,
        fun connect_should_honor_active_once/1,
        fun connect_should_honor_active_true/1,
        fun connect_should_respect_packet_options/1,
        fun socket_should_hold_peername/1,
        fun socket_should_hold_sockname/1
    ]}].

communication_test_() ->
    [{foreach, fun start_connection/0, fun stop_connection/1, [
        fun send_should_send_a_message/1,
        fun receive_should_receive_a_message/1,
        fun receive_should_receive_a_message_when_size_is_zero/1,
        fun setopts_should_honor_active_once/1,
        fun setopts_should_honor_active_true/1,
        fun socket_should_notify_about_closure_when_active/1,
        fun setopts_should_respect_packet_options/1,
        fun recv_should_allow_for_new_caller_after_timeout/1,
        fun recv_should_allow_for_recv_while_active/1,
        fun socket_should_allow_to_set_controlling_process/1,
        fun socket_should_be_closeable/1,
        fun socket_should_return_peer_certificate/1,
        fun socket_should_return_error_closed_when_closed/1,
        fun socket_should_be_read_shutdownable/1,
        fun socket_should_close_on_remote_write_shutdown/1,
        fun socket_should_not_close_on_shutdown_when_no_exit_on_close/1
    ]}].

server_test_() ->
    [{foreach, fun prepare_args/0, fun cleanup/1, [
        fun accept_should_accept_connections/1,
        fun sockets_should_communicate/1,
        fun acceptor_should_hold_sockname/1
    ]}].

%%%===================================================================
%%% Test functions
%%%===================================================================

listen_should_be_callable_test() ->
    etls:listen(12345, [{certfile, "server.pem"}]).

connect_should_establish_a_secure_connection({Ref, _Server, Port}) ->
    etls:connect("localhost", Port, [], ?TIMEOUT),
    receive
        {Ref, Result} ->
            ?_assertEqual(connected, Result)
    end.

send_should_send_a_message({Ref, Server, Sock}) ->
    Data = random_data(),
    ok = etls:send(Sock, Data),
    Server ! {'receive', byte_size(Data)},
    receive
        {Ref, 'receive', Result} ->
            ?_assertEqual({ok, Data}, Result)
    end.

receive_should_receive_a_message({Ref, Server, Sock}) ->
    Data = random_data(),
    Server ! {send, Data},
    receive
        {Ref, send, Result} ->
            RecvResult = etls:recv(Sock, byte_size(Data), ?TIMEOUT),

            {?LINE, fun() ->
                ?assertEqual(ok, Result),
                ?assertEqual({ok, Data}, RecvResult)
            end}
    end.

receive_should_receive_a_message_when_size_is_zero({Ref, Server, Sock}) ->
    Data = random_data(),
    Server ! {send, Data},
    receive
        {Ref, send, Result} ->
            RecvResult = etls:recv(Sock, 0, ?TIMEOUT),

            {?LINE, fun() ->
                ?assertEqual(ok, Result),
                ?assertEqual({ok, Data}, RecvResult)
            end}
    end.

accept_should_accept_connections({Ref, Port}) ->
    Self = self(),

    {ok, ListenSock} = etls:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}]),

    spawn(
        fun() ->
            Self ! {Ref, gen_tcp:connect("localhost", Port, [], ?TIMEOUT)}
        end),

    {ok, _Sock} = etls:accept(ListenSock, ?TIMEOUT),

    receive
        {Ref, Result} ->
            ?_assertMatch({ok, _}, Result)
    end.

sockets_should_communicate({Ref, Port}) ->
    Self = self(),

    {ok, ListenSock} = etls:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}]),
    spawn(
        fun() ->
            Self ! {Ref, etls:connect("localhost", Port, [], ?TIMEOUT)}
        end),

    {ok, ServerSock} = etls:accept(ListenSock, ?TIMEOUT),
    ok = etls:handshake(ServerSock, ?TIMEOUT),

    {ok, ClientSock} =
        receive
            {Ref, Result} ->
                Result
        end,

    ServerSend = random_data(),
    ClientSend = random_data(),

    ok = etls:send(ServerSock, ServerSend),
    ok = etls:send(ClientSock, ClientSend),

    ServerRecv = etls:recv(ServerSock, byte_size(ClientSend), ?TIMEOUT),
    ClientRecv = etls:recv(ClientSock, byte_size(ServerSend), ?TIMEOUT),

    {?LINE, fun() ->
        ?assertEqual({ok, ClientSend}, ServerRecv),
        ?assertEqual({ok, ServerSend}, ClientRecv)
    end}.

connect_should_honor_active_once({_Ref, Server, Port}) ->
    {ok, Sock} = etls:connect("localhost", Port, [{active, once}], ?TIMEOUT),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Result1 =
        receive
            {etls, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end,

    Result2 = etls:recv(Sock, byte_size(Data2), ?TIMEOUT),

    {?LINE, fun() ->
        ?assertEqual({ok, Data1}, Result1),
        ?assertEqual({ok, Data2}, Result2)
    end}.

connect_should_honor_active_true({_Ref, Server, Port}) ->
    {ok, Sock} = etls:connect("localhost", Port, [{active, true}], ?TIMEOUT),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Receive = fun() ->
        receive
            {etls, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end
    end,

    Result1 = Receive(),
    Result2 = Receive(),

    {?LINE, fun() ->
        ?assertEqual({ok, Data1}, Result1),
        ?assertEqual({ok, Data2}, Result2)
    end}.

setopts_should_honor_active_once({_Ref, Server, Sock}) ->
    etls:setopts(Sock, [{active, once}]),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Result1 =
        receive
            {etls, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end,

    Result2 = etls:recv(Sock, byte_size(Data2), ?TIMEOUT),

    {?LINE, fun() ->
        ?assertEqual({ok, Data1}, Result1),
        ?assertEqual({ok, Data2}, Result2)
    end}.

setopts_should_honor_active_true({_Ref, Server, Sock}) ->
    etls:setopts(Sock, [{active, true}]),

    Data1 = random_data(),
    Data2 = random_data(),

    Server ! {send, Data1},
    Server ! {send, Data2},

    Receive = fun() ->
        receive
            {etls, Sock, ReceivedData} ->
                {ok, ReceivedData}
        after ?TIMEOUT ->
            {error, test_timeout}
        end
    end,

    Result1 = Receive(),
    Result2 = Receive(),

    {?LINE, fun() ->
        ?assertEqual({ok, Data1}, Result1),
        ?assertEqual({ok, Data2}, Result2)
    end}.

socket_should_notify_about_closure_when_active({_Ref, Server, Sock}) ->
    etls:setopts(Sock, [{active, true}]),
    Server ! stop,
    Result =
        receive
            {etls_closed, Sock} -> ok
        after ?TIMEOUT ->
            {error, test_timeout}
        end,

    ?_assertEqual(ok, Result).

connect_should_respect_packet_options({Ref, Server, Port}) ->
    {ok, Sock} = etls:connect("localhost", Port, [{packet, 2}]),

    Data = random_data(),
    DS = byte_size(Data),

    ok = etls:send(Sock, Data),
    ExpectedData = <<DS:2/big-unsigned-integer-unit:8, Data/binary>>,

    Server ! {'receive', DS + 2},
    Server ! {send, ExpectedData},

    receive
        {Ref, 'receive', Result} ->
            Received = etls:recv(Sock, 12345, ?TIMEOUT),
            {?LINE, fun() ->
                ?assertEqual({ok, ExpectedData}, Result),
                ?assertEqual({ok, Data}, Received)
            end}
    end.

setopts_should_respect_packet_options({Ref, Server, Sock}) ->
    ok = etls:setopts(Sock, [{packet, 4}]),

    Data = random_data(),
    DS = byte_size(Data),

    ok = etls:send(Sock, Data),
    ExpectedData = <<DS:4/big-unsigned-integer-unit:8, Data/binary>>,

    Server ! {'receive', DS + 4},
    Server ! {send, ExpectedData},

    receive
        {Ref, 'receive', Result} ->
            Received = etls:recv(Sock, 54321, ?TIMEOUT),
            {?LINE, fun() ->
                ?assertEqual({ok, ExpectedData}, Result),
                ?assertEqual({ok, Data}, Received)
            end}
    end.

recv_should_allow_for_new_caller_after_timeout({_Ref, Server, Sock}) ->
    Data = random_data(),
    {error, timeout} = etls:recv(Sock, byte_size(Data), 0),
    Server ! {send, Data},
    Result = etls:recv(Sock, byte_size(Data), ?TIMEOUT),
    ?_assertEqual({ok, Data}, Result).

recv_should_allow_for_recv_while_active({_Ref, Server, Sock}) ->
    ok = etls:setopts(Sock, [{active, true}]),

    Data1 = random_data(),
    Data2 = random_data(),
    Self = self(),
    SpawnRef = make_ref(),

    spawn(fun() ->
        Self ! {SpawnRef, etls:recv(Sock, byte_size(Data1), ?TIMEOUT)} end),

    Server ! {send, Data1},
    Server ! {send, Data2},

    receive
        {SpawnRef, Result} ->
            Result2 =
                receive
                    {etls, Sock, Data2} -> Data2
                after ?TIMEOUT ->
                    {error, test_timeout}
                end,

            {?LINE, fun() ->
                ?assertEqual({ok, Data1}, Result),
                ?assertEqual(Data2, Result2)
            end}
    end.

socket_should_allow_to_set_controlling_process({_Ref, Server, Sock}) ->
    ok = etls:setopts(Sock, [{active, true}]),
    Data = random_data(),
    Self = self(),
    NewRef = make_ref(),

    Pid = spawn(fun() ->
        receive
            {etls, Sock, Result} -> Self ! {NewRef, Result}
        after ?TIMEOUT ->
            Self ! {NewRef, {error, test_timeout}}
        end
    end),

    ok = etls:controlling_process(Sock, Pid),
    Server ! {send, Data},

    receive
        {NewRef, Res} ->
            ?_assertEqual(Data, Res)
    end.

socket_should_hold_peername({_Ref, _Server, Port}) ->
    {ok, Sock} = etls:connect("localhost", Port, [], ?TIMEOUT),
    [?_assertEqual({ok, {{127, 0, 0, 1}, Port}}, etls:peername(Sock))].

socket_should_hold_sockname({_Ref, _Server, Port}) ->
    {ok, Sock} = etls:connect("localhost", Port, [], ?TIMEOUT),
    {?LINE, fun() ->
        ?assertMatch({ok, {{127, 0, 0, 1}, _}}, etls:sockname(Sock)),
        ?assertNotEqual({ok, {{127, 0, 0, 1}, Port}}, etls:sockname(Sock))
    end}.

acceptor_should_hold_sockname({_Ref, Port}) ->
    {ok, Acceptor} = etls:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}]),
    ?_assertEqual({ok, {{0, 0, 0, 0}, Port}}, etls:sockname(Acceptor)).

socket_should_be_closeable({_Ref, _Server, Sock}) ->
    etls:setopts(Sock, [{active, once}]),
    ok = etls:close(Sock),
    Result =
        receive
            {etls_closed, Sock} = R -> R
        after ?TIMEOUT ->
            {error, test_timeout}
        end,
    ?_assertEqual({etls_closed, Sock}, Result).

socket_should_return_peer_certificate({_Ref, _Server, Sock}) ->
    {ok, Der} = etls:peercert(Sock),
    Cert = public_key:pkix_decode_cert(Der, otp),
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    Serial = TBSCert#'OTPTBSCertificate'.serialNumber,
    ?_assertEqual(10728077368415183536, Serial).

socket_should_return_error_closed_when_closed({_Ref, _Server, Sock}) ->
    ok = etls:close(Sock),
    SendResult = etls:send(Sock, random_data()),
    RecvResult = etls:recv(Sock, 1234, ?TIMEOUT),
    {?LINE, fun() ->
        ?assertEqual({error, closed}, SendResult),
        ?assertEqual({error, closed}, RecvResult)
    end}.

socket_should_be_read_shutdownable({_Ref, _Server, Sock}) ->
    ok = etls:shutdown(Sock, read),
    SendResult = etls:send(Sock, random_data()),
    RecvResult = etls:recv(Sock, 1234, ?TIMEOUT),
    {?LINE, fun() ->
        ?assertEqual(ok, SendResult),
        ?assertEqual({error, closed}, RecvResult)
    end}.

socket_should_close_on_remote_write_shutdown({_Ref, Server, Sock}) ->
    {_, _, Supervisor, _, _} = Sock,
    Server ! {shutdown, write},
    RecvResult = etls:recv(Sock, 1234, ?TIMEOUT),

    timer:sleep(250),

    {?LINE, fun() ->
        ?assertEqual(false, is_process_alive(Supervisor)),
        ?assertEqual({error, closed}, RecvResult)
    end}.

socket_should_not_close_on_shutdown_when_no_exit_on_close({_Ref, Server, Sock}) ->
    etls:setopts(Sock, [{exit_on_close, false}]),

    {_, _, Supervisor, _, _} = Sock,
    Server ! {shutdown, write},
    RecvResult = etls:recv(Sock, 1234),

    timer:sleep(250),

    {?LINE, fun() ->
        ?assertEqual(true, is_process_alive(Supervisor)),
        ?assertEqual({error, closed}, RecvResult)
    end}.

%%%===================================================================
%%% Test fixtures
%%%===================================================================

start_server() ->
    ssl:start(temporary),
    etls_app:start(temporary, []),

    Self = self(),
    Ref = make_ref(),
    Port = random_port(),
    Server = spawn(fun() -> run_server(Self, Ref, Port) end),

    receive
        {Ref, ready} ->
            {Ref, Server, Port}
    end.

stop_server({_Ref, Server, _Port}) ->
    Server ! stop,
    clear_queue().

start_connection() ->
    {Ref, Server, Port} = start_server(),
    {ok, Sock} = etls:connect("localhost", Port, [], ?TIMEOUT),
    receive
        {Ref, connected} -> ok
    after ?TIMEOUT ->
        error(not_connected)
    end,
    {Ref, Server, Sock}.

stop_connection({_Ref, Server, _Sock}) ->
    Server ! stop,
    clear_queue().

prepare_args() ->
    etls_app:start(temporary, []),
    {make_ref(), random_port()}.

cleanup({_Ref, _Port}) ->
    clear_queue().

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

        {shutdown, write} ->
            ssl:shutdown(Sock, write),
            server_loop(Pid, Ref, Sock);

        stop ->
            ssl:close(Sock)
    end.

clear_queue() ->
    receive
        _ -> clear_queue()
    after 0 ->
        ok
    end.
