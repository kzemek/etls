#!/usr/bin/env escript

%-module(benchmark_parallel).
%-export([main/1]).

main(_) ->
    code:add_path("../ebin"),
    application:start(ssl2),
    ssl:start(),
    KB = 1024,
    MB = 1024 * KB,
    Clients = 10,
    TotalSize = 100 * MB,

    lists:foreach(
        fun(Transport) ->
            io:format("~p ~p clients~n", [Transport, Clients]),
            lists:foreach(
                fun(MessageSize) ->
                    Messages = TotalSize div MessageSize,
                    io:format("  ~p messages of size ~p:~n", [Messages, MessageSize]),
                    {A, _} = timer:tc(fun() -> test(Clients, Transport, MessageSize, Messages, []) end),
                    print(A, Clients, TotalSize, Messages, passive, raw),
                    {B, _} = timer:tc(fun() -> test(Clients, Transport, MessageSize, Messages, [active]) end),
                    print(B, Clients, TotalSize, Messages, active, raw),
                    {C, _} = timer:tc(fun() -> test(Clients, Transport, MessageSize, Messages, [packet]) end),
                    print(C, Clients, TotalSize, Messages, passive, packet),
                    {D, _} = timer:tc(fun() -> test(Clients, Transport, MessageSize, Messages, [active, packet]) end),
                    print(D, Clients, TotalSize, Messages, active, packet),
                    io:format("~n")
                end,
                [1 * KB, 2 * KB, 5 * KB, 10 * KB]),
            io:format("~n")
        end,
        [ssl2, ssl]).

print(Time, Clients, TotalSize, Messages, ActOrPas, RawOrPack) ->
    Seconds = Time / 1000000,
    Speed = Clients * TotalSize * 2 / 1024 / 1024 / Seconds,
    MPS = Clients * Messages / Seconds,
    io:format("    ~p, ~p:  ~p s   ~p MB/s~n   ~p messages/s", [ActOrPas, RawOrPack, round(Seconds), round(Speed), round(MPS)]).

test(Clients, Transport, MessageSize, Messages, Options) ->
    Self = self(),
    spawn_link(fun() -> test_process(Self, Clients, Transport, MessageSize, Messages, Options) end),
    done = receive A -> A end.

test_process(Self, Clients, Transport, MessageSize, Messages, Options) ->
    Active = proplists:get_value(active, Options, false),
    Packet = proplists:get_value(packet, Options, false),

    Self2 = self(),
    {ok, Acceptor} = Transport:listen(5000, [{certfile, "server.pem"}, {keyfile, "server.key"}, {reuseaddr, true}]),

    [spawn_link(fun() -> client_process(Self2, Acceptor, Transport, Active, Packet, MessageSize, Messages) end)
        || _ <- lists:seq(1, Clients)],

    [done = receive A -> A end || _ <- lists:seq(1, Clients)],

    Self ! done.

client_process(Self, Acceptor, Transport, Active, Packet, MessageSize, Messages) ->
    Self2 = self(),
    spawn_link(fun() -> server_loop(Self2, Acceptor, Transport, Active, Packet, MessageSize, Messages) end),


    {ok, Sock} = Transport:connect("127.0.0.1", 5000, [{verify, verify_none}, {nodelay, true}]),
    receive ready -> ok end,

    ok = Transport:setopts(Sock, [{active, false}, binary]),

    case Packet of
        true -> ok = Transport:setopts(Sock, [{packet, 4}]);
        false -> ok
    end,

    Message = crypto:rand_bytes(MessageSize),
    lists:foreach(
        fun(_) ->
            ok = Transport:send(Sock, Message),
            case Active of
                true ->
                    ok = Transport:setopts(Sock, [{active, once}]),
                    receive
                        {Transport, Sock, Message} -> ok;
                        Other -> error(Other)
                    end;

                false ->
                    {ok, Message} = Transport:recv(Sock, MessageSize)
            end
        end,
        lists:seq(1, Messages)),

    ok = Transport:close(Sock),
    case Transport of
        ssl -> ssl:close(Acceptor);
        _ -> ok
    end,

    Self ! done.


server_loop(Self, Acc, Transport, Active, Packet, MessageSize, Messages) ->
    {ok, Sock} =
        case Transport of
            ssl ->
                {ok, S} = ssl:transport_accept(Acc),
                ok = ssl:ssl_accept(S),
                {ok, S};

            ssl2 ->
                {ok, S} = ssl2:accept(Acc),
                ok = ssl2:handshake(S),
                {ok, S}
        end,

    ok = Transport:setopts(Sock, [{active, false}, binary, {nodelay, true}]),

    case Packet of
        true -> ok = Transport:setopts(Sock, [{packet, 4}]);
        false -> ok
    end,

    Self ! ready,
    server_loop2(Sock, Transport, Active, Packet, MessageSize, Messages).

server_loop2(Sock, Transport, _, _, _, 0) -> ok = Transport:close(Sock);
server_loop2(Sock, Transport, Active, _Packet, MessageSize, Messages) ->
    {ok, Data} =
        case Active of
            true ->
                ok = Transport:setopts(Sock, [{active, once}]),
                receive
                    {Transport, Sock, Msg} -> {ok, Msg};
                    Other -> error(Other)
                end;

            false ->
                Transport:recv(Sock, MessageSize)
        end,

    ok = Transport:send(Sock, Data),
    server_loop2(Sock, Transport, Active, _Packet, MessageSize, Messages - 1).
