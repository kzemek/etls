#!/usr/bin/env escript

main(_) ->
  code:add_path("../ebin"),
  application:start(ssl2),
  ssl:start(),
  KB = 1024,
  MB = 1024 * KB,
  TotalSize = 200 * MB,

  lists:foreach(
    fun(Transport) ->
      io:format("~p~n", [Transport]),
      lists:foreach(
        fun(MessageSize) ->
          Messages = TotalSize div MessageSize,
          io:format("  ~p messages of size ~p:~n", [Messages, MessageSize]),
          {A, _} = timer:tc(fun() -> test(Transport, MessageSize, Messages, []) end),
          print(A, TotalSize, passive, raw),
          {B, _} = timer:tc(fun() -> test(Transport, MessageSize, Messages, [active]) end),
          print(B, TotalSize, active, raw),
          {C, _} = timer:tc(fun() -> test(Transport, MessageSize, Messages, [packet]) end),
          print(C, TotalSize, passive, packet),
          {D, _} = timer:tc(fun() -> test(Transport, MessageSize, Messages, [active, packet]) end),
          print(D, TotalSize, active, packet),
          io:format("~n")
        end,
        [1 * KB, 2 * KB, 5 * KB, 10 * KB]),
      io:format("~n")
    end,
    [ssl2, ssl]).

print(Time, TotalSize, ActOrPas, RawOrPack) ->
  Seconds = Time / 1000000,
  Speed = TotalSize * 2 / 1024 / 1024 / Seconds,
  io:format("    ~p, ~p:  ~p s   ~p MB/s~n", [ActOrPas, RawOrPack, round(Seconds), round(Speed)]).

test(Transport, MessageSize, Messages, Options) ->
  Self = self(),
  spawn_link(fun() -> test_process(Self, Transport, MessageSize, Messages, Options) end),
  done = receive A -> A end.

test_process(Self, Transport, MessageSize, Messages, Options) ->
  try
    Active = proplists:get_value(active, Options, false),
    Packet = proplists:get_value(packet, Options, false),

    Self2 = self(),
    spawn_link(fun() -> server_loop(Self2, Transport, Active, Packet, MessageSize, Messages) end),

    {ok, Sock} = Transport:connect("localhost", 5000, [{verify, verify_none}, {nodelay, true}]),
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
    Self ! done
  catch
    A:B -> Self ! {A, B}
  end.


server_loop(Self, Transport, Active, Packet, MessageSize, Messages) ->
  {ok, Acc} = Transport:listen(5000, [{certfile, "server.pem"}, {keyfile, "server.key"}, {reuseaddr, true}]),
  {ok, Sock} =
    case Transport of
      ssl ->
        {ok, S} = ssl:transport_accept(Acc),
        ok = ssl:ssl_accept(S),
        ssl:close(Acc),
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
