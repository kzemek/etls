%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------

main(_) ->
    run().


run() ->
    ssl:start(),
    application:start(ssl2),
    [{Transport, run(Transport)} || Transport <- [reference, ssl2_nif, ssl2]].


run(reference) ->
    BenchmarkOutput = os:cmd("cd ../_build/test && ./bandwidthCap"),
    [MBps, _, MSps, _] = string:tokens(BenchmarkOutput, " \n\t"),
    {MBps, MSps};
run(Transport) ->
    B = 1,
    KB = 1024 * B,
    MB = 1024 * KB,
    GB = 1024 * MB,
    
    BandwidthTestSize = 20 * GB,
    BandwidthMessageSize = 100 * MB,
    BandwidthTimes = BandwidthTestSize div BandwidthMessageSize,
    BandwidthRunTime = run(Transport, BandwidthMessageSize, BandwidthTimes),
    MBps = (BandwidthTestSize div MB) / BandwidthRunTime,

    MessageTestSize = 1 * MB,
    MessageMessageSize = 1 * B,
    MessageTimes = MessageTestSize div MessageMessageSize,
    MessageRunTime = run(Transport, MessageMessageSize, MessageTimes),
    MSps = MessageTimes / MessageRunTime,
    {MBps, MSps}.
    

run(Transport, Size, Times) ->
    Self = self(),
    spawn_link(fun() -> run_server(Self, Transport, Size, Times) end),
    receive listening -> ok end,
    spawn_link(fun() -> run_client(Self, Transport, Size, Times) end),

    StartTime = receive {start_time, S} -> S end,
    EndTime = receive {end_time, E} -> E end,
    timer:now_diff(EndTime, StartTime) div 1000000.


run_client(Parent, Transport, Size, Times) ->
    {ok, Sock} = connect(Transport, "127.0.0.1", 5556),

    Message = <<0:Size/little-signed-integer-unit:8>>,
    StartTime = erlang:now(),
    Parent ! {start_time, StartTime},
    send_n(Transport, Sock, Message, Times).


run_server(Parent, Transport, Size, Times) ->
    {ok, Acc} = listen(Transport, 5556),
    Parent ! listening,

    {ok, Sock} = accept(Transport, Acc),

    recv_n(Transport, Sock, Size, Times),
    EndTime = erlang:now(),
    Parent ! {end_time, EndTime}.


send_n(_Transport, _Sock, _Message, 0) -> ok;
send_n(Transport, Sock, Message, Times) ->
    ok = send(Transport, Sock, Message),
    send_n(Transport, Sock, Message, Times - 1).


recv_n(_Transport, _Sock, _Size, 0) -> ok;
recv_n(Transport, Sock, Size, Times) ->
    {ok, _} = recv(Transport, Sock, Size),
    recv_n(Transport, Sock, Size, Times - 1).


listen(ssl2, Port) -> ssl2:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}]);
listen(ssl, Port) -> ssl:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}, {reuseaddr, true}]);
listen(ssl2_nif, Port) ->
    ssl2_nif:listen(Port, "server.pem", "server.key").


connect(ssl2, Host, Port) -> ssl2:connect(Host, Port, []);
connect(ssl, Host, Port) ->
    {ok, Sock} = ssl:connect(Host, Port, [{verify, verify_none}]),
    ok = ssl:setopts(Sock, [{binary, true}, {nodelay, true}, {active, false}]),
    {ok, Sock};
connect(ssl2_nif, Host, Port) ->
    Ref = make_ref(),
    ok = ssl2_nif:connect(Ref, Host, Port),
    receive {Ref, R} -> R end.


accept(ssl2, Acc) ->
    {ok, Sock} = ssl2:accept(Acc),
    ok = ssl2:handshake(Sock),
    {ok, Sock};
accept(ssl, Acc) ->
    {ok, Sock} = ssl:transport_accept(Acc),
    ok = ssl:ssl_accept(Sock),
    ok = ssl:setopts(Sock, [{binary, true}, {nodelay, true}, {active, false}]),
    {ok, Sock};
accept(ssl2_nif, Acc) ->
    Ref = make_ref(),
    ok = ssl2_nif:accept(Ref, Acc),
    {ok, Sock} = receive {Ref, R} -> R end,
    ok = ssl2_nif:handshake(Ref, Sock),
    ok = receive {Ref, R2} -> R2 end,
    {ok, Sock}.


send(ssl2, Sock, Message) -> ssl2:send(Sock, Message);
send(ssl, Sock, Message) -> ssl:send(Sock, Message);
send(ssl2_nif, Sock, Message) ->
    ok = ssl2_nif:send(Sock, Message),
    receive R -> R end.


recv(ssl2, Sock, Size) -> ssl2:recv(Sock, Size);
recv(ssl, Sock, Size) -> ssl:recv(Sock, Size);
recv(ssl2_nif, Sock, Size) ->
    ok = ssl2_nif:recv(Sock, Size),
    receive R -> R end.
