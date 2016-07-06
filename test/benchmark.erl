%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Performance benchmarks of erlang-tls.
%%% The module benchmarks connection's bandwidth and message throughput
%%% using various configuration parameters specified in config/1.
%%% The benchmark is run from console by calling run/0. etls's and
%%% dependencies' ebin directories must be present in the code path,
%%% and the CWD must be set to <erlang-tls dir>/test.
%%% @end
%%%--------------------------------------------------------------------

-module(benchmark).
-author("Konrad Zemek").

-define(B, 1).
-define(KB, (1024 * ?B)).
-define(MB, (1024 * ?KB)).
-define(GB, (1024 * ?MB)).

-record(bandwidth_config, {transport}).
-record(parameters_config, {transport, active, packet, connections, message_size, messages_num}).
-record(test_output, {mbps, msps}).

-export([run/0]).

run() ->
    ssl:start(),
    application:start(etls),
    OutputCases = lists:foldl(
        fun(TestCase, Acc) ->
            io:format(user, "Running case ~p~n", [TestCase]),

            Configs = config(TestCase),
            ConfigsNum = length(Configs),
            Nums = lists:seq(1, ConfigsNum),
            EnumeratedConfigs = lists:zip(Nums, Configs),

            OutputConfigs = lists:foldl(
                fun({Num, Config}, Acc2) ->
                    io:format(user, "  Running config ~p/~p: ~p~n", [Num, ConfigsNum, Config]),

                    BinaryNum = integer_to_binary(Num),
                    Out = run(Config),
                    maps:merge(config_output(BinaryNum, Config, Out), Acc2)
                end,
                #{}, EnumeratedConfigs),

            maps:merge(case_output(TestCase, OutputConfigs), Acc)
        end,
        #{}, cases()
    ),

    Output = output(OutputCases),
    file:write_file("performance.json", jiffy:encode(Output, [pretty])).


cases() ->
    [parameters_benchmark].


config(bandwidth_benchmark) ->
    [
        #bandwidth_config{transport = Transport} ||
        Transport <- [reference, etls_nif, etls]
    ];
config(parameters_benchmark) ->
    [
        #parameters_config{
            transport = Transport,
            active = Active,
            packet = Packet,
            connections = Connections,
            message_size = element(1, Size),
            messages_num = element(2, Size)
        } ||
        Transport <- [etls, etls_nif],
        Active <- [passive],
        Packet <- [0],
        Connections <- [10],
        Size <- [{?B, 1000000}, {100 * ?MB, 100}]
    ].


run(#bandwidth_config{transport = reference}) ->
    BenchmarkOutput = os:cmd("cd ../_build/test && ./bandwidthCap"),
    [MBps, _, MSps, _] = string:tokens(BenchmarkOutput, " \n\t"),
    #test_output{mbps = list_to_float(MBps), msps = list_to_float(MSps)};
run(#bandwidth_config{transport = Transport}) ->
    Port = random_port(),
    {ok, Acceptor} = listen(Transport, Port),

    BandwidthTestSize = 20 * ?GB,
    BandwidthMessageSize = 100 * ?MB,
    BandwidthTimes = BandwidthTestSize div BandwidthMessageSize,
    {BStartTime, BEndTime} = run(Transport, Acceptor, Port, passive, 0, BandwidthMessageSize, BandwidthTimes),
    BandwidthRunTime = timer:now_diff(BEndTime, BStartTime) div 1000000,
    MBps = (BandwidthTestSize div ?MB) / BandwidthRunTime,

    MessageTestSize = 1 * ?MB,
    MessageMessageSize = 1 * ?B,
    MessageTimes = MessageTestSize div MessageMessageSize,
    {MStartTime, MEndTime} = run(Transport, Acceptor, Port, passive, 0, MessageMessageSize, MessageTimes),
    MessageRunTime = timer:now_diff(MEndTime, MStartTime) div 1000000,
    MSps = MessageTimes / MessageRunTime,
    #test_output{mbps = MBps, msps = MSps};


run(#parameters_config{transport = Transport, packet = Packet, active = Active, connections = Connections,
    message_size = MessageSize, messages_num = MessagesNum}) ->

    Port = random_port(),
    {ok, Acceptor} = listen(Transport, Port),

    Refs = lists:map(
        fun(_) ->
            Self = self(),
            Ref = make_ref(),
            spawn_link(
                fun() ->
                    Out = (catch run(Transport, Acceptor, Port, Active, Packet, MessageSize, MessagesNum)),
                    Self ! {Ref, Out}
                end),
            Ref
        end,
        lists:seq(1, Connections)
    ),

    StartEndTimes = lists:map(
        fun(Ref) -> receive {Ref, Out} -> Out end end,
        Refs
    ),

    {StartTimes, EndTimes} = lists:unzip(StartEndTimes),
    {StartTime, EndTime} = {lists:min(StartTimes), lists:max(EndTimes)},

    TotalTime = timer:now_diff(EndTime, StartTime) div 1000000,
    MBps = MessageSize * MessagesNum * Connections / ?MB / TotalTime,
    MSps = MessagesNum * Connections / TotalTime,

    #test_output{mbps = MBps, msps = MSps}.


run(Transport, Acceptor, Port, Active, Packet, Size, Times) ->
    Self = self(),

    spawn_link(fun() ->
        StartTime = run_server(Transport, Acceptor, Active, Packet, Size, Times),
        Self ! {end_time, StartTime}
    end),

    spawn_link(fun() ->
        EndTime = run_client(Transport, Port, Packet, Size, Times),
        Self ! {start_time, EndTime}
    end),

    StartTime = receive {start_time, S} -> S end,
    EndTime = receive {end_time, E} -> E end,
    {StartTime, EndTime}.


run_client(Transport, Port, Packet, Size, Times) ->
    {ok, Sock} = connect(Transport, Packet, "127.0.0.1", Port),

    Message = <<0:Size/little-signed-integer-unit:8>>,
    StartTime = erlang:now(),
    send_n(Transport, Sock, Message, Times),
    StartTime.


run_server(Transport, Acceptor, Active, Packet, Size, Times) ->
    {ok, Sock} = accept(Transport, Acceptor, Packet),
    recv_n(Transport, Active, Sock, Size, Times),
    EndTime = erlang:now(),
    EndTime.


send_n(_Transport, _Sock, _Message, 0) -> ok;
send_n(Transport, Sock, Message, Times) ->
    ok = send(Transport, Sock, Message),
    send_n(Transport, Sock, Message, Times - 1).


recv_n(_Transport, _Active, _Sock, _Size, 0) -> ok;
recv_n(Transport, passive, Sock, Size, Times) ->
    {ok, _} = recv(Transport, Sock, Size),
    recv_n(Transport, passive, Sock, Size, Times - 1);
recv_n(Transport, active, Sock, Size, Times) ->
    recv_n(Transport, active, Sock, Size, Size, Times).

recv_n(Transport, active, Sock, Size, 0, Times) ->
    recv_n(Transport, active, Sock, Size, Times - 1);
recv_n(Transport, active, Sock, Size, SizeLeft, Times) ->
    ok = Transport:setopts(Sock, [{active, once}]),
    receive
        {Transport, Sock, D} -> recv_n(Transport, active, Sock, Size, SizeLeft - byte_size(D), Times);
        Else -> error(Else)
    end.


listen(etls, Port) -> etls:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}]);
listen(ssl, Port) -> ssl:listen(Port, [{certfile, "server.pem"}, {keyfile, "server.key"}, {reuseaddr, true}]);
listen(etls_nif, Port) ->
    etls_nif:listen(Port, "server.pem", "server.key", "verify_none",
                    false, false, "", [], [], []).


connect(etls, Packet, Host, Port) -> etls:connect(Host, Port, [{packet, Packet}]);
connect(ssl, Packet, Host, Port) ->
    {ok, Sock} = ssl:connect(Host, Port, [{verify, verify_none}]),
    ok = ssl:setopts(Sock, [{binary, true}, {nodelay, true}, {active, false}, {packet, Packet}]),
    {ok, Sock};
connect(etls_nif, _Packet, Host, Port) ->
    Ref = make_ref(),
    ok = etls_nif:connect(Ref, Host, Port, "", "", "verify_none",
                          false, false, "", [], [], []),
    receive {Ref, R} -> R end.


accept(etls, Acc, Packet) ->
    {ok, Sock} = etls:accept(Acc),
    ok = etls:handshake(Sock),
    ok = etls:setopts(Sock, [{packet, Packet}]),
    {ok, Sock};
accept(ssl, Acc, Packet) ->
    {ok, Sock} = ssl:transport_accept(Acc),
    ok = ssl:ssl_accept(Sock),
    ok = ssl:setopts(Sock, [{binary, true}, {nodelay, true}, {active, false}, {packet, Packet}]),
    {ok, Sock};
accept(etls_nif, Acc, _Packet) ->
    Ref = make_ref(),
    ok = etls_nif:accept(Ref, Acc),
    {ok, Sock} = receive {Ref, R} -> R end,
    ok = etls_nif:handshake(Ref, Sock),
    ok = receive {Ref, R2} -> R2 end,
    {ok, Sock}.


send(etls, Sock, Message) -> etls:send(Sock, Message);
send(ssl, Sock, Message) -> ssl:send(Sock, Message);
send(etls_nif, Sock, Message) ->
    ok = etls_nif:send(Sock, Message),
    receive R -> R end.


recv(etls, Sock, Size) -> etls:recv(Sock, Size);
recv(ssl, Sock, Size) -> ssl:recv(Sock, Size);
recv(etls_nif, Sock, Size) ->
    ok = etls_nif:recv(Sock, Size),
    receive R -> R end.


random_port() ->
    random:uniform(65535 - 49152) + 49152.


output(Cases) ->
    #{performance => #{
        branch => <<"feature/VFS-1162-performance-tests-for-erlang-tls">>,
        commit => <<"">>,
        repository => <<"erlang-tls">>,
        suites => #{
            ?MODULE => #{
                name => ?MODULE,
                description => <<"Performance benchmarks of erlang-tls.">>,
                copyright => <<"(C) 2015 ACK CYFRONET AGH\n"
                "This software is released under the MIT license\n"
                "cited in 'LICENSE.txt'.">>,
                authors => [<<"Konrad Zemek">>],
                cases => Cases
            }
        }
    }}.


case_output(bandwidth_benchmark, Configs) ->
    #{bandwidth_benchmark => #{
        configs => Configs,
        name => bandwidth_benchmark,
        description => <<"Benchmark of maximum bandwidth/throughput achievable with ssl modules.">>
    }};
case_output(parameters_benchmark, Configs) ->
    #{parameters_benchmark => #{
        configs => Configs,
        name => parameters_benchmark,
        description => <<"Benchmark of bandwidth/throughput of etls relative to its configuration.">>
    }}.


output_details(MBps, MSps) ->
    [
        #{
            description => <<"Average transfer speed.">>,
            name => mbps,
            unit => <<"MB/s">>,
            value => MBps
        },
        #{
            description => <<"Average message throughput.">>,
            name => msps,
            unit => <<"messages/s">>,
            value => MSps
        }
    ].


config_output(Num, #bandwidth_config{transport = Transport}, #test_output{mbps = MBps, msps = MSps}) ->
    maps:put(Num, #{
        name => Num,
        completed => timestamp(),
        description => <<"Config #", Num/binary>>,
        parameters => [#{
            description => <<"Name of a module providing the transport.">>,
            name => transport,
            unit => <<"">>,
            value => Transport
        }],
        repeats_number => 1,
        failed_repeats_details => #{},
        successful_repeats_number => 1,
        successful_repeats_details => output_details(#{<<"1">> => MBps}, #{<<"1">> => MSps}),
        successful_repeats_average => output_details(MBps, MSps),
        successful_repeats_summary => output_details(MBps, MSps)
    }, #{});
config_output(Num, #parameters_config{transport = Transport, packet = Packet, active = Active, connections = Connections,
    message_size = MessageSize, messages_num = MessagesNum}, #test_output{mbps = MBps, msps = MSps}) ->
    maps:put(Num, #{
        name => Num,
        completed => timestamp(),
        description => <<"Config #", Num/binary>>,
        parameters => [
            #{
                description => <<"Name of a module providing the transport.">>,
                name => transport,
                unit => <<"">>,
                value => Transport
            },
            #{
                description => <<"Receive type (see inet module).">>,
                name => active,
                unit => <<"">>,
                value => Active
            },
            #{
                description => <<"Packet header (see inet module).">>,
                name => packet,
                unit => <<"byte">>,
                value => Packet
            },
            #{
                description => <<"Number of concurrent connections.">>,
                name => connections,
                unit => <<"">>,
                value => Connections
            },
            #{
                description => <<"Message size">>,
                name => message_size,
                unit => <<"byte">>,
                value => MessageSize
            },
            #{
                description => <<"Message number">>,
                name => message_num,
                unit => <<"">>,
                value => MessagesNum
            }
        ],
        repeats_number => 1,
        failed_repeats_details => #{},
        successful_repeats_number => 1,
        successful_repeats_details => output_details([MBps], [MSps]),
        successful_repeats_average => output_details(MBps, MSps),
        successful_repeats_summary => output_details(MBps, MSps)
    }, #{}).


timestamp() ->
    {M, S, U} = erlang:now(),
    integer_to_binary(M * 1000000000 + S * 1000 + U).
