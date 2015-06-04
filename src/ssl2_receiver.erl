%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc A gen_fsm responsible for receive-related actions on a socket,
%%% including active notifications.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl2_receiver).
-author("Konrad Zemek").

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
    idle/2,
    idle/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4, receiving/2, receiving/3]).

-define(SERVER, ?MODULE).

-record(state, {
    socket :: term(),
    buffer = <<>> :: binary(),
    caller :: pid(),
    needed :: integer(),
    timer :: timer:timers(),
    active = false :: false | once | true,
    controlling_pid :: pid(),
    sock_ref :: term(),
    packet = raw :: raw | 0 | 1 | 2 | 4
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Sock :: term(), Options :: list(), Pid :: pid()) ->
    {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Sock, Options, Pid) ->
    gen_fsm:start_link(?MODULE, [Sock, Options, Pid], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: #state{}} |
    {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Sock, Options, Pid]) ->
    process_flag(trap_exit, true),
    gen_fsm:send_all_state_event(self(), {setopts, Options}),
    {ok, idle, #state{socket = Sock, controlling_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(idle(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
idle(_Event, State) ->
    {next_state, idle, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(idle(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewState :: #state{}} |
    {stop, Reason :: normal | term(), Reply :: term(),
        NewState :: #state{}}).
idle({recv, Size, Timeout}, From, State) ->
    #state{buffer = Buffer, packet = Packet} = State,

    Timer =
        case Timeout of
            infinity -> undefined;
            _ -> gen_fsm:start_timer(Timeout, timeout)
        end,

    case {Size, Packet, Buffer} of
        {0, 0, <<>>} ->
            recv(Size, Size, From, idle, State#state{timer = Timer});

        {_, _, _} when (Size =:= 0 orelse Packet > 0)
            andalso erlang:byte_size(Buffer) > 0 ->
            {reply, {ok, Buffer}, idle, State#state{buffer = <<>>}};

        _ ->
            case byte_size(Buffer) of
                BS when BS >= Size ->
                    <<SubBin:Size/binary, Rest/binary>> = Buffer,
                    gen_fsm:cancel_timer(Timer),
                    {reply, {ok, SubBin}, idle, State#state{buffer = Rest}};

                BS ->
                    NeedToFetch = Size - BS,
                    recv(NeedToFetch, Size, From, idle,
                        State#state{timer = Timer})
            end
    end;
idle(Event, _From, State) ->
    {reply, {error, {bad_event_for_state, idle, Event}}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(receiving(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
receiving({timeout, _, _}, State) ->
    gen_fsm:send_all_state_event(self(), {reply, {error, timeout}}),
    {next_state, receiving, State};
receiving(_Event, State) ->
    {next_state, receiving, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(receiving(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewState :: #state{}} |
    {stop, Reason :: normal | term(), Reply :: term(),
        NewState :: #state{}}).
receiving(Event, _From, State) ->
    {reply, {error, {bad_event_for_state, receiving, Event}}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
    {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
    {next_state, NextStateName :: atom(), NewStateData :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event({sock_ref, SockRef}, StateName, State) ->
    {next_state, StateName, State#state{sock_ref = SockRef}};
handle_event({setopts, Opts}, StateName, State) ->
    #state{active = OldActive, buffer = Buffer, sock_ref = Ref} = State,

    Packet =
        case proplists:get_value(packet, Opts, 0) of
            raw -> 0;
            Other -> Other
        end,

    UpdatedState = State#state{packet = Packet},
    Active = proplists:get_value(active, Opts, false),

    case OldActive of
        false when Active =:= once andalso Buffer =/= <<>> ->
            gen_fsm:send_all_state_event(self(), {notify, {ssl2, Ref, Buffer}}),
            {next_state, idle, UpdatedState#state{buffer = <<>>}};

        false when Active =:= true andalso Buffer =/= <<>> ->
            gen_fsm:send_all_state_event(self(), {notify, {ssl2, Ref, Buffer}}),
            recv(0, 0, undefined, StateName,
                UpdatedState#state{buffer = <<>>, active = Active});

        false when Active =:= once orelse Active =:= true ->
            recv(0, 0, undefined, StateName,
                UpdatedState#state{active = Active});

        _ ->
            {next_state, StateName, UpdatedState#state{active = Active}}
    end;
handle_event({notify, Msg}, StateName, #state{controlling_pid = Pid} = State) ->
    Pid ! Msg,
    {next_state, StateName, State};
handle_event({reply, Msg}, StateName, State) ->
    #state{timer = Timer, caller = Caller} = State,
    gen_fsm:cancel_timer(Timer),
    gen_fsm:reply(Caller, Msg),
    {next_state, StateName,
        State#state{timer = undefined, caller = undefined, needed = undefined}};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
    {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info({ok, Data}, idle, State) ->
    OldBuffer = State#state.buffer,
    {next_state, idle, State#state{buffer = <<OldBuffer/binary, Data/binary>>}};

handle_info({ok, Data}, receiving_header, #state{packet = Packet} = State) ->
    <<SizeToRead:Packet/big-unsigned-integer-unit:8>> = Data,
    recv(SizeToRead, SizeToRead, State#state.caller, receiving_header, State);

handle_info({ok, Data}, receiving, #state{caller = undefined} = State) ->
    #state{buffer = Buffer, sock_ref = Ref, active = Active} = State,
    AData = <<Buffer/binary, Data/binary>>,

    case Active of
        once ->
            gen_fsm:send_all_state_event(self(), {notify, {ssl2, Ref, AData}}),
            {next_state, idle, State#state{buffer = <<>>, active = false}};

        true ->
            gen_fsm:send_all_state_event(self(), {notify, {ssl2, Ref, AData}}),
            recv(0, 0, undefined, receiving, State#state{buffer = <<>>});

        false ->
            {next_state, idle, State#state{buffer = AData}}
    end;

handle_info({ok, Data}, receiving, State) ->
    #state{needed = Needed, buffer = Buffer, caller = Caller} = State,
    AData = <<Buffer/binary, Data/binary>>,

    case byte_size(AData) of
        BS when BS =:= Needed orelse Needed =:= 0 ->
            gen_fsm:send_all_state_event(self(), {reply, {ok, AData}}),
            {next_state, idle, State#state{buffer = <<>>}};

    % Following should logically not be possible, but let's handle it anyway
        TooMuch when TooMuch > Needed ->
            <<ReplyData:Needed/binary, Rest/binary>> = AData,
            Reply = {ok, ReplyData},
            gen_fsm:send_all_state_event(self(), {reply, Reply}),
            {next_state, idle, State#state{buffer = Rest}};

    % What is even happening
        TooLittle ->
            ReallyNeeded = Needed - TooLittle,
            recv(ReallyNeeded, Needed, Caller, receiving,
                State#state{buffer = AData})
    end;

handle_info({error, Reason}, _StateName, #state{caller = undefined} = State) ->
    {stop, parse_reason(Reason), State};
handle_info({error, Reason}, _StateName, #state{caller = Caller} = State) ->
    gen_fsm:reply(Caller, {error, Reason}),
    {stop, parse_reason(Reason), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(Reason, _StateName, State) ->
    #state{controlling_pid = Pid, active = Active, sock_ref = Ref} = State,
    case Active of
        false -> ok;
        _ ->
            Message =
                case Reason of
                    normal -> {ssl2_closed, Ref};
                    shutdown -> {ssl2_closed, Ref};
                    {shutdown, _} -> {ssl2_closed, Ref};
                    _ -> {ssl2_error, Ref, Reason}
                end,

            Pid ! Message
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

recv(Size, Needed, From, CurrentState, State) ->
    #state{socket = Sock, packet = Packet} = State,

    {RecvSize, NewState} =
        case {Packet, CurrentState} of
            {0, _} -> {Size, receiving};
            {_, receiving_header} -> {Size, receiving};
            _ -> {Packet, receiving_header}
        end,

    case ssl2_nif:recv(Sock, RecvSize) of
        {error, Reason} ->
            gen_fsm:reply(From, {error, Reason}),
            {stop, Reason, State};

        ok ->
            {next_state, NewState, State#state{caller = From, needed = Needed}}
    end.

parse_reason(Reason) ->
    case Reason of
        "End of file" -> {shutdown, Reason};
        _ -> Reason
    end.
