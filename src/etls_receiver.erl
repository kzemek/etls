%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.md'.
%%% @end
%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% A gen_fsm responsible for receive-related actions on a socket,
%%% including active notifications.
%%% @end
%%%--------------------------------------------------------------------
-module(etls_receiver).
-author("Konrad Zemek").

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
    idle/2, idle/3,
    receiving/2, receiving/3,
    receiving_header/2, receiving_header/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {
    socket :: etls_nif:socket(),
    buffer = <<>> :: binary(),
    caller :: {pid(), term()},
    needed :: integer(),
    timer = make_ref() :: reference(),
    active = false :: false | once | true,
    controlling_pid :: pid(),
    sock_ref :: term(),
    packet = 0 :: 0 | 1 | 2 | 4,
    exit_on_close = true :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process for this module.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Sock :: term(), Options :: list(), Pid :: pid()) ->
    {ok, pid()} | ignore | {error, Reason :: term()}.
start_link(Sock, Options, Pid) ->
    gen_fsm:start_link(?MODULE, [Sock, Options, Pid], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the gen_fsm.
%% The option settings is deferred to reuse code in handle_event.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: #state{}} |
    {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Sock, Options, Pid]) ->
    process_flag(trap_exit, true),
    gen_fsm:send_all_state_event(self(), {setopts, Options}),
    {ok, idle, #state{socket = Sock, controlling_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Idle state callback.
%% @end
%%--------------------------------------------------------------------
-spec idle(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
idle(_Event, State) ->
    {next_state, idle, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Synchronous idle state callback.
%% This callback will be called to start receiving data through the
%% socket. If for any reason the request can already be satisfied
%% from the buffer, it is, and the gen_fsm remains in the idle state.
%% Otherwise a NIF's receive is called and the gen_fsm's state is
%% changed to receiving_header or receiving (depending on the packet
%% option).
%% @end
%%--------------------------------------------------------------------
-spec idle(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewState :: #state{}} |
    {stop, Reason :: normal | term(), Reply :: term(),
        NewState :: #state{}}.
idle({recv, Size, Timeout}, From, #state{packet = 0} = State) ->
    #state{buffer = Buffer} = State,
    case {Size, byte_size(Buffer)} of
        {_, 0} ->
            Timer = create_timer(Timeout),
            recv_body(Size,
                State#state{timer = Timer, caller = From, needed = Size});

        {0, _} ->
            {reply, {ok, Buffer}, idle, State#state{buffer = <<>>}};

        {_, BS} when BS >= Size ->
            <<SubBin:Size/binary, Rest/binary>> = Buffer,
            {reply, {ok, SubBin}, idle, State#state{buffer = Rest}};

        {_, BS} ->
            Timer = create_timer(Timeout),
            RecvSize = Size - BS,
            recv_body(RecvSize,
                State#state{timer = Timer, caller = From, needed = Size})
    end;

idle({recv, _Size, Timeout}, From, State) ->
    #state{buffer = Buffer, packet = Packet} = State,
    case Buffer of
        <<>> ->
            Timer = create_timer(Timeout),
            recv_header(
                State#state{timer = Timer, caller = From, needed = Packet});

        _ ->
            {reply, {ok, Buffer}, idle, State#state{buffer = <<>>}}
    end;

idle(Event, _From, State) ->
    {reply, {error, {bad_event_for_state, idle, Event}}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Receiving state callback.
%% @end
%%--------------------------------------------------------------------
-spec receiving(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
receiving(_Event, State) ->
    {next_state, receiving, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Synchronous receiving state callback.
%% The receiving state waits for results of receive operation.
%% If a client has timed out, and the gen_fsm still remains in the
%% receiving state, this callback is used to set a new caller who will
%% receive the data from the socket.
%% @end
%%--------------------------------------------------------------------
-spec receiving(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewState :: #state{}} |
    {stop, Reason :: normal | term(), Reply :: term(),
        NewState :: #state{}}.
receiving({recv, Size, Timeout}, From,
    #state{caller = undefined, packet = 0} = State) ->

    Timer = create_timer(Timeout),
    {next_state, receiving,
        State#state{caller = From, needed = Size, timer = Timer}};

receiving({recv, _Size, Timeout}, From, #state{caller = undefined} = State) ->
    Timer = create_timer(Timeout),
    {next_state, receiving, State#state{caller = From, timer = Timer}};

receiving(Event, _From, State) ->
    {reply, {error, {bad_event_for_state, receiving, Event}}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Receiving header state callback.
%% @end
%%--------------------------------------------------------------------
-spec receiving_header(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
receiving_header(_Event, State) ->
    {next_state, receiving_header, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Synchronous receiving header state callback.
%% The receiving header state waits for {packet, N} header.
%% If a client has timed out, and the gen_fsm still remains in the
%% receiving state, this callback is used to set a new caller who will
%% receive the data from the socket.
%% @end
%%--------------------------------------------------------------------
-spec receiving_header(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewState :: #state{}} |
    {stop, Reason :: normal | term(), Reply :: term(),
        NewState :: #state{}}.
receiving_header({recv, _Size, Timeout}, From,
    #state{caller = undefined} = State) ->

    Timer = create_timer(Timeout),
    {next_state, receiving_header, State#state{caller = From, timer = Timer}};

receiving_header(Event, _From, State) ->
    {reply, {error, {bad_event_for_state, receiving_header, Event}}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles events independent of state: setting options and
%% communicating with other processes.
%% Inter-process communication isimplemented in terms of event
%% handling because replying is often deferred by the original
%% handler, and the final handler wants to change state _first_ before
%% communicating with a client.
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
    {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
    {next_state, NextStateName :: atom(), NewStateData :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewStateData :: #state{}}.
handle_event({sock_ref, SockRef}, StateName, State) ->
    {next_state, StateName, State#state{sock_ref = SockRef}};

handle_event({setopts, Opts}, idle, State) ->
    #state{active = OldActive, buffer = Buffer, sock_ref = Ref,
        exit_on_close = OldExitOnClose} = State,

    Packet = get_packet(Opts, State),
    Active = proplists:get_value(active, Opts, OldActive),
    ExitOnClose = proplists:get_value(exit_on_close, Opts, OldExitOnClose),
    UpdatedState = State#state{packet = Packet, exit_on_close = ExitOnClose},

    %% Handle active change
    case {OldActive, Active, Buffer} of
        {false, _, <<>>} when Active =:= once; Active =:= true ->
            recv_packet(UpdatedState#state{active = Active});

        {false, once, _} ->
            gen_fsm:send_all_state_event(self(), {notify, {etls, Ref, Buffer}}),
            {next_state, idle, UpdatedState#state{buffer = <<>>}};

        {false, true, _} ->
            gen_fsm:send_all_state_event(self(), {notify, {etls, Ref, Buffer}}),
            recv_packet(UpdatedState#state{buffer = <<>>, active = true});

        _ ->
            {next_state, idle, UpdatedState#state{active = Active}}
    end;

handle_event({setopts, Opts}, StateName, State) ->
    #state{active = OldActive, exit_on_close = OldExitOnClose} = State,
    Packet = get_packet(Opts, State),
    Active = proplists:get_value(active, Opts, OldActive),
    ExitOnClose = proplists:get_value(exit_on_close, Opts, OldExitOnClose),
    {next_state, StateName, State#state{
        active = Active, packet = Packet, exit_on_close = ExitOnClose}};

handle_event({controlling_process, Pid}, StateName, State) ->
    {next_state, StateName, State#state{controlling_pid = Pid}};

handle_event({notify, Msg}, StateName, #state{controlling_pid = Pid} = State) ->
    Pid ! Msg,
    {next_state, StateName, State};

handle_event({reply, Msg}, StateName, State) ->
    #state{timer = Timer, caller = Caller} = State,
    gen_fsm:cancel_timer(Timer),
    reply(Caller, Msg),
    {next_state, StateName,
        State#state{timer = make_ref(), caller = undefined}};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles synchronous inter-state events.
%% @end
%%--------------------------------------------------------------------
-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
    {stop, Reason :: term(), NewStateData :: term()}.
handle_sync_event(Event, _From, StateName, State) ->
    {reply, {error, {bad_event_for_state, StateName, Event}}, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles messages from other processes.
%% This callback is used for communication with NIF.
%% The timeout message is sent by a timer. A timer is created for each
%% client that calls a function with a timeout. When the timer
%% expires, the client is cleared but the gen_fsm remains in the
%% receiving state until the data is received or an error occurs.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}.
handle_info(timeout, StateName, State) ->
    gen_fsm:send_all_state_event(self(), {reply, {error, timeout}}),
    {next_state, StateName, State};

handle_info({ok, Data}, receiving_header, State) ->
    #state{packet = Packet, caller = Caller} = State,

    case Data of
        <<SizeToRead:Packet/big-unsigned-integer-unit:8>> ->
            recv_body(SizeToRead, State#state{needed = SizeToRead});

        _ ->
            reply(Caller, {error, bad_header}),
            {stop, bad_header, State}
    end;

handle_info({ok, Data}, receiving, #state{caller = undefined} = State) ->
    #state{buffer = Buffer, sock_ref = Ref, active = Active} = State,
    AData = <<Buffer/binary, Data/binary>>,

    case Active of
        once ->
            gen_fsm:send_all_state_event(self(), {notify, {etls, Ref, AData}}),
            {next_state, idle, State#state{buffer = <<>>, active = false}};

        true ->
            gen_fsm:send_all_state_event(self(), {notify, {etls, Ref, AData}}),
            recv_packet(State#state{buffer = <<>>});

        false ->
            {next_state, idle, State#state{buffer = AData}}
    end;

handle_info({ok, Data}, receiving, State) ->
    #state{needed = Needed, buffer = Buffer, active = Active} = State,
    AData = <<Buffer/binary, Data/binary>>,

    Return = fun(NewBuffer) ->
        case Active of
            false ->
                {next_state, idle, State#state{buffer = NewBuffer, needed = 0}};

            _ ->
                recv_packet(State#state{buffer = NewBuffer})
        end
    end,

    case byte_size(AData) of
        BS when BS =:= Needed orelse Needed =:= 0 ->
            gen_fsm:send_all_state_event(self(), {reply, {ok, AData}}),
            Return(<<>>);

        TooMuch when TooMuch > Needed ->
            <<ReplyData:Needed/binary, Rest/binary>> = AData,
            gen_fsm:send_all_state_event(self(), {reply, {ok, ReplyData}}),
            Return(Rest);

        TooLittle ->
            ReallyNeeded = Needed - TooLittle,
            recv_body(ReallyNeeded, State#state{buffer = AData})
    end;

handle_info({error, Closed}, _StateName, State)
  when Closed =:= 'End of file'; Closed =:= 'UNEXPECTED_RECORD' ->
    reply(State#state.caller, {error, closed}),
    {stop, {shutdown, closed}, State};

handle_info({error, Reason}, _StateName, State) ->
    reply(State#state.caller, {error, Reason}),
    {stop, Reason, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Cleans up the receiver's process.
%% If active is set, the controlling process receives a etls_closed
%% or etls_error message.
%% If the connection was closed by the remote end and exit_on_close
%% option is set, a process that calls etls:close(Socket) is spawned.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term().
terminate(Reason, _StateName, State) ->
    #state{controlling_pid = Pid, active = Active, sock_ref = SockRef,
        exit_on_close = ExitOnClose} = State,

    case Active of
        false -> ok;
        _ ->
            Message =
                case Reason of
                    normal -> {etls_closed, SockRef};
                    shutdown -> {etls_closed, SockRef};
                    {shutdown, _} -> {etls_closed, SockRef};
                    _ -> {etls_error, SockRef, Reason}
                end,

            Pid ! Message
    end,

    case {Reason, ExitOnClose} of
        {{shutdown, closed}, true} -> spawn(etls, close, [SockRef]);
        _ -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: #state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Receives a "packet", i.e. a message without a set size. If
%% {packet, N} is set, every message is a packet. Otherwise tries to
%% receive any message from the socket.
%% @end
%%--------------------------------------------------------------------
-spec recv_packet(State :: #state{}) ->
    {next_state, receiving_header | receiving, NextState :: #state{}} |
    {stop, Reason :: atom(), State :: #state{}}.
recv_packet(#state{packet = 0} = NextState) ->
    recv_body(0, NextState#state{needed = 0});
recv_packet(#state{packet = Packet} = NextState) ->
    recv_header(NextState#state{needed = Packet}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Receives a header of a size given in {packet, N}.
%% @end
%%--------------------------------------------------------------------
-spec recv_header(State :: #state{}) ->
    {next_state, receiving_header, NextState :: #state{}} |
    {stop, Reason :: atom(), State :: #state{}}.
recv_header(State) ->
    #state{socket = Sock, packet = Packet, caller = Caller} = State,
    case etls_nif:recv(Sock, Packet) of
        ok -> {next_state, receiving_header, State};
        {error, Reason} when is_atom(Reason) ->
            reply(Caller, {error, Reason}),
            {stop, Reason, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Receives body of the message, with size set either by the heaader
%% message (if {packet, N} is set) or explicitely by the client.
%% @end
%%--------------------------------------------------------------------
-spec recv_body(Size :: non_neg_integer(), State :: #state{}) ->
    {next_state, receiving, NextState :: #state{}} |
    {stop, Reason :: atom(), State :: #state{}}.
recv_body(Size, State) ->
    #state{socket = Sock, caller = Caller} = State,
    case etls_nif:recv(Sock, Size) of
        ok -> {next_state, receiving, State};
        {error, Reason} when is_atom(Reason) ->
            reply(Caller, {error, Reason}),
            {stop, Reason, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a timer with a given timeout.
%% To allow consisten timer handling, a timeout of infinity produces a
%% reference() that does not correspond to a valid timer, but can be
%% used in erlang:cancel_timer.
%% @end
%%--------------------------------------------------------------------
-spec create_timer(Timeout :: timeout()) -> Timer :: reference().
create_timer(infinity) -> make_ref();
create_timer(Timeout) ->
    erlang:send_after(Timeout, self(), timeout).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves packet value from the options proplist.
%% A 'raw' value is converted to 0.
%% @end
%%--------------------------------------------------------------------
-spec get_packet(Opts :: [etls:option() | etls:ssl_option()],
    State :: #state{}) ->
    0 | 1 | 2 | 4.
get_packet(Opts, #state{packet = OldPacket}) ->
    case proplists:get_value(packet, Opts, OldPacket) of
        raw -> 0;
        Other -> Other
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls gen_fsm:reply() for a defined caller and does nothing
%% otherwise.
%% @end
%%--------------------------------------------------------------------
-spec reply(Caller :: undefined | {pid(), term()}, Message :: term()) -> ok.
reply(undefined, _Msg) -> ok;
reply(Caller, Msg) ->
    gen_fsm:reply(Caller, Msg),
    ok.
