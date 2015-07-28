%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% A gen_fsm responsible for send-related actions on a socket.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl2_sender).
-author("Konrad Zemek").

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1,
    idle/2, idle/3,
    sending/2, sending/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {
    socket :: ssl2_nif:socket(),
    caller :: {pid(), term()},
    packet = 0 :: 0 | 1 | 2 | 4
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process for this module.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Sock :: term(), Options :: list()) ->
    {ok, pid()} | ignore | {error, Reason :: term()}.
start_link(Sock, Options) ->
    gen_fsm:start_link(?MODULE, [Sock, Options], []).

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
init([Sock, Options]) ->
    process_flag(trap_exit, true),
    gen_fsm:send_all_state_event(self(), {setopts, Options}),
    {ok, idle, #state{socket = Sock}}.

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
%% This callback will be called to start sending data through the
%% socket.
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
idle({send, Data}, From, State) ->
    #state{socket = Sock, packet = Packet} = State,

    SendData =
        case Packet of
            0 -> Data;
            _ ->
                DS = byte_size(Data),
                <<DS:Packet/big-unsigned-integer-unit:8, Data/binary>>
        end,

    case ssl2_nif:send(Sock, SendData) of
        ok -> {next_state, sending, State#state{caller = From}};
        {error, Reason} when is_atom(Reason) ->
            {stop, Reason, {error, Reason}, State}
    end;

idle(Event, _From, State) ->
    {reply, {error, {bad_event_for_state, idle, Event}}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sending state callback.
%% @end
%%--------------------------------------------------------------------
-spec sending(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
sending(_Event, State) ->
    {next_state, sending, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Synchronous sending state callback.
%% The sending state waits for results of send operation.
%% @end
%%--------------------------------------------------------------------
-spec sending(Event :: term(), From :: {pid(), term()},
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
sending(Event, _From, State) ->
    {reply, {error, {bad_event_for_state, sending, Event}}, sending, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles events independent of state: setting options and
%% communicating with gen_fsm caller.
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
handle_event({setopts, Opts}, StateName, State) ->
    Packet = get_packet(Opts, State),
    {next_state, StateName, State#state{packet = Packet}};

handle_event({reply, Msg}, StateName, #state{caller = Caller} = State) ->
    reply(Caller, Msg),
    {next_state, StateName, State#state{caller = undefined}};

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
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}.
handle_info(ok, _StateName, State) ->
    gen_fsm:send_all_state_event(self(), {reply, ok}),
    {next_state, idle, State};

handle_info({error, Reason}, _StateName, #state{caller = Caller} = State) ->
    reply(Caller, {error, Reason}),
    {stop, Reason, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Cleans up the receiver's process.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term().
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
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
%% Retrieves packet value from the options proplist.
%% A 'raw' value is converted to 0.
%% @end
%%--------------------------------------------------------------------
-spec get_packet(Opts :: [ssl2:option() | ssl2:ssl_option()],
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
