%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% A supervisor responsible for managing send and receive gen_fsms
%%% for a single socket.
%%% @end
%%%--------------------------------------------------------------------
-module(ssl2_socket_sup).
-author("Konrad Zemek").

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a supervisor for this module.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Sock :: term(), Options :: list(), CtrlPid :: pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Sock, Options, CtrlPid) ->
    supervisor:start_link(?MODULE, [Sock, Options, CtrlPid]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the supervisor.
%% The socket supervisor creates two children based on ssl2_sender and
%% ssl2_receiver modules.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore.
init([Sock, Options, CtrlPid]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    Receiver = {receiver, {ssl2_receiver, start_link, [Sock, Options, CtrlPid]},
        Restart, Shutdown, Type, [ssl2_receiver]},

    Sender = {sender, {ssl2_sender, start_link, [Sock, Options]},
        Restart, Shutdown, Type, [ssl2_sender]},

    {ok, {SupFlags, [Receiver, Sender]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
