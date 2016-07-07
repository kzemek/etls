%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.md'.
%%% @end
%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% A supervisor responsible for managing send and receive gen_fsms
%%% for a single socket.
%%% @end
%%%--------------------------------------------------------------------
-module(etls_socket_sup).
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
%% The socket supervisor creates two children based on etls_sender and
%% etls_receiver modules.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(),
          [ChildSpec :: supervisor:child_spec()]}}.
init([Sock, Options, CtrlPid]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    Receiver = {receiver, {etls_receiver, start_link, [Sock, Options, CtrlPid]},
        Restart, Shutdown, Type, [etls_receiver]},

    Sender = {sender, {etls_sender, start_link, [Sock, Options]},
        Restart, Shutdown, Type, [etls_sender]},

    {ok, {SupFlags, [Receiver, Sender]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
