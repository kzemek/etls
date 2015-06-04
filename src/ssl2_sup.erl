%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc The main SSL2 supervisor.
%%%--------------------------------------------------------------------
-module(ssl2_sup).
-author("Konrad Zemek").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 100, timer:minutes(1)}, [
        {connection, {ssl2_socket_sup, start_link, []},
            temporary, 1000, supervisor, [ssl2_socket_sup]}
    ]}}.
