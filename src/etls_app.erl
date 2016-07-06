%%%--------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.md'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% The etls application module.
%%% @end
%%%--------------------------------------------------------------------
-module(etls_app).
-author("Konrad Zemek").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the main supervisor of etls application.
%% @end
%%--------------------------------------------------------------------
-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    etls_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops the etls application.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
