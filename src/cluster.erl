%%%-------------------------------------------------------------------
%% @doc cluster public API
%% @end
%%%-------------------------------------------------------------------

-module(cluster).

-export([start/0, stop/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
	application:ensure_all_started(cluster).
    % application:start(cluster).

%%--------------------------------------------------------------------
stop() ->
	application:stop(cluster).
