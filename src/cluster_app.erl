%%%-------------------------------------------------------------------
%% @doc cluster public API
%% @end
%%%-------------------------------------------------------------------

-module(cluster_app).

-behaviour(application).

-include("cluster.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	{ok, Pid} = cluster_sup:start_link(),
	IsMaster = application:get_env(cluster, is_master, ?DEFAULT_IS_MASTER),	
	Servers = servers(IsMaster),
	start_child(Servers),
	connect(IsMaster),
	<<A:32,B:32,C:32>> = crypto:strong_rand_bytes(12),
	random:seed(A, B, C),
	lager:info("start cluster application successe !"),
	{ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

start_child([]) ->
	ok;
start_child([H | T]) ->
	case cluster_sup:start_child(H) of
		{ok, _} ->
			start_child(T);
		_Error ->
			exit({error, _Error})
	end.

connect(true) ->
	ignore;
connect(false) ->
	cluster_client:async(connect).

servers(true) ->
	[
		{cluster_server, {cluster_server, start_link, []}, transient, 10000, worker, [cluster_server]},
		{cluster_event, {cluster_event_stdlib, start_link, [?CLUSTER_EVENT_NAME]}, transient, 10000, worker, [cluster_event]}
	];
servers(false) ->
	[
		{cluster_client, {cluster_client, start_link, []}, transient, 10000, worker, [cluster_client]},
		{cluster_event, {cluster_event_stdlib, start_link, [?CLUSTER_EVENT_NAME]}, transient, 10000, worker, [cluster_event]}
	].

%%====================================================================
%% Internal functions
%%====================================================================
