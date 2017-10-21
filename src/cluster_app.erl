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
	NodeType = application:get_env(cluster, node_type, ?DEFAULT_NODE_TYPE),		
	Servers = servers(NodeType),
	start_child(Servers),
	connect(NodeType),
	<<A:32,B:32,C:32>> = crypto:strong_rand_bytes(12),
	random:seed(A, B, C),
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

connect(center) ->
	ignore;
connect(_) ->
	cluster_client:async(connect).

servers(center) ->
	[
		{cluster_server, {cluster_server, start_link, []}, transient, 10000, worker, [cluster_server]}
	];
servers(_) ->
	[
		{cluster_client, {cluster_client, start_link, []}, transient, 10000, worker, [cluster_client]}
	].

%%====================================================================
%% Internal functions
%%====================================================================
