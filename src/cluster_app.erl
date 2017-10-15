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

connect(normal) ->
	cluster_client:async(connect);
connect(cloud) ->
	cluster_client:async(connect);
connect(center) ->
	cluster_server:async(connect).

servers(normal) ->
	[
		{cluster_msg, {cluster_msg, start_link, []}, transient, 10000, worker, [cluster_msg]},
		{cluster_client, {cluster_client, start_link, []}, transient, 10000, worker, [cluster_client]}
	];

servers(cloud) ->
	[
		{cluster_msg, {cluster_msg, start_link, []}, transient, 10000, worker, [cluster_msg]},
		{cluster_client, {cluster_client, start_link, []}, transient, 10000, worker, [cluster_client]}
	];

servers(center) ->
	[
		{cluster_msg, {cluster_msg, start_link, []}, transient, 10000, worker, [cluster_msg]},
		{cluster_server, {cluster_server, start_link, []}, transient, 10000, worker, [cluster_server]}
	].

%%====================================================================
%% Internal functions
%%====================================================================
