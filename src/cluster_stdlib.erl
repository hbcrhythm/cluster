%% ---------------------------------------
%% @doc cluster stdlib
%% @author labihbc@gmail.com
%% @end
%% ---------------------------------------
-module(cluster_stdlib).

-include("cluster.hrl").

-export([call/4, cast/4, call_broadcast/4, cast_broadcast/4, open/1, close/1]).

%% @doc do call
call(ServerId, M, F, A) when is_integer(ServerId) ->
	case ets:lookup(?CLUSTER_SERVER_ID, ServerId) of
		[#cluster_server_id{id = Id, node = Node}] ->
			{ok, NodeType} = application:get_env(cluster, node_type),
			case ets:lookup(?CLUSTER_SERVER, Id) of
				[#cluster_server{type = NodeType}] ->
					{error, same_node_type};
				[#cluster_server{node = Node}] ->
					call({node, Node}, M, F, A)
			end;
		_ ->
			{error, not_serverid}
	end;

%% @doc random a node of NodeType to do call
call(NodeType, M, F, A) when is_atom(NodeType) ->
	ClusterServers = ets:match_object(?CLUSTER_SERVER, #cluster_server{type = NodeType, is_open = ?CLUSTER_OPEN_STATUS, _ = '_'}),
	case ClusterServers =:= [] of
		false ->
			RandomId = random:uniform(length(ClusterServers)),
			#cluster_server{node = Node} = lists:nth(RandomId, ClusterServers),
			call({node, Node}, M, F, A);
		true ->
			{error, not_node_type}
	end;

call({node, Node}, M, F, A) ->
	rpc:call(Node, M, F, A);
call({pid, Pid}, M, F, A) ->
	rpc:call(node(Pid), M, F, A).

%% @doc call broadcast to cluster
call_broadcast({type, NodeType}, M, F, A) ->
	ClusterServers = ets:match_object(?CLUSTER_SERVER, #cluster_server{type = NodeType, _ = '_'}),
	case ClusterServers =:= [] of
		false ->
			[call({node, Node}, M, F, A) || #cluster_server{node = Node} <- ClusterServers];
		true ->
			{error, not_node_type}
	end.


cast(ServerId, M, F, A) when is_integer(ServerId) ->
	case ets:lookup(?CLUSTER_SERVER_ID, ServerId) of
		[#cluster_server_id{id = Id, node = Node}] ->
			{ok, NodeType} = application:get_env(cluster, node_type),
			case ets:lookup(?CLUSTER_SERVER, Id) of
				[#cluster_server{type = NodeType}] ->
					{error, same_node_type};
				[#cluster_server{node = Node}] ->
					cast({node, Node}, M, F, A)
			end;
		_ ->
			{error, not_serverid}
	end;

%% @doc random a node of NodeType to do cast
cast(NodeType, M, F, A) when is_atom(NodeType) ->
	ClusterServers = ets:match_object(?CLUSTER_SERVER, #cluster_server{type = NodeType, is_open = ?CLUSTER_OPEN_STATUS, _ = '_'}),
	case ClusterServers =:= [] of
		false ->
			RandomId = random:uniform(length(ClusterServers)),
			#cluster_server{node = Node} = lists:nth(RandomId, ClusterServers),
			cast({node, Node}, M, F, A);
		true ->
			{error, not_node_type}
	end;

cast({node, Node}, M, F, A) ->
	rpc:cast(Node, M, F, A);
cast({pid, Pid}, M, F, A) ->
	rpc:cast(node(Pid), M, F, A).

%% @doc call broadcast to cluster
cast_broadcast({type, NodeType}, M, F, A) ->
	ClusterServers = ets:match_object(?CLUSTER_SERVER, #cluster_server{type = NodeType, _ = '_'}),
	case ClusterServers =:= [] of
		false ->
			[cast({node, Node}, M, F, A) || #cluster_server{node = Node} <- ClusterServers];
		true ->
			{error, not_node_type}
	end.

open(ServerId) ->
	case application:get_env(cluster, srv_id) of
		{ok, ServerId} ->
			cluster_client:async({is_open, ?CLUSTER_OPEN_STATUS});
		_ ->
			{error, need_local_call}
	end.

close(ServerId) ->
	case application:get_env(cluster, srv_id) of
		{ok, ServerId} ->
			cluster_client:async({is_open, ?CLUSTER_CLOSE_STATUS});
		_ ->
			{error, need_local_call}
	end.
