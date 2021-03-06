%% ---------------------------------------
%% @doc cluster stdlib
%% @author labihbc@gmail.com
%% @end
%% ---------------------------------------
-module(cluster_stdlib).

-include("cluster.hrl").

-export([call/4, cast/4, call_broadcast/4, cast_broadcast/4, open/0, close/0]).
-export([add_srvup_event/1, add_srvdown_event/1, del_srvup_event/0, del_srvup_event/1, del_srvdown_event/0, del_srvdown_event/1]).
-export([open_connect/0, close_connect/0, cluster_connect/0]).
-export([get_srv/2, get_srvid_list/0, get_srvid_list/1]).

%% @doc do call
call(ServerId, M, F, A) when is_integer(ServerId) ->
	FullId = application:get_env(cluster, full_id, []),	
	case lists:member(ServerId, FullId) of
		true ->
			call({node, node()}, M, F, A);
		false ->
			case ets:lookup(?CLUSTER_SERVER_ID, ServerId) of
				[#cluster_server_id{id = Id, node = Node}] ->
					% {ok, NodeType} = application:get_env(cluster, node_type),
					case ets:lookup(?CLUSTER_SERVER, Id) of
						% [#cluster_server{type = NodeType}] ->
							% {error, same_node_type};
						[#cluster_server{node = Node}] ->
							call({node, Node}, M, F, A);
						[] ->
							{error, not_serverid}
					end;
				_ ->
					{error, not_serverid}
			end
	end;

%% @doc random a node of NodeType to do call
call(NodeType, M, F, A) when is_atom(NodeType) ->
	ClusterServers = ets:match_object(?CLUSTER_SERVER, #cluster_server{type = NodeType, is_open = ?CLUSTER_OPEN_STATUS, _ = '_'}),
	case ClusterServers =:= [] of
		false ->
			RandomId = rand:uniform(length(ClusterServers)),
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
	FullId = application:get_env(cluster, full_id, []),	
	case lists:member(ServerId, FullId) of
		true ->
			cast({node, node()}, M, F, A);
		false ->
			case ets:lookup(?CLUSTER_SERVER_ID, ServerId) of
				[#cluster_server_id{id = Id, node = Node}] ->
					% {ok, NodeType} = application:get_env(cluster, node_type),
					case ets:lookup(?CLUSTER_SERVER, Id) of
						% [#cluster_server{type = NodeType}] ->
							% {error, same_node_type};
						[#cluster_server{node = Node}] ->
							cast({node, Node}, M, F, A);
						[] ->
							{error, not_serverid}
					end;
				_ ->
					{error, not_serverid}
			end
	end;

%% @doc random a node of NodeType to do cast
cast(NodeType, M, F, A) when is_atom(NodeType) ->
	ClusterServers = ets:match_object(?CLUSTER_SERVER, #cluster_server{type = NodeType, is_open = ?CLUSTER_OPEN_STATUS, _ = '_'}),
	case ClusterServers =:= [] of
		false ->
			RandomId = rand:uniform(length(ClusterServers)),
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

open() ->
	cluster_client:async({is_open, ?CLUSTER_OPEN_STATUS}).

close() ->
	cluster_client:async({is_open, ?CLUSTER_CLOSE_STATUS}).

%% @doc SevUp, SevDown event, Need to be before cluster_connect().
add_srvup_event({F, A}) ->
	add_srvup_event({undefined, F, A});
add_srvup_event(Mfa = {_M, _F, _A}) ->
	cluster_event_stdlib:event_add(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVUP, Mfa).

add_srvdown_event({F, A}) ->
	add_srvup_event({undefined, F, A});
add_srvdown_event(Mfa = {_M, _F, _A}) ->
	cluster_event_stdlib:event_add(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVDOWN, Mfa).

del_srvup_event() ->
	cluster_event_stdlib:event_del(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVUP).
del_srvup_event({F, A}) ->
	del_srvup_event({undefined, F, A});
del_srvup_event(Mfa = {_M, _F, _A}) ->
	cluster_event_stdlib:event_del(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVUP, Mfa).

del_srvdown_event() ->
	cluster_event_stdlib:event_del(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVDOWN).
del_srvdown_event({F, A}) ->
	del_srvdown_event({undefined, F, A});
del_srvdown_event(Mfa = {_M, _F, _A}) ->
	cluster_event_stdlib:event_del(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVDOWN, Mfa).

%% @doc change is_open_connect to true
open_connect() ->
	application:set_env(cluster, is_open_connect, true).

%% @doc change is_open_connect to false
close_connect() ->
	application:set_env(cluster, is_open_connect, false).

cluster_connect() ->
	cluster_app:connect().

get_srv(id, #cluster_server{id = Id}) ->
	Id;
get_srv(platform, #cluster_server{platform = Platform}) ->
	Platform;
get_srv(ver, #cluster_server{ver = Ver}) ->
	Ver;
get_srv(type, #cluster_server{type = Type}) ->
	Type;
get_srv(full_id, #cluster_server{full_id = FullId}) ->
	FullId;
get_srv(pid, #cluster_server{pid = Pid}) ->
	Pid;
get_srv(is_master, #cluster_server{is_master = IsMaster}) ->
	IsMaster;
get_srv(node, #cluster_server{node = Node}) ->
	Node;
get_srv(_, _) ->
	undefined.

get_srvid_list() ->
	[Id || #cluster_server{id = Id} <- ets:tab2list(?CLUSTER_SERVER)].	
get_srvid_list({type, Type}) ->
	[Id || #cluster_server{id = Id} <- ets:match_object(?CLUSTER_SERVER, #cluster_server{type = Type, _ = '_'})];
get_srvid_list({platform, Platform}) ->
	[Id || #cluster_server{id = Id} <- ets:match_object(?CLUSTER_SERVER, #cluster_server{platform = Platform, _ = '_'})].
