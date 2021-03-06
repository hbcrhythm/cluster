%% ---------------------------------------
%% @doc cluster server
%% @author labihbc@gmail.com
%% @end
%% ---------------------------------------

-module(cluster_server).

-behaviour(gen_server).

-export([async/1]).
-export([start_link/0, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("cluster.hrl").

-record(state, {
        server_local
    }).

async(Msg) ->
    ?MODULE ! Msg.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> ok. 

other_srvs(Node, _Platform) ->
    [XSrv|| XSrv = #cluster_server{node = XNode} <- ets:tab2list(?CLUSTER_SERVER), XNode /= Node].

init([]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true, [{node_type, all}]),
    ets:new(?CLUSTER_SERVER, [ordered_set, named_table, public, {keypos, #cluster_server.id}]),
    ets:new(?CLUSTER_SERVER_ID, [set, named_table, public, {keypos, #cluster_server_id.sub_id}]),
    ets:new(?CLUSTER_CLOUD_TASK, [set, named_table, public, {keypos, #cluster_cloud_task.id}]),
    {ok, SrvId} = application:get_env(cluster, srv_id),
    {ok, FullId} = application:get_env(cluster, full_id),
    {ok, NodeType} = application:get_env(cluster, node_type),
    {ok, Plaform} = application:get_env(cluster, platform),
    {ok, Ver} = application:get_env(cluster, ver),
    ClusterServer = #cluster_server{
        id          = SrvId
        ,full_id    = FullId
        ,type       = NodeType
        ,pid        = self()
        ,platform   = Plaform
        ,node       = node()
        ,cookie     = erlang:get_cookie()
        ,ver        = Ver
        ,is_master  = true
    },
    ets:insert(?CLUSTER_SERVER, ClusterServer),
    [ets:insert(?CLUSTER_SERVER_ID, #cluster_server_id{sub_id = SubId, id = SrvId, node = node()}) || SubId <- FullId],
    {ok, #state{server_local = ClusterServer}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    {noreply, State};
handle_info({connect, ClusterServer = #cluster_server{id = Id, full_id = FullId, node = Node, ver = Ver, pid = Pid, platform = Platform}}, State) ->
    IsOpenConnect = application:get_env(cluster, is_open_connect, ?DEFAULT_OPEN_CONNECT),
    case IsOpenConnect of
        true ->
            case ets:lookup(?CLUSTER_SERVER, Id) of
                [#cluster_server{}] ->
                    lager:error("server [~w]~w is exist", [Id, Node]);
                [] -> 
                    ets:insert(?CLUSTER_SERVER, ClusterServer),
                    [ets:insert(?CLUSTER_SERVER_ID, #cluster_server_id{sub_id = SubId, id = Id, node = Node}) || SubId <- FullId],
                    erlang:send(Pid, ready),
                    OtherSrvs = other_srvs(Node, Platform),
                    erlang:send(Pid, {sync_add_servers, OtherSrvs}),   %% 通知新节点，其他的节点
                    [ erlang:send(XPid, {sync_add_servers, [ClusterServer]}) || #cluster_server{pid = XPid} <- OtherSrvs, XPid =/= self()],    %% 通知现有的节点，新开了节点
                    cluster_event_stdlib:event_trigger(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVUP, [ClusterServer]),
                    lager:info("server [~w]~w accept success ~p", [Id, Node, Ver])
            end;
        false ->
            lager:error("cluster not open connect !~n")
    end,
    {noreply, State};

handle_info({nodeup, _Node}, State) ->
    {noreply, State};
handle_info({nodeup, _Node, _}, State) ->
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    nodedown(Node),
    {noreply, State};
handle_info({nodedown, Node, _}, State) ->
    nodedown(Node),
    {noreply, State};

handle_info({is_open, _}, State) ->
    {noreply, State};
handle_info({update_cluster_server, ClusterServer = #cluster_server{node = Node, platform = Platform}}, State) ->
    ets:insert(?CLUSTER_SERVER, ClusterServer),
    OtherSrvs = other_srvs(Node, Platform),
    [erlang:send(XPid, {cluster_servers, [ClusterServer]}) || #cluster_server{pid = XPid} <- OtherSrvs, XPid =/= self()],
    {noreply, State};

handle_info(_Info, State) ->
    lager:error("not know message: ~w", [_Info]),
    {noreply, State}.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:error("cluster server terminate reason: ~w", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

nodedown(Node) ->
    case ets:match_object(?CLUSTER_SERVER, #cluster_server{node=Node, _='_'}) of
        [] -> ignore;
        [ClusterServer = #cluster_server{id = Id, full_id = FullId, platform = Platform}| _] -> 
            ets:delete(?CLUSTER_SERVER, Id),
            [ets:delete(?CLUSTER_SERVER_ID, SubId) || SubId <- FullId],
            cluster_event_stdlib:event_trigger(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVDOWN, [ClusterServer]),
            OtherSrvs = other_srvs(Node, Platform),
            [erlang:send(XPid, {nodedown, ClusterServer}) || #cluster_server{pid = XPid} <- OtherSrvs, XPid =/= self()],
            lager:info("server [~w] ~w nodedown ", [Id, Node])
    end.
