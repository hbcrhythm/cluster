%% ---------------------------------------
%% @doc cluster client
%% @author labihbc@gmail.com
%% @end
%% ---------------------------------------
-module(cluster_client).

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

init([]) ->
    ets:new(?CLUSTER_SERVER, [ordered_set, named_table, public, {keypos, #cluster_server.id}]),
    ets:new(?CLUSTER_SERVER_ID, [set, named_table, public, {keypos, #cluster_server_id.sub_id}]),
    application:set_env(cluster, is_connect_cluster, false),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    {ok, SrvId} = application:get_env(cluster, srv_id),
    {ok, ServerFullId} = application:get_env(cluster, server_full_id),
    {ok, NodeType} = application:get_env(cluster, node_type),
    {ok, Plaform} = application:get_env(cluster, platform),
    {ok, Ver} = application:get_env(cluster, ver),
    ClusterServer = #cluster_server{
        id          = SrvId
        ,full_id    = ServerFullId
        ,type       = NodeType
        ,pid        = self()
        ,platform   = Plaform
        ,node       = node()
        ,cookie     = erlang:get_cookie()
        ,ver        = Ver
    },
    {ok, CenterNode}    = application:get_env(cluster, center_node),
    {ok, CenterCookie}  = application:get_env(cluster, center_cookie),
    case erlang:is_atom(CenterCookie) andalso CenterNode =/= undefined andalso CenterCookie =/= undefined 
        andalso CenterNode =/= '' andalso CenterCookie =/= '' 
    of
        true -> 
            erlang:set_cookie(CenterNode, CenterCookie),
            case net_adm:ping(CenterNode) of
                pong -> 
                    erlang:monitor_node(CenterNode, true),  %% 
                    erlang:send({cluster_server, CenterNode}, {connect, ClusterServer}),    %% 通知客户端节点接入
                    erlang:send_after(?CLUSTER_CONNECT_INTERVAL, self(), check_connect),
                    {noreply, State#state{server_local = ClusterServer}};
                pang -> 
                    lager:error("can not connect Node: ~w, Cookie:~w", [CenterNode, CenterCookie]),
                    erlang:send_after(?CLUSTER_CONNECT_INTERVAL, self(), check_connect),
                    {noreply, State}
            end;
        false -> 
            lager:error("not config center server"),
            {noreply, State}
    end;

handle_info(check_connect, State) ->
    case application:get_env(cluster, is_connect_cluster) of
        {ok, true} -> ingore;
        _ -> self() ! connect
    end,
    {noreply, State};

handle_info(ready, State) ->
    application:set_env(cluster, is_connect_cluster, true),
    lager:info("connect center server [~w] success ! ", [application:get_env(center_node)]),
    {noreply, State};

%% @doc CenterCookie is cluster handshake Cookie
handle_info({sync_add_servers, Servers}, State) ->
    {ok, CenterCookie}  = application:get_env(cluster, center_cookie),
    [
        begin
            erlang:set_cookie(Node, CenterCookie),
            ets:insert(?CLUSTER_SERVER, ClusterServer),
            [ets:insert(?CLUSTER_SERVER_ID, #cluster_server_id{sub_id = SubId, id = Id, node = Node}) || SubId <- FullId],
            cluster_event_stdlib:event_trigger(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVUP, [ClusterServer])
        end
        || ClusterServer = #cluster_server{id = Id, full_id = FullId, node = Node} <- Servers
    ],
    {noreply, State};

handle_info({is_open, Status}, State = #state{server_local = ClusterServer}) ->
    {ok, CenterNode}    = application:get_env(cluster, center_node),
    NewClusterServer = ClusterServer#cluster_server{is_open = Status},
    erlang:send({cluster_server, CenterNode}, {update_cluster_server, NewClusterServer}),
    {noreply, State#state{server_local = NewClusterServer}};

handle_info({nodeup, _Node}, State) ->
    {noreply, State};
handle_info({nodeup, _Node, _}, State) ->
    {noreply, State};

handle_info({nodedown, ClusterServer = #cluster_server{}}, State) ->
    nodedown(ClusterServer),
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    nodedown(Node),
    {noreply, State};

handle_info({nodedown, Node, _}, State) ->
    nodedown(Node),
    {noreply, State};

handle_info(_Info, State) ->
    lager:error("not know message: ~w", [_Info]),
    {noreply, State}.

terminate(normal, _State) ->
    ok;
    
terminate(Reason, _State) ->
    lager:error("terminate reason: ~w", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc is not master node down
nodedown(ClusterServer = #cluster_server{id = Id, full_id = FullId}) ->
    ets:delete(?CLUSTER_SERVER, Id),
    [ets:delete(?CLUSTER_SERVER_ID, SubId) || SubId <- FullId],
    cluster_event_stdlib:event_trigger(?CLUSTER_EVENT_NAME, ?CLUSTER_EVENT_SRVDOWN, [ClusterServer]),
    ignore;

%% @doc master node down
nodedown(Node) ->
    case application:get_env(cluster, center_node) of
        {ok, Node} -> 
            self() ! connect,
            application:set_env(cluster, is_connect_cluster, false),
            lager:error("nodedown : ~w", [Node]),
            ignore;
        _ -> 
            ignore
    end.

