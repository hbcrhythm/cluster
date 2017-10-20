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
    lager:info("start cluster client"),
    ets:new(?CLUSTER_SERVER, [ordered_set, named_table, public, {keypos, #cluster_server.id}]),
    ets:new(?CLUSTER_SERVER_ID, [set, named_table, public, {keypos, #cluster_server_id.sub_id}]),
    application:set_env(cluster, is_connect_cluster, false),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% handle_info(connect, State) ->
%     self() ! connect,
%     {noreply, State};

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

handle_info({cluster_servers, Servers}, State) ->
    %% 设置cookie，产生跨节点调用时可以自动连接
    [
        begin
            erlang:set_cookie(Node, Cookie),
            ets:insert(?CLUSTER_SERVER, ClusterServer),
            [ets:insert(?CLUSTER_SERVER_ID, #cluster_server_id{sub_id = SubId, id = ID, node = Node}) || SubId <- FullId]
        end
        || ClusterServer = #cluster_server{id=ID, full_id=FullId, node=Node, cookie = Cookie} <- Servers
    ],
    {noreply, State};

% handle_info(ping, State) ->
%     erlang:send({cluster_srv, sys_env:get(center_node)}, {ping, self()}),         %% 通知客户端节点接入
%     {noreply, State};

% handle_info(reply_ping, State) ->
%     util:set_timer(ping, ?PING_INTERVAL, ping),
%     {noreply, State};

handle_info({nodedown, Node}, State) ->
    case {ok, Node} == application:get_env(cluster, center_node) of
        true -> 
            self() ! connect,
            application:set_env(cluster, is_link_cluster, false),
            lager:error("nodedown : ~w", [Node]),
            ignore;
        false -> 
            ignore
    end,
    {noreply, State};

handle_info(_Info, State) ->
    lager:error("not know message: ~w", [_Info]),
    {noreply, State}.

terminate(normal, _State) ->
    % util:unset_all_timer(),
    ok;
terminate(Reason, _State) ->
    % util:unset_all_timer(),
    lager:error("terminate reason: ~w", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
