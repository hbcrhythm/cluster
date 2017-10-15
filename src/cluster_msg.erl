%% -*- coding: latin-1 -*-
%% ---------------------------------------
%% 集群信息分发进程
%% 其作用是做并包，然后再分发，减少节点间传输
%% @author cimn1989@qq.com
%% @end
%% ---------------------------------------
-module(cluster_msg).
-behaviour(gen_server).
-export([
        group_send/2, group_send_pack/2
    ]
).
-export([start_link/0, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("cluster.hrl").
-record(state, {}).

%% @hidden
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec group_send(Pids, Msg) -> void()
%% Pids = [pid()]
%% Msg = term()
%% @doc 群发消息，给任何进程
group_send([], _Msg) -> ok;
group_send(Pids, Msg) ->
    erlang:send(?MODULE, {group_send_1, Pids, Msg}).

%% @spec group_send_pack(Pids, Bin) -> void()
%% Pids = [ConnPid]
%% Bin = binary()
%% @doc 群发包（这个是给连接进程conn_pid发数据
group_send_pack(Pids, Bin) when is_binary(Bin) -> 
    group_send(Pids, {send_data, Bin}).  %% sys_conn需要的发包格式

%% @hidden
stop() -> ok.

%% @hidden
init([]) ->
    {ok, #state{}}.
%% @hidden
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.
%% @hidden
handle_info({group_send_1, Pids, Msg}, State) ->
    [Pid|T] = lists:sort(Pids),
    group_send_1(T, erlang:node(Pid), [Pid], Msg),
    {noreply, State};
handle_info({group_send_2, Pids, Msg}, State) ->
    %% ?INFO("我在发包~w", [Pids]),
    [erlang:send(ConnPid, Msg) || ConnPid <- Pids],
    {noreply, State};
handle_info(_Info, State) ->
    lager:error("未知消息~w", [_Info]),
    {noreply, State}.
%% @hidden
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:error("~w", [Reason]),
    ok.
    
%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

group_send_1([], Node, NodePids, Msg) -> 
    erlang:send({?MODULE, Node}, {group_send_2, NodePids, Msg});
group_send_1([Pid|T], Node, NodePids, Msg) ->
    case erlang:node(Pid) of
    Node -> group_send_1(T, Node, [Pid|NodePids], Msg);
    NewNode -> 
        erlang:send({?MODULE, Node}, {group_send_2, NodePids, Msg}),
        group_send_1(T, NewNode, [Pid], Msg)
    end.
