%%------------------------------------------------------------
%% @author labihbc@gmail.com
%% @doc logic | center
%% @doc logic is server type, example: chat, instance, manager, path, logic, auth, area, connector etc.
%% @doc same server type is can't remote call or cast
%%------------------------------------------------------------

-define(DEFAULT_NODE_TYPE, logic).
-define(DEFAULT_IS_MASTER, false).

-define(CLUSTER_SERVER, cluster_server).
-define(CLUSTER_SERVER_ID, cluster_server_id).
-define(CLUSTER_CLOUD_TASK, cluster_cloud_task).
-define(CLUSTER_OPEN_STATUS, true).
-define(CLUSTER_CLOSE_STATUS, false).


-define(CLUSTER_CONNECT_INTERVAL, 1000).   %% 单位秒
-define(PING_INTERVAL, 3000).   %% 单位秒

-define(CLUSTER_EVENT_NAME, '@cluster_event').
-define(CLUSTER_EVENT_SRVUP, cluster_srv_up).
-define(CLUSTER_EVENT_SRVDOWN, cluster_srv_down).

-record(cluster_server, {
        id = 0                              %% 服务器唯一id
        ,full_id = []                       %% 服务器唯一id（全称
        ,pid
        ,type = ?DEFAULT_NODE_TYPE          %% 服务器类型
        ,platform                           %% 平台名
        ,name = <<>>                        %% 游戏小名
        ,node                               %% 节点名
        ,cookie
        ,ver                                %% 版本
        ,is_open = ?CLUSTER_OPEN_STATUS     %% 是否开放node_type访问，实现负载均衡控制. bool()
        ,is_master
    }).

-record(cluster_server_id, {
        sub_id
        ,id
        ,node
    }).

-record(cluster_cloud_task, {
        id                  %% 模块唯一标识
        ,pos = 0            %% 下标
    }).

% -record(cluster_event_callback, {
%         m
%         ,f
%         ,a
%         ,is_once = false
%     }).