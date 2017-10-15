%% @doc normal | cloud | center
-define(DEFAULT_NODE_TYPE, normal).

-define(CLUSTER_SERVER, cluster_server).
-define(CLUSTER_SERVER_ID, cluster_server_id).
-define(CLUSTER_CLOUD_TASK, cluster_cloud_task).

-define(CLUSTER_CONNECT_INTERVAL, 1000).   %% 单位秒
-define(PING_INTERVAL, 3000).   %% 单位秒

-record(cluster_server, {
        id = 0              %% 服务器唯一id
        ,full_id = []       %% 服务器唯一id（全称
        ,pid
        ,type = normal      %% 服务器类型
        ,platform           %% 平台名
        ,name = <<>>        %% 游戏小名
        ,node               %% 节点名
        ,cookie
        ,ver                %% 版本
    }
).

-record(cluster_cloud_task, {
        id                  %% 模块唯一标识
        ,pos = 0            %% 下标
    }
).
