cluster
=====

An OTP application

Build
-----

    $ rebar3 compile

Application Config
-----
example
	
	[
		{cluster, [
			{srv_id, 1}						%% required: @doc ServerId
		    ,{node_type, normal}			%% required: @doc node type: normal | cloud | center
		    ,{platform, <<"ios">>}			%% optional: @doc platform
		    ,{ver, <<"1.0.0">>}				%% optional: @doc cluster version
		    ,{center_node, 'cluster_center_1@127.0.0.1'}	%% required: normal, cloud 。@doc valid on node type of normal | cloud
		    ,{center_cookie, '666'}							%% required: normal, cloud 。valid on node type of normal | cloud
    	]
    }].
