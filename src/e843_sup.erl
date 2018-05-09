-module(e843_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% 注意：2000以下的端口需要运行root权限的用户来启动
-define(TCP_PORT,843).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Port = case application:get_env(port) of
		{ok,T_Port}->
			T_Port;
		_->
			?TCP_PORT
	end,
    {ok, { {one_for_one, 5, 10}, [{e843_tcp, {e843_tcp, start_link, [Port]},permanent, brutal_kill, supervisor, [e843_tcp]}]} }.

