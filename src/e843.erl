%% @author bai
%% @doc @todo Add description to e843.


-module(e843).

-export([
	start/0,
	stop/0		 
]).

%% 启动方法
start()->
	%% 含连接从节点过程。
	ok = start(?MODULE),
	ok.

%% 关闭方法
stop()->
	application:stop(?MODULE),
	timer:sleep(5000),
	erlang:halt(),
	ok.	


%% 启动App
start(App) ->
    start_ok(App, application:start(App, permanent)).
start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({aps_start_failed, App, Reason}).
