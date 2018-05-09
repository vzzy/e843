%% @author bai
%% @doc @todo Add description to e843_tcp.


-module(e843_tcp).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).
%% 定义宏 -define(宏名称, 值).
%% 调用它可以 ?TCP_OPTIONS
-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr,true}]). %这里是设置tcp的一些选项


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
	port	,
	listen			
}).

%% 启动 {ok,Pid} | ignore | {error,Error}
start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Port]) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, Listen}->
			erlang:start_timer(0, self(), accept),
			{ok, #state{
				port = Port,
				listen = Listen			
			}};
		{error, Reason}->
			case Reason of
				eaddrinuse->%%占用
					void;
				_->
					ok
			end,
			{stop, Reason}
	end.

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, #state{ listen = Listen} = State) ->
	{ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> handle_client(Socket) end),
	erlang:start_timer(0, self(), accept),
    {noreply, State}.

%% 处里接收到客户端的消息
handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of %接收到的数据
        {ok, _Data} -> %如果匹配这样的就发送一条安全协议给客户端
            % Xml 是Flash的socket连接服务器时要返回的一条协议
            Xml = "<cross-domain-policy><site-control permitted-cross-domain-policies=\"all\"/><allow-access-from domain=\"*\" to-ports=\"*\"/></cross-domain-policy> \0",
            _R = gen_tcp:send(Socket, Xml),
            handle_client(Socket); 
        {error, closed} -> %如果匹配这样的就表示客户端关闭了连接  
			catch gen_tcp:close(Socket),                      
			void;
		_R->
			handle_client(Socket)
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

