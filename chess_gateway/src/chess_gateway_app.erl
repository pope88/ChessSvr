-module(chess_gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0, stop/0, reload/0]).

start() ->
	heroku_crashdumps_app:start(),
	application:start(log4erl),
	log4erl:conf("../log/l4e_gateway.conf"),

	application:start(crontab),
	application:start(chess_gateway),
	ok.

stop() ->
	application:stop(crontab),
	application:stop(chess_gateway),
	application:stop(log4erl),
	ok.

reload() ->
	log4erl:info("App reload start"),

	%% stop timers
	stop_all_tiemrs(),
	%% config reload

	%% logic reload
	{ok, [ModuleList]} = file:consult("./proj"),
	lists:foreach(fun(Module) -> lib_sys:reload(Module) end, ModuleList),
	
	%% start timers
	start_all_timers(),
	log4erl:info("App reload finish"),
	ok.

start_all_timers() ->
	ok.

stop_all_tiemrs() ->
	ok.


%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
%%总监控启动

start(_StartType, _StartArgs) ->
	io:format("application start gateway_svr~n"),
	case chess_gateway_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Other ->
			{error, Other}
	end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
	io:format("application stop gateway_svr~n"),
    ok.
