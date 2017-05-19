-module(svr).
-behaviour(gen_server).

-export([start_link/3,
		connect/5,
		call_execute/2]).

-define(SVR_DISPATCHER, svr_dispatcher).

%% Internal exports - gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).


-include("gate_svr_pb.hrl").

start_link(PoolId, Host, Port) ->
	gen_server:start_link({local, ?SVR_DISPATCHER}, ?MODULE, [PoolId, Host, Port], []).

handle_cast(_R, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{noreply, State}.

terminate(Reason, Status) ->
	log4erl:info("svr stop...[reason]:~p, ~n", [Reason]),
	{ok, Status}.


init([PoolId, Host, Port]) ->
	case svr_conn:start(PoolId, Host, Port) of
		{ok, ConnPid} ->
			NewConn = new_conn(PoolId, ConnPid, true, Host, Port),
			State = #svr_state{
					conn_pools = maps:new(),
					pids_pools = maps:new()
				},
			NewState = add_conn(NewConn, State),
			{ok, NewState};
		{error, _Reason} ->
			NewConn = new_conn(PoolId, undefined, true, Host, Port),
			start_reconnect(NewConn),
			{ok, #svr_state{conn_pools = undefined, pids_pools = undefined}};
		_ ->
			io:format("svr init error else~n")
	end.

new_conn(PoolId, ConnPid, Reconnect, Host, Port) ->
	case Reconnect of
		true ->
			#svr_conn{
				pool_id = PoolId,
				conn_pid = ConnPid,
				reconnect = true,
				host = Host,
				port = Port
			};
		false ->
			#svr_conn{
				pool_id = PoolId,
				conn_pid = ConnPid,
				reconnect = false
			}
	end.


add_conn(Conn, State) ->
    Pid = Conn#svr_conn.conn_pid,
    erlang:monitor(process, Conn#svr_conn.conn_pid),
    PoolId = Conn#svr_conn.pool_id,
    ConnPools = State#svr_state.conn_pools,
    NewPool = 
	case maps:is_key(PoolId, ConnPools) of
	    false ->
			{[Conn],[]};
		_ ->
			{Unused, Used} = maps:get(PoolId),
			{[Conn | Unused], Used}
	end,
    State#svr_state{
    	conn_pools = maps:put(PoolId, NewPool, ConnPools),
		pids_pools = maps:put(Pid, PoolId, State#svr_state.pids_pools)}.

start_reconnect(_Conn) ->
	ok.

connect(PoolId, Host, Port, Reconnect, LinkConnection) ->
	Fun =   if 
				LinkConnection ->
					fun svr_conn:start/3;   %% 暂时只支持非链式
				true ->
					fun svr_conn:start/3
		    end,

	case Fun(PoolId, Host, Port) of
		{ok, ConnPid} ->
			Conn =  new_conn(PoolId, ConnPid, Reconnect, Host, Port),
			case call_add_conn(Conn) of
				ok ->
					{ok, ConnPid};
				Res ->
					Res
			end;
		Err ->
			Err
	end.

%%------------------------------------call----------------------------------%%
call_add_conn(Conn) ->
	gen_server:call(?SVR_DISPATCHER, {add_conn, Conn}).

call_execute(PoolId, Pkg) ->
	gen_server:call(?SVR_DISPATCHER, {execute, PoolId, Pkg}).

%%----------------------------------handle----------------------------------%%
handle_call({add_conn, Conn}, _From, State) ->
	NewState = add_conn(Conn, State), 
	%% {PoolId, ConnPid} = {Conn#svr_state.pool_id, Conn#svr_conn.pid},
	{reply, ok, NewState};

handle_call({execute, PoolId, Pkg}, From, State) ->
	Ret = get_next_conn(PoolId, State),
	case Ret of
		error ->
			{reply, {error, {no_conn_in_pool, PoolId}}, State};
		{ok, Conn, NewState} ->
			svr_conn:execute(From, Conn#svr_conn.conn_pid, Pkg),
			{noreply, NewState}
	end. 
	
handle_info(timeout, State)  ->
	io:format("gate svr receive timeout event from gate~n"),
	{noreply, State}.
handle_info(_Reason, State) ->
	{noreply, State}.
	

get_next_conn(PoolId, State) ->
	ConnPools = State#svr_state.conn_pools,
	case maps:is_key(PoolId, ConnPools) of
		false ->
			error;
		_ ->
			case maps:get(PoolId, ConnPools) of
				{[], []} ->
					error;
				{[], Used} ->
					[Conn | Conns] = lists:reverse(Used),
					NewPool = {Conns, [Conn]},
					NewConnPools = maps:put(PoolId, NewPool, ConnPools),
					NewState = State#svr_state{conn_pools = NewConnPools},
					{ok, Conn, NewState};
				{[Conn|Unused], Used} ->
					NewPool = {Unused, [Conn|Used]},
					NewConnPools = maps:put(PoolId, NewPool, ConnPools),
					NewState = State#svr_state{conn_pools = NewConnPools},
					{ok, Conn, NewState}
			end
	end.
