
-module(svr_conn).

-export ([start/3
		,execute/3]).
-include("gate_svr_pb.hrl").

-define(DEFAULT_STANDALONE_TIMEOUT, 5000).

start(PoolId, Host, Port) ->
	ConnPid = self(),
	Pid = spawn(fun() ->
					init(Host, Port, PoolId, ConnPid)
				end),
	post_start(Pid).


init(Host, Port, PoolId, Parent) ->
	case svr_recv:start_link(Host, Port, self()) of
		{ok, RecvPid, Sock} ->
			State = #gate_svr_conn_state{
				recv_pid = RecvPid,
				socket = Sock,
				pool_id = PoolId,
				data = <<>>
			},
			loop(State);
		_E ->
			Parent ! {svr_conn, self(), {error, connect_failed}}
	end.

post_start(Pid) ->
	{ok, Pid}.

loop(State) ->
	RecvPid = State#gate_svr_conn_state.recv_pid,
	receive
		{mysql_recv, RecvPid, data, _Packet, _Num} ->
			loop(State);
		{execute, Pkg, From} ->	
			case do_execute(State, Pkg, From) of
				{error, _} = Err ->
					send_reply(From, Err),
					State;
				{ok, Result, NewState} ->
					io:format("From ---------> ~p ~n", [From]),
					send_reply(From, Result),
					NewState
			end;
		_ ->
			loop(State)
	end.

execute(From, Pid, Pkg) ->
	execute(From, Pid, Pkg, ?DEFAULT_STANDALONE_TIMEOUT).
execute(From, Pid, Pkg, Timeout) ->
	send_msg(Pid, {execute, Pkg, From}, From, Timeout).

do_execute(State, Pkg, _From) ->
	case do_send(State#gate_svr_conn_state.socket, Pkg) of
		ok ->
			io:format("svr_conn --->do_execute~n"),
			get_execute_response(State);
		{error, _Reason} ->
			Msg = "Failed sending data",
			{error, Msg}
	end.

do_send(Sock, Pkg) ->
	gen_tcp:send(Sock, Pkg).
	

send_msg(Pid, Msg, _From, _Timeout) ->
	Pid ! Msg. 
send_reply(GenSvrFrom, Res) ->
	gen_server:reply(GenSvrFrom, Res).
  
get_execute_response(State) ->
	RecvPid = State#gate_svr_conn_state.recv_pid,
	case do_recv(RecvPid, undefined) of
		{ok, Packet} ->
			{ok, Packet, State};
		{error, Reason} ->
			{error, Reason}
	end.

do_recv(RecvPid, SeqNum) when SeqNum == undefined ->
	receive
		{svr_recv, RecvPid, data, Packet} ->
			io:format("svr_conn --->do_recv~n"),
			{ok, Packet};
		{svr_recv, RecvPid, closed, _E} ->
			{error, "mysql_recv: socket was closed"}
	end.

