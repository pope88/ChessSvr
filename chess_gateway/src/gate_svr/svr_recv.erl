-module(svr_recv).

-export([start_link/3]).

-define(SECURE_CONNECTION, 32768).
-define(CONNECT_TIMEOUT, 5000).
-define(PKG_LEN, 0).
-define(HEAD_LEN, 80).
-define(HEAD_BYTE_LEN, 10).
-include("gate_svr_pb.hrl").
-include("game_pb.hrl").

%% 连接远程端口
start_link(Host, Port, Parent) when is_list(Host), is_integer(Port) ->
	RecvPid = 
	spawn_link(fun() ->
				init(Host, Port, Parent)
				end
				),
	receive 
		{svr_recv, RecvPid, init, {error, E}} ->
			io:format("svr_recv start_link error~n"),
			{error, E};
		{svr_recv, RecvPid, init, {ok, Socket}} ->
			io:format("svr_recv start_link ok~n"),
			{ok, RecvPid, Socket}
	after ?CONNECT_TIMEOUT ->
		catch exit(RecvPid, kill),
			{error, "timeout"}
	end.


init(Host, Port, Parent) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, ?PKG_LEN}]) of
		{ok, Sock} ->
			Parent ! {svr_recv, self(), init, {ok, Sock}},
			State = #gate_svr_recv_state {
				socket = Sock,
				parent = Parent,
				data = <<>>
			},
			loop(State);
		E ->
			Msg = lists:flatten(io_lib:format("connect failed : ~p", [E])),
			Parent ! {svr_recv, self(), init, {error, Msg}}
	end.

loop(State) ->
	Sock = State#gate_svr_recv_state.socket,
	receive 
		{tcp, Sock, InData} ->
			io:format("svr_recv---loop---Sock:~p----InData:~p----->~n", [Sock, InData]),
			NewData = list_to_binary([State#gate_svr_recv_state.data, InData]),
			%% send data to parent if we have enough data
			Rest = send_packet(State#gate_svr_recv_state.parent, NewData),
			loop(State#gate_svr_recv_state{data = Rest});
		{tcp_error, Sock, Reason} ->
			State#gate_svr_recv_state.parent ! {svr_recv, self(), closed, {error, Reason}},
			error;
		{tcp_closed, Sock} ->
			State#gate_svr_recv_state.parent ! {svr_recv, self(), closed, normal},
			error
	end.

%% send data to parent if we have enough data
send_packet(Parent, Data) ->
    case size(Data) >= ?HEAD_BYTE_LEN of
    	true ->
			{HeadBin, PkgBin} = split_binary(Data, ?HEAD_BYTE_LEN),
			io:format("HeadBin:~p~n, PkgBin:~p ~n", [HeadBin, size(PkgBin)]),
			Head = game_pb:decode_packlen(HeadBin),
		    if
				Head#packlen.size >= size(PkgBin) ->
				    {Packet, Rest} = split_binary(Data, Head#packlen.size+?HEAD_BYTE_LEN),
				    Parent ! {svr_recv, self(), data, Packet},
				    send_packet(Parent, Rest);
				true ->
					%% 解包发生问题,丢包
				    Data
		    end;
		_ ->
			io:format("Data:format ~p ~n", [Data]),
		    Data
    end.