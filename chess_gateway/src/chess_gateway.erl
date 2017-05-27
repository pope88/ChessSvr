-module(chess_gateway).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, start_all_timers/0, stop_all_timers/0, get_pool_id/0]).

-include("game_pb.hrl").
-export([test/0]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

s_init_mod() ->
	ok = gate_mgr:mod_init(),
	ok.

init(_State) ->
	log4erl:info("Gateway_svr start begin."),
	process_flag(trap_exit, true), 
	ok = s_init_mod(),
	log4erl:info("Gateway_svr start ok."),
	{ok, true}.

terminate(Reason, Status) ->
	stop_all_timers(),
	%% TODO
	log4erl:info("Gateway_svr stop...[reason]:~p, ~n", [Reason]),
	{ok, Status}.

handle_cast(_R, State) ->
	{noreply, State}.

handle_call(_R, _FROM, State) ->
	{reply, ok, State}.

handle_info(_Reason, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{noreply, State}.

stop_all_timers() ->
	ok.

start_all_timers() ->
	ok.

get_pool_id() ->
	self().

test() ->
	Pkg = #csplayerlogin_0x01{
		account = "xiaohui",
		token = "008",
		wallow = 1
	},
	Bin = game_pb:encode_csplayerlogin_0x01(Pkg),
	Len = iolist_size(Bin),
	Op = 2,
	PkgHead = #packlen{size = Len, op = Op},
	HeadBin = game_pb:encode_packlen(PkgHead),
	io:format("iolist_size(HeadBin) : ~p", [iolist_size(HeadBin)]),
	PkgBin = [HeadBin, Bin],
	RetFromGameSvr = svr:call_execute(chess_gateway, PkgBin),
	io:format("RetFromGameSvr:~p ~n", [RetFromGameSvr]).
