-module(actor_mgr).
-export([mod_init/0]).
-include("chess_svr.hrl").
-include("gate_cli_pb.hrl").
mod_init() ->
	ets:new(?ETS_ACTOR, [{keypos,#ets_actor.rid}, named_table, public, set, compressed, {write_concurrency,true}]),
	ok.