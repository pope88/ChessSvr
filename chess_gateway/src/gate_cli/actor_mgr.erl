-module(actor_mgr).
-export([mod_init/0]).
-include("chess_svr.hrl").
-include("gate_cli_pb.hrl").

-export([
	init_actor/1,
	delete_actor/1
	]).

mod_init() ->
	ets:new(?ETS_ACTOR, [{keypos,#ets_actor.rid}, named_table, public, set, compressed, {write_concurrency,true}]),
	ok.

% %如果存在玩家上次登录记录,则保留上次的login_ts
init_actor(ActorRid) ->
    case ets:lookup(?ETS_ACTOR, ActorRid) of
        [] -> LoginTs =  lib_util:timenow();
        [OldV] -> LoginTs = OldV#ets_actor.login_ts
    end,
    Session = lib_seed:rand(1, 1000000),
    Seq = lib_seed:rand(1, 1000),
    Actor = #ets_actor{rid = ActorRid, session = Session, seq = Seq, login_ts = LoginTs},
    ets:insert(?ETS_ACTOR, Actor),
    {Session, Seq}.

delete_actor(ActorRid) ->
    %需要上报微讯玩家在线
    case ets:lookup(?ETS_ACTOR, ActorRid) of
        [] -> OnlineSeconds = 7200;%默认在线小时嘿嘿
        [V] -> 
            LoginTs = V#ets_actor.login_ts,
            LastMsgTs = actor:get(#ets_actor.last_msg_ts, ActorRid),
            OnlineSeconds = LastMsgTs - LoginTs
    end,
    ets:delete(?ETS_ACTOR, ActorRid).