-ifndef(ETS_ACTOR_PB_H).
-define(ETS_ACTOR_PB_H, true).
-record(ets_actor, {
    rid = erlang:error({required, rid}),
    name,
    uin = erlang:error({required, uin}),
    icon,
    create_ts,
    last_msg_ts,
    freshman_id,
    vip_level,
    vip_exp_total
}).
-endif.

-ifndef(ETS_GAME_SVR_PB_H).
-define(ETS_GAME_SVR_PB_H, true).
-record(ets_game_svr, {
    svr_id = erlang:error({required, svr_id}),
    svr_name,
    svr_type
}).
-endif.

