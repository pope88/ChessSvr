%%%-----------------------------------
%%% @Module  : lib_seed
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2015.01.21
%%% @Description: 随机数服务
%%%-----------------------------------
-module(lib_seed).
-export([
        mod_init/0
        ,rand/0
        ,rand/1
        ,rand/2
    ]).

mod_init() ->
    ets:new(ets_random_seed, [named_table, public, set, {write_concurrency,true}]),
    S = erlang:now(),
    ets:insert(ets_random_seed, {seed, S}),
    ok.

rand() ->
    [{seed, S}] = ets:lookup(ets_random_seed, seed),
    {R, S2} = random:uniform_s(S),
    ets:insert(ets_random_seed, {seed, S2}),
    R.

rand(M) ->
    [{seed, S}] = ets:lookup(ets_random_seed, seed),
    {R, S2} = random:uniform_s(M, S),
    ets:insert(ets_random_seed, {seed, S2}),
    R.

rand(M, N) ->
    [{seed, S}] = ets:lookup(ets_random_seed, seed),
    {R, S2} = random:uniform_s(S),
    ets:insert(ets_random_seed, {seed, S2}),
    M + round(R * (N - M)).
