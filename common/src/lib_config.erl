%%%-----------------------------------
%%% @Module  : lib_config
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2014.12.17
%%% @Description: 载入服务器config
%%%-----------------------------------
-module(lib_config).
-export([
        mod_init/0
        ,reload/0

        ,get/1
        ,set/1
    ]).

-include("common.hrl").

-define(CONFIG, "./config").

mod_init() ->
    mod_init(?CONFIG).
mod_init(FilePath) ->
    ets:new(?ETS_CONFIG, [named_table, public, set, {read_concurrency,true}]),
    config_load(FilePath),
    ok.

reload() ->
    reload(?CONFIG).
reload(FilePath) ->
    config_load(FilePath),
    ok.

config_load(FilePath) ->
    {ok, ConfigList} = file:consult(FilePath),
    Fun = fun(Config) -> ets:insert(?ETS_CONFIG, Config) end,
    lists:foreach(Fun, ConfigList),
    ok.

get(Key) ->
    case ets:member(?ETS_CONFIG, Key) of
        true ->
            ets:lookup_element(?ETS_CONFIG, Key, 2);
        _ ->
            false
    end.

set(Config) ->
    ets:insert(?ETS_CONFIG, Config).