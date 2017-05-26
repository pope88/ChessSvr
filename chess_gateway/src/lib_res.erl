%%%-----------------------------------
%%% @Module  : lib_res
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2014.12.15
%%% @Description: 资源载入注册模块
%%%-----------------------------------
-module(lib_res).
-export([
        mod_init/0
        ,reload/0
        ,get_meta_pos/2
    ]).

-include("chaos_svr_res.hrl").


%%上线后策划资源要求
%%1.不能删资源！！！！！！
%%2.部分表格不能在热更新的时候改资源，例如duplicate_bag的数据,可以暂时先全删,再插入
s_res_reg_list() ->
[
%%{1.ets_res_name,          2.Table_type,   3.Mod,                      4.ResName                               5.meta                          6.reload}

{?RES_GATE_CONFIG,          set,            gate_config_res,            "../res/res_gate.config.csv",              ?RES_LANG_META,                {undefined, undefined}}
].    

mod_init() ->
    ResRegisterList = s_res_reg_list(), 

    FunInit = fun(ResRegister) -> s_fun_init(ResRegister) end,
    lists:foreach(FunInit, ResRegisterList),

    FunParser = fun(ResRegister) -> s_fun_parser(ResRegister) end,
    lists:foreach(FunParser, ResRegisterList),
    FunChecker = fun(ResRegister) -> s_fun_checker(ResRegister) end,
    lists:foreach(FunChecker, ResRegisterList),
    %%compose 组装
    compose(),
    ok.

get_linelist(ResBin, EtsMetaName) ->
    [H | T] = string:tokens(unicode:characters_to_list(ResBin), "\r\n"),
    [_ | Meta] = H,
    meta_parse(EtsMetaName, Meta),
    T.

get_tokelist(ResLine) ->
    string:tokens(ResLine, ";").

s_fun_init(ResRegister) ->
    EtsResName = element(1, ResRegister),
    EtsTableType = element(2, ResRegister),
    EtsMetaName = element(5, ResRegister),
    %%默认ets中的第二列存key！,第一列是ets名
    % io:format("~p ~p ~p ~n",[EtsResName,EtsTableType,EtsMetaName]),
    ets:new(EtsResName, [{keypos, 2}, named_table, public, EtsTableType, {read_concurrency,true}]),
    %%meta
    ets:new(EtsMetaName, [named_table, public, set, {read_concurrency,true}]).

s_fun_parser(ResRegister) ->
    EtsName = element(1, ResRegister),
    Mod = element(3, ResRegister),
    ResFile = element(4, ResRegister),
    EtsMetaName = element(5, ResRegister),
    parse(Mod, EtsName, ResFile, EtsMetaName).

s_fun_checker(ResRegister) ->
    Mod = element(3, ResRegister),
    case erlang:function_exported(Mod, check_lookup, 1) of
        false  -> ok;
        true ->
            EtsName = element(1, ResRegister), 
            check(Mod, EtsName)
    end.

%%-------------------------------------------------------------------------
meta_parse(EtsMetaName, Meta) ->
    Cols = get_tokelist(Meta),
    meta_parse(EtsMetaName, Cols, 1).

meta_parse(_EtsMetaName, [], _Num) -> 
    ok;
meta_parse(EtsMetaName, [H|T], Num) ->
    ets:insert(EtsMetaName, {list_to_atom(H), Num}),
    meta_parse(EtsMetaName, T, Num+1).

parse(Mod, EtsName, ResFile, EtsMetaName) ->
    % io:format("res ~p~n", [ResFile]),
    {ok,Data} = file:read_file(ResFile),
    LineList = get_linelist(Data, EtsMetaName),
    Fun = fun(Line) ->
            Tokens = get_tokelist(Line),
            Value = Mod:parse_line(Tokens),
            ets:insert(EtsName, Value)
        end,
    lists:foreach(Fun, LineList).

check(Mod, EtsName) ->
    ets:safe_fixtable(EtsName, true),
    case ets:first(EtsName) of
        '$end_of_table' -> 
            ets:safe_fixtable(EtsName, false);
        Key ->
            check(Mod, EtsName, Key)
    end.

check(Mod, EtsName, Key) ->
    List = ets:lookup(EtsName, Key), 
    Mod:check_lookup(List),
    case ets:next(EtsName, Key) of
        '$end_of_table' ->
            ets:safe_fixtable(EtsName, false);
        Next ->
            check(Mod, EtsName, Next)
    end.           

%%-------------------------------------------------------------------------
reload() ->
    log4erl:info("res reload start time ~p~n", [lib_util:unixtime()]),
    
    ResRegisterList = s_res_reg_list(),
    FunInit = fun(ResRegister) ->
        EtsResName = element(1, ResRegister),
        case  ets:info(EtsResName) of
            undefined -> s_fun_init(ResRegister);
            _ -> ok
        end
    end,
    lists:foreach(FunInit, ResRegisterList),

    FunReload = fun(ResRegister) -> s_fun_reload(ResRegister) end,
    lists:foreach(FunReload, ResRegisterList),
    FunChecher = fun(ResRegister) -> s_fun_checker(ResRegister) end,
    lists:foreach(FunChecher, ResRegisterList),
    %%compose 组装
    compose_reload(),

    log4erl:info("res reload finish time ~p~n", [lib_util:unixtime()]),
    ok.

%%todo 要求策划不能删除Id，如果要对produceid加内容，则最好换一个id
s_fun_reload(ResRegister) ->
    EtsName = element(1, ResRegister),
    % TableType = element(2, ResRegister),
    Mod = element(3, ResRegister),
    ResFile = element(4, ResRegister),
    EtsMetaName = element(5, ResRegister),
    {Before, After} = element(6, ResRegister),
    %%reload before
    case Before of
        undefined -> ok;
        clean -> ets:delete_all_objects(EtsName);
        _ -> Mod:Before()
    end,
    %%reload
    parse(Mod, EtsName, ResFile, EtsMetaName),
    %%reload after
    case After of
        undefined -> ok;
        _ -> Mod:After()
    end,
    ok.
%%--------------------------------------------------------------------------
get_meta_pos(EtsMetaName, ColName) ->
    [{_ ,Pos}] = ets:lookup(EtsMetaName, ColName),
    Pos.

compose() ->
    relic_soul_res:reverse(),
    miracle_source_res:reverse(),
    raid_select_res:len(),
    new_activity_res:compose(),
    family_warfare_time_res:compose(),
    dst_res:compose(),
    ok.

compose_reload() ->
    relic_soul_res:reverse_reload(),
    miracle_source_res:reverse_reload(),
    raid_select_res:len_reload(),
    new_activity_res:compose_reload(),
    ok.
