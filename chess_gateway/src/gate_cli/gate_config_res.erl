%%%--------------------------------------
%%% @Module  : gate_config_res
%%% @Author  : zhangxin
%%% @Email   : zhangxin<zhangxin@maplegame.cn>
%%% @Created : 2016.8.9
%%% @Description: lang_res 模块 
%%%--------------------------------------

-module(gate_config_res).

-export([
         parse_line/1
        ,check_tid/1
       ]).

-include("chess_gate_res.hrl").
-include("gate_cli_pb.hl").

parse_line(Tokens) ->
    #res_lang{
         gate_id =  g(gate_id, Tokens)
        ,gate_name = g(gate_name, Tokens)
        ,gate_type = g(gate_type, Tokens)
    }.

g(Atom, Tokens) ->
    lists:nth(pos(Atom), Tokens).

pos(Atom) ->
    lib_res:get_meta_pos(?RES_GATE_CONFIG_META, Atom).

check_tid(Id) ->
    ets:member(?RES_GATE_CONFIG, Id).

