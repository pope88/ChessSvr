%%%-----------------------------------
%%% @Module  : lib_http
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2016.09.01
%%% @Description: 简单封装下http请求
%%%               s前缀是https
%%%               ContentType: "application/x-www-form-urlencoded", "application/json"
%%%-----------------------------------
-module(lib_http).
-export([
        get/1
        ,post/3
        ,sget/1
        ,spost/3
    ]).

-define(TIMEOUT_DEFAULT, 3000).

get(QueryStr) ->
    Ret = httpc:request(get,{QueryStr,[]},[{timeout, ?TIMEOUT_DEFAULT}],[]),
    ret_handle(Ret).

sget(QueryStr) ->
    Ret = httpc:request(get,{QueryStr,[]},[{timeout, ?TIMEOUT_DEFAULT}, {ssl,[{verify,0}]}],[]),
    ret_handle(Ret).    

post(Url, ContentType, ContentBin) ->
    Ret = httpc:request(post,{Url,[], ContentType, ContentBin},[{timeout, ?TIMEOUT_DEFAULT}],[]),
    ret_handle(Ret).    

spost(Url, ContentType, ContentBin) ->
    Ret = httpc:request(post,{Url,[], ContentType, ContentBin},[{timeout, ?TIMEOUT_DEFAULT}, {ssl,[{verify,0}]} ],[]),
    ret_handle(Ret).  

ret_handle({ok, {_, _, RetBin}}) -> {ok, RetBin};
ret_handle({error,{failed_connect,[_,{inet,[inet],timeout}]}}) -> {error, timeout};
ret_handle(RetErr) -> {error, RetErr}.    