%%%--------------------------------------
%%% @Module  : jpush
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2016.03.14
%%% @Description: jpush
%%%
%%%--------------------------------------
-module(jpush).
-export([
            push_all/2
            ,push_batch/3
        ]).

-compile(export_all).

-define(JPUSH_URL, "https://api.jpush.cn/v3/push").
-define(JPUSH_TEST_URL, "https://api.jpush.cn/v3/push/validate").
-define(JPUSH_TIMEOUT, 15000).
-define(JPUSH_NUM, 2).
-define(JPUSH_TIME_TO_LIVE, lib_config:get(jpush_time_to_live)).%默认 86400 （1 天），最长 10 天。设置为 0 表示不保留离线消息，只有推送当前在线的用户可以收到

% base64(appKey:masterSecret)
gen_auth(AppKey, MasterSecret) ->
    base64:encode_to_string(AppKey++":"++MasterSecret).

%全量推送
push_all(JPUSH_BASE64_AUTH_STR, Desc) ->
    push_all(JPUSH_BASE64_AUTH_STR, Desc, ?JPUSH_NUM).    
push_all(JPUSH_BASE64_AUTH_STR, Desc, Num) ->
    Notif = {struct, [
            {"alert", Desc}
            ,{"android", {struct, []}}
            ,{"ios", {struct, [{"extras", {struct, [{"sound", "default"}, {"badge", "1"}]}}]} }
            ]
        },
    JsonL = [
        {"platform", "all"}
        ,{"audience", "all"}
        ,{"notification", Notif}
        ,{"options", {struct, [{"time_to_live", ?JPUSH_TIME_TO_LIVE}]}}
    ],
    JsonBody = mochijson:encode({struct, JsonL}),
    Bin = iolist_to_binary(JsonBody),
    case httpc:request(post,{?JPUSH_URL,[{"Authorization", JPUSH_BASE64_AUTH_STR}], "application/json", Bin},[{timeout, ?JPUSH_TIMEOUT}, {ssl,[{verify,0}]} ],[]) of
        {ok, {_, _, RetJsonBin}} ->
            log4erl:info("~p push_all success parm(~p), ret(~p)", [?MODULE, {Desc, Num}, RetJsonBin]),
            ok;
        Err ->
            case Num > 0 of
                true ->
                    log4erl:error("~p push_all faild parm(~p), ret(~p)", [?MODULE, {Desc, Num}, Err]), 
                    push_all(JPUSH_BASE64_AUTH_STR, Desc, Num-1);
                _ ->
                    log4erl:fatal("~p push_all faild parm(~p), ret(~p)", [?MODULE, {Desc, Num}, Err])
            end   
    end.

push_batch(JPUSH_BASE64_AUTH_STR, Desc, UinList) ->
    push_batch(JPUSH_BASE64_AUTH_STR, Desc, UinList, ?JPUSH_NUM).
push_batch(JPUSH_BASE64_AUTH_STR, Desc, UinList, Num) ->
    Notif = {struct, [
                    {"alert", Desc}
                    ,{"android", {struct, []}}
                    ,{"ios", {struct, [{"extras", {struct, [{"sound", "default"}, {"badge", "1"}]}}]} }
                    ]
        },
    JsonL = [
        {"platform", "all"}
        ,{"audience", {struct, 
                        [
                            {"alias", {array, UinList}}
                        ]
                    }
        }
        ,{"notification", Notif}
        ,{"options", {struct, [{"time_to_live", ?JPUSH_TIME_TO_LIVE}]}}
    ],
    JsonBody = mochijson:encode({struct, JsonL}),
    Bin = iolist_to_binary(JsonBody),
    case httpc:request(post,{?JPUSH_URL,[{"Authorization", JPUSH_BASE64_AUTH_STR}], "application/json", Bin},[{timeout, ?JPUSH_TIMEOUT}, {ssl,[{verify,0}]} ],[]) of
        {ok, {_, _, RetJsonBin}} ->
            log4erl:info("~p push_batch success parm(~p), ret(~p)", [?MODULE, {Desc, Num}, RetJsonBin]),
            ok;
        Err ->
            case Num > 0 of
                true ->
                    log4erl:error("~p push_batch faild parm(~p), ret(~p)", [?MODULE, {Desc, Num}, Err]), 
                    push_batch(JPUSH_BASE64_AUTH_STR, Desc, UinList, Num-1);
                _ ->
                    log4erl:fatal("~p push_batch faild parm(~p), ret(~p)", [?MODULE, {Desc, Num}, Err])
            end   
    end.    
%%------------------------test---------------------------------------------
% -define(JPUSH_BASE64_AUTH_STR, "Basic Y2ViYWViNDA0ZTg3YTI2N2Q3ZGVlMDBmOjgxNTBlY2RmYTE0Yjk2MTM0ODUwMjU1YQ==").

% {
%    "platform": "all",
%    "audience" : "all",
%    "notification" : {
%       "alert" : "Hi, JPush!",
%       "android" : {}, 
%       "ios" : {
%          "extras" : { "newsid" : 321}
%       }
%    }
% }
% Notif =  {struct, [
        %     {"alert", "Hi, JPush!"}
        %     ,{"android", {struct, []}}
        %     ,{"ios", {struct, [{"extras", {struct, [{"newsid", 321}]}}]} }
        %     ]
        % }

% {
%     "audience" : {
%         "alias" : ["cht_23455249"]
%     }
% }

% test({Notif}, 1) ->
%    test({Notif}, ?JPUSH_TEST_URL); 
% test({Notif}, 0) ->
%    test({Notif}, ?JPUSH_URL); 
% test({Notif}, URL) ->
%     JsonL = [
%         {"platform", "all"}
%         ,{"audience", {struct, 
%                 [
%                     {"alias", {array, ["cht_45992237"]}}
%                 ]
%             } 
%         } 
%         ,{"notification", Notif}
%     ],
%     JsonBody = mochijson:encode({struct, JsonL}),
%     Bin = iolist_to_binary(JsonBody),
%     {ok, {_, _, RetJsonBin}} = httpc:request(post,{URL,[{"Authorization", ?JPUSH_BASE64_AUTH_STR}], "application/json", Bin},[{timeout, 3000}, {ssl,[{verify,0}]} ],[]),
%     RetJsonBin.