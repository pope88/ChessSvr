%%%-----------------------------------
%%% @Module  : lib_util
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2014.12.3
%%% @Description: util
%%%-----------------------------------
-module(lib_util).
-export([
        check_length/2,
        check_str_length/2,
        unicode_to_utf8_list/1,
        
        unixtime/0,
        timenow/0,
        is_same_day/2,
        is_same_week/2,
        is_same_month/2,
        longunixtime/0,
        get_weekday/0,
        timestamp_to_datetime/1,
        datetime_to_timestamp/1,
        db_datetime/0,
        timestamp_to_db_datetime/1,
        str2time/1,
        str2date/1,
        time_check/1,%当日时间是否在指定时间段内
        weekday_from_int/1,
        between_weekday/1,
        
        md5/1,
        sha256/1,
        gen_rsakey/1,
        ceil/1,
        floor/1,
        sleep/1,
        sleep/2,
        get_list/2,
        implode/2,
        implode/3,
        explode/2,
        explode/3,
        for/3,
        for/4,
        get_val/3,
        get_res_str/1,
        get_res_str_to_atom/1,
        is_in_range/3,
        to_hex/1,
        list_to_num/1,
        peername/1,
        droplast/1,
        rand_select/2,
        enum_check/2,
        gm_enum_check/2,
        thing_to_list/1,
        md5_format/1,
        sharp_day/2  %%两个ts相隔多少天
    ]).

-define(DAYSECONDS, 86400).

check_length(List, LenLimit) ->
    length(List) =< LenLimit.

unicode_to_utf8_list(Str) ->
    binary_to_list(unicode:characters_to_binary(Str)).

check_str_length(Str, LenLimit) ->
    case is_list(Str) of
        true ->
            Uft8List = unicode_to_utf8_list(Str),
            length(Uft8List) =< LenLimit;
        _ -> false
    end.

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, [])->
    [<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% 字符->列
explode(S, B)->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X. 

%% 得到的是从1970年1月1日零时起，到现在经过的时间 取得当前的unix时间戳, 返回{MegaSecs, Secs, MicroSecs}
%% os:timestamp() 用来提高效率，舍弃精度
%%返回秒
timenow() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

is_same_day(Sec1, Sec2) ->
    {{Y1, M1, D1}, _} = timestamp_to_datetime(Sec1),
    {{Y2, M2, D2}, _} = timestamp_to_datetime(Sec2),
    (Y1 == Y2) andalso (M1 == M2) andalso (D1 == D2).

is_same_week(Sec1, Sec2) ->
    {Date, _} = timestamp_to_datetime(Sec1),
    WeekTs = datetime_to_timestamp({Date, {0, 0, 0}}),
    WeekDay = calendar:day_of_the_week(Date),
    WeeKBeginTs = WeekTs - (WeekDay-1)*?DAYSECONDS,
    WeeKEndTs = WeekTs + (7-WeekDay+1)*?DAYSECONDS,
    Sec2 >= WeeKBeginTs andalso Sec2 < WeeKEndTs.

is_same_month(Sec1, Sec2) ->
    {{Y1, M1, _}, _} = timestamp_to_datetime(Sec1),
    {{Y2, M2, _}, _} = timestamp_to_datetime(Sec2),
    (Y1 == Y2) andalso (M1 == M2).

% erlang:now()每次获取都会生成唯一的时间,会有多余的开销
unixtime() ->
    {M, S, _} = now(),
    M * 1000000 + S.

%%返回毫秒
longunixtime() ->
    {M, S, Ms} = now(),
    M * 1000000000 + S*1000 + Ms div 1000.

%% 当日是周几
%% Returns: 1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7.
get_weekday() ->
    {Day, _} = erlang:localtime(),
    calendar:day_of_the_week(Day).

% 时间戳转当地时间
timestamp_to_datetime(Timestamp) ->
    M = Timestamp div 1000000,
    S = Timestamp rem 1000000,
    Time = {M, S, 0},
    calendar:now_to_local_time(Time).
%DateTime格式：{{2015,11,10}, {18,0,0}}
%return timestamp为utc时间
datetime_to_timestamp(DateTime) ->
    UTC1970 = {{1970,1,1}, {0,0,0}},
    [DT] = calendar:local_time_to_universal_time_dst(DateTime),
    %return 有 [],[DstDateTimeUTC, DateTimeUTC],[DateTimeUTC] 前2个是异常情况
    calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC1970).      

% db datetime 2015-11-01 12:01:00
db_datetime() ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
    lists:flatten(
        io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second])
    ).
timestamp_to_db_datetime(Timestamp) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = timestamp_to_datetime(Timestamp),
    lists:flatten(
        io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Year, Month, Day, Hour, Minute, Second])
    ).

%Str like "1:21:03"
str2time(Str) ->
    [StrH,StrM,StrS] = string:tokens(Str, ":"),
    {list_to_integer(StrH), list_to_integer(StrM), list_to_integer(StrS)}.
%Str like "2017-12-30,23:59:59"
str2date(Str) ->
    [PartOne, PartTwo] = string:tokens(Str, ","),
    [Year, Mon, Day] = string:tokens(PartOne, "-"),
    [Hour, Min, Sec] = string:tokens(PartTwo, ":"),
    DPartOne = {list_to_integer(Year), list_to_integer(Mon), list_to_integer(Day)},
    DPartTwo = {list_to_integer(Hour), list_to_integer(Min), list_to_integer(Sec)},
    {DPartOne, DPartTwo}.

%是否在指定当天的时间段内 eg {19, 7, 42}
time_check({StartTime, EndTime}) ->
    {_,T} = lib_util:timestamp_to_datetime(timenow()),
    StartSec = calendar:time_to_seconds(StartTime),
    NowSec = calendar:time_to_seconds(T),
    case NowSec < StartSec of
        true -> false;
        _ ->
            EndSec = calendar:time_to_seconds(EndTime),
            NowSec =< EndSec
    end.
%获得atom
weekday_from_int(IntWeekDay) ->
    L = [monday,tuesday,wednesday,thursday,friday,saturday,sunday],
    lists:nth(IntWeekDay, L).

%%是否在策划配置的时间周时间范围内
%{ {6,{12,01,01}}, {1,{10,00,01}} }
between_weekday({ {SW,{SH,SM,SS}}, {EW,{EH,EM,ES}} }) ->
    Weekday = get_weekday(),
    case is_cross_weekday({SW,{SH,SM,SS}}, {EW,{EH,EM,ES}}) of
        true ->
            if 
                (Weekday > SW) orelse (Weekday < EW) ->
                    true;
                Weekday == SW ->
                    time_check({{SH,SM,SS}, {23,59,59}});
                Weekday == EW ->
                    time_check({{0,0,0}, {EH,EM,ES}});
                true ->
                    true
            end;            
        _ ->
            if 
                (Weekday < SW) orelse (Weekday > EW) ->
                    false;
                Weekday == SW ->
                    time_check({{SH,SM,SS}, {23,59,59}});
                Weekday == EW ->
                    time_check({{0,0,0}, {EH,EM,ES}});
                true ->
                    true
            end
    end.
% {SW,{SH,SM,SS}}, {EW,{EH,EM,ES}}
is_cross_weekday({SW,StartTime}, {EW,EndTime}) ->
    if
        SW > EW ->
            true;
        SW == EW ->
            StartSec = calendar:time_to_seconds(StartTime),
            EndSec = calendar:time_to_seconds(EndTime),
            StartSec > EndSec;
        true ->
            false 
    end.
%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% 转换成HEX格式的sha256
sha256(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(crypto:hash(sha256, S))]).

gen_rsakey(StrKey) ->
    PemEntries = public_key:pem_decode(list_to_binary(StrKey)),  
    public_key:pem_entry_decode(hd(PemEntries)).

%%正数向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%正数向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

 sleep(T) ->
    receive
    after T -> ok
    end.

 sleep(T, F) ->
    receive
    after T -> F()
    end.

get_list([], _) ->
    [];
get_list(X, F) ->
    F(X).

%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

get_val(Flag, A, B) ->
    case Flag of
        true -> A;
        false -> B
    end.

get_res_str(Str) ->
    case Str of
        "0" -> undefined;
        _ -> Str
    end.

get_res_str_to_atom(Str) ->
    case Str of
        "0" -> undefined;
        _ -> list_to_atom(Str)
    end.

is_in_range(T, B, E) ->
    case B > T of
        true -> false;
        false ->
            case T > E of
                true -> false;
                false -> true
            end
    end.

%% @spec to_hex(integer | iolist()) -> string()
%% @doc Convert an iolist to a hexadecimal string.
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.
    
to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

list_to_num(S) ->
    case string:chr(S, $.) of
        0 -> list_to_integer(S);
        1 ->
            NS = "0" ++ S,
            %csv 0.2 被弄成了.2 
            list_to_float(NS);
        _ -> list_to_float(S)
    end.

peername(Socket) ->
    case inet:peername(Socket) of
        {ok,{Ip, Port}} -> 
            L = tuple_to_list(Ip),
            Fun = fun(N, Str) ->
                Str++integer_to_list(N)++"."
            end,
            Format = lists:foldl(Fun, "", L),
            {lib_util:droplast(Format)++":"++integer_to_list(Port)};
        Err -> Err
    end.

droplast([]) -> [];
droplast(L) ->
    RL = lists:reverse(L),
    [_H|Left] = RL,
    lists:reverse(Left).


rand_select(L, Cnt) ->
    Len = length(L),
    case Len =< Cnt of
        true -> L;
        _ -> s_rand_select(L, Cnt, [], Len)
    end.
s_rand_select(_L, 0, EL, _Len) -> EL;
s_rand_select(L, Cnt, ExistL, Len) ->
    Pos = lib_seed:rand(1, Len),
    V = lists:nth(Pos, L),
    s_rand_select(lists:delete(V, L), Cnt-1, [V|ExistL], Len-1). 


enum_check(Type, Enum) ->
    try 
        ets_pb:enum_to_int(Type, Enum),
        true
    catch
        error:_ -> false
    end. 

gm_enum_check(Type, Enum) ->
    try 
        gm_pb:enum_to_int(Type, Enum),
        true
    catch
        error:_ -> false
    end.

%%Data is a binary or a list of small integers and binaries.
md5_format(Data) ->
    lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(Data))]).

sharp_day(Sec1, Sec2) ->
    case Sec1 =< Sec2 of
        true ->
            {Date, _} = timestamp_to_datetime(Sec1),
            Ts = datetime_to_timestamp({Date, {0, 0, 0}}),
            (Sec2 - Ts) div ?DAYSECONDS;
        _ ->
            {Date, _} = timestamp_to_datetime(Sec2),
            Ts = datetime_to_timestamp({Date, {0, 0, 0}}),
            (Sec1 - Ts) div ?DAYSECONDS
    end.
