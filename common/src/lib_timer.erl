%%%-----------------------------------
%%% @Module  : lib_timer
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2014.12.3
%%% @Description: 计时器,一个timer是一个进程
%%%-----------------------------------
-module(lib_timer).
-export([
        add_timer/3
        ,add_register_timer/4
        ,del_timer/1
    ]).

%%间隔Interval(单位毫秒)， 重复Repeats(0表示永久)次， 不马上生效
%%Fun为回调函数，需要实现
%%Args参数为[]
add_timer(Interval, Repeats, Fun) ->
    case Repeats =:= 0 of
        true  -> proc_lib:spawn(fun() -> s_tick(Interval, Fun) end);
        false -> proc_lib:spawn(fun() -> s_tick(Interval, Repeats, Fun) end)
    end.

%%TimerName为atom
add_register_timer(TimerName, Interval, Repeats, Fun) -> 
    case Repeats =:= 0 of
        true  -> register(TimerName, proc_lib:spawn(fun() -> s_tick(Interval, Fun) end));
        false -> register(TimerName, proc_lib:spawn(fun() -> s_tick(Interval, Repeats, Fun) end))
    end.

del_timer(Pid) -> Pid ! stop.


%%内部API
s_tick(Interval, Repeats, Fun) ->
    case Repeats =:= 0 of
        true -> void;
        false ->
            receive
            stop ->
                void
            after Interval ->
                Fun(),
                s_tick(Interval, Repeats-1, Fun)
            end
    end.

%%永久
s_tick(Interval, Fun) ->
    receive
    stop ->
        void
    after Interval ->
        Fun(),
        s_tick(Interval, Fun)
    end.    