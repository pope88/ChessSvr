%%%-----------------------------------
%%% @Module  : lib_sys
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2015.02.05
%%% @Description: 
%%%-----------------------------------
-module(lib_sys).
-compile(export_all).
-export([
        reload/1
    ]).

reload(Module) ->
    case code:soft_purge(Module) of
        true ->
            case code:load_file(Module) of
                {module, Module} ->
                    log4erl:debug("Reload ok: ~p.", [Module]),
                    ok;
                {error, Reason} ->
                    log4erl:error("Reload fail: ~p ~p.", [Module, Reason]),
                    error
            end;
        false ->
            log4erl:error("Reload fail: ~p soft_purge false.", [Module]),
            error
    end. 

%进程CPU占用排名
etop() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 5}, {lines, 20}, {sort, reductions}]) end).
%进程Mem占用排名
etop_mem() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 5}, {lines, 20}, {sort, memory}]) end).
%停止etop
etop_stop() ->
    etop:stop().

% 进程内存过高时，来一发，看看是内存泄露还是gc不过来
% 对所有process做gc
gc_all() ->
    [erlang:garbage_collect(Pid) || Pid <- processes()].

% 对MFA 执行分析，会严重减缓运行，建议只对小量业务执行
% 结果:
% fprof 结果比较详细，能够输出热点调用路径
fprof(M, F, A) ->
    fprof:start(),
    fprof:apply(M, F, A),
    fprof:profile(),
    fprof:analyse(),
    fprof:stop().

% 对整个节点内所有进程执行eprof, eprof 对线上业务有一定影响,慎用!
% 建议TimeoutSec<10s，且进程数< 1000，否则可能导致节点crash
% 结果:
% 输出每个方法实际执行时间（不会累计方法内其他mod调用执行时间）
% 只能得到mod - Fun 执行次数 执行耗时
eprof_all(TimeoutSec) ->
    eprof(processes() -- [whereis(eprof)], TimeoutSec).

eprof(Pids, TimeoutSec) ->
    eprof:start(),
    eprof:start_profiling(Pids),
    timer:sleep(TimeoutSec),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().

scheduler_usage() ->
    scheduler_usage(1000).

scheduler_usage(RunMs) ->
    erlang:system_flag(scheduler_wall_time, true),
    Ts0 = lists:sort(erlang:statistics(scheduler_wall_time)),
    timer:sleep(RunMs),
    Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)),
    erlang:system_flag(scheduler_wall_time, false),
    Cores = lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
                    {I, (A1 - A0) / (T1 - T0)} end, lists:zip(Ts0, Ts1)),
    {A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
                    {Ai + (A1 - A0), Ti + (T1 - T0)} end, {0, 0}, lists:zip(Ts0, Ts1)),
    Total = A/T,
    io:format("~p~n", [[{total, Total} | Cores]]).

% 统计下1s内调度进程数量(含义：第一个数字执行进程数量，第二个数字迁移进程数量)
scheduler_stat() ->
    scheduler_stat(1000).

scheduler_stat(RunMs) ->
    erlang:system_flag(scheduling_statistics, enable),
    Ts0 = erlang:system_info(total_scheduling_statistics),
    timer:sleep(RunMs),
    Ts1 = erlang:system_info(total_scheduling_statistics),
    erlang:system_flag(scheduling_statistics, disable),
    lists:map(fun({{Key, In0, Out0}, {Key, In1, Out1}}) ->
                {Key, In1 - In0, Out1 - Out0} end, lists:zip(Ts0, Ts1)).

%trace Mod 所有方法的调用
trace(Mod) ->
    dbg:tracer(),
    dbg:tpl(Mod, '_', []),
    dbg:p(all, c).

%trace Node上指定 Mod 所有方法的调用, 结果将输出到本地shell
trace(Node, Mod) ->
    dbg:tracer(),
    dbg:n(Node),
    dbg:tpl(Mod, '_', []),
    dbg:p(all, c).

%停止trace
trace_stop() ->
    dbg:stop_clear().

% etop 无法应对10w+ 进程节点, 下面代码就没问题了；找到可疑proc后通过pstack、message_queu_len 排查原因

proc_mem_all(SizeLimitKb) ->
  Procs = [{undefined, Pid} || Pid<- erlang:processes()],
  proc_mem(Procs, SizeLimitKb).

proc_mem(SizeLimitKb) ->
  Procs = [{Name, Pid} || {_, Name, Pid, _} <- release_handler_1:get_supervised_procs(),
                          is_process_alive(Pid)],
  proc_mem(Procs, SizeLimitKb).

proc_mem(Procs, SizeLimitKb) ->
  SizeLimit = SizeLimitKb * 1024,
  {R, Total} = lists:foldl(fun({Name, Pid}, {Acc, TotalSize}) ->
      case erlang:process_info(Pid, total_heap_size) of
          {_, Size0} ->
              Size = Size0*8,
              case Size > SizeLimit of
                  true -> {[{Name, Pid, Size} | Acc], TotalSize+Size};
                  false -> {Acc, TotalSize}
              end;
          _ -> {Acc, TotalSize}
          end
      end, {[], 0}, Procs),
  R1 = lists:keysort(3, R),
  {Total, lists:reverse(R1)}.
