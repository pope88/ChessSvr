-ifndef(GATE_SVR_RECV_STATE_PB_H).
-define(GATE_SVR_RECV_STATE_PB_H, true).
-record(gate_svr_recv_state, {
    socket = erlang:error({required, socket}),
    parent = erlang:error({required, parent}),
    log_fun,
    data
}).
-endif.

-ifndef(GATE_SVR_CONN_STATE_PB_H).
-define(GATE_SVR_CONN_STATE_PB_H, true).
-record(gate_svr_conn_state, {
    recv_pid = erlang:error({required, recv_pid}),
    socket = erlang:error({required, socket}),
    log_fun,
    pool_id,
    data
}).
-endif.

-ifndef(SVR_CONN_PB_H).
-define(SVR_CONN_PB_H, true).
-record(svr_conn, {
    pool_id = erlang:error({required, pool_id}),
    conn_pid = erlang:error({required, conn_pid}),
    reconnect,
    host,
    port
}).
-endif.

-ifndef(SVR_STATE_PB_H).
-define(SVR_STATE_PB_H, true).
-record(svr_state, {
    conn_pools = erlang:error({required, conn_pools}),
    pids_pools = erlang:error({required, pids_pools})
}).
-endif.

