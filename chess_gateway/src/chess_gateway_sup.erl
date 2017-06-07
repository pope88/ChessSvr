-module(chess_gateway_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
        start_new_gate_svr/1,
        stop_new_gate_svr/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	io:format("chess_gateway_sup_begin~n"),
	ok = lib_config:mod_init(),
    Server = {chess_gateway, {chess_gateway, start_link, []},
          permanent, infinity, worker, [chess_gateway]
        },
    Svr_cli = {svr_cli, {svr_cli, start_link, []}, permanent, infinity, worker, [svr_cli]},
    PoolId = chess_gateway,
    % io:format("chess_gateway_sup PoolId:~p ~n", [PoolId]),
    % Svr = {svr, {svr, start_link, [PoolId, "192.168.136.1", 7771]},
    %   permanent, infinity, worker, [svr]
    % },
    % io:format("chess_gateway_sup_end~n"),
    Children = [Server, Svr_cli],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, { RestartStrategy, Children} }.
%% Type:game, lobby 
start_new_gate_svr(Type, GateName, Ip, Port)>
    supervisor:start_child({Type, GateName}, {svr, start_link, [GateName, Ip, Port]}, permanent, infinity, worker, [svr]}).

stop_new_gate_svr(_GateName) ->
    ok.