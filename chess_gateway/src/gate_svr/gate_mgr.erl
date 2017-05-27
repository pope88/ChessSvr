-module(gate_mgr).
-export(mod_init/0).

-include(chess_gate_res.hrl).

mod_init() ->
	GateL = ets:tabtolist(RES_GATE_CONFIG),
	Fun = fun(Gate) ->
		GateName = Gate#res_gate_config.gate_name,
		GateId = Gate#res_gate_config.gate_id,
		GateType = Gate#res_gate_config.gateType,
		GateIp = Gate#res_gate_config.gate_ip,
		GatePort = Gate#res_gate_config.gate_port,
		cheess_gateway_sup:start_new_gate_svr(Type, GateName, Ip, Port)
	end,
	list:foreach(Fun, GateL),
	ok.