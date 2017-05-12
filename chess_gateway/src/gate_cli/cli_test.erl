-module(cli_test).
-include("game_pb.hrl").
-export ([test1/0]).


test1() ->
	Pkg = #csplayerlogin_0x01{
		account = "xiaohui",
		token = "008" 
	},
	Bin = game_pb:encode_csplayerlogin_0x01(Pkg),
	svr:call_execute(self(), Bin).
