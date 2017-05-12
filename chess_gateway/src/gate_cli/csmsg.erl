-module(csmsg).

-export([msg_handle/1]).

msg_handle(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			log4erl:debug("Server Rece peername ~p", [lib_util:peername(Socket)]),
			msg_dispatch(Socket, Bin),
			inet:setopts(Socket, [{active, once}]),
			msg_handle(Socket);
		{tcp_closed, Socket} ->
			log4erl:debug("Server socket[~p] closed",[Socket]);
		_Err ->
			socket_closed
	end.

msg_head_check(MergeHead, Head) ->
    case Head#csmsghead.msg_id of
        ptlogin_login_req -> 
            ptauth:is_valid_login(MergeHead);
        ptlogin_name_req ->
            ptauth:is_valid(MergeHead);
        _ ->
            ActorRid = MergeHead#csmsgmergehead.rid,
            case actor:is_valid_actor_rid(ActorRid) of
                true -> 
                    ptauth:is_valid(MergeHead);
                _ -> 
                    log4erl:fatal("~p msg_head_check invalid actor(~p)", [?MODULE, {MergeHead, Head}]),
                    err_actor_session
            end
    end.

msg_dispatch(Socket, EnBin) ->
    Bin = EnBin,%%lib_crypto:decrypt(EnBin),
    #csmsgmergehead{head = MergeHead, msg = [Msg]} = gate_cli_pb:decode_csmsgmerge(Bin),
    Head = Msg#csmsg.head,
    Body = Msg#csmsg.body,
    ActorRid = MergeHead#csmsgmergehead.rid,
    log4erl:debug("Server Handle Actor ~p Msg ~p Seq ~p", [ActorRid, Head#csmsghead.msg_id, MergeHead#csmsgmergehead.seq]),
    case msg_head_check(MergeHead, Head) of
        