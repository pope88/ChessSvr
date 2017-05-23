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
    %%decrypt
    Bin = lib_crypto:decrypt(EnBin),
    %%pb
    #csmsgmerge{head=MergeHead, msg=[Msg]} = cs_pb:decode_csmsgmerge(Bin),
    Head = Msg#csmsg.head,
    Body = Msg#csmsg.body,
    ActorRid = MergeHead#csmsgmergehead.rid,
    log4erl:debug("Server Handle Actor ~p Msg ~p Seq ~p", [ActorRid, Head#csmsghead.msg_id, MergeHead#csmsgmergehead.seq]),
    %上行先不允许并包%
    % log4erl:debug("Server dispatch msg Mhead:~p, Head:~p, Body:~p", [MergeHead, Head, Body]),
    case switch:is_on(in_service) of
        true ->
            case msg_head_check(MergeHead, Head) of
                true ->
                    Mod = Head#csmsghead.msg_type,
                    case mod_open:check(ActorRid, Mod) of
                        true ->
                            case actor:is_valid_actor_rid(ActorRid) of
                                true -> 
                                    %%clean actor msg queue
                                    msg_queue_clean(ActorRid),
                                    %%活动开启检测
                                    new_activity_actor:msg_before_handle(ActorRid),
                                    %%更新玩家最近一次上行消息时间戳
                                    actor:update_last_msg_ts(ActorRid),
                                    %%取信箱交互信息
                                    msgbox:fetch(ActorRid),
                                    ok;
                                _ ->
                                    ok
                            end,
                            %%消息处理
                            MsgId = Head#csmsghead.msg_id,
                            ok = Mod:recv(MsgId, {MergeHead, Socket}, Body);
                        false ->
                            ptlogin:send_login_rsp(ActorRid, {MergeHead, Socket}, err_mod_not_open)
                    end;
                last_msg ->
                    %%还是上次的消息，则把缓存的最后一个包发过去
                    case actor_online:get_last_msg(ActorRid) of
                        undefined ->
                            log4erl:error("msg invalid reason:last_msg not get, MergeHead:~p, Head:~p", [MergeHead, Head]);
                        LastEnBin ->
                            gen_tcp:send(Socket, LastEnBin),
                            log4erl:debug("Server Resend finish ts ~p~n",[lib_util:timenow()])
                    end;           
                Err ->
                    ptlogin:send_login_rsp(ActorRid, {MergeHead, Socket}, Err),
                    log4erl:error("msg invalid reason:~p, MergeHead:~p, Head:~p", [Err, MergeHead, Head])
            end;
        _ ->
            ptlogin:send_login_rsp(ActorRid, {MergeHead, Socket}, err_server_not_in_service)
    end.

msg_send({MHead, Socket}, Msg, Type) ->
    ActorRid = MHead#csmsgmergehead.rid,
    case actor:is_valid_actor_rid(ActorRid) of
        true ->
            SCMsg = Msg;
        _ ->
            % log4erl:fatal("~p msg_send invalid actor ~p",[?MODULE, ActorRid]),
            SCMsg = Msg
    end,


    Bin = cs_pb:encode_csmsgmerge(SCMsg),
    % io:format("Server Send Bin ~p~n", [lib_util:to_hex(iolist_to_binary(Bin))]),
    % log4erl:debug("Server send msg Socket[~p], SCMsg [~p]", [Socket, SCMsg]),
    %%encrypt
  %%  EnBin = lib_crypto:encrypt(Bin),
    % io:format("Send EnBin :~p~n", [EnBin]),

    %缓存最后一个消息,seq+1(login,loginerr,logout除外)
    %logout清理ets_online_actor
    %login 产生session和seq 
    if
        Type == login orelse Type == loginerr ->
            ok;
        Type == logout ->
            actor:delete_actor(ActorRid);
        true ->
            actor:save_last_msg(ActorRid, EnBin),
            actor:update_seq(ActorRid)
    end,
    %发送
    Frame = build_frame(EnBin),
    gen_tcp:send(Socket, Frame),
    log4erl:debug("Server send finish ts ~p~n",[lib_util:timenow()]),
    ok.