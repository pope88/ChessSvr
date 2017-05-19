-module(csmsg_websocket).

-export([msg_ws_handle/1]).

%% socket status
-record(ws_state, {ws_socket, 
                           ws_state,  % shake_hand, ready, unfinished
                           timer_ref}).


msg_ws_handle(Socket, State) ->
    receive
        {tcp,Socket,Bin} ->
            NewState = process_ws({tcp, Socket, Bin}, State);
            msg_ws_handle(Socket, NewState);
        Any ->
            io:format("Received(2): ~p~n",[Any]),
            msg_ws_handle(Socket, State)
    end.

process_ws({tcp, WebSocket, Bin}, #ws_state{ws_socket = WebSocket} = State)
    when State#state.ws_state =:= shake_hand




handshake(Bin) ->
    Key = list_to_binary(lists:last(string:tokens(hd(lists:filter(fun(S) -> lists:prefix("Sec-WebSocket-Key:", S) end, string:tokens(binary_to_list(Bin), "\r\n"))), ": "))),
    Accept = base64:encode(crypto:hash(sha,<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
    %%{ok, Write_log} = file:open("D:/Erlang/erlang_log",[append]),
    %%io:format(Write_log, "Accept: ~s~n", [Accept]),
    [
     "HTTP/1.1 101 Switching Protocols\r\n",
    "connection: Upgrade\r\n",
    "upgrade: websocket\r\n",
    "Blog: http://blog.csdn.net/jom_ch\r\n",
    "sec-websocket-accept: ", Accept, "\r\n",
    "\r\n"
    ].

%% 仅处理长度为125以内的文本消息
websocket_data(Data) when is_list(Data) ->
    websocket_data(list_to_binary(Data));

websocket_data(<< 1:1, 0:3, 1:4, 1:1, Len:7, MaskKey:32, Rest/bits >>) when Len < 126 ->
    <<End:Len/binary, _/bits>> = Rest,
    Text = websocket_unmask(End, MaskKey, <<>>),
    Text;

websocket_data(_) ->
    <<>>. 

%% 由于Browser发过来的数据都是mask的,所以需要unmask
websocket_unmask(<<>>, _, Unmasked) ->
    Unmasked;

websocket_unmask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
    T = O bxor MaskKey,
    websocket_unmask(Rest, MaskKey, << Acc/binary, T:32 >>);

websocket_unmask(<< O:24 >>, MaskKey, Acc) ->
    << MaskKey2:24, _:8 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:24 >>;

websocket_unmask(<< O:16 >>, MaskKey, Acc) ->
    << MaskKey2:16, _:16 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:16 >>;

websocket_unmask(<< O:8 >>, MaskKey, Acc) ->
    << MaskKey2:8, _:24 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:8 >>.



%%--------------------my prase----------------------
msg_handle(Socket) ->
	receive
		{tcp, Socket, WebBin} ->
			log4erl:debug("Server Rece peername ~p", [lib_util:peername(Socket)]),
            Bin = websocket_data(WebBin),
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
        