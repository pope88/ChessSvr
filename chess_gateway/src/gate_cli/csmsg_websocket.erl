-module(csmsg_websocket).

-export([msg_ws_handle/1]).

%% socket status
-record(ws_state, { ws_socket, 
                    ws_state,  % shake_hand, ready, unfinished
                    rest_len,
                    rest_data}).


msg_ws_handle(Socket, State) ->
    receive
        {tcp,Socket,Bin} ->
            NewState = process_ws({tcp, Socket, Bin}, State);
            msg_ws_handle(Socket, NewState);
        {tcp_closed,Socket} ->
             io:format("msg handle socket closed",[Any]),
        _Err ->
            io:format("Received(2): ~p~n",[_Err]),
            socket_closed
    end.

process_ws({tcp, WebSocket, Bin}, #ws_state{ws_socket = WebSocket} = State)
   when State#ws_state.ws_state =:= shake_hand ->
        % io:format ("request header = ~p~n", [Bin]),
        HeaderList = binary:split (Bin, <<"\r\n">>, [global]),
        HeaderTupleList = [list_to_tuple(binary:split (Header, <<": ">>)) || Header <- HeaderList],
        SecWebSocketKey = proplists:get_value(<<"Sec-WebSocket-Key">>, HeaderTupleList),
        Sha = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
        Base64 = base64:encode(Sha),
        HandshakeHeader = [
            <<"HTTP/1.1 101 Switching Protocols\r\n">>,
            <<"Upgrade: websocket\r\n">>,
            <<"Connection: Upgrade\r\n">>,
            <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
            <<"\r\n">>
        ],
        ok = gen_tcp:send(WsSwocket, HandshakeHeader),
        TimerRef = timer_callback(),
        NewState = State#state{ws_state = ready},
        NewState;
process_ws({tcp, WebSocket, Bin}, #ws_state{ws_socket = WebSocket} = State) 
    when State#ws_state.ws_state =:= ready ->
        <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = Bin,
        case Len of
            126 ->
                <<PayloadLength:16, RestData/binary>> = Rest;
            127 ->
                <<PayloadLength:64, RestData/binary>> = Rest;
            _ ->
                PayloadLength = Len,
                RestData = Rest
        end,
        case PayloadLength > size(RestData) of
            true ->
                NewState = State#ws_state{ws_state = unfinished,
                                       rest_len = PayloadLength,
                                       rest_data = RestData};
            false ->
                NewState = State,
                websocket_data(WebSocket, ReceivedData)
        end,
        NewState;
process_ws({tcp, WebSocket, Bin}, #ws_state{ws_socket = WebSocket, rest_data = UnfinishedData, rest_len = UnfinishedLen} = State)
    when State#ws_state.ws_state =:= unfinished ->
        % masking length is 4
        ReceivedData = list_to_binary([UnfinishedData, Bin]),
        case UnfinishedLen + 4 - size(ReceivedData) > 0  of
            true ->
                NewState = State#ws_state{rest_data = ReceivedData};
            false ->
                NewState = State#ws_state{  ws_state = ready,
                                            rest_data = <<>>,
                                            rest_len = 0
                                        },
                websocket_data(WebSocket, ReceivedData)
        end,
        NewState.

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


websocket_data(WebSocket, Data) ->
    OriginData = websocket_unmask(Data),
    case unicode:characters_to_list(OriginData) of
        {incomplete, _, _} ->
            io:format("Server can't decode data");
        DataContent ->
            msg_dispatch(WebSocket, DataContent)
    end.

%% 由于Browser发过来的数据都是mask的,所以需要unmask
websocket_unmask(Data) ->   
    <<Masking:4/binary, MaskedData/binary>> = Data,
    websocket_unmask(MaskedData, Masking).
websocket_unmask(MaskedData, Masking) ->
    websocket_unmask(MaskedData, Masking, <<>>).
websocket_unmask(MaskedData, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    case size (MaskedData) of
        0 -> Acc;
        1 ->
            <<A:8>> = MaskedData,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = MaskedData,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = MaskedData,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = MaskedData,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask (Rest, Masking, Acc1)
    end.

build_frame (Content) ->
    Bin = unicode:characters_to_binary (Content),
    DataLength = size (Bin),
    build_frame (DataLength, Bin).


build_frame (DataLength, Bin) when DataLength =< 125 ->
    <<1:1, 0:3, 1:4, 0:1, DataLength:7, Bin/binary>>;
build_frame (DataLength, Bin) when DataLength >= 125, DataLength =< 65535 ->
    <<1:1, 0:3, 1:4, 0:1, 126:7, DataLength:16, Bin/binary>>;
build_frame (DataLength, Bin) when DataLength > 65535 ->
    <<1:1, 0:3, 1:4, 0:1, 127:7, DataLength:64, Bin/binary>>.

%%--------------------my prase----------------------
% msg_handle(Socket) ->
% 	receive
% 		{tcp, Socket, WebBin} ->
% 			log4erl:debug("Server Rece peername ~p", [lib_util:peername(Socket)]),
%             Bin = websocket_data(WebBin),
% 			msg_dispatch(Socket, Bin),
% 			inet:setopts(Socket, [{active, once}]),
% 			msg_handle(Socket);
% 		{tcp_closed, Socket} ->
% 			log4erl:debug("Server socket[~p] closed",[Socket]);
% 		_Err ->
% 			socket_closed
% 	end.

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
        true  ->
            case msg_head_check(MergeHead, Head) of
                true ->
                    Mod = Head#csmsghead.msg_type,
                    case mod_open:check(MergeHead, Head) of
                        true ->
                            case actor:is_valid_actor_rid(ActorRid) of
                                true ->
                                    msg_queue_clean(ActorRid),
                                    new_activity_actor:msg_before_handle(ActorRid),
                                    actor:update_last_msg_ts(ActorRid),
                                    msgbox:fetch(ActorRid),
                                    ok;
                                _ ->
                                    ok
                            end,
                            MsgId = Head#csmsghead.msg_id,
                            ok = Mod:recv(MsgId, {MergeHead, Socket}, Body);
                        false ->
                            ptlogin:send_login_rsp(ActorRid, {MergeHead, Socket}, err_mod_not_open)