-ifndef(PACKLEN_PB_H).
-define(PACKLEN_PB_H, true).
-record(packlen, {
    op = erlang:error({required, op}),
    size = erlang:error({required, size})
}).
-endif.

-ifndef(CSPLAYERLOGIN_0X01_PB_H).
-define(CSPLAYERLOGIN_0X01_PB_H, true).
-record(csplayerlogin_0x01, {
    account = erlang:error({required, account}),
    token,
    wallow
}).
-endif.

