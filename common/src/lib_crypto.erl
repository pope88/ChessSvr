%%%-----------------------------------
%%% @Module  : lib_crypto
%%% @Author  : Blueice
%%% @Email   : Blueice Mao<blueice@maplegame.cn>
%%% @Created : 2015.04.27
%%% @Description: 协议加解密
%%%-----------------------------------
-module(lib_crypto).
-export([
        init/0
        ,encrypt/1
        ,decrypt/1
    ]).

%%crypto:rand_bytes(16).
%% 128 bit / 16 bytes
%% same as client
-define(CRYPTO_KEY, <<16,32,50,24,178,148,98,40,190,134,123,233,171,36,195,21>>).
-define(CRYPTO_IVEC, <<203,41,52,120,147,240,43,233,83,108,40,68,122,2,90,42>>).

init() ->
    ssl:start().%for https, crypto

% block_encrypt(Type, Key, Ivec, PlainText) -> CipherText
% Types:
% Type = block_cipher() 
% Key = block_key() 
% PlainText = iodata() 
% IVec = CipherText = binary()
encrypt(PlainText) ->
    case switch:is_on(crypto) of
        true -> crypto:block_encrypt(aes_cfb128, ?CRYPTO_KEY, ?CRYPTO_IVEC, PlainText);
        _ -> PlainText
    end.

% block_decrypt(Type, Key, Ivec, CipherText) -> PlainText
% Types:=
% Type = block_cipher() 
% Key = block_key() 
% PlainText = iodata() 
% IVec = CipherText = binary()
decrypt(CipherText) ->
    case switch:is_on(crypto) of
        true -> crypto:block_decrypt(aes_cfb128, ?CRYPTO_KEY, ?CRYPTO_IVEC, CipherText);
        _ -> CipherText
    end.