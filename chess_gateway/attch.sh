#!/bin/bash
source ../chess.env

CHESS_GATEWAY_SVR=`cat $CONFIG_FILE | grep "{node" | awk -F"'" '{print $2}'`
DUMP_NAME="attch_"$DUMP_NAME

ERL_PARAM+=" -pa ./ebin"
ERL_PARAM+=" -pa ${LIB_PATH}"
ERL_PARAM+=" -name attch_${CHESS_GATEWAY_SVR}"
ERL_PARAM+=" -setcookie ${COOKIE}"

ERL_CRASH_DUMP=${DUMP_DIR} INSTANCE_NAME=${DUMP_NAME} erl  ${ERL_PARAM} -remsh ${CHESS_GATEWAY_SVR} 
