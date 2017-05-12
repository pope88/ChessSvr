#!/bin/bash
source ../chess.env

CHESS_GATEWAY_SVR=`cat $CONFIG_FILE | grep "{node" | awk -F"'" '{print $2}'`

ERL_PARAM=" -name ${CHESS_GATEWAY_SVR}"
ERL_PARAM+=" -c ${COOKIE}"
ERL_PARAM+=" -r"

#M F A
${ERLCALL} ${ERL_PARAM} -a "$1 $2 $3"