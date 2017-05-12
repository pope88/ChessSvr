#!/bin/bash
source ../chess.env


CHESS_GATEWAY_SVR=`cat $CONFIG_FILE | grep "{node" | awk -F"'" '{print $2}'`
DUMP_NAME="${CHESS_GATEWAY_SVR}_"$DUMP_NAME	

ERL_PARAM=" +P 300000" 
ERL_PARAM+=" -pa ./ebin"
ERL_PARAM+=" -pa ${LIB_PATH}"
ERL_PARAM+=" -name ${CHESS_GATEWAY_SVR}"
ERL_PARAM+=" -setcookie ${COOKIE}"
ERL_PARAM+=" -smp enable"
ERL_PARAM+=" +K true +S 4 +c"
ERL_PARAM+=" +zdbbl 2048"
#ERL_PARAM+=" -detached"

echo "start chess gateway ..."
ERL_CRASH_DUMP=${DUMP_DIR} INSTANCE_NAME=${DUMP_NAME} erl -boot start_sasl -config ${LOG_HOME}/log ${ERL_PARAM} -s chess_gateway_app
