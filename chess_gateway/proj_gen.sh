#!/bin/bash
source ../chess.env

echo "GEN gate_way PROJ ..."
FILE="proj"
FIRST=0
EBIN_HOME_LIST=(${LIB_PROTO_HOME} ${LIB_COMM_HOME} ebin)

echo "[" > $FILE

for EBIN in ${EBIN_HOME_LIST[@]}
do
    MODULELIST=`ls -l ${EBIN}/*.beam | awk -F'/' '{print $NF}' | awk -F'.' '{print $1}'`
    for MODULE in ${MODULELIST}
    do
        if [ $FIRST -eq 0 ];then 
            echo "${MODULE}" >> $FILE
            FIRST=1
        else
            echo ",${MODULE}" >> $FILE
        fi
    done
done

echo "]." >> $FILE


