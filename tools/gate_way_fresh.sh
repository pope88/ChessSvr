#/bin/bash
source ../chess.env
source ../chess.lan

SKIP_FRESH_DB=0
print_error()
{
    if [ $1 -eq 0 ]; then
        echo ">> done."
    else
        echo ">> failed."
        exit $1;
    fi
}

#mysql info
MYSQL_HOST=localhost
MYSQL_USER=root
MYSQL_USER_PASSWD=""
MYSQL_OP="mysql -u$MYSQL_USER "
#CREATE DBS
recreate_tables()
{
    CUR_CDKEY_NAME="chaoscdkey"
    CDKEY_SQL_NAME="sql/chaoscdkey.sql"
    $MYSQL_OP -e "DROP DATABASE IF EXISTS $CUR_CDKEY_NAME" 
    $MYSQL_OP -e "CREATE DATABASE $CUR_CDKEY_NAME default character set utf8" 
    $MYSQL_OP $CUR_CDKEY_NAME < $CDKEY_SQL_NAME
}

###############  MAIN #################################
while getopts "D" option
do
    case "$option" in
        D)
            SKIP_FRESH_DB=1
            ;;
        :|\?)
            echo "参数设置错误"
            exit 1;
            ;;
    esac
done

echo "========== CDKEY FRESH START ============="
echo "======= [1] GEN CONFIG  ========================="
#config
CONFIG=../chess_gateway/config
echo "{node,'chess_gateway@$CHESS_GATEWAY_IP_DEV'}." > $CONFIG
#echo "{db_id,chaoscdkey}." >> $CONFIG
#echo "{db_host,\"$MYSQL_HOST\"}." >> $CONFIG
#echo "{db_port,3306}." >> $CONFIG
#echo "{db_user,\"$MYSQL_USER\"}." >> $CONFIG
#echo "{db_pass,\"\"}." >> $CONFIG
#echo "{db_name,\"chaoscdkey\"}." >> $CONFIG
#echo "{db_encode,utf8}." >> $CONFIG

echo "======= [2] RECREATING DB TABLES ================="
if [ $SKIP_FRESH_DB -eq 0 ]; then
    #recreate_tables
    print_error $?
else
    echo "skip fresh db"
fi

echo "========== CDKEY FRESH FINISH ============"
