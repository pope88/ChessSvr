%%游戏数据库表名
-define(DB_TABLE_ACTOR, "actor").
-define(DB_TABLE_GUILD, "guid").
-define(DB_TABLE_ACTOR_MSG_BOX, "actor_msg_box").
-define(DB_TABLE_RANK, "rank").
-define(DB_TABLE_MISC, "misc").
-define(DB_TABLE_INTERACTION, "actor_interaction").
-define(DB_TABLE_FAMILYS, "familys").
-define(DB_TABLE_FAMILYPLAYER, "family_player").
-define(DB_TABLE_TESTCDKEY, "testcdkeys").
-define(DB_TABLE_FAMILY_OF_ID, "familys_of_id").
-define(DB_BLOB_SIZE, 60000).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS,        86400).
-define(ONE_YEAR_SECONDS,       31536000).
-define(COMM_RATIO, 0.0001).

-define(ETS_CONFIG, ets_config).
-define(ETS_VERSION, ets_version).

-define(HTTP_PAY_CONTENT_TYPE, "application/x-www-form-urlencoded").
-define(HTTP_PAY_TIMEOUT, 3000).

-define(ONEKEY_CHAOS_PRODUCECODE, "5010").%自由战歌
-define(ONEKEY_CHAOS_PRODUCECODE_YH, "5016").%自由战歌(硬核)

-define(MALE, 1).%男
-define(FEMALE, 2).%女

-define(OS_TYPE_MIX, 0).%0:混服
-define(OS_TYPE_ANDROID, 1).%1:安卓
-define(OS_TYPE_IOS_JAILBREAK, 2).%2:IOS越狱
-define(OS_TYPE_IOS, 3).%3:IOS正版
-define(OS_TYPE_IOS_AUDIT, 4).%4:苹果审核