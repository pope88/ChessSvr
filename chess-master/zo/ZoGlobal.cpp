#include "Config.h"
#include "ZoGlobal.h"

ZoGlobal zoGlobal;
void ZoGlobal::init()
{
	_serverVer = "1.1.0";
	_roomNum = 6;
	_tableNum = 100;
	_playerNum = 4;
	_readyMode = 0;
	_gameType = 0;
	_gameMoneyLimit = 10000;

}