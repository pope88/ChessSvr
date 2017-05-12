#ifndef _GameTable_H_
#define _GameTable_H_
#include "../BaseModel/Mplayer.h"
#include "../BaseModel/Mtable.h"
#include "RoomPlayerManager.h"
#include "ZoGlobal.h"


namespace Object
{
class User;

class GameTable: public ICoreTable
{
public:
	enum
	{
		TS_EMPTY,				//空房间
		TS_WATING,			//等待状态
		TS_RACING,			//比赛状态
		TS_VIPWATING,		//有密码等待状态
		TS_VIPRACING,		//有密码比赛状态
	};
	enum
	{
		eVOTE_EVENT = 9999,
		eVOTE_PEROID = 1000 * 10,
		eKICK_EVENT = 9998,
		eKICK_PEROID = 20 * 1000,
	};

	GameTable(UInt16 tableId, RoomPlayerManager* roomPlayerManager);
	virtual ~GameTable() {}
public:
	virtual UInt8 getGameType() { return zoGlobal.getGameType();}
	virtual UInt8 getCurPlayerNum() { return curPlayerNum;}
	virtual ICorePlayer* getCorePlayer(UInt8 nChairId);
	virtual void endGame();
	virtual void startClientTimer(UInt8 cChairID, UInt32 nPeriod);
	virtual UInt32 changePlayerMoney(UInt32 userId, int nMoney);
	int	getStatus() { return 0; }
	void gmBreakGame(int nPID) {}

public:
	bool leaveTable(User *pUser);

private:
	UInt16 tableId;
	UInt8 maxPlayerNum;
	UInt8 curPlayerNum;
	UInt32 timerId;
	UInt8 masterChairId;
	UInt32 baseScore;
	UInt8 kickChairId;
	//UInt8 kickTimeId;
	std::vector<User*>  vecPlayers;   //players
	RoomPlayerManager *roomPlayerManager;
	ITable*	m_pTable;

};
}

#endif